#' Function to extract sequences from symportal output
#'
#' @param folder location of the root Symportal output
#' @param type returns either "relative" or "absolute"
#' @param clade filter by single "C" or multiple clades c("C", "D") to filter sequences by
#' @param cutoff Set threshold to remove samples if less than the cutoff (defaults to 1000)
#' @param silent defaults to TRUE, if FALSE then prints a list of removed sample names
#' @export
#' @return A data.frame of seq.ID (columns) and sample.ID (rows) with either relative or absolute abundance of sequences.
#' @examples
#' ## not run:
#' # tmp <- extract_seqs(folder="/Users/rof011/symbiodinium/20220919T102058_esampayo", type="absolute", clade=c("C", "D"))
#
#' # tmp2 <- extract_seqs(folder="/Users/rof011/symbiodinium/20220919T102058_esampayo", type="relative", clade="C", cutoff=100)


extract_seqs <- function(folder, type = "relative", clade = LETTERS[1:10], cutoff=1000, silent=TRUE) {

  file_list <- list.files(path = folder, pattern = "seqs.absolute.abund_and_meta.txt", include.dirs = TRUE, recursive = TRUE)

  absolute <- read.delim(paste0(folder, "/", file_list)) %>%
    dplyr::select(sample_name, 40:ncol(.)) %>% # select just the symbiodinium columns
    tibble::column_to_rownames("sample_name") %>% # sample_name column to rowname
    dplyr::slice(-n()) %>% # remove the last row, summary data
    dplyr::filter(rowSums(select(., where(is.numeric))) > as.numeric(cutoff)) %>% # remove samples where <1000 sequences
    dplyr::select(dplyr::matches(clade, ignore.case = FALSE)) %>% # keep only columns matching "clade"
    dplyr::filter(rowSums(dplyr::select(., dplyr::where(is.numeric))) != 0) %>% # drop zero sum rows
    dplyr::select(dplyr::where(~ sum(. != 0) > 0)) %>% # drop zero sum columns
    dplyr::select(dplyr::where(~ any(!is.na(.))))  # drop blank columns

  excluded_samples <- read.delim(paste0(folder, "/", file_list)) %>%
    dplyr::select(sample_name, 40:ncol(.)) %>% # select just the symbiodinium columns
    tibble::column_to_rownames("sample_name") %>% # sample_name column to rowname
    dplyr::slice(-n()) %>% # remove the last row, summary data
    dplyr::select(dplyr::matches(clade, ignore.case = FALSE)) %>% # keep only columns matching "clade"
    dplyr::filter(rowSums(select(., where(is.numeric))) < as.numeric(cutoff)) # remove samples where <1000 sequences

  if (silent==FALSE){
    cat("Excluded samples \n")
    cat(paste0(" - ", unique(rownames(excluded_samples)), collapse="\n"), " \n")
  }

  relative <- read.delim(paste0(folder, "/", file_list)) %>%
    dplyr::select(sample_name, 40:ncol(.)) %>% # select just the symbiodinium columns
    tibble::column_to_rownames("sample_name") %>% # sample_name column to rowname
    dplyr::slice(-n()) %>% # remove the last row, summary data
    dplyr::filter(rowSums(select(., where(is.numeric))) > as.numeric(cutoff)) %>% # remove samples where <1000 sequences
    dplyr::select(dplyr::matches(clade, ignore.case = FALSE)) %>% # keep only columns matching "clade"
    dplyr::filter(rowSums(dplyr::select(., dplyr::where(is.numeric))) != 0) %>% # drop zero sum rows
    dplyr::select(dplyr::where(~ sum(. != 0) > 0)) %>% # drop zero sum columns
    dplyr::select(dplyr::where(~ any(!is.na(.)))) %>% # drop blank columns
    dplyr::mutate(row_sum = rowSums(select(., dplyr::where(is.numeric)))) %>%
    dplyr::mutate(across(dplyr::where(is.numeric), ~ . / row_sum)) %>%
    dplyr::select(-row_sum)

  if (type == "absolute") {
    return(absolute)
  } else if (type == "relative") {
    return(relative)
  }
}
