#' Function to extract sequences from symportal output
#'
#' @param folder location of the root Symportal output
#' @param type returns either "relative" or "absolute"
#' @param clade filter by single "C" or multiple clades c("C", "D") to filter sequences by
#' @param drop_samples drop samples by named vector, e.g. c("H00B07", "H00B06"), or by one or more partial matches, e.g. c("07","B06")
#' @param drop_seqs drop seqs by named vector, e.g. c("X2777817_G", "X2777816_G"), or by one or more partial matches, e.g. c("X2","OT")
#' @param threshold Set threshold to remove samples if less than the threshold (defaults to 1000)
#' @param silent defaults to TRUE, if FALSE then prints a list of removed sample names
#' @export
#' @return A data.frame of seq.ID (columns) and sample.ID (rows) with either relative or absolute abundance of sequences.

extract_seqs <- function(folder, type = "relative", clade = LETTERS[1:10], threshold = 1000, drop_samples = NULL, drop_seqs = NULL, silent = TRUE) {
  # get matches with dropped samples:
  drop_samples_str <- ifelse(length(drop_samples) == 0, "NA_character_", paste(drop_samples, collapse = "|"))
  drop_seqs_str <- ifelse(length(drop_seqs) == 0, "NA_character_", paste(drop_seqs, collapse = "|"))

  # read files
  file_list <- list.files(path = folder, pattern = "seqs.absolute.abund_and_meta.txt", include.dirs = TRUE, recursive = TRUE)

  full_data <- read.delim(paste0(folder, "/", file_list)) %>%
    dplyr::select(sample_name, 40:ncol(.)) %>% # select just the symbiodinium columns
    dplyr::slice(-dplyr::n()) # remove the last row, summary data


  absolute <- full_data %>%
    dplyr::select(sample_name, dplyr::matches(clade, ignore.case = FALSE)) %>% # keep only columns matching "clade"
    dplyr::filter(dplyr::case_when(
      drop_samples_str == "NA_character_" ~ TRUE, # drop rows by sample name
      TRUE ~ !stringi::stri_detect_regex(sample_name, drop_samples_str, opts_regex = stringi::stri_opts_regex(case_insensitive = FALSE))
    )) %>%
    dplyr::select(-matches(drop_seqs_str, ignore.case = FALSE)) %>% # drop columns by seq name
    # dplyr::filter(dplyr::case_when(drop_samples_str == "NA_character_" ~ TRUE, TRUE ~ !str_detect(sample_name, drop_samples_str))) %>% # drop rows by sample name
    # dplyr::select(-matches(drop_seqs_str)) %>% # drop columns by seq name
    dplyr::filter(rowSums(dplyr::select(., dplyr::where(is.numeric))) > as.numeric(threshold)) %>% # remove samples where <1000 sequences

    tibble::column_to_rownames("sample_name") %>% # sample_name column to rowname
    dplyr::filter(rowSums(dplyr::select(., dplyr::where(is.numeric))) != 0) %>% # drop zero sum rows
    dplyr::select(dplyr::where(~ sum(. != 0) > 0)) %>% # drop zero sum columns
    dplyr::select(dplyr::where(~ any(!is.na(.)))) # drop blank columns

  excluded_samples <- !full_data$sample_name %in% row.names(absolute)
  excluded_sample_names <- full_data$sample_name[excluded_samples]

  if (silent == FALSE) {
    cat("Excluded samples \n")
    cat(paste0(" - ", excluded_sample_names, collapse = "\n"), " \n")
  }

  relative <- absolute %>%
    dplyr::mutate(row_sum = rowSums(dplyr::select(., dplyr::where(is.numeric)))) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ . / row_sum)) %>%
    dplyr::select(-row_sum)

  ########################
  ## add checks:
  if (mean(rowSums(relative) / 1) != 1) {
    stop("STOP: mean of row sums is not equal to 1.")
  }

  if (any(colSums(absolute) == 0) || any(is.na(colSums(absolute)))) {
    stop("STOP: code error, column sums contain 0 or NA.")
  }

  if (any(colSums(relative) == 0) || any(is.na(colSums(relative)))) {
    stop("STOP: code error, column sums contain 0 or NA.")
  }
  ########################

  # return functions:
  if (type == "absolute") {
    return(absolute)
  } else if (type == "relative") {
    return(relative)
  }
}
