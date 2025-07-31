#' Function to extract sequences from symportal output
#'
#'
#' extract sequences from symportal output
#'
#' @param folder location of the root Symportal output
#' @param type returns either "relative" or "absolute"
#' @param metadata location of a metadata file in csv, must contain a column called sample_name with matches
#' @param factors option to include a vector list of column names, e.g: c("location", "latitude") to not import every column in a metadata file, if left NULL then will import all columns
#' @param onlyprofile TRUE/FALSE return only profiles
#' @param arrange arrange by seq_id alphanumerically, TRUE by default
#' @param silent print summary statistics, FALSE by default
#' @param janitor FALSE by default, set to TRUE to use janitor clean_names on inputs
#' @param names print names list, FALSE by default
#' @export
#' @return A data.frame of seq_id (columns) and sample_name (rows) with either relative or absolute abundance of sequences.


extract_seqs <-  function(folder, metadata=NULL, type = "absolute",
                          factors=NULL, onlyprofile=FALSE, names=FALSE,
                          arrange=TRUE, seed=NULL, janitor=FALSE, silent=TRUE, ...) {

  # read absolute abundances:
  file_list <- list.files(path = folder, pattern = "seqs.absolute.abund_and_meta.txt", include.dirs = TRUE, recursive = TRUE)
  full_data <- read.delim(paste0(folder, "/", file_list)) %>%
    dplyr::select(sample_name, 40:ncol(.)) %>% # select just the symbiodinium columns
    dplyr::slice(-dplyr::n()) # remove the last row, summary data

  # read profiles
  its2_profile <- extract_its2_profile(folder)


  ### absolute abundance
  absolute <- full_data

  if(silent==FALSE){
  cat("Number of its2_profile samples = ", length(unique(its2_profile$sample_name)), "\n")
  cat("Number of input samples = ", length(unique(absolute$sample_name)), "\n")
  }


  #columns matching "onlyprofile"
  if (isTRUE(onlyprofile)) {

    if(silent==FALSE){

      drop_samples <- absolute %>%
        dplyr::filter(!full_data$sample_name %in% its2_profile$sample_name) |>
        dplyr::pull(sample_name)



    cat("onlyprofile=TRUE\n")
    cat("its2_profile sample_names removed:\n")
    print(data.frame(drop_samples))#), sep=" | ")   # show only samples not in onlyprofile
    }

    absolute <- absolute %>%
      dplyr::filter(full_data$sample_name %in% its2_profile$sample_name)

  }

  # tidy
  absolute <- absolute %>%
    tibble::column_to_rownames("sample_name") %>% # sample_name column to rowname
    dplyr::filter(rowSums(dplyr::select(., dplyr::where(is.numeric))) != 0) %>% # drop zero sum rows
    dplyr::select(dplyr::where(~ sum(. != 0) > 0)) %>% # drop zero sum columns
    dplyr::select(dplyr::where(~ any(!is.na(.)))) # drop blank columns


  relative <- absolute %>%
    dplyr::mutate(row_sum = rowSums(dplyr::select(., dplyr::where(is.numeric)))) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ . / row_sum)) %>%
    dplyr::select(-row_sum)

  # pivot
  absolute <- absolute %>%
    tibble::rownames_to_column("sample_name") %>%
    tidyr::pivot_longer(cols = -sample_name, names_to = "seq_id", values_to = "abundance") %>%
    dplyr::filter(abundance>0.0001) %>%
    dplyr::mutate(seq_id = stringr::str_replace(seq_id, "^X", ""))  # drop X if first in seq_id


  relative <- relative %>%
    tibble::rownames_to_column("sample_name") %>%
    tidyr::pivot_longer(cols = -sample_name, names_to = "seq_id", values_to = "abundance") %>%
    dplyr::filter(abundance>0.0001) %>%
    dplyr::mutate(seq_id = stringr::str_replace(seq_id, "^X", ""))# drop X if first in seq_id

  if (isTRUE(arrange)) {
      absolute <- absolute %>% dplyr::group_by(sample_name) %>% dplyr::arrange(desc(abundance))
      relative <- relative %>% dplyr::arrange(sample_name, desc(abundance))
  }


  if (isTRUE(janitor)) {
    metadata <- janitor::clean_names(metadata)
  }

  if (!is.null(metadata)) {
    absolute <- left_join(absolute, metadata, by="sample_name")
  }


  # return functions:
  if (type == "absolute") {
    if(silent==FALSE){
      cat("Number of output samples = ", length(unique(absolute$sample_name)), "\n")
    }
    if(names==TRUE){
      print(unique(absolute$sample_name))
    }

    return(absolute)
  } else if (type == "relative") {
    if(silent==FALSE){
      cat("Number of input samples = ", length(unique(relative$sample_name)), "\n")
    }
    if(names==TRUE){
      print(unique(absolute$sample_name))
    }
    return(relative)
  }
}
