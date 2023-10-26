#' Function to extract sequences from symportal output
#'
#'
#' same as extract_seqs but in long format
#'
#' @param folder location of the root Symportal output
#' @param type returns either "relative" or "absolute"
#' @param clade filter by single "C" or multiple clades c("C", "D") to filter sequences by
#' @param drop_samples drop samples by named vector, e.g. c("H00B07", "H00B06"), or by one or more partial matches, e.g. c("07","B06")
#' @param keep_samples drop samples by named vector, e.g. c("H00B07", "H00B06"), or by one or more partial matches, e.g. c("07","B06")
#' @param drop_seqs drop seqs by named vector, e.g. c("X2777817_G", "X2777816_G"), or by one or more partial matches, e.g. c("X2","OT")
#' @param threshold Set threshold to remove samples if less than the threshold (defaults to 1000)
#' @param silent defaults to TRUE, if FALSE then prints a list of removed sample names
#' @export
#' @return A data.frame of seq.ID (columns) and sample.ID (rows) with either relative or absolute abundance of sequences.


extract_seqs_long <-  function(folder, type = "relative",
                               onlyProfiles=FALSE, threshold = 1, remove_zero=FALSE,
                               drop_samples = NULL, keep_samples=NULL, drop_seqs = NULL) {

  # read absolute abundances:
  file_list <- list.files(path = folder, pattern = "seqs.absolute.abund_and_meta.txt", include.dirs = TRUE, recursive = TRUE)
  full_data <- read.delim(paste0(folder, "/", file_list)) %>%
    dplyr::select(sample_name, 40:ncol(.)) %>% # select just the symbiodinium columns
    dplyr::slice(-dplyr::n()) # remove the last row, summary data

  # read profiles
  its2_profile <- extract_its2_profile(folder)

  ### absolute abundance
  absolute <- full_data


  # if (!is.null(metadata)) {
  #
  #   metadf <- read.csv(paste0(folder, "/", metadata)) |>
  #     dplyr::select(sample_name, all_of(factors)) |>
  #     rename(sample_name)
  #
  #
  #   absolute <- left_join(absolute, metadf, by="sample.ID")
  #
  # }

  # #columns matching "genus"
  # if (!is.null(host_genus)) {
  #
  #   metadf_genus <- metadf |> filter(hostgenus %in% hostgenus)
  #   print(head(metadf_genus))
  #       absolute <- absolute %>%
  #     filter(sample_name %in% its2_profile$sample.ID)
  # }

  #columns matching "onlyProfiles"
  if (is.null(onlyProfiles)) {
    absolute <- absolute %>%
      dplyr::filter(sample_name %in% its2_profile$sample.ID)
  }

  # Drop rows by sample name
  if (!is.null(keep_samples)) {
    absolute <- absolute %>% as.data.frame() %>%
      dplyr::filter(grepl(paste(keep_samples, collapse = "|"), sample_name))
  }

  # Keep rows only with the sample names
  if (!is.null(drop_samples)) {
    absolute <- absolute %>% as.data.frame() %>%
      dplyr::filter(!grepl(paste(drop_samples, collapse = "|"), sample_name))
  }

  # Drop columns by seq name partial match
  # if (!is.null(drop_seqs)) {
  #   absolute <- absolute %>%
  #     dplyr::select(sample_name, !matches(paste(drop_seqs, collapse = "|"), ignore.case = FALSE))
  # }

  # Drop columns by seq name full match
  if (!is.null(drop_seqs)) {
    # Add "X" to strings that start with a number
    drop_seqs <- ifelse(grepl("^\\d", drop_seqs), paste("X", drop_seqs, sep = ""), drop_seqs)
    absolute <- absolute %>%
      dplyr::select(-one_of(drop_seqs))
  }

  # threshold
  drop_thresh <- data.frame(
    sample_names=absolute$sample_name,
    rowsums=rowSums(absolute[,2:ncol(absolute)])) |>
    dplyr::filter(rowsums > threshold)

  absolute <- absolute %>%
    dplyr::filter(sample_name %in% drop_thresh$sample_names)

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


  absolute <- absolute %>%
    tibble::rownames_to_column("sample.ID") %>%
    tidyr::pivot_longer(cols = -sample.ID, names_to = "seq.ID", values_to = "abundance") %>%
    dplyr::filter(abundance>0.0001) %>%
    dplyr::mutate(seq.ID = stringr::str_replace(seq.ID, "^X", "")) %>% # drop X if first in seq.ID
    dplyr::group_by(sample.ID) |>
    dplyr::arrange(desc(abundance))

  relative <- relative %>%
    tibble::rownames_to_column("sample.ID") %>%
    tidyr::pivot_longer(cols = -sample.ID, names_to = "seq.ID", values_to = "abundance") %>%
    dplyr::filter(abundance>0.0001) %>%
    dplyr::mutate(seq.ID = stringr::str_replace(seq.ID, "^X", "")) %>% # drop X if first in seq.ID
    dplyr::arrange(sample.ID, desc(abundance))

  if (remove_zero == TRUE) {
    absolute <- absolute %>% dplyr::filter(abundance != 0)
    relative <- relative %>% dplyr::filter(abundance != 0)
  }

  # return functions:
  if (type == "absolute") {
    return(absolute)
  } else if (type == "relative") {
    return(relative)
  }
}
