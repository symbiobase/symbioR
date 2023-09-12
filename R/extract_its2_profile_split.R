#' Function to extract seqs from symportal output
#'
#' @param folder location of the root Symportal output
#' @param print.seqs prints unique seqs output
#' @export
#' @return A data.frame of seq.ID (columns) and sample.ID (rows) with either relative or absolute abundance of sequences.

extract_its2_profile_split <- function(folder, print.seqs = FALSE) {
  its2.profile.tmp <- symportalfunctions::extract_its2_profile(sym_folder)

  process_its2 <- function(col_name, df) {
    df %>%
      dplyr::select(sample.ID, all_of(col_name)) %>%
      tidyr::separate_rows(all_of(col_name), sep = "[-/]") %>%
      dplyr::group_by(sample.ID) %>%
      dplyr::mutate(new_col = paste0(col_name, row_number())) %>%
      tidyr::pivot_wider(names_from = new_col, values_from = all_of(col_name))
  }

  # Column names to loop over
  col_names <- colnames(select(its2.profile.tmp, -sample.ID))

  # Loop over column names, applying function

  result <- purrr::map(col_names, ~ process_its2(.x, its2.profile.tmp))

  result_joined <- purrr::reduce(result, function(x, y) {
    dplyr::left_join(x, y, by = "sample.ID")
  }) %>%
    tibble::column_to_rownames("sample.ID")

  return(result_joined)

  if (print.seqs == TRUE) {
    cat("Excluded samples \n")
    cat(paste0(" - ", excluded_sample_names, collapse = "\n"), " \n")

    cat(paste0(" - ", (sort(unique(unlist(result_joined[2:ncol(result_joined)])))), collapse = "\n"), " \n")


  }
}
