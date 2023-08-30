#' Function to extract seqs from symportal output in long format
#'
#' @param folder location of the root Symportal output
#' @param print.seqs prints unique seqs output
#' @export
#' @return A data.frame of seq.ID (columns) and sample.ID (rows) with either relative or absolute abundance of sequences.
#' @examples
#'
#' tmp <- extract_its2_profile_seqs(folder = "/Users/rof011/symbiodinium/20220919T102058_esampayo", print.seqs = TRUE)
#'
#' tmp <- extract_its2_profile_seqs(folder = "/Users/rof011/symbiodinium/20220919T102058_esampayo", print.seqs = TRUE)
#'
extract_its2_profile_seqs <- function(folder, print.seqs = FALSE) {
  its2.profile.tmp <- extract_its2_profile(sym_folder)

  process_its2 <- function(col_name, df) {
    df %>%
      select(sample.ID, all_of(col_name)) %>%
      separate_rows(all_of(col_name), sep = "[-/]") %>%
      group_by(sample.ID) %>%
      mutate(new_col = paste0(col_name, row_number())) %>%
      pivot_wider(names_from = new_col, values_from = all_of(col_name))
  }

  # Column names to loop over
  col_names <- colnames(select(its2.profile.tmp, -sample.ID))

  # Loop over column names, applying function

  result <- map(col_names, ~ process_its2(.x, its2.profile.tmp))

  result_joined <- reduce(result, function(x, y) {
    left_join(x, y, by = "sample.ID")
  })


  if (print.seqs == TRUE) {
    cat("Excluded samples \n")
    cat(paste0(" - ", excluded_sample_names, collapse = "\n"), " \n")

    cat(paste0(" - ", (sort(unique(unlist(result_joined[2:ncol(result_joined)])))), collapse = "\n"), " \n")
  }

  result_joined_long <- result_joined %>% pivot_longer(-sample.ID, names_to="ITS.profile.match", values_to="seq.ID") %>% select(-ITS.profile.match) %>% na.omit() %>% unique()

  return(result_joined_long)

}
