
#' Function to remove samples by name
#'
#'
#'
#'
#' @param input location of the root Symportal output
#' @param genus drop columns summing to zero
#' @export

calculate_postQC_postmed_rel <- function(input){

  input |> group_by(sample_name) %>%
    mutate(total_abundance = sum(abundance)) %>%
    mutate(relative_abundance = abundance / total_abundance * 100) %>%
    ungroup()


}

