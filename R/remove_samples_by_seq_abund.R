#' Function to plot sequences from symportal output
#'
#'
#'
#'
#' @param folder location of the root Symportal output
#' @param type returns either "relative" or "absolute"
#' @export

remove_samples_by_seq_abund <- function(input, value, ...){

  input |> filter(abundance > value)

}
