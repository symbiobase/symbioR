
#' Function to remove samples by name
#'
#'
#'
#'
#' @param input location of the root Symportal output
#' @param genus drop columns summing to zero
#' @export

calculate_qc_postmed_bygenus <- function(input, genus){

      input |> filter(str_detect(seq.ID, genus))


}

