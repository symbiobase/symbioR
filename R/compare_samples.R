#' compare_samples
#'
#'
#'
#'
#' @param input1 first dataframe to compare
#' @param input2 first dataframe to compare against the first
#' @export
#' @return A data.frame of seq.ID (columns) and sample.ID (rows) with either relative or absolute abundance of sequences.


compare_samples <- function(input1, input2, silent=FALSE){

  cat(paste0("sample_names in input1 that are not in input2 \n"))
  output <- setdiff(unique(input1$sample_name), unique(input2$sample_name))

  return(output)

}
