#' Function to extract metadata from symportal output
#'
#' @param folder location of the root Symportal output
#' @export
#' @return A data.frame of seq.ID (columns) and sample.ID (rows) with either relative or absolute abundance of sequences.
#' @examples
#'
#' tmp <- extract_metadata(folder="/Users/rof011/symbiodinium/20220919T102058_esampayo")


extract_metadata <- function(folder, type = "relative", clade = LETTERS[1:10], cutoff=1000, silent=TRUE) {

  file_list <- list.files(path = folder, pattern = "seqs.absolute.abund_and_meta.txt", include.dirs = TRUE, recursive = TRUE)

  metadata <- read.delim(paste0(folder, "/", file_list)) %>%
    dplyr::select(sample_name, 1:39)
  return(metadata)

}
