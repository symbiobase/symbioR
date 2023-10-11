#' Function to extract sequences from symportal output
#'
#'
#' same as extract_seqs but in long format
#'
#' @param folder location of the root Symportal output
#' @param type returns either "relative" or "absolute"
#' @export

import_profiles_rel <- function(folder){

  file_list <- list.files(path = folder, pattern = "profiles.relative.abund_and_meta.txt", include.dirs = TRUE, recursive = TRUE)
  its_profile <- read.delim(paste0(folder, "/", file_list), check.names = FALSE, header = F) %>%
    janitor::row_to_names(row_number = 7) %>%
    dplyr::rename("sample.name" = 2)

  return(its_profile)


}
