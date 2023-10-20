#' Function to extract sequences from symportal output
#'
#'
#' same as extract_seqs but in long format
#'
#' @param folder location of the root Symportal output
#' @param metadata name of metadata file (e.g. "metadata_20230120T102936.csv") within the Symportal output folder
#' @param factors list of columns for the metadata
#' @export

import_profiles_abs <- function(folder, metadata, factors){

  file_list <- list.files(path = folder, pattern = "profiles.absolute.abund_and_meta.txt", include.dirs = TRUE, recursive = TRUE)
  its_profile <- read.delim(paste0(folder, "/", file_list), check.names = FALSE, header = F) %>%
    janitor::row_to_names(row_number = 7) %>%
    dplyr::rename("sample_name" = 2)

  metadf <- read.csv(paste0(folder, "/", metadata)) |>
    dplyr::select(all_of(factors))

  #metadf$sample_names %in% full_data$sample_name # add STOP here

  its_profile <- dplyr::left_join(metadf, its_profile, by="sample_name")


  return(its_profile)


}
