#' Function to extract its2 names from Symportal
#'
#' @param folder location of the root Symportal output
#' @export
#' @return its_profile A data.frame of ITS profiles for each sample.ID

#'
extract_its2_names <- function(folder) {

  file_list <- list.files(path = folder, pattern = "profiles.relative.abund_and_meta.txt", include.dirs = TRUE, recursive = TRUE)

  its2_names <- read.delim(paste0(folder, "/", file_list), check.names = FALSE, header = F) %>%
    janitor::row_to_names(row_number = 1) %>%
    dplyr::rename("sample_name" = 2) %>%
    dplyr::select(-1) %>%
    dplyr::filter(!sample_name == "") %>% #
    tidyr::pivot_longer(-sample_name) %>%
    dplyr::filter(value > 0) %>%
    dplyr::arrange(sample_name) %>%
    dplyr::filter(!sample_name == "") %>%
    dplyr::select(-value) %>%
    tidyr::pivot_wider(names_from = name, values_from = name) %>%
    # ignore warning below.
    tidyr::unite("strings", 2:ncol(.), na.rm = TRUE, sep = "=") %>%
    tidyr::separate(strings, paste0("ITS2.profile.", 1:5), sep = "=", fill="right") %>%
    tidyr::unite("UID", 2:6, na.rm = TRUE, sep = "*")

  return(its2_names)
}
