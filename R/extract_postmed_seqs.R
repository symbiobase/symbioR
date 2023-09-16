#' Function to extract postmed seqs from Symportal
#'
#' @param folder location of the root Symportal output
#' @param type relative or absolute
#' @export
#' @return its_profile A data.frame of ITS profiles for each sample.ID

extract_postmed_seqs <- function(folder, type="absolute") {

  if (type=="absolute"){
    suppressWarnings({
      # function to get ITS2 type profile from Symportal output
      file_list <- list.files(path = folder, pattern = "seqs.absolute.abund_and_meta.txt", include.dirs = TRUE, recursive = TRUE)
      its_profile <- read.delim(paste0(folder, "/", file_list), check.names = FALSE, header = F) %>%
        janitor::row_to_names(row_number = 7) %>%
        dplyr::rename("sample.ID" = 2) %>%
        dplyr::select(-1) %>%
        dplyr::filter(!sample.ID == "") %>%
        tidyr::pivot_longer(-sample.ID) %>%
        dplyr::filter(value > 0) %>%
        dplyr::arrange(sample.ID) %>%
        dplyr::filter(!sample.ID == "") %>%
        dplyr::select(-value) %>%
        tidyr::pivot_wider(id_cols = "sample.ID", names_from = "name", values_from = "name", values_fn = list(am = length)) %>%
        tidyr::unite("strings", 2:ncol(.), na.rm = TRUE, sep = "=") %>%
        tidyr::separate(strings, paste0("ITS2.profile.", 1:5), sep = "=") %>%
        as.data.frame() %>%
        dplyr::rename(sample.ID = sample.ID)
      return(its_profile)
    }) }

  else if (type=="relative"){
    suppressWarnings({
      # function to get ITS2 type profile from Symportal output
      file_list <- list.files(path = folder, pattern = "seqs.relative.abund_and_meta", include.dirs = TRUE, recursive = TRUE)
      its_profile <- read.delim(paste0(folder, "/", file_list), check.names = FALSE, header = F) %>%
        janitor::row_to_names(row_number = 7) %>%
        dplyr::rename("sample.ID" = 2) %>%
        dplyr::select(-1) %>%
        dplyr::filter(!sample.ID == "") %>%
        tidyr::pivot_longer(-sample.ID) %>%
        dplyr::filter(value > 0) %>%
        dplyr::arrange(sample.ID) %>%
        dplyr::filter(!sample.ID == "") %>%
        dplyr::select(-value) %>%
        tidyr::pivot_wider(id_cols = "sample.ID", names_from = "name", values_from = "name", values_fn = list(am = length)) %>%
        tidyr::unite("strings", 2:ncol(.), na.rm = TRUE, sep = "=") %>%
        tidyr::separate(strings, paste0("ITS2.profile.", 1:5), sep = "=") %>%
        as.data.frame() %>%
        dplyr::rename(sample.ID = sample.ID)
      return(its_profile)
    }) }
}



