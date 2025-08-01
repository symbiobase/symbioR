#' Function to extract its2 profiles from Symportal
#'
#' type = "relative" sources "profiles.relative.abund_and_meta.txt"
#' type = "absolute" sources  "profiles.absolute.abund_and_meta.txt"
#'
#'
#' @param folder location of the root Symportal output
#' @param type relative or absolute
#' @export
#' @return its_profile A data.frame of ITS profiles for each sample_name


extract_its2_profile <- function(folder, type="absolute"){

  if (type=="absolute"){

    suppressWarnings({
      # function to get ITS2 type profile from Symportal output
      file_list <- list.files(path = folder, pattern = "profiles.absolute.abund_and_meta.txt", include.dirs = TRUE, recursive = TRUE)
      its_profile <- read.delim(paste0(folder, "/", file_list), check.names = FALSE, header = F) %>%
        janitor::row_to_names(row_number = 7) %>%
        dplyr::rename("sample_name" = 2) %>%
        dplyr::select(-1) %>%
        dplyr::filter(!sample_name == "") %>%
        tidyr::pivot_longer(-sample_name) %>%
        dplyr::filter(value > 0) %>%
        dplyr::arrange(sample_name) %>%
        dplyr::filter(!sample_name == "") %>%
        dplyr::select(-value) %>%
        tidyr::pivot_wider(id_cols = "sample_name", names_from = "name", values_from = "name", values_fn = list(am = length)) %>%
        tidyr::unite("strings", 2:ncol(.), na.rm = TRUE, sep = "=") %>%
        tidyr::separate(strings, paste0("ITS2_profile_", 1:5), sep = "=") %>%
        as.data.frame() %>%
        dplyr::rename(sample_name = sample_name)
      return(its_profile)
    })
  } else if (type=="relative"){

    suppressWarnings({
      # function to get ITS2 type profile from Symportal output
      file_list <- list.files(path = folder, pattern = "profiles.relative.abund_and_meta.txt", include.dirs = TRUE, recursive = TRUE)
      its_profile <- read.delim(paste0(folder, "/", file_list), check.names = FALSE, header = F) %>%
        janitor::row_to_names(row_number = 7) %>%
        dplyr::rename("sample_name" = 2) %>%
        dplyr::select(-1) %>%
        dplyr::filter(!sample_name == "") %>%
        tidyr::pivot_longer(-sample_name) %>%
        dplyr::filter(value > 0) %>%
        dplyr::arrange(sample_name) %>%
        dplyr::filter(!sample_name == "") %>%
        dplyr::select(-value) %>%
        tidyr::pivot_wider(id_cols = "sample_name", names_from = "name", values_from = "name", values_fn = list(am = length)) %>%
        tidyr::unite("strings", 2:ncol(.), na.rm = TRUE, sep = "=") %>%
        tidyr::separate(strings, paste0("ITS2_profile_", 1:5), sep = "=") %>%
        as.data.frame() %>%
        dplyr::rename(sample_name = sample_name)
      return(its_profile)
    })

  }

}



