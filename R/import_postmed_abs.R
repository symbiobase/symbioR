#' Function to extract postmed sequences from symportal output
#'
#'
#'
#'
#' @param folder location of the root Symportal output
#' @param type "relative" or "absolute"
#' @export

import_postmed_abs <- function(folder, format = "long", ...){

  file_list <- list.files(path = folder, pattern = "seqs.absolute.abund_and_meta.txt", include.dirs = TRUE, recursive = TRUE)

  full_data <- read.delim(paste0(folder, "/", file_list)) %>%
    dplyr::select(sample_name, 40:ncol(.)) %>% # select just the symbiodinium columns
    dplyr::slice(-dplyr::n())  # remove the last row, summary data

  if (format=="long"){


  full_data_long <- full_data %>%
    tidyr::pivot_longer(cols = -sample_name, names_to = "seq.ID", values_to = "abundance")

  return(full_data_long)

  } else if (format=="wide"){

  return(full_data)

  } else {

    print("one of [long] or [wide]")
  }

}
