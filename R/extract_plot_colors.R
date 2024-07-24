#' Function to extract colors for plots from Symportal
#'
#'  internal function
#'
#' @param folder location of the root Symportal output
#' @export
#' @return list of colors for plotting

extract_plot_colors <- function(folder){

  file_list <- list.files(path = folder, pattern = "color_dict_post_med.json", include.dirs = TRUE, recursive = TRUE)

  colors <- RJSONIO::fromJSON(paste0(folder, "/", file_list)) %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::rename(seq=1, colorhtml=2) %>%
    tibble::deframe()

  return(colors)
}

