#' Function to extract its2 profile and UIDs from Symportal
#'
#' @param folder location of the root Symportal output
#' @export
#' @return its_profile_UID A data.frame of ITS profiles for each UID
#' @examples
#' tmp <- extract_its2(folder="/Users/rof011/Symbiodinium/20220919T102058_esampayo/")

extract_its2 <- function(folder) {

    a <- symportalfunctions::extract_its2_profile(folder = folder)
    b <- symportalfunctions::extract_its2_profile_UID(folder = folder)
    its_profile <- dplyr::left_join(a, b, by = "sample.ID") |>
      as.data.frame()


    return(its_profile)
}



