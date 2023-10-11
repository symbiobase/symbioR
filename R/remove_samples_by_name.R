#' Function to remove samples by name
#'
#'
#'
#'
#' @param input location of the root Symportal output
#' @param drop drop list
#' @param remove_zero drop columns summing to zero
#' @export

remove_samples_by_name <- function(input, drop, remove_zero=TRUE, ...){

  result <- input %>% filter(!sample_name %in% drop)

  if(isTRUE(remove_zero)){

  result_zero <- result %>%
    select(where(~any(. != 0)))

    return(result_zero)

  } else {

    return(result)


  }


}
