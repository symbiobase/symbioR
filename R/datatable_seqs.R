
#' Wrapper around DT for printing sequence tables
#'
#' @param input filename for table
#' @export
#' @return datatable


datatable_seqs <- function(input){
  datatable_output <- input |> DT::datatable(class = 'cell-border stripe', rownames = FALSE,
                                             options = list(autoWidth = TRUE,  pageLength = 10)) %>%
    DT::formatStyle(columns = colnames(input), fontSize = '75%')

  return(datatable_output)
}

