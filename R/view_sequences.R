#' Function run shiny app
#'
#' @export

view_sequences <- function(default_folder = NULL) {
  options(default_folder = default_folder)
  appDir <- system.file("shiny", "seqs-viewer", package = "symbioR")
  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)

  # shiny::runApp(appDir, display.mode = "normal",
  #               launch.browser = getOption("shiny.launch.browser", interactive()))

}
