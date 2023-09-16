#' Function run shiny app
#'
#' @export

seqs_viewer <- function() {
  appDir <- system.file("shiny-examples", "seqs-viewer", package = "symportalfunctions")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal",
                launch.browser = getOption("shiny.launch.browser", interactive()),)


}
