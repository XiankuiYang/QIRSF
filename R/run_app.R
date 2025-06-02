#' Launch the QIRSF Shiny App
#'
#' @export
run_app <- function() {
  appDir <- system.file("apps/QIRSF", package = "QIRSF")
  shiny::runApp(appDir, launch.browser = TRUE)
}
