#' Runs the ShinyAppPackage Shiny web application.
#' @export
run_outliers_app <- function(x)
{
  shiny::runApp(appDir = system.file("App", package = "Outliers.App"))
}

