
#' Run the Clinical Pharmacometrics Suite
#'
#' This function launches the production-ready standalone Shiny application dashboard.
#'
#' @export
#' @importFrom shiny shinyApp
run_app <- function() {
  # This guarantees R reads the production block of your config.yml file.
  Sys.setenv(R_CONFIG_ACTIVE = "production")
  # The launch.browser option is explicitly forced active here
  options(shiny.launch.browser = TRUE)
  shinyApp(
    ui = app_ui(),
    server = app_server
  )
}