#' Run the NorSpis Shiny Application
#'
#' @return An object representing the NorSpis app
#' @export

norspisApp <- function(browser = FALSE,
                       logAsJson = FALSE) {
  if (logAsJson) {
    rapbase::loggerSetup()
  }
  if (browser) {
    options(shiny.launch.browser = TRUE)
  }
  if (!capabilities()["cairo"]) {
    knitr::opts_chunk$set(dev = "svglite") # To make it work without cairo.
  }
  shiny::shinyApp(ui = appUi, server = appServer)
}
