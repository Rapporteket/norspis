#' Run the NorSpis Shiny Application
#'
#' @return An object representing the NorSpis app
#' @export

norspisApp <- function() {
  library(lubridate)
  shiny::shinyApp(ui = appUi, server = appServer)
}
