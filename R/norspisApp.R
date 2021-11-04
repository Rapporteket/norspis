#' Run the NorSpis Shiny Application
#'
#' @return An object representing the NorSpis app
#' @export

norspisApp <- function() {

  shiny::shinyApp(ui = appUi, server = appServer)
}
