#' Run the imongr Shiny Application
#'
#' @return An object representeing the imongr app
#' @export

runApp <- function() {

  shiny::shinyApp(ui = appUi, server = appServer)
}
