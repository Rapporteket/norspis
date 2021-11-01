#' Client (ui) for the norspis app
#'
#' @return An shiny app ui object
#' @export

appUi <- function() {

  shiny::addResourcePath("rap", system.file("www", package = "rapbase"))
  appTitle <- "NorSpis"

  shiny::tagList(

    shiny::navbarPage(
      title = shiny::div(appTitle),
      windowTitle = appTitle,
      theme = "rap/bootstrap.css",
      id = "tabs",

      shiny::tabPanel(
        "Start",
        shiny::mainPanel(
          shiny::p("Startside"),
          rapbase::navbarWidgetInput("norspisNavbarWidget")
        )
      ),

      shiny::navbarMenu(
        "Verkt\u00f8y",
        shiny::tabPanel(
          "Bruksstatistikk",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              rapbase::statsInput("norspisStats"),
              rapbase::statsGuideUI("norspisStats")
            ),
            shiny::mainPanel(
              rapbase::statsUI("norspisStats")
            )
          )
        )
      )
    )
  )
}
