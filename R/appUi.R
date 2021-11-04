#' Client (ui) for the norspis app
#'
#' @return An shiny app ui object
#' @export

appUi <- function() {

  shiny::addResourcePath("rap", system.file("www", package = "rapbase"))
  appTitle <- "NorSpis"

  shiny::tagList(

    shiny::navbarPage(
      title = shiny::div(
        shiny::a(
          shiny::includeHTML(
            system.file("www/logo.svg", package = "rapbase")
          )
        ),
        appTitle
      ),
      windowTitle = appTitle,
      theme = "rap/bootstrap.css",
      id = "tabs",

      shiny::tabPanel(
        "Start",
        shiny::mainPanel(
          rapbase::renderRmd(
            system.file("veiledning.Rmd", package = "norspis"),
            outputType = "html_fragment"
          ),
          rapbase::navbarWidgetInput("norspisNavbarWidget")
        )
      ),

      shiny::tabPanel(
        "Abonnement",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::autoReportInput("norspisSubscription")
          ),
          shiny::mainPanel(
            rapbase::autoReportUI("norspisSubscription")
          )
        )
      ),

      shiny::navbarMenu(
        "Verkt\u00f8y",

        shiny::tabPanel(
          "Utsending",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              rapbase::autoReportFormatInput("norspisDispatchFormat"),
              rapbase::autoReportOrgInput("norspisDispatchOrg"),
              rapbase::autoReportInput("norspisDispatch")
            ),
            shiny::mainPanel(
              rapbase::autoReportUI("norspisDispatch")
            )
          )
        ),

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
        ),

        shiny::tabPanel(
          "Eksport",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              rapbase::exportUCInput("norspisExport")
            ),
            shiny::mainPanel(
              rapbase::exportGuideUI("norspisExport")
            )
          )
        ),
      )
    )
  )
}
