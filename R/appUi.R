#' Client (ui) for the norspis app
#'
#' @return An shiny app ui object
#' @export

appUi <- function() {

  regTitle <- "NorSpis"

  shiny::tagList(
    shiny::navbarPage(
      title = rapbase::title(regTitle),
      windowTitle = regTitle,
      theme = rapbase::theme(),
      id = "tabs",

      shiny::tabPanel(
        "Start",
        shiny::mainPanel(
          rapbase::renderRmd(
            system.file("veiledning.Rmd", package = "norspis"),
            outputType = "html_fragment"
          ),
          rapbase::navbarWidgetInput("norspisNavbarWidget",
                                     selectOrganization = TRUE)
        )
      ),

      # shiny::tabPanel(
      #   "Eksempelrapport",
      #   shiny::sidebarLayout(
      #     shiny::sidebarPanel(
      #       shiny::radioButtons("formatReport",
      #                           "Format for nedlasting",
      #                           list(PDF = "pdf", HTML = "html"),
      #                           inline = FALSE),
      #       shiny::downloadButton("downloadReport", "Last ned!")
      #     ),
      #     shiny::mainPanel(
      #       shiny::htmlOutput("exReport", inline = TRUE)
      #     )
      #
      #   )
      # ),

      # shiny::tabPanel(
      #   "Abonnement",
      #   shiny::sidebarLayout(
      #     shiny::sidebarPanel(
      #       rapbase::autoReportInput("norspisSubscription")
      #     ),
      #     shiny::mainPanel(
      #       rapbase::autoReportUI("norspisSubscription")
      #     )
      #   )
      # ),

      shiny::tabPanel(
        "Indikatorer",
        value = "indikatorer",
        norspis::indikatorfig_UI(id = "indikatorfig_id")
      ),
      shiny::uiOutput("dataDumpUI"),

      shiny::tabPanel(
        "Administrative tabeller",
        norspis::admtab_ui("admtabell")
      ),
      shiny::uiOutput("verktoyNavMenu")
    )
  )
}
