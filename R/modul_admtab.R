#' Shiny modul for administrative tabeller for Norspis
#'
#' Kun til bruk i Shiny, returner modulfunksjoner
#'
#' @param id Tekststreng med shiny modul id.
#' @param skjemaoversikt dataramme med oversikt over registreringer i Norspis
#'
#' @return Shiny objekter for Norspis sine administrative tabeller
#'
#' @name admtab
#' @aliases admtab_ui admtab_server
#'
#' @rdname admtab
#' @export
admtab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 3,
                        id = ns("id_adm_panel"),
                        shiny::dateRangeInput(inputId=ns("datovalg"), label = "Dato fra og til",
                                              min = '2015-01-01', language = "nb",
                                              max = Sys.Date(), start  = Sys.Date() %m-% months(12),
                                              end = Sys.Date(), separator = " til "),
                        shiny::selectInput(inputId = ns("regstatus"), label = "Skjemastatus",
                                           choices = c('Ferdigstilt'=1, 'I kladd'=0, 'Opprettet'=-1)),
                        shiny::tags$hr(),
                        shiny::actionButton(ns("reset_input"), "Nullstill valg")
    ),
    shiny::mainPanel(shiny::tabsetPanel(id= ns("admtabeller"),
                          shiny::tabPanel("Antall skjema", value = "id_ant_skjema",
                                          shiny::h2('Innregistreringer i NorSpis etter skjematype',
                                                    align='center'),
                                          shiny::br(),
                                          shiny::br(),
                                          DT::DTOutput(ns("Tabell_adm1")),
                                          shiny::downloadButton(ns("lastNed_adm1"),
                                                                "Last ned tabell")
                          )
    )
    )
  )

}

#' @rdname admtab
#' @export
admtab_server <- function(id, skjemaoversikt) {

  moduleServer(id, function(input, output, session) {

    # admtab <- function(input, output, session, skjemaoversikt){

    antskjema <- function() {
      ant_skjema <- skjemaoversikt %>%
        dplyr::filter(HovedDato >= input$datovalg[1] & HovedDato <= input$datovalg[2]) %>%
        dplyr::filter(SkjemaStatus %in% input$regstatus) %>%
        dplyr::select("shusnavn", "Skjemanavn") %>%
        table() %>%
        addmargins(1) %>%
        as.data.frame.matrix() %>%
        tidyr::as_tibble(rownames = "Sykehusnavn")

      sketch <- htmltools::withTags(table(
        DT::tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
        DT::tableFooter(c('Sum' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
      list(ant_skjema=ant_skjema, sketch=sketch)
    }

    output$Tabell_adm1 = DT::renderDT(
      DT::datatable(antskjema()$ant_skjema[-dim(antskjema()$ant_skjema)[1], ],
                    container = antskjema()$sketch,
                    rownames = F,
                    options = list(pageLength = 40)
      )
    )




  })


}


#' @rdname admtab
#' @export
admtab_demo <- function() {
  # library(lubridate)
  SkjemaOversikt <- norspis::querySkjemaOversikt("norspis")
  shusnavn <- norspis::queryReshNames("norspis")
  SkjemaOversikt$shusnavn <-
    shusnavn$shortName[match(SkjemaOversikt$AvdRESH, shusnavn$reshId)]
  ui <- shiny::navbarPage(id = "tmp_app_id",
                          title = shiny::div(shiny::a(shiny::includeHTML(system.file('www/logo.svg', package='rapbase'))),
                                             "NorSpis"),
                          windowTitle = "NorSpis",
                          theme = "rap/bootstrap.css",
                          shiny::tabPanel("Administrative tabeller", norspis::admtab_ui("x"))
  )
  server <- function(input, output, session) {
    norspis::admtab_server("x", SkjemaOversikt)
  }
  shiny::shinyApp(ui, server)
}







