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
                                           choices = c('Ferdigstilt'=1, 'I kladd'=0, 'Opprettet'=-1),
                                           multiple = TRUE,
                                           selected = 1),
                        shiny::tags$hr(),
                        shiny::actionButton(ns("reset_input"), "Nullstill valg")
    ),
    shiny::mainPanel(shiny::tabsetPanel(id= ns("admtabeller"),
                                        shiny::tabPanel("Antall skjema", value = "id_ant_skjema",
                                                        shiny::h2('Innregistreringer i NorSpis etter skjematype',
                                                                  align='center'),
                                                        shiny::br(),
                                                        shiny::br(),
                                                        DT::DTOutput(ns("Tabell_adm1"))
                                        )
    )
    )
  )

}

#' @rdname admtab
#' @export
admtab_server <- function(id, skjemaoversikt) {

  moduleServer(id, function(input, output, session) {

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
                    extensions = 'Buttons',

                    options = list(
                      paging = TRUE,
                      pageLength = 40,
                      searching = TRUE,
                      fixedColumns = TRUE,
                      autoWidth = TRUE,
                      ordering = TRUE,
                      dom = 'tB',
                      buttons = c('copy', 'csv', 'excel')
                    ),

                    class = "display"
      )
    )




  })


}


#' @rdname admtab
#' @export
admtab_demo <- function() {
  library(lubridate)
  shiny::addResourcePath("rap", system.file("www", package = "rapbase"))
  appTitle <- "NorSpis"
  SkjemaOversikt <- norspis::querySkjemaOversikt("norspis")
  shusnavn <- norspis::queryReshNames("norspis")
  SkjemaOversikt$shusnavn <-
    shusnavn$shortName[match(SkjemaOversikt$AvdRESH, shusnavn$reshId)]
  SkjemaOversikt$Skjemanavn[SkjemaOversikt$SkjemaRekkeflg==9] <-
    paste0(SkjemaOversikt$Skjemanavn[SkjemaOversikt$SkjemaRekkeflg==9], " (2)")
  SkjemaOversikt$Skjemanavn <-
    factor(SkjemaOversikt$Skjemanavn,
           levels = SkjemaOversikt$Skjemanavn[match(sort(as.numeric(unique(SkjemaOversikt$SkjemaRekkeflg))),
                                                    SkjemaOversikt$SkjemaRekkeflg)])
  ui <- shiny::navbarPage(
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
    shiny::tabPanel("Administrative tabeller", norspis::admtab_ui("x"))
  )
  server <- function(input, output, session) {
    norspis::admtab_server("x", SkjemaOversikt)
  }
  shiny::shinyApp(ui, server)
}







