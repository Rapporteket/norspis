#' Modul for datadump-fane i Norspis sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @name datadump
#' @aliases datadump_ui datadump_server
#'
#' @rdname datadump
#' @export
datadump_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      id = ns("id_dump_panel"),
      dateRangeInput(inputId=ns("datovalg"),
                     label = "Dato fra og til",
                     language = "nb",
                     max = Sys.Date(),
                     start  = '2014-01-01',
                     end = Sys.Date(),
                     separator = " til "),
      selectInput(inputId = ns("dumptype"),
                  label = "Velg type datadump",
                  choices = c("allescorer",
                              "enkeltleddnum",
                              "forlopsoversikt",
                              "skjemaoversikt",
                              "Norspisdata_utflatet")),
      tags$hr(),
      downloadButton(ns("lastNed_dump"), "Last ned datadump")
    ),
    mainPanel(
      h2('Datadump - Norspis', align='center'),
      br(),
      h4('Her kan du laste ned forskjellige varianter av datadump for Norspis.
         Lokale brukere vil bare kunne laste ned data for egen avdeling.'),
      br(),
      h4(tags$b(tags$u('Forklaring til de ulike datadump-typene:'))),
      h4(tags$b('allescorer: '), 'forklaring fra register'),
      h4(tags$b('enkeltleddnum: '), 'forklaring fra register'),
      h4(tags$b('forlopsoversikt: '), 'forklaring fra register'),
      h4(tags$b('skjemaoversikt: '), 'En oversikt med informasjon om status på
         alle påbegynte og avsluttede skjema.'),
      h4(tags$b('Norspisdata_utflatet: '), 'inneholder alle kliniske variabler
      i registeret og benytter tallkodene til kategoriske variabler.
         At tabellen er utflatet innebærer at sluttregistrering er koblet til
         startregistrering slik at en linje utgjør et forløp.')
    )
  )
}

#' @rdname datadump
#' @export
datadump_server <- function(id, RegData, userRole, hvd_session, reshID){
  moduleServer(
    id,

    function(input, output, session) {

      output$lastNed_dump <- downloadHandler(
        filename = function(){
          fs::path_sanitize(paste0(input$dumptype, Sys.time(), '.csv'))
        },
        content = function(file){
          if (input$dumptype == "Norspisdata_utflatet") {
            dumpdata <- RegData
          } else {
            query <- paste0("SELECT * FROM ", input$dumptype)
            dumpdata <- rapbase::loadRegData("data", query)
          }
          # dumpdata <- tmpData[which(as.Date(tmpData$S1b_DateOfCompletion, format="%d.%m.%Y") >= input$datovalg[1] &
          #                             as.Date(tmpData$S1b_DateOfCompletion, format="%d.%m.%Y") <= input$datovalg[2]), ]
          if (userRole() != 'SC') {
            dumpdata <- dumpdata[dumpdata$AvdRESH %in% reshID(), ]
          }
          # write.csv2(dumpdata, file, row.names = F, na = '', fileEncoding = 'Latin1')
          readr::write_excel_csv2(dumpdata, file, na = "")
        }
      )

      shiny::observe({
        if (rapbase::isRapContext()) {

          shinyjs::onclick(
            "lastNed_dump",
            rapbase::repLogger(
              session = hvd_session,
              msg = paste0("Norspis: nedlasting datadump: ", input$dumptype)
            )
          )
        }
      })

    }
  )}
