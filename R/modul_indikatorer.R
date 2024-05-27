#' UI-modul for indikatorfigurer i Norspis sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @export
#'
indikatorfig_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      width = 3,
      id = ns("id_indikator_panel"),
      selectInput(
        inputId = ns("valgtVar"), label = "Velg indikator",
        choices = c("Symptomreduksjon EDEQ, voksne" = "norspis_KI1_symptomreduksjon_EDEQ_V",
                    "Symptomreduksjon EDEQ, barn/ungdom" = "norspis_KI1_symptomreduksjon_EDEQ_BU",
                    "Bedring i livskvalitet, voksne" = "norspis_KI2_funksjonsbedring_CIA_V",
                    "Bedring i livskvalitet, barn/ungdom" = "norspis_KI2_funksjonsbedring_CIA_BU",
                    "Endring i undervektsstatus, voksne" = "norspis_KI3_undervektsreduksjon_V",
                    "Endring i undervektsstatus, barn/ungdom" = "norspis_KI3_undervektsreduksjon_BU",
                    "Utfallsvurdering, voksne" = "norspis_KI4_utfallsvurdering_V",
                    "Utfallsvurdering, barn/ungdom" = "norspis_KI4_utfallsvurdering_BU",
                    "Familie/venner involvert i behandlingen, voksne" = "norspis_involvering_famlie_venner_V",
                    "Familie/venner involvert i behandlingen, barn/ungdom" = "norspis_involvering_famlie_venner_BU",
                    "Blodprøver ved start, undervektige, voksne" = "norspis_undervekt_prodprove_V",
                    "Blodprøver ved start, undervektige, barn/ungdom" = "norspis_undervekt_prodprove_BU",
                    "Blodprøver ved start, oppkast" = "norspis_oppkast_prodprove",
                    "Blodprøver ved start, oppkast, voksne" = "norspis_oppkast_prodprove_V",
                    "Blodprøver ved start, oppkast, barn/ungdom" = "norspis_oppkast_prodprove_BU",
                    "Beintetthetsmåling ved start, undervektige voksne" = "norspis_beintetthetsmaling_V",
                    "Beintetthetsmåling ved start, undervektige barn/ungdom" = "norspis_beintetthetsmaling_BU"
        )
      ),
      uiOutput(outputId = ns('tilAar_ui')),
      selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                  choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg')),
      tags$hr(),
      actionButton(ns("reset_input"), "Nullstill valg")
    ),
    mainPanel(
      tabsetPanel(id = ns("tab"),
                  tabPanel("Figur", value = "fig",
                           plotOutput(ns("Figur1"), height="auto"),
                           downloadButton(ns("lastNedBilde"), "Last ned figur")),
                  tabPanel("Tabell", value = "tab",
                           DT::DTOutput(ns("tabell"))
                  )
      )
    )
  )

}

#' Server-modul for indikatorfigurer i Norspis sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @export
#'
indikatorfigServer <- function(id, RegData, userRole, hvd_session){
  moduleServer(
    id,

    function(input, output, session) {

      observeEvent(input$reset_input, {
        shinyjs::reset("id_indikator_panel")
      })

      output$tilAar_ui <- renderUI({
        ns <- session$ns
        selectInput(inputId = ns("tilAar"), label = "T.o.m. år",
                    choices = rev((min(RegData$StartAar)+2):max(RegData$StartAar)))
      })


      tabellReager <- reactive({
        indikatordata <- norspis::norspisBeregnIndikator(RegData = RegData,
                                                         ind_id = input$valgtVar)
        TabellData <- indikatordata$indikator
        TabellData <- TabellData[which(TabellData$year <= as.numeric(req(input$tilAar))), ]
        indikatordata$indikator <- TabellData
        indikatordata
      })

      output$Figur1 <- renderPlot({
        norspis::norspisPlotIndikator(indikatordata = tabellReager(),
                                      graaUt=NA, outfile = '',
                                      lavDG=NA, inkl_konf=F)
      }, width = 700, height = 700)

      # output$tabell <- DT::renderDT({
      #   indikatordata = tabellReager()
      #   tabell <- indikatordata$indikator %>%
      #     dplyr::group_by(SykehusNavn, year) %>%
      #     dplyr::summarise(Antall = sum(var),
      #                      N = sum(denominator),
      #                      Andel = Antall/N*100)
      #   # antall <-
      #   #
      #   # %>%
      #   #   janitor::adorn_totals()
      #
      # })

      output$tabell  <- DT::renderDataTable(
        server = FALSE,
        DT::datatable(
          {tabellReager()$indikator %>%
              dplyr::group_by(SykehusNavn, year) %>%
              dplyr::summarise(Antall = sum(var),
                               N = sum(denominator),
                               Andel = Antall/N*100)},

          extensions = 'Buttons',
          rownames = FALSE,

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
        ) %>% DT::formatRound(columns = "Andel", digits = 1)
      )

      output$lastNedBilde <- downloadHandler(
        filename = function(){
          paste0(input$valgtVar, Sys.time(), '.', input$bildeformat)
        },

        content = function(file){
          norspis::norspisPlotIndikator(indikatordata = tabellReager(),
                                        graaUt=NA, lavDG=NA, inkl_konf=F,
                                        outfile = file)
        }
      )

    }
  )
}
