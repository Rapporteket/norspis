#' Server logic for the norspis app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

appServer <- function(input, output, session) {

  rapbase::appLogger(session = session, msg = "Starting norspis application")
  rapbase::logShinyInputChanges(input)

  # Last data
  norspisdata <- norspis::norspisLesOgProsesser()
  SkjemaOversikt <- norspisdata$SkjemaOversikt
  ForlopsOversikt <- norspisdata$ForlopsOversikt
  RegData <- norspisdata$RegData
  map_avdeling <- data.frame(
    UnitId = unique(RegData$AvdRESH),
    orgname = RegData$SykehusNavn[match(unique(RegData$AvdRESH),
                                        RegData$AvdRESH)])

  user <- rapbase::navbarWidgetServer2(
    "norspisNavbarWidget",
    orgName = "norspis",
    caller = "norspis",
    map_orgname = shiny::req(map_avdeling)
  )

  registryName <- "norspis"
  hospitalName <- shiny::reactive(
    ForlopsOversikt$Kortnavn[match(shiny::req(user$org), ForlopsOversikt$AvdRESH)])

  dataDump_tab_added <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(shiny::req(user$role()), {
    if (user$role() == 'SC') {
      shiny::insertTab(
        inputId = "norspis_app_id",
        tab = shiny::tabPanel(
          title = "Datadump",
          value = "datadump",
          norspis::datadump_UI(id = "datadump_id")
        ),
        target = "indikatorer",
        position = "after"
      )
      dataDump_tab_added(TRUE)
    } else {
      shiny::removeTab(inputId = "norspis_app_id", target = "datadump")
      dataDump_tab_added(FALSE)
    }
  })

  # tool_tabs_added <- shiny::reactiveVal(FALSE)
  # shiny::observeEvent(shiny::req(user$role()), {
  #   if (user$role() == "SC") {
  #     if (!tool_tabs_added()) {
  #       shiny::appendTab(
  #         inputId = "norspis_app_id",
  #         tab = shiny::navbarMenu(
  #           "Verktøy",
  #           shiny::tabPanel(
  #             "Bruksstatistikk",
  #             shiny::sidebarLayout(
  #               shiny::sidebarPanel(
  #                 rapbase::statsInput("norspisStats"),
  #                 rapbase::statsGuideUI("norspisStats")
  #               ),
  #               shiny::mainPanel(
  #                 rapbase::statsUI("norspisStats")
  #               )
  #             )
  #           ),
  #           shiny::tabPanel(
  #             "Eksport",
  #             shiny::sidebarLayout(
  #               shiny::sidebarPanel(
  #                 rapbase::exportUCInput("norspisExport")
  #               ),
  #               shiny::mainPanel(
  #                 rapbase::exportGuideUI("norspisExport")
  #               )
  #             )
  #           )
  #         )
  #       )
  #       tool_tabs_added(TRUE)
  #     }
  #   } else {
  #     if (tool_tabs_added()) {
  #       shiny::removeTab(inputId = "norspis_app_id", target = "Verktøy")
  #       tool_tabs_added(FALSE)
  #     }
  #   }
  # })

  # Legg til verktøy-fanen for SC-brukere, og fjern den for andre roller
  tool_tabs_added <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(shiny::req(user$role()), {
    if (user$role() == "SC") {
      if (!tool_tabs_added()) {
        message("Adding Verktøy-tab for user with role ", user$role())
        shiny::appendTab(
          inputId = "norspis_app_id",
          tab = shiny::navbarMenu(
            "Verktøy",
            # shiny::tabPanel(
            #   "Utsending",
            #   shiny::sidebarLayout(
            #     shiny::sidebarPanel(
            #       rapbase::autoReportFormatInput("norspisDispatch"),
            #       rapbase::autoReportOrgInput("norspisDispatch"),
            #       rapbase::autoReportInput("norspisDispatch")
            #     ),
            #     shiny::mainPanel(
            #       rapbase::autoReportUI("norspisDispatch")
            #     )
            #   )
            # ),
            shiny::tabPanel(
              "Metadata",
              shiny::sidebarLayout(
                shiny::sidebarPanel(shiny::uiOutput("metaControl")),
                shiny::mainPanel(
                  shiny::uiOutput("n_lines"),
                  shiny::htmlOutput("metaData")
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
                  rapbase::exportGuideUI("norspisExportGuide")
                )
              )
            ),
            shiny::tabPanel(
              "Bruksstatistikk",
              shiny::sidebarLayout(
                shiny::sidebarPanel(
                  rapbase::statsInput("norspisStats"),
                  rapbase::statsGuideUI("norspisStatsGuide")
                ),
                shiny::mainPanel(
                  rapbase::statsUI("norspisStats")
                )
              )
            )
          )
        )
        tool_tabs_added(TRUE)
      }
    } else {
      if (tool_tabs_added()) {
        message("Removing Verktøy-tab for user with role ", user$role())
        shiny::removeTab("norspis_app_id", target = "Verktøy")
        tool_tabs_added(FALSE)
      }
    }
  })

  # Indikatorfigur
  norspis::indikatorfigServer("indikatorfig_id",
                     RegData = RegData, userRole = user$role,
                     hvd_session = session)

  # Datadump
  norspis::datadump_server("datadump_id", reshID = user$org,
                  RegData = RegData, userRole = user$role, hvd_session = session)

  # Administrative tabeller
  norspis::admtab_server("admtabell", SkjemaOversikt, ForlopsOversikt)

  # Eksempelrapport
  output$exReport <- shiny::renderUI({
    rapbase::renderRmd(
      system.file("eksSamlerapport.Rmd", package = "norspis"),
      outputType = "html_fragment",
      params = list(
        author = user$fullName(),
        hospitalName = hospitalName(),
        tableFormat = "html",
        reshId = user$org(),
        registryName = registryName,
        userRole = user$role()
      )
    )
  })

  output$downloadReport <- shiny::downloadHandler(
    filename = function() {
      basename(tempfile(pattern = "NorSpis_eksRapport",
                        fileext = paste0(".", input$formatReport)))
    },
    content = function(file) {
      fn <- rapbase::renderRmd(
        system.file("eksSamlerapport.Rmd", package = "norspis"),
        outputType = input$formatReport,
        params = list(
          author = user$fullName(),
          hospitalName = hospitalName(),
          tableFormat = input$formatReport,
          reshId = user$org(),
          registryName = registryName,
          userFullName = user$fullName(),
          userRole = user$role()
        )
      )
      file.rename(fn, file)
    }
  )

  ## Metadata
  meta <- shiny::reactive({
    rapbase::describeRegistryDb("data")
  })

  output$metaControl <- shiny::renderUI({
    tabs <- names(meta())
    selectInput("metaTab", "Velg tabell:", tabs)
  })


  output$metaDataTable <- DT::renderDataTable(
    meta()[[input$metaTab]],
    rownames = FALSE,
    options = list(lengthMenu = c(25, 50, 100, 200, 400))
  )

  output$metaData <- shiny::renderUI({
    DT::dataTableOutput("metaDataTable")
  })

  output$n_lines <- shiny::renderUI({
    shiny::h4(paste0(
      input$metaTab,
      " har ",
      rapbase::nlinesRegistryDb(
        registryName = "data",
        tab = shiny::req(input$metaTab)
      ),
      " linjer"
    )
    )
  })

  ## Stats
  observe(
    rapbase::statsServer("norspisStats",
                         registryName = "norspis",
                         app_id = Sys.getenv("FALK_APP_ID"),
                         eligible = (user$role() == "SC")
    )
  )
  rapbase::statsGuideServer("norspisStatsGuide",
                            registryName = "norspis")


  ##############################################################################
  # Eksport  ###################################################################
  ##############################################################################

  ## brukerkontroller
  rapbase::exportUCServer(
    "norspisExport",
    dbName = "data",
    teamName = "norspis"
  )
  ## veiledning
  rapbase::exportGuideServer("norspisExportGuide", dbName = "data")

  # # use stats
  # rapbase::statsServer("norspisStats", registryName = registryName, app_id = Sys.getenv("FALK_APP_ID"))
  #
  # # export
  # rapbase::exportGuideServer("norspisExport", registryName)
  # rapbase::exportUCServer("norspisExport", registryName)
}
