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

  shiny::observeEvent(user$role(), {
    if (user$role() != 'SC') {
      shiny::hideTab("tabs", target = "Verkt\u00f8y")
    }
    if (user$role() == 'SC') {
      shiny::showTab("tabs", target = "Verkt\u00f8y")
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


#
#   # dummy report and orgs to subscribe and dispatch
#   orgs <- list(
#     TestOrg = 999999
#   )
#   report <- list(
#     Veiledning = list(
#       synopsis = "Testrapport kun for illustrasjon",
#       fun = "reportProcessor",
#       paramNames = c("report", "outputFormat", "title"),
#       paramValues = c("veiledning", "pdf", "Testrapport")
#     ),
#     Eksempelrapport = list(
#       synopsis = "Eksempelrapport med data fra NorSpis",
#       fun = "reportProcessor",
#       paramNames = c("report", "outputFormat", "title"),
#       paramValues = c("eksSamlerapport", "pdf", "Eksempelrapport")
#     )
#   )
#
#   # subscribe
#   rapbase::autoReportServer(
#     "norspisSubscription", registryName = registryName, type = "subscription",
#     reports = report, orgs = orgs
#   )
#
#   # dispatch
#   org <- rapbase::autoReportOrgServer("norspisDispatchOrg", orgs)
#   fileFormat <- rapbase::autoReportFormatServer("norspisDispatchFormat")
#   paramNames <- shiny::reactive(c("outputFormat"))
#   paramValues <- shiny::reactive(c(fileFormat()))
#   rapbase::autoReportServer(
#     "norspisDispatch", registryName = registryName, type = "dispatchment",
#     org = org$value,
#     paramNames = paramNames, paramValues = paramValues, reports = report,
#     orgs = orgs
#   )

  # use stats
  rapbase::statsServer("norspisStats", registryName = registryName, app_id = Sys.getenv("FALK_APP_ID"))

  # export
  rapbase::exportGuideServer("norspisExport", registryName)
  rapbase::exportUCServer("norspisExport", registryName)
}
