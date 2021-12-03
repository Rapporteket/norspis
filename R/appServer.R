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

  registryName <- "norspis"
  hospitalName <- "Syke Hus"
  userFullName <- rapbase::getUserFullName(session)
  userRole <- rapbase::getUserRole(session)
  userReshId <- rapbase::getUserReshId(session)

  rapbase::navbarWidgetServer("norspisNavbarWidget", "norspis",
                              caller = "norspis")

  # Eksempelrapport
  output$exReport <- shiny::renderUI({
    rapbase::renderRmd(
      system.file("eksSamlerapport.Rmd", package = "norspis"),
      outputType = "html_fragment",
      params = list(
        author = userFullName,
        hospitalName = hospitalName,
        tableFormat = "html",
        reshId = userReshId,
        registryName = registryName,
        userRole = userRole
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
          author = userFullName,
          hospitalName = hospitalName,
          tableFormat = input$formatReport,
          reshId = userReshId,
          registryName = registryName,
          userFullName = userFullName,
          userRole = userRole
        )
      )
      file.rename(fn, file)
    }
  )

  # dummy report and orgs to subscribe and dispatch
  orgs <- list(
    TestOrg = 999999
  )
  report <- list(
    Veiledning = list(
      synopsis = "Testrapport kun for illustrasjon",
      fun = "reportProcessor",
      paramNames = c("report", "outputFormat", "title"),
      paramValues = c("veiledning", "pdf", "Testrapport")
    )
  )

  # subscribe
  rapbase::autoReportServer(
    "norspisSubscription", registryName = registryName, type = "subscription",
    reports = report, orgs = orgs
  )

  # dispatch
  org <- rapbase::autoReportOrgServer("norspisDispatchOrg", orgs)
  fileFormat <- rapbase::autoReportFormatServer("norspisDispatchFormat")
  paramNames <- shiny::reactive(c("outputFormat"))
  paramValues <- shiny::reactive(c(fileFormat()))
  rapbase::autoReportServer(
    "norspisDispatch", registryName = registryName, type = "dispatchment",
    org = org$value,
    paramNames = paramNames, paramValues = paramValues, reports = report,
    orgs = orgs
  )

  # use stats
  rapbase::statsServer("norspisStats", registryName = registryName)

  # export
  rapbase::exportGuideServer("norspisExport", registryName)
  rapbase::exportUCServer("norspisExport", registryName)
}
