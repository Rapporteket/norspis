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

  rapbase::navbarWidgetServer("norspisNavbarWidget", "norspis",
                              caller = "norspis")

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