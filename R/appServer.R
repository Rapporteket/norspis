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
  #mapOrgId <- ablanor::getNameReshId(registryName)
  reshId <- rapbase::getUserReshId(session)
  #hospitalName <- ablanor::getHospitalName(registryName, reshId)
  userFullName <- rapbase::getUserFullName(session)
  userRole <- rapbase::getUserRole(session)

  rapbase::navbarWidgetServer("norspisNavbarWidget", "norspis")

  rapbase::statsServer("norspisStats", registryName = registryName)

}
