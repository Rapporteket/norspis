#' Get data from NorSpis
#'
#' Functions to query data from a database holding NorSpis data at Rapporteket.
#' Providing these function with the shiny session object logging of queries
#' will also be performed
#'
#' @param registryName Character string with name of the registry from which
#' data are to be queried
#' @param reshId Integer providing the organization id. Usefull for data
#' filtering
#' @param ... Optional arguments to pass to the function. If \code{session} is
#' provided this will be assumed a valid shiny session object and hence logging
#' will be performed
#'
#' @name queryData
#' @aliases queryAlleScorer queryBehandling queryBehandlingNum
#' queryEnkeltLedd queryEnkeltLeddNum queryForlopsOversikt
#' @return Data frame of registry data
NULL


#' @rdname queryData
#' @export
queryAlleScorer <- function(registryName, reshId, ...) {

  query <- "SELECT * FROM AlleScorer;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load AlleScorer data from ", registryName, ": ", query)
    raplog::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registryName, query)
}


#' @rdname queryData
#' @export
queryBehandling <- function(registryName, reshId, ...) {

  query <- "SELECT * FROM Behandling;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load Behandling data from ", registryName, ": ", query)
    raplog::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registryName, query)
}


#' @rdname queryData
#' @export
queryBehandlingNum <- function(registryName, reshId, ...) {

  query <- "SELECT * FROM BehandlingNum;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load BehandlingNum data from ", registryName, ": ", query)
    raplog::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registryName, query)
}


#' @rdname queryData
#' @export
queryEnkeltLedd <- function(registryName, reshId, ...) {

  query <- "SELECT * FROM EnkeltLedd;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load EnkeltLedd data from ", registryName, ": ", query)
    raplog::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registryName, query)
}


#' @rdname queryData
#' @export
queryEnkeltLeddNum <- function(registryName, reshId, ...) {

  query <- "SELECT * FROM EnkeltLeddNum;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load EnkeltLeddNum data from ", registryName, ": ", query)
    raplog::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registryName, query)
}


#' @rdname queryData
#' @export
queryForlopsOversikt <- function(registryName, reshId, ...) {

  query <- "SELECT * FROM ForlopsOversikt;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load ForlopsOversikt data from ", registryName, ": ", query)
    raplog::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registryName, query)
}
