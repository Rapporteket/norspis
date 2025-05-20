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
#' queryEnkeltLedd queryEnkeltLeddNum queryForlopsOversikt queryReshNames
#' @return Data frame of registry data
NULL


#' @rdname queryData
#' @export
queryAlleScorer <- function(registryName, reshId, ...) {

  query <- "SELECT * FROM allescorer;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load allescorer data from ", registryName, ": ", query)
    rapbase::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registryName, query)
}


#' @rdname queryData
#' @export
queryBehandling <- function(registryName, reshId, ...) {

  query <- "SELECT * FROM behandling;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load behandling data from ", registryName, ": ", query)
    rapbase::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registryName, query)
}


#' @rdname queryData
#' @export
queryBehandlingNum <- function(registryName, reshId, ...) {

  query <- "SELECT * FROM behandlingnum;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load behandlingnum data from ", registryName, ": ", query)
    rapbase::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registryName, query)
}


#' @rdname queryData
#' @export
queryEnkeltLedd <- function(registryName, reshId, ...) {

  query <- "SELECT * FROM enkeltledd;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load enkeltledd data from ", registryName, ": ", query)
    rapbase::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registryName, query)
}


#' @rdname queryData
#' @export
queryEnkeltLeddNum <- function(registryName, reshId, ...) {

  query <- "SELECT * FROM enkeltleddnum;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load enkeltleddnum data from ", registryName, ": ", query)
    rapbase::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registryName, query)
}


#' @rdname queryData
#' @export
queryForlopsOversikt <- function(registryName, reshId, ...) {

  query <- "SELECT * FROM forlopsoversikt;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load forlopsoversikt data from ", registryName, ": ", query)
    rapbase::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registryName, query)
}


#' @rdname queryData
#' @export
queryReshNames <- function(registryName, ...) {

  query <- paste0("
SELECT
  AvdRESH as reshId,
  Sykehusnavn AS name,
  SykehusKort AS shortName
FROM
  brukerliste
GROUP BY
  AvdRESH,
  SykehusKort,
  Sykehusnavn
")

  if ("session" %in% names(list(...))) {
    msg = paste0("Load dept. names for each ReshId from ", registryName, ": ", query)
    rapbase::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registryName, query)
}

#' @rdname queryData
#' @export
querySkjemaOversikt <- function(registryName, reshId, ...) {

  query <- "SELECT * FROM skjemaoversikt;"

  if ("session" %in% names(list(...))) {
    msg = paste0("Load skjemaoversikt data from ", registryName, ": ", query)
    rapbase::repLogger(session = list(...)[["session"]], msg)
  }

  rapbase::loadRegData(registryName, query)
}
