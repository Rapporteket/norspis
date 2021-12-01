#' Functions for processing raw NorSpis data
#'
#' @param df Data frame of registry data
#' @param id Vector of unique ids for organization names. Must be of the same
#' length as \code{name}. Normally this should be an ReshId.
#' @param name Vector of unique organization names for corresponding ids. Must
#' be of the same length as \code{id}.
#'
#' @return A data frame of registry data with a new column providing department
#' or organizational name based on AvdRESH
#' @export
#'
#' @examples
#' # sample data
#' df <- data.frame(AvdRESH = c(1, 2, 3))
#' id <- c(1, 2, 3)
#' name <- c("A", "B", "C")
#'
#' addDeptName(df, code, value)

addDeptName <- function(df, id, name) {

  stopifnot("AvdRESH" %in% names(df))
  stopifnot(length(id) == length(name))

  dplyr::left_join(
    df, data.frame(id, deptName = name), by = c("AvdRESH" = "id")
  )
}
