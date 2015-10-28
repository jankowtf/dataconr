#' @title
#' Format to R format
#'
#' @description
#' Formats data to R format.
#'
#' @details
#' TODO
#'
#' @param con A connection object
#' @return See respective methods.
#' @example inst/examples/example-applyRFormat.R
#' @export
applyRFormat <- function(con, ...) {
  UseMethod("applyRFormat", con)
}

#' @title
#' Format to external format
#'
#' @description
#' Formats data to external format.
#'
#' @details
#' TODO
#'
#' @param con A connection object
#' @return See respective methods.
#' @example inst/examples/example-applyExternalFormat.R
#' @export
applyExternalFormat <- function(con) {
  UseMethod("applyExternalFormat", con)
}

#' @title
#' Pull from a connection
#'
#' @description
#' Pulls data from a connection.
#'
#' @details
#' TODO
#'
#' @param con A connection object
#' @return See respective methods.
#' @example inst/examples/example-pullFromCon.R
#' @export
pullFromCon <- function(con) {
  UseMethod("pullFromCon", con)
}
