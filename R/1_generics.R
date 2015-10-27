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
#' @example inst/examples/toRFormat.R
#' @export
toRFormat <- function(con, ...) {
  UseMethod("toRFormat", con)
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
#' @example inst/examples/toExternalFormat.R
#' @export
toExternalFormat <- function(con) {
  UseMethod("toExternalFormat", con)
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
#' @example inst/examples/pullFromCon.R
#' @export
pullFromCon <- function(con) {
  UseMethod("pullFromCon", con)
}
