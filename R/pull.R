
# Generic -----------------------------------------------------------------

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

# DataCon.IntelligentForecaster.Csv ---------------------------------------

#' @title
#' Pull from a connection
#'
#' @description
#' Pulls data from a connection object of class
#' \code{\link[idata]{DataCon.IntelligentForecaster.Csv}}
#'
#' @details
#' TODO
#'
#' @param con \code{\link[idata]{DataCon.IntelligentForecaster.Csv}}.
#' @return \code{\link[base]{data.frame}}.
#' @example inst/examples/pullFromCon.R
#' @export
pullFromCon.DataCon.IntelligentForecaster.Csv <- function(
  con
) {
  path <- con$con
  data <- read.csv2(path, sep = ";", dec = ",", header = FALSE,
    stringsAsFactors = FALSE)[-1, ]
  # head(data)
  nms <- tolower(as.character(data[1, ]))
  nms <- gsub("^moment$", "date", nms)
  data <- data[-1, ]
  names(data) <- nms
  data
}
