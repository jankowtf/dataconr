
# IDataCon ---------------------------------------------------------------

#' @title
#' Class that functions as an interface for data connections
#'
#' @description
#' Defines the interface for data connections.
#'
#' @details
#' The terms \emph{interace} is used in a looser context than in more
#' rigid OOP contexts such as \emph{C#} or the like
#'
#' @section Getters/setters:
#'
#' \itemize{
#'  \item{getCached} {
#'  }
#'  \item{getCachedActive} {
#'  }
#'  \item{setCached} {
#'  }
#'  \item{setCachedActive} {
#'  }
#' }
#'
#' @section Public methods:
#'
#' \itemize{
#'  \item{initialize} {
#'  }
#'  \item{toExternalFormat} {
#'  }
#'  \item{toRFormat} {
#'  }
#'  \item{pull} {
#'  }
#'  \item{push} {
#'  }
#' }
#'
#' @template authors
#' @template references
#' @example inst/examples/IDataCon.R
#'
#' @import R6
#' @export
IDataCon <- R6Class(
  classname = "IDataCon",
  portable = TRUE,
  public = list(
    ## Methods //
    getCached = function(...) {},
    getCachedActive = function(...) {},
    setCached = function(...) {},
    setCachedActive = function(...) {},
    toExternalFormat = function(...) {},
    toRFormat = function(...) {},
    pull = function(...) {},
    push = function(...) {}
  )
)

# DataCon ---------------------------------------------------------------

#' @title
#' Generic class for inheritance for data connections
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#'
#' @field con \code{\link{ANY}}
#'  Connection to a data source
#' @field cached \code{\link{ANY}}
#'  Cached data state
#'
#' @section Getters/setters:
#'
#' \itemize{
#'  \item{See superclass} {
#'    \code{\link[idata]{IDataCon}}
#'  }
#' }
#'
#' @section Public methods:
#'
#' \itemize{
#'  \item{See interface} {
#'    \code{\link[idata]{IDataCon}}
#'  }
#' }
#'
#' @template authors
#' @template references
#' @example inst/examples/DataCon.R
#'
#' @import R6
#' @export
DataCon <- R6Class(
  classname = "DataCon",
  inherit = IDataCon,
  portable = TRUE,
  public = list(
    ## Fields //
    con = NULL,
    cached = NULL,
    meta = list(),

    ## Methods //
    initialize = function(
      ...,
      con = NULL,
      meta = list()
    ) {
      self$con <- con
      self$meta <- meta
    },
    getCached = function() {
      self$cached
    },
    getCachedActive = function() {
      self$cached_active
    },
    setCached = function(value) {
      self$cached <- value
    },
    setCachedActive = function(value) {
      self$cached_active <- value
    },
    toExternalFormat = function() {
      stop("DataCon: toExternalFormat: not implemented")
    },
    toRFormat = function(...) {
      stop("DataCon: toRFormat: not implemented")
    },
    pull = function(...) {
      stop("DataCon: pull: not implemented")
    },
    push = function(...) {
      stop("DataCon: push: not implemented")
    }
  )
)

