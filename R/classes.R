
# IDatasource ---------------------------------------------------------------

#' @title
#' General interface class that other interfaces inherit from/implement
#'
#' @description
#' This is the class that defines the overall interfaces that is inherited
#' from/implemented by other interfaces.
#'
#' @details
#' The terms \emph{interace} is used in a more loose context than in more
#' rigid OOP contexts such as \emph{C#} or the like
#'
#' @field con \code{\link{ANY}}
#'  Connection to a data source
#'
#' @section Methods:
#'
#' \itemize{
#'  \item{initialize} {
#'
#'    \itemize{
#'      \item{con }{\code{\link{ANY}}}
#'    }
#'  }
#' }
#'
#' @template authors
#' @template references
#' @example inst/examples/IDatasource.R
#'
#' @import R6
#' @export
IDatasource <- R6Class(
  classname = "IDatasource",
  portable = TRUE,
  public = list(
    ## Fields //
    con = NULL,

    ## Methods //
    initialize = function(
      con = NULL
    ) {
      self$con <- con
    }
  )
)

# Datasource.Neo4j ------------------------------------------------------------

#' @title
#' Connector for Neo4j
#'
#' @description
#' Main class to handle communication with Neo4j
#'
#' @details
#' TODO
#'
#' @field cache \code{\link{character}}
#'  File path to cached R object.
#'  Takes precedence over other meta information if specified
#'
#' @section Methods:
#'
#' \itemize{
#'  \item{initialize} {
#'
#'    \itemize{
#'      \item{path }{\code{\link{character}}}
#'    }
#'  }
#' }
#'
#' @template authors
#' @template references
#' @example inst/examples/Datasource.Neo4j.R
#'
#' @import R6
#' @export
Datasource.Neo4j <- R6Class(
  classname = "Datasource.Neo4j",
  portable = TRUE,
  public = list(
    ## Fields //
    con = NULL,

    ## Methods //
    initialize = function(
      con = NULL
    ) {
      self$con <- con
    },
    mapToList = function() {

    }
  )
)
