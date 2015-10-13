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
#' @template author
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


    ## Methods //
    initialize = function(

    ) {

    }
  )
)
