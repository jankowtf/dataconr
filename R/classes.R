
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
#'  \item{asExpectedInDatasource} {
#'
#'    \itemize{
#'      \item{input }{\code{\link{ANY}}}
#'    }
#'  }
#'  \item{asExpectedInR} {
#'
#'    \itemize{
#'      \item{input }{\code{\link{ANY}}}
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
    },
    asExpectedInDatasource = function(
      input
    ) {

    },
    asExpectedInR = function(
      input
    ) {

    }
  )
)

# Datasource ---------------------------------------------------------------

#' @title
#' Generic class that specific classes for data sources inherit from
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#'
#' @field con \code{\link{ANY}}
#'  Connection to a data source
#'
#' @section Methods:
#'
#' \itemize{
#'  \item{See superclass} {
#'    \code{\link[idata]{IDatasource}}
#'  }
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
#' @example inst/examples/Datasource.R
#'
#' @import R6
#' @export
Datasource <- R6Class(
  classname = "Datasource",
  inherit = IDatasource,
  portable = TRUE,
  public = list(
    ## Fields //
    con = NULL,
    hello_world = NULL,

    ## Methods //
    initialize = function(
      ...,
      hello_world = NULL
    ) {
      # self$con <- con
      super$initialize(...)
      # print(self)
      self$hello_world <- hello_world
    },
    asExpectedInDatasource = function(
      input
    ) {

    },
    asExpectedInR = function(
      input
    ) {

    }
  )
)

# Datasource.Neo4j ------------------------------------------------------------

#' @title
#' Connector for Neo4j
#'
#' @description
#' This class wraps objects for connecting to Neo4j and provides
#' respective methods.
#'
#' @details
#' TODO
#'
#' @field con \code{\link{graph}}
#'  Connection object
#'
#' @section Methods:
#'
#' \itemize{
#'  \item{See superclass} {
#'    \code{\link[idata]{IDatasource}}
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
  inherit = Datasource,
  portable = TRUE,
  public = list(
    ## Fields //
    con = "graph",

    ## Methods //
    initialize = function(
      ...
    ) {
      # self$con <- con
      super$initialize(...)
    },
    asExpectedInDatasource = function(
      input
    ) {
      ## TODO 2015-1015: implement mechanism for rule sets
      rules <- list()
      rules$classes_invalid <- c(
        "POSIXlt",
        "POSIXct"
      )

      input_2 <- input
      classes <- lapply(input_2, class)

      # col=1
      for (col in 1:length(classes)) {
        if (any(classes[[col]] %in% rules$classes_invalid)) {
          colname <- names(classes)[col]
          input_2[[colname]] <- as.character(input_2[[colname]])
        }
      }
      input_2
    },
    asExpectedInR = function(
      input
    ) {
      ## TODO 2015-1015: implement mechanism for rule sets
      rules <- list()
      rules$rapid <- list(
        function(input, name) {
          if (grepl("^date*", name)) {
            input[[name]] <- as.POSIXlt(input[[name]])
          }
          input
        }
      )

      input_2 <- input
      columns <- names(input)

      rules_this <- rules$rapid
      # col=columns[1]
      # rule=rules_this[[1]]

      for (col in columns) {
        for (rule in rules_this) {
          input_2 <- rule(input = input_2, name = col)
        }
      }
      input_2
    }
  )
)
