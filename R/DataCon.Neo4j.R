# DataCon.Neo4j ------------------------------------------------------------

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
#' @field cached \code{\link[base]{data.frame}}
#'  Cached data state as data frame
#'
#' @section Public methods:
#'
#' \itemize{
#'  \item{See superclass} {
#'    \code{\link[idata]{IDataCon}}
#'  }
#' }
#'
#' @template authors
#' @template references
#' @example inst/examples/example-DataCon.Neo4j.R
#'
#' @import R6
#' @export
DataCon.Neo4j <- R6Class(
  classname = "DataCon.Neo4j",
  inherit = DataCon,
  portable = TRUE,
  public = list(
    ## Fields //
    con = "graph",
    cached = "IData",

    ## Methods //
    initialize = function(
      ...
    ) {
      super$initialize(...)
    },
    applyExternalFormat = function(
      ...
    ) {
      applyExternalFormat(con = self, ...)
    },
    applyRFormat = function(
      ...
    ) {
      applyRFormat(con = self, ...)
    },
    pull = function(...) {
      methodNotImplemented(self)
    },
    push = function(

    ) {
      methodNotImplemented(self)
    }
  ),
  private = list(
    factories = list(
      production = function() {
        DataCon.Neo4j$new(
          cached = Data$new(
            r_format = DataFormat$new(),
            ext_format = DataFormat$new()
          )
        )
      },
      test = function(
        con
      ) {
        DataCon.Neo4j$new(
          con = con,
          cached = Data$new(
            r_format = DataFormat$new(),
            ext_format = DataFormat$new()
          )
        )
      }
    )
  )
)
DataCon.Neo4j$factories <- DataCon.Neo4j$private_fields$factories

# applyRFormat.DataCon.Neo4j -------------------------------------------------

#' @title
#' Format to R format
#'
#' @description
#' Formats data to R format for objects of class
#' \code{\link[idata]{DataCon.Neo4j}}
#'
#' @details
#' TODO
#'
#' @param con \code{\link[idata]{DataCon.Neo4j}}.
#' @return Formated \code{\link[base]{data.frame}}.
#' @example inst/examples/example-applyRFormat.R
#' @export
applyRFormat.DataCon.Neo4j <- function(
  con
) {
  data <- con$getCached()$getData()
  ## TODO 2015-1015: implement mechanism for rule sets
  rules <- list()
  rules$rapid <- list(
    function(data, name) {
      if (grepl("^date$", name)) {
#         print(name)
#         print(class(data[[name]]))
        data[[name]] <- as.POSIXlt(data[[name]])
      }
      data
    }
  )

  data_2 <- data
  columns <- names(data)

  rules_this <- rules$rapid
  # col=columns[1]
  # rule=rules_this[[1]]
# class(data$date)

  for (col in columns) {
    for (rule in rules_this) {
      data_2 <- rule(data = data_2, name = col)
    }
  }

  ## Meta information //
  if (length(idx <- con$getCached()$getRFormat()$getStructure())) {
    data_2 <- data_2[ , idx$columns]
  }

  data_2
}

# applyExternalFormat.DataCon.Neo4j ------------------------------------------

#' @title
#' Format to external format
#'
#' @description
#' Formats data to external format for objects of class
#' \code{\link[idata]{DataCon.Neo4j}}
#'
#' @details
#' TODO
#'
#' @param con \code{\link[idata]{DataCon.Neo4j}}.
#' @return Formated \code{\link[base]{data.frame}}.
#' @example inst/examples/example-applyExternalFormat.R
#' @export
applyExternalFormat.DataCon.Neo4j <- function(
  con
) {
  data <- con$getCached()$getData()
  ## TODO 2015-1015: implement mechanism for rule sets
  rules <- list()
  rules$classes_invalid <- c(
    "POSIXlt",
    "POSIXct"
  )

  data_2 <- data
  classes <- lapply(data_2, class)

  # col=1
  for (col in 1:length(classes)) {
    if (any(classes[[col]] %in% rules$classes_invalid)) {
      colname <- names(classes)[col]
      data_2[[colname]] <- as.character(data_2[[colname]])
    }
  }
  data_2
}
