
# DataFormat --------------------------------------------------------------

#' @title
#' Superclass for data format information
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#'
#' @field format \code{\link{list}}
#'  Data format information
#'
#' @section Getters/setters:
#'
#' \itemize{
#'  \item{See interface} {
#'    \code{\link[idata]{IDataFormat}}
#'  }
#' }
#'
#' @section Public methods:
#'
#' \itemize{
#'  \item{See interface} {
#'    \code{\link[idata]{IDataFormat}}
#'  }
#' }
#'
#' @template authors
#' @template references
#' @example inst/examples/example-DataFormat.R
#'
#' @import R6
#' @export
DataFormat <- R6Class(
  classname = "DataFormat",
  inherit = IDataFormat,
  portable = TRUE,
  public = list(
    ## Fields //
    format = list(),
    struc = list(),

    ## Methods //
    initialize = function(
      format = list(),
      struc = list()
    ) {
      self$format <- format
      self$struc <- struc
    },
    getFormat = function() {
      self$format
    },
    setFormat = function(value) {
      self$format <- value
    },
    getStructure = function() {
      self$struc
    },
    setStructure = function(value) {
      # self$struc <- getStructure(value)
      self$struc <- handleSetStructure(value)
    }
  )
)

# Data ------------------------------------------------------------------

#' @title
#' Superclass for unified data respresentations
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#'
#' @field data \code{\link{ANY}}
#'  Actual data
#' @field r_format \code{\link{IDataFormat}}
#'  Instance of a class that implements the \code{\link[idata]{IDataFormat}}
#'  interface and that contains meta format information for R.
#' @field ext_format \code{\link{IDataFormat}}
#'  Instance of a class that implements the \code{\link[idata]{IDataFormat}}
#'  interface and that contains meta format information for external data
#'  location.
#' @field order \code{\link{ANY}}
#'  Data order information (rows and columns currently).
#'
#' @section Getters/setters:
#'
#' \itemize{
#'  \item{See superclass} {
#'    \code{\link[idata]{IData}}
#'  }
#' }
#'
#' @section Public methods:
#'
#' \itemize{
#'  \item{See interface} {
#'    \code{\link[idata]{IData}}
#'  }
#' }
#'
#' @template authors
#' @template references
#' @example inst/examples/example-Data.R
#'
#' @import R6
#' @export
Data <- R6Class(
  classname = "Data",
  inherit = IData,
  portable = TRUE,
  public = list(
    ## Fields //
    data = NULL,
    injected = "IDataCon",
    r_format = "IDataFormat",
    ext_format = "IDataFormat",

    ## Methods //
    initialize = function(
      data = data.frame(),
      injected = IDataCon$new(),
      r_format = IDataFormat$new(),
      ext_format = IDataFormat$new()
    ) {
      self$data <- data
      self$injected <- injected
      self$r_format <- r_format
      self$ext_format <- ext_format
    },
    getData = function() {
      self$data
    },
    setData = function(value) {
      self$data <- value
    },
    getInjected = function() {
      self$injected
    },
    setInjected = function(value) {
      self$injected <- value
    },
    getRFormat = function() {
      self$r_format
    },
    setRFormat = function(value) {
      self$r_format <- value
    },
    getExternalFormat = function() {
      self$ext_format
    },
    setExternalFormat = function(value) {
      self$ext_format <- value
    },
    applyRFormat = function(
      ...
    ) {
      data <- self$getData()
      ## Format //
      format <- self$getRFormat()$getFormat()
      if (length(format)) {
        for (handler in format) {
          data <- handler(data)
        }
      }
      ## Structure //
      struc <- self$getRFormat()$getStructure()
      if (length(struc)) {
        data <- applyStructure(struc, data = data, ...)
      }
      self$setData(data)
      # self$getData()
    },
    applyExternalFormat = function(
      ...
    ) {
      data <- self$getData()
      ## Format //
      format <- self$getExternalFormat()$getFormat()
      if (length(format)) {
        for (handler in format) {
          data <- handler(data)
        }
        self$setData(data)
      }
      ## Structure //
      struc <- self$getExternalFormat()$getStructure()
      if (length(struc)) {
        data <- applyStructure(struc, data = data, ...)
      }
      self$setData(data)
      self$data
    },
    applyInjectedRFormat = function(
      injected = self$getInjected(),
      ...
    ) {
      if (!is.null(injected)) {
        injected$applyRFormat(...)
      }
    },
    applyInjectedExternalFormat = function(
      injected = self$getInjected(),
      ...
    ) {
      if (!is.null(injected)) {
        injected$applyExternalFormat(...)
      }
    }
#     applyExternalStructure = function(
#       scope = c("columns", "rows", "both")
#     ) {
#       scope <- match.arg(scope, c("columns", "rows", "both"))
#       struc <- self$getRStructure()
#       data <- self$getData()
#       if (scope == "columns" && !is.null(idx <- struc$columns)) {
#         data <- data[ , idx]
#       } else if (scope == "rows" && !is.null(idx <- struc$rows)) {
#         data <- data[idx , ]
#       } else if (
#         scope == "both" &&
#           !is.null(idx_r <- struc$rows) &&
#           !is.null(idx_c <- struc$columns)
#       ) {
#         data <- data[idx_r , idx_c]
#       }
#       self$setData(data)
#     }
  )
)

# DataCon ---------------------------------------------------------------

#' @title
#' Superclass for inheritance for data connections
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
#'  \item{See interface} {
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
#' @example inst/examples/example-DataCon.R
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
    # cached = NULL,
    cached = "IData",
    meta = list(),

    ## Methods //
    initialize = function(
      con = NULL,
      meta = list(),
      cached = IData$new()
    ) {
      self$con <- con
      self$meta <- meta
      self$cached <- cached
    },
    applyExternalFormat = function() {
      methodNotImplemented(self)
    },
    applyRFormat = function(...) {
      methodNotImplemented(self)
    },
    getCached = function() {
      self$cached
    },
    setCached = function(value) {
      self$cached <- value
    },
    getCachedActive = function() {
      self$cached_active
    },
    setCachedActive = function(value) {
      self$cached_active <- value
    },
    getConnection = function() {
      self$con
    },
    setConnection = function(value) {
      self$con <- value
    },
    pull = function(...) {
      methodNotImplemented(self)
    },
    push = function(...) {
      methodNotImplemented(self)
    }
  ),
  active = list(
    cached_active = function(
      value
    ) {
      if (missing(value)) {
        if (!length(self$getCached()$getData())) {
          self$pull()
        }
      } else {
        self$getCached()$setData(value)
      }
      self$getCached()$getData()
    }
  ),
  private = list(
    factories = list(
      production = function() {
        DataCon$new(
          cached = Data$new(
            r_format = DataFormat$new(),
            ext_format = DataFormat$new()
          )
        )
      },
      test = function(
        con
      ) {
        DataCon$new(
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
DataCon$factories <- DataCon$private_fields$factories
