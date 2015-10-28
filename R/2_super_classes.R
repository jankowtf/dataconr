
# DataFormat --------------------------------------------------------------

#' @title
#' Generic class for inheritance for data format information
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
#'  \item{See superclass} {
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

    ## Methods //
    initialize = function(
      format = list()
    ) {
      self$format <- format
    },
    getFormat = function() {
      self$format
    },
    setFormat = function(value) {
      self$format <- value
    }
  )
)

# Data ------------------------------------------------------------------

#' @title
#' Generic class for inheritance for unified data respresentations
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#'
#' @field data \code{\link{ANY}}
#'  Actual data
#' @field r_meta_format \code{\link{IDataFormat}}
#'  Instance of a class that implements the \code{\link[idata]{IDataFormat}}
#'  interface and that contains meta format information for R.
#' @field ext_meta_format \code{\link{IDataFormat}}
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
    injected = NULL,
    r_meta_format = "IDataFormat",
    ext_meta_format = "IDataFormat",

    r_structure = list(),
    ext_structure = list(),

    ## Methods //
    initialize = function(
      data = data.frame(),
      injected = NULL,
      r_meta_format = IDataFormat$new(),
      ext_meta_format = IDataFormat$new(),
      r_structure = list(),
      ext_structure = list()
    ) {
      self$data <- data
      self$injected <- injected
      self$r_meta_format <- r_meta_format
      self$ext_meta_format <- ext_meta_format
      self$r_structure <- r_structure
      self$ext_structure <- ext_structure
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
    getRMetaFormat = function() {
      self$r_meta_format
    },
    setRMetaFormat = function(value) {
      self$r_meta_format <- value
    },
    getExternalMetaFormat = function() {
      self$ext_meta_format
    },
    setExternalMetaFormat = function(value) {
      self$ext_meta_format <- value
    },
    getRStructure = function() {
      self$r_structure
    },
    setRStructure = function(value) {
      self$r_structure <- value
    },
    getExternalStructure = function() {
      self$ext_structure
    },
    setExternalStructure = function(value) {
      self$ext_structure <- value
    },
    applyRFormat = function(
      injected = self$getInjected(),
      ...
    ) {
      if (!is.null(injected)) {
        injected$applyRFormat(...)
      }
    },
    applyExternalFormat = function(
      injected = self$getInjected(),
      ...
    ) {
      if (!is.null(injected)) {
        injected$applyExternalFormat(...)
      }
    },
    applyRMetaFormat = function() {
      data <- self$getData()
      format <- self$getRMetaFormat()$getFormat()
      if (length(format)) {
        for (handler in format) {
          data <- handler(data)
        }
        self$setData(data)
      }
      self$data
    },
    applyExternalMetaFormat = function() {
      data <- self$getData()
      format <- self$getExternalMetaFormat()$getFormat()
      if (length(format)) {
        for (handler in format) {
          data <- handler(data)
        }
        self$setData(data)
      }
      self$data
    },
    cacheRStructure = function() {
      struc <- list(
        rows = rownames(self$data),
        columns = names(self$data)
      )
      self$setRStructure(struc)
      struc
    },
    cacheExternalStructure = function() {
      struc <- list(
        rows = rownames(self$data),
        columns = names(self$data)
      )
      self$setExternalStructure(struc)
      struc
    },
    applyRStructure = function(
      scope = c("columns", "rows", "both")
    ) {
      scope <- match.arg(scope, c("columns", "rows", "both"))
      struc <- self$getRStructure()
      data <- self$getData()
      if (scope == "columns" && !is.null(idx <- struc$columns)) {
        data <- data[ , idx]
      } else if (scope == "rows" && !is.null(idx <- struc$rows)) {
        data <- data[idx , ]
      } else if (
          scope == "both" &&
          !is.null(idx_r <- struc$rows) &&
          !is.null(idx_c <- struc$columns)
      ) {
        data <- data[idx_r , idx_c]
      }
      self$setData(data)
    },
    applyExternalStructure = function(
      scope = c("columns", "rows", "both")
    ) {
      scope <- match.arg(scope, c("columns", "rows", "both"))
      struc <- self$getRStructure()
      data <- self$getData()
      if (scope == "columns" && !is.null(idx <- struc$columns)) {
        data <- data[ , idx]
      } else if (scope == "rows" && !is.null(idx <- struc$rows)) {
        data <- data[idx , ]
      } else if (
        scope == "both" &&
          !is.null(idx_r <- struc$rows) &&
          !is.null(idx_c <- struc$columns)
      ) {
        data <- data[idx_r , idx_c]
      }
      self$setData(data)
    }
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
    toExternalData = function() {
      stop("DataCon: toExternalData: not implemented")
    },
    toRData = function(...) {
      stop("DataCon: toRData: not implemented")
    },
    pull = function(...) {
      stop("DataCon: pull: not implemented")
    },
    push = function(...) {
      stop("DataCon: push: not implemented")
    }
  )
)
