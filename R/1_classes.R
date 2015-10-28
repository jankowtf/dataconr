
# IDataCon ---------------------------------------------------------------

#' @title
#' Class that functions as an interface for data connections
#'
#' @description
#' Defines the interface for data connections.
#'
#' @template interface_section
#'
#' @section Getters/setters:
#'
#' \itemize{
#'  \item{getCached} {
#'    TODO
#'  }
#'  \item{getCachedActive} {
#'    TODO
#'  }
#'  \item{setCached} {
#'    TODO
#'  }
#'  \item{setCachedActive} {
#'    TODO
#'  }
#' }
#'
#' @section Public methods:
#'
#' \itemize{
#'  \item{toExternalData} {
#'    TODO
#'  }
#'  \item{toRData} {
#'    TODO
#'  }
#'  \item{pull} {
#'    TODO
#'  }
#'  \item{push} {
#'    TODO
#'  }
#' }
#'
#' @section Known interface implementations:
#'
#' \itemize{
#'  \item{\code{\link[idata]{DataCon}}}
#' }
#'
#' @template authors
#' @template references
#' @example inst/examples/example-IDataCon.R
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
    toExternalData = function(...) {},
    toRData = function(...) {},
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
    cached = NULL,
    meta = list(),

    ## Methods //
    initialize = function(
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

# IDataFormat -----------------------------------------------------------------

#' @title
#' Class that functions as an interface for data format information
#'
#' @description
#' Defines the interface for data format information.
#'
#' @template interface_section
#'
#' @section Getters/setters:
#'
#' \itemize{
#'  \item{getFormat} {
#'    TODO
#'  }
#'  \item{setFormat} {
#'    TODO
#'  }
#' }
#'
#' @section Public methods:
#'
#' \itemize{
#'  TODO
#' }
#'
#' @section Known interface implementations:
#'
#' \itemize{
#'  \item{\code{\link[idata]{DataFormat}}}
#' }
#'
#' @template authors
#' @template references
#' @example inst/examples/example-IDataFormat.R
#'
#' @import R6
#' @export
IDataFormat <- R6Class(
  classname = "IDataFormat",
  portable = TRUE,
  public = list(
    ## Methods //
    getFormat = function() {},
    setFormat = function() {}
  )
)

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

# IData -----------------------------------------------------------------

#' @title
#' Class that functions as an interface for unified data respresentations
#'
#' @description
#' Defines the interface for unified data respresentations.
#'
#' @template interface_section
#'
#' @section Getters/setters:
#'
#' \itemize{
#'  \item{getData} {
#'    TODO
#'  }
#'  \item{setData} {
#'    TODO
#'  }
#'  \item{getRFormat} {
#'    TODO
#'  }
#'  \item{setRFormat} {
#'    TODO
#'  }
#'  \item{getExternalFormat} {
#'    TODO
#'  }
#'  \item{setExternalFormat} {
#'    TODO
#'  }
#'  \item{getRMetaFormat} {
#'    TODO
#'  }
#'  \item{setRMetaFormat} {
#'    TODO
#'  }
#'  \item{getExternalMetaFormat} {
#'    TODO
#'  }
#'  \item{setExternalMetaFormat} {
#'    TODO
#'  }
#'  \item{getOrder} {
#'    TODO
#'  }
#'  \item{setOrder} {
#'    TODO
#'  }
#' }
#'
#' @section Public methods:
#'
#' \itemize{
#'  \item{applyRMetaFormat} {
#'    TODO
#'  }
#'  \item{applyExternalMetaFormat} {
#'    TODO
#'  }
#'  \item{applyRFormat} {
#'    TODO
#'  }
#'  \item{applyExternalFormat} {
#'    TODO
#'  }
#'  \item{cacheOrder} {
#'    TODO
#'  }
#'  \item{applyOrder} {
#'    TODO
#'  }
#' }
#'
#' @section Known interface implementations:
#'
#' \itemize{
#'  \item{\code{\link[idata]{Data}}}
#' }
#'
#' @template authors
#' @template references
#' @example inst/examples/example-IData.R
#'
#' @import R6
#' @export
IData <- R6Class(
  classname = "IData",
  portable = TRUE,
  public = list(
    ## Methods //
    getData = function() {},
    setData = function() {},
    getRFormat = function() {},
    setRFormat = function() {},
    getExternalFormat = function() {},
    setExternalFormat = function() {},
    getRMetaFormat = function() {},
    setRMetaFormat = function() {},
    getExternalMetaFormat = function() {},
    setExternalMetaFormat = function() {},
    getOrder = function() {},
    setOrder = function() {},
    applyRMetaFormat = function() {},
    applyExternalMetaFormat = function() {},
    applyRFormat = function() {},
    applyExternalFormat = function() {},
    cacheOrder = function() {},
    applyOrder = function() {}
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
#' @field r_format \code{\link{IDataFormat}}
#'  Instance of a class that implements the \code{\link[idata]{IDataFormat}}
#'  interface and that contains specific format information for R.
#'  \strong{Not implemented/used yet}.
#' @field ext_format \code{\link{IDataFormat}}
#'  Instance of a class that implements the \code{\link[idata]{IDataFormat}}
#'  interface and that contains meta specific format information for external
#'  data locations.
#'  \strong{Not implemented/used yet}.
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
    r_meta_format = "IDataFormat",
    ext_meta_format = "IDataFormat",
    r_format = "IDataFormat",
    ext_format = "IDataFormat",

    order = list(),

    ## Methods //
    initialize = function(
      data = data.frame(),
      r_meta_format = IDataFormat$new(),
      ext_meta_format = IDataFormat$new(),
      r_format = IDataFormat$new(),
      ext_format = IDataFormat$new(),
      order = list()
    ) {
      self$data <- data
      self$r_meta_format <- r_meta_format
      self$ext_meta_format <- ext_meta_format
      self$r_format <- r_format
      self$ext_format <- ext_format
      self$order <- order
    },
    getData = function() {
      self$data
    },
    setData = function(value) {
      self$data <- value
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
    getOrder = function() {
      self$order
    },
    setOrder = function(value) {
      self$order <- value
    },
    applyRFormat = function(
      type = c("r", "ext")
    ) {
      stop("Data: applyRFormat: not implemented yet")
      data <- self$data
      format <- self$meta_format
      type <- match.arg(type, c("r", "ext"))
      if (length(format)) {
        lapply(format, function(this) {
          if (type == "r") {
            pattern_1 <- "_r"
            pattern_2 <- "_r_?"
          } else if (type == "ext") {
            pattern_1 <- "_ext"
            pattern_2 <- "_ext_?"
          }
          this <- this[grep(pattern_2, names(this), value = TRUE)]
          names(this) <- gsub(pattern_1, "", names(this))

          name <- this$name_r_handler(this$name_r)
          data[[this$name_r]]
        })
      }
    },
    applyExternalFormat = function() {
      stop("Data: applyExternalFormat: not implemented yet")
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
    cacheOrder = function() {
      ord <- list(
        rows = rownames(self$data),
        columns = names(self$data)
      )
      self$setOrder(ord)
      ord
    },
    applyOrder = function(
      scope = c("columns", "rows", "both")
    ) {
      scope <- match.arg(scope, c("columns", "rows", "both"))
      ord <- self$getOrder()
      data <- self$getData()
      if (scope == "columns" && !is.null(idx <- ord$columns)) {
        data <- data[ , idx]
      } else if (scope == "rows" && !is.null(idx <- ord$rows)) {
        data <- data[idx , ]
      } else if (
          scope == "both" &&
          !is.null(idx_r <- ord$rows) &&
          !is.null(idx_c <- ord$columns)
      ) {
        data <- data[idx_r , idx_c]
      }
      self$setData(data)
    }
  )
)

