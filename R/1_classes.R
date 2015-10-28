
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
#'  \item{toExternalData} {
#'  }
#'  \item{toRData} {
#'  }
#'  \item{pull} {
#'  }
#'  \item{push} {
#'  }
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

# IData -----------------------------------------------------------------

#' @title
#' Class that functions as an interface for format information
#'
#' @description
#' Defines the interface for format information.
#'
#' @details
#' The terms \emph{interace} is used in a looser context than in more
#' rigid OOP contexts such as \emph{C#} or the like
#'
#' @section Getters/setters:
#'
#' \itemize{
#'  \item{getData} {
#'  }
#'  \item{setData} {
#'  }
#'  \item{getFormat} {
#'  }
#'  \item{setFormat} {
#'  }
#'  \item{getMetaFormat} {
#'  }
#'  \item{setMetaFormat} {
#'  }
#'  \item{getOrder} {
#'  }
#'  \item{setOrder} {
#'  }
#' }
#'
#' @section Public methods:
#'
#' \itemize{
#'  \item{initialize} {
#'  }
#'  \item{applyMetaFormat} {
#'  }
#'  \item{applyFormat} {
#'  }
#'  \item{cacheOrder} {
#'  }
#'  \item{applyOrder} {
#'  }
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
    getFormat = function() {},
    setFormat = function() {},
    getOrder = function() {},
    setOrder = function() {},
    getMetaFormat = function() {},
    setMetaFormat = function() {},
    applyMetaFormat = function() {},
    applyFormat = function() {},
    cacheOrder = function() {},
    applyOrder = function() {}
  )
)

# Data ------------------------------------------------------------------

#' @title
#' Generic class for inheritance for format information
#'
#' @description
#' TODO
#'
#' @details
#' TODO
#'
#' @field con \code{\link{ANY}}
#'  Connection to a data source
#' @field format \code{\link{list}}
#'  Data information
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
    format = list(),
    meta_format = list(),
    order = list(),

    ## Methods //
    initialize = function(
      data = data.frame(),
      meta_format = list(),
      format = list(),
      order = list()
    ) {
      self$data <- data
      self$meta_format <- meta_format
      self$format <- format
      self$order <- order
    },
    getData = function() {
      self$data
    },
    setData = function(value) {
      self$data <- value
    },
    getMetaFormat = function() {
      self$meta_format
    },
    setMetaFormat = function(value) {
      self$meta_format <- value
    },
    getFormat = function() {
      self$format
    },
    setFormat = function(value) {
      self$format <- value
    },
    getOrder = function() {
      self$order
    },
    setOrder = function(value) {
      self$order <- value
    },
    applyFormat = function(
      type = c("r", "ext")
    ) {
      stop("Data: applyFormat: not implemented yet")
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
    applyMetaFormat = function(
      type = c("r", "ext")
    ) {
      data <- self$data
      format <- self$meta_format
      type <- match.arg(type, c("r", "ext"))
      if (length(format)) {
        format <- format[[type]]
        for (handler in format) {
          data <- handler(data)
        }
        self$data <- data
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

