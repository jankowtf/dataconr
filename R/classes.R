
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

    ## Methods //
    initialize = function(
      ...,
      con = NULL
    ) {
      self$con <- con
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

# DataCon.IntelligentForecaster ----------------------------------------

#' @title
#' Connector for Intelligent Forecaster
#'
#' @description
#' This class wraps objects for connecting to Intelligent Forecaster and
#' provides respective methods.
#'
#' @details
#' TODO
#'
#' @field con \code{\link{character}}
#'  File path
#'
#' @section Getters/setters:
#'
#' \itemize{
#'  \item{See superclass} {
#'    \code{\link[idata]{DataCon}}
#'  }
#' }
#'
#' @section Public methods:
#'
#' \itemize{
#'  \item{See superclass} {
#'    \code{\link[idata]{DataCon}}
#'  }
#' }
#'
#' @template authors
#' @template references
#' @example inst/examples/DataCon.IntelligentForecaster.R
#'
#' @import R6
#' @export
DataCon.IntelligentForecaster <- R6Class(
  classname = "DataCon.IntelligentForecaster",
  inherit = DataCon,
  portable = TRUE,
  public = list(
    ## Fields //
    con = character(),
    cached = data.frame(),

    ## Methods //
    initialize = function(
      ...
    ) {
      super$initialize(...)
    }
  )
)

# DataCon.IntelligentForecaster.Csv -------------------------------------

#' @title
#' Connector for Intelligent Forecaster CSV format
#'
#' @description
#' This class wraps objects for connecting to data stored by
#' Intelligent Forecaster in a CSV format and provides respective methods.
#'
#' @details
#' TODO
#'
#' @field con \code{\link[base]{character}}
#'  File path
#' @field cached \code{\link[base]{data.frame}}
#'  Cached data state as data frame
#'
#' @section Getters/setters:
#'
#' \itemize{
#'  \item{See superclass} {
#'    \code{\link[idata]{DataCon}}
#'  }
#' }
#'
#' @section Methods:
#'
#' \itemize{
#'  \item{See superclass} {
#'    \code{\link[idata]{DataCon}}
#'  }
#' }
#'
#' @template authors
#' @template references
#' @example inst/examples/DataCon.IntelligentForecaster.Csv.R
#'
#' @import R6
#' @export
DataCon.IntelligentForecaster.Csv <- R6Class(
  classname = "DataCon.IntelligentForecaster.Csv",
  inherit = DataCon.IntelligentForecaster,
  portable = TRUE,
  public = list(
    ## Fields //
    con = character(),
    cached = data.frame(),

    ## Methods //
    initialize = function(
      ...
    ) {
      super$initialize(...)
    },
    toExternalFormat = function() {
      ## TODO 2015-10-19: implement advanced csv writer
      self$cached
      # TRUE
    },
    toRFormat = function(
      cache = TRUE,
      date_col = "date",
      date_format = "%m/%d/%Y %H:%M:%S",
      extended = FALSE,
      with_ids = FALSE
    ) {
      toRFormat(con = self,
        data_col = date_col,
        date_format = date_format,
        extended = extended,
        with_ids = with_ids
      )
    },
    pull = function(
      format = TRUE,
      cache = TRUE,
      overwrite = FALSE
    ) {
      data <- pullFromCon(con = self)
      if (cache) {
        self$cached <- data
      }
      if (format) {
        data <- toRFormat(con = self)
        if (cache) {
          self$cached <- data
        }
      }
      data
    },
    push = function(

    ) {
      stop("DataCon.IntelligentForecaster.Csv: push: not implemented yet ")
    }
  ),
  active = list(
    cached_active = function(
      value
    ) {
      if (missing(value)) {
        if (!length(self$cached)) {
          self$pull(cache = TRUE, format = TRUE)
        }
      } else {
        self$cached <- value
      }
      self$cached
    }
  )
)

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
#' @example inst/examples/DataCon.Neo4j.R
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
    cached = data.frame(),

    ## Methods //
    initialize = function(
      ...
    ) {
      super$initialize(...)
    },
    toExternalFormat = function(
      con
    ) {
      toExternalFormat(con = self)
    },
    toRFormat = function(
      con
    ) {
      toRFormat(con = self)
    },
    pull = function(...) {
      stop("DataCon.Neo4j: pull: not implemented yet ")
    },
    push = function(

    ) {
      stop("DataCon.Neo4j: push: not implemented yet ")
    }
  )
)
