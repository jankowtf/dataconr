
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
#'  \item{getStructure} {
#'    TODO
#'  }
#'  \item{setStructure} {
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
    getFormat = function(...) {
      stopOnInterfaceCall(self)
    },
    setFormat = function(...) {
      stopOnInterfaceCall(self)
    },
    getStructure = function(...) {
      stopOnInterfaceCall(self)
    },
    setStructure = function(...) {
      stopOnInterfaceCall(self)
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
#'  \item{setRStructure} {
#'    TODO
#'  }
#'  \item{setRStructure} {
#'    TODO
#'  }
#'  \item{setExternalStructure} {
#'    TODO
#'  }
#'  \item{setExternalStructure} {
#'    TODO
#'  }
#' }
#'
#' @section Public methods:
#'
#' \itemize{
#'  \item{applyRFormat} {
#'    TODO
#'  }
#'  \item{applyExternalFormat} {
#'    TODO
#'  }
#'  \item{applyInjectedRFormat} {
#'    TODO
#'  }
#'  \item{applyInjectedExternalFormat} {
#'    TODO
#'  }
#'  \item{cacheRStructure} {
#'    TODO
#'  }
#'  \item{cacheExternalStructure} {
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
    getData = function(...) {
      stopOnInterfaceCall(self)
    },
    setData = function(...) {
      stopOnInterfaceCall(self)
    },
    getInjected = function(...) {
      stopOnInterfaceCall(self)
    },
    setInjected = function(...) {
      stopOnInterfaceCall(self)
    },
    getRFormat = function(...) {
      stopOnInterfaceCall(self)
    },
    setRFormat = function(...) {
      stopOnInterfaceCall(self)
    },
    getExternalFormat = function(...) {
      stopOnInterfaceCall(self)
    },
    setExternalFormat = function(...) {
      stopOnInterfaceCall(self)
    },
    applyRFormat = function(...) {
      stopOnInterfaceCall(self)
    },
    applyExternalFormat = function(...) {
      stopOnInterfaceCall(self)
    },
    applyInjectedRFormat = function(...) {
      stopOnInterfaceCall(self)
    },
    applyInjectedExternalFormat = function(...) {
      stopOnInterfaceCall(self)
    }
  )
)

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
#'  \item{applyExternalFormat} {
#'    TODO
#'  }
#'  \item{applyRFormat} {
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
#'  \item{\code{\link[idata]{DataCon.IntelligentForecaster}}}
#'  \item{\code{\link[idata]{DataCon.IntelligentForecaster.Csv}}}
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
    applyRFormat = function(...) {
      stopOnInterfaceCall(self)
    },
    applyExternalFormat = function(...) {
      stopOnInterfaceCall(self)
    },
    getCached = function(...) {
      stopOnInterfaceCall(self)
    },
    getCachedActive = function(...) {
      stopOnInterfaceCall(self)
    },
    setCached = function(...) {
      stopOnInterfaceCall(self)
    },
    setCachedActive = function(...) {
      stopOnInterfaceCall(self)
    },
    pull = function(...) {
      stopOnInterfaceCall(self)
    },
    push = function(...) {
      stopOnInterfaceCall(self)
    }
  )
)
