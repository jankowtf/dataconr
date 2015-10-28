
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
#'  \item{cacheRStructure} {
#'    TODO
#'  }
#'  \item{cacheExternalStructure} {
#'    TODO
#'  }
#'  \item{applyRStructure} {
#'    TODO
#'  }
#'  \item{applyExternalStructure} {
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
    getInjected = function() {},
    setInjected = function() {},
    getRMetaFormat = function() {},
    setRMetaFormat = function() {},
    getExternalMetaFormat = function() {},
    setExternalMetaFormat = function() {},
    getRStructure = function() {},
    setRStructure = function() {},
    getExternalStructure = function() {},
    setExternalStructure = function() {},
    applyRMetaFormat = function() {},
    applyExternalMetaFormat = function() {},
    applyRFormat = function() {},
    applyExternalFormat = function() {},
    cacheRStructure = function() {},
    cacheExternalStructure = function() {},
    applyRStructure = function() {},
    applyExternalStructure = function() {}
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
