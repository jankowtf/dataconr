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
#' @example inst/examples/example-DataCon.IntelligentForecaster.R
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
    cached = "IData",
    meta = list(),

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
#' @example inst/examples/example-DataCon.IntelligentForecaster.Csv.R
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
    cached = "IData",
    meta = list(),

    ## Methods //
    initialize = function(
      ...,
      meta = list(
        ## Format //
        applyRFormat = list(
          extended = FALSE,
          with_ids = FALSE
        ),

        ## Pull //
        pull = list(
          format = TRUE,
          overwrite = FALSE
        )
      ),
      factory
    ) {
      super$initialize(...)
      self$meta <- meta

      ## Factory //
      if (!missing(factory)) {
        private$factory(factory)
      }
    },
    applyExternalFormat = function(...) {
      ## TODO 2015-10-19: implement advanced csv writer
      self$getCached()$getData()
    },
    applyRFormat = function(
      extended = self$meta$applyRFormat$extended,
      with_ids = self$meta$applyRFormat$with_ids,
      ...
    ) {
      data <- applyRFormat(
        con = self,
        extended = extended,
        with_ids = with_ids,
        ...
      )
      self$getCached()$setData(data)
    },
    pull = function(
      format = self$meta$pull$format,
      overwrite = self$meta$pull$overwrite,
      ...
    ) {
      if (!length(self$getCached()$getData()) || overwrite) {
        ## Retrieve //
        data <- pullFromCon(con = self)

        ## Cache //
        self$getCached()$setData(data)

        ## Set Structure //
        self$getCached()$getExternalFormat()$setStructure(
          self$getCached()$getData()
        )

        ## Inject //
        self$getCached()$setInjected(self)

        ## Format //
        if (format) {
          # self$meta$applyRFormat$extended <- TRUE
  #         self$getCached()$getInjected()$meta$applyRFormat$extended
          self$getCached()$applyInjectedRFormat(...)
          self$getCached()$applyRFormat()

          ## Set Structure
          self$getCached()$getRFormat()$setStructure(
            self$getCached()$getData()
          )
        }
      } else {
        warning(sprintf("%s: pull: cached data exists (no overwrite)",
          class(self)[1]))
      }
      self$getCached()$getData()
    },
    push = function(

    ) {
      methodNotImplemented(self)
    }
  ),
#   active = list(
#     cached_active = function(
#       value
#     ) {
#       if (missing(value)) {
#         if (!length(self$getCached()$getData())) {
#           self$pull()
#         }
#       } else {
#         self$getCached()$setData(value)
#       }
#       self$getCached()$getData()
#     }
#   ),
  private = list(
    factories = list(
      production = function() {
        DataCon.IntelligentForecaster.Csv$new(
          cached = Data$new(
            r_format = DataFormat$new(),
            ext_format = DataFormat$new()
          )
        )
      },
      test = function(
        con
      ) {
        DataCon.IntelligentForecaster.Csv$new(
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
DataCon.IntelligentForecaster.Csv$factories <- DataCon.IntelligentForecaster.Csv$private_fields$factories

# applyRFormat.DataCon.IntelligentForecaster.Csv ----------------------------------

#' @title
#' Format to R format
#'
#' @description
#' Formats data to R format for objects of class
#' \code{\link[idata]{DataCon.IntelligentForecaster.Csv}}
#'
#' @details
#' TODO
#'
#' @param con \code{\link[idata]{DataCon.IntelligentForecaster.Csv}}.
#' @return Formated \code{\link[base]{data.frame}}.
#' @example inst/examples/example-applyRFormat.R
#' @export
applyRFormat.DataCon.IntelligentForecaster.Csv <- function(
  con,
  extended = FALSE,
  with_ids = FALSE,
  ...
) {
  data <- con$getCached()$getData()

  if (!length(data)) {
    stop("applyRFormat.DataCon.IntelligentForecaster.Csv: no data available")
  }

  ## Meta arguments //
  date_col = "date"
  date_format = "%m/%d/%Y %H:%M:%S"
  ## TODO 2015-10-28: possibly make those accessible via arguments or options

  ## Names //
  nms <- tolower(names(data))
  nms <- gsub("^moment$", "date", nms)
  names(data) <- nms

  date_format_fallback <- "%d.%m.%Y %H:%M:%S"
  datevec <- data[ , date_col]
  posix <- as.POSIXlt(datevec, format = date_format)
  tmp <- as.character(posix)
  if (all(is.na(tmp))) {
    # which(is.na(datevec))
    posix <- as.POSIXlt(datevec, format = date_format_fallback)
    tmp <- as.character(posix)
  }
  if (all(is.na(tmp))) {
    stop("applyRFormat: invalid date format")
  }

  ## New date vector //
  data$date <- posix
  # idx <- which(is.na(data$date))
  # data[idx,]
  # data$date[idx]
  ## NOTE 2015-09-09 //
  ## Sometimes there are missing observations which are encoded as `Missing`
  ## coming from IF but have NA in the date colum
  ## This leads to errors/undesired results
  ## --> check back with HK on how to handle this

  if (extended) {
    date_atoms <- getDateAtoms(
      data$date,
      with_ids = with_ids,
      include_date = TRUE
    )

    ## Match to make absolutely sure that order is correct //
    idx <- match(data$date, date_atoms$date)
    # table(diff(idx))
    date_atoms_2 <- date_atoms[idx , -which(names(date_atoms) == "date")]
    # names(date_atoms_2)
    # names(data)
    data <- cbind(data, date_atoms_2)
  }

  idx_na <- is.na(colnames(data))
  data <- data[ , !idx_na]
  idx <- grep("^id$|^date$|^value$", colnames(data))
  idx <- c(idx, setdiff(1:ncol(data), idx))

  data <- data[ , idx]

  ## Ensuring correct decimal separator //
  data$value <- as.numeric(gsub(",", ".", data$value))

  data
}

# pullFromCon -------------------------------------------------------------

#' @title
#' Pull from DataCon.IntelligentForecaster.Csv data connection
#'
#' @description
#' Pulls data from a connection object of class
#' \code{\link[idata]{DataCon.IntelligentForecaster.Csv}}
#'
#' @details
#' TODO
#'
#' @param con \code{\link[idata]{DataCon.IntelligentForecaster.Csv}}.
#' @return \code{\link[base]{data.frame}}.
#' @example inst/examples/example-pullFromCon.R
#' @export
pullFromCon.DataCon.IntelligentForecaster.Csv <- function(
  con
) {
  path <- con$con
  data <- read.csv2(path, sep = ";", dec = ",", header = FALSE,
    stringsAsFactors = FALSE)[-1, ]
#   data <- read.csv2(path, sep = ";", dec = ",", header = FALSE,
#     stringsAsFactors = FALSE)
  # head(data)
  # nrow(data)
  # nms <- tolower(as.character(data[1, ]))
  nms <- as.character(data[1, ])
  # nms <- gsub("^moment$", "date", nms)
  data <- data[-1, ]
  names(data) <- nms
  row.names(data) <- NULL

  data
}
