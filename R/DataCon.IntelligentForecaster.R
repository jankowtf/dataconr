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
    meta = list(),

    ## Methods //
    initialize = function(
      ...,
      meta = list(
        ## Format //
        toRFormat = list(
          date_col = "date",
          date_format = "%m/%d/%Y %H:%M:%S",
          extended = FALSE,
          with_ids = FALSE
        ),

        ## Pull //
        pull = list(
          format = TRUE,
          cache = TRUE,
          overwrite = FALSE
        )
      )
    ) {
      super$initialize(...)
      self$meta <- meta
    },
    toExternalFormat = function() {
      ## TODO 2015-10-19: implement advanced csv writer
      self$cached
      # TRUE
    },
    toRFormat = function(
      date_col = self$meta$toRFormat$date_col,
      date_format = self$meta$toRFormat$date_format,
      extended = self$meta$toRFormat$extended,
      with_ids = self$meta$toRFormat$with_ids
    ) {
      ## TODO 2015-10-26: implement private options
      toRFormat(
        con = self,
        data_col = date_col,
        date_format = date_format,
        extended = extended,
        with_ids = with_ids
      )
    },
    pull = function(
      format = self$meta$pull$format,
      cache = self$meta$pull$cache,
      overwrite = self$meta$pull$overwrite,
      ...
    ) {
      data <- pullFromCon(con = self)
      if (cache) {
        self$cached <- data
      }
      if (format) {
        # data <- toRFormat(con = self, ...)
        data <- self$toRFormat(...)
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
          self$pull()
        }
      } else {
        self$cached <- value
      }
      self$cached
    }
  )
)

# toRFormat.DataCon.IntelligentForecaster.Csv ----------------------------------

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
#' @example inst/examples/toRFormat.R
#' @export
toRFormat.DataCon.IntelligentForecaster.Csv <- function(
  con,
  date_col = "date",
  date_format = "%m/%d/%Y %H:%M:%S",
  extended = FALSE,
  with_ids = FALSE,
  ...
) {
  data <- con$cached
  if (!length(data)) {
    stop("toRFormat.DataCon.IntelligentForecaster.Csv: no data available")
  }
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
    stop("toRFormat :: invalid date format")
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
    date_atoms <- getDateAtoms(data$date, with_ids = with_ids, include_date = TRUE)

    ## Match to make absolutely sure that order is correct //
    idx <- match(data$date, date_atoms$date)
    # table(diff(idx))
    date_atoms_2 <- date_atoms[idx , -which(names(date_atoms) == "date")]
    # names(date_atoms_2)
    # names(data)
    data <- cbind(data, date_atoms_2)
  }

  # print(data[data$date_day == "2012-12-24", ])

  idx_na <- is.na(colnames(data))
  data <- data[ , !idx_na]
  idx <- grep("^id$|^date", colnames(data))
  idx <- c(idx, setdiff(1:ncol(data), idx))

  data <- data[ , idx]

  ## Ensuring correct decimal separator //
  data$value <- as.numeric(gsub(",", ".", data$value))

  ## Saving meta information //
  con$meta$toRFormat$columns <- names(data)

  data
}

# DataCon.IntelligentForecaster.Csv ---------------------------------------

#' @title
#' Pull from a connection
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
#' @example inst/examples/pullFromCon.R
#' @export
pullFromCon.DataCon.IntelligentForecaster.Csv <- function(
  con
) {
  path <- con$con
  data <- read.csv2(path, sep = ";", dec = ",", header = FALSE,
    stringsAsFactors = FALSE)[-1, ]
  # head(data)
  # nrow(data)
  nms <- tolower(as.character(data[1, ]))
  nms <- gsub("^moment$", "date", nms)
  data <- data[-1, ]
  names(data) <- nms
  data
}
