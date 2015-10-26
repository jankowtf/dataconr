
# Generic -----------------------------------------------------------------

#' @title
#' Format to R format
#'
#' @description
#' Formats data to R format.
#'
#' @details
#' TODO
#'
#' @param con A connection object
#' @return See respective methods.
#' @example inst/examples/toRFormat.R
#' @export
toRFormat <- function(con) {
  UseMethod("toRFormat", con)
}

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
  with_ids = FALSE
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
  data
}
