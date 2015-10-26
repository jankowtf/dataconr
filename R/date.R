getTimeId <- function(x, col, suffix = "_id") {
  id_0 <- sort(unique(x[[col]]))
  # id_1 <- seq(along = as.character(id_0))
  id_1 <- 1:length(id_0)
  structure(data.frame(
    id_0,
    id_1,
    stringsAsFactors = FALSE
  ), names = c(col, paste0(col, suffix)))

  #   id_0 <- table(x[[col]])
  #   id_1 <- names(id_0)
  #   id_2 <- seq(along = id_1)
  #   id <- lapply(seq(along = id_0), function(ii) {
  # #     structure(data.frame(
  # #       rep(id_1[ii], id_0[ii]),
  # #       rep(id_2[ii], id_0[ii]),
  # #       stringsAsFactors = FALSE
  # #     ), names = c(col, paste0(col, suffix)))
  #     structure(data.frame(
  #       rep(id_2[ii], id_0[ii]),
  #       stringsAsFactors = FALSE
  #     ), names = paste0(col, suffix))
  #   })
  # do.call(rbind, id)
}

getDateAtoms <- function(
  x,
  as_list = FALSE,
  suffix = character(),
  drop_date_in_name = FALSE,
  with_ids = FALSE,
  include_date = FALSE,
  posix = FALSE
) {

  date_this <- x
  # if (include_date) {
  out <- structure(data.frame(
    # date = date_this,
    date = as.character(date_this),
    date_day = format(date_this, format = "%Y-%m-%d"),
    date_year = format(date_this, format = "%Y"),
    date_month = format(date_this, format = "%m"),
    date_week = format(date_this, format = "%V"),
    date_day_year = format(date_this, format = "%j"),
    date_day_month = format(date_this, format = "%d"),
    date_day_week = format(date_this, format = "%u"),
    date_hour = format(date_this, format = "%H"),
    date_minute = format(date_this, format = "%M"),
    date_second = format(date_this, format = "%S"),
    stringsAsFactors = FALSE
  ), class = c("DateAtoms", "data.frame"))
  #   } else {
  #     out <- structure(data.frame(
  #       date_day = format(date_this, format = "%Y-%m-%d"),
  #       date_year = format(date_this, format = "%Y"),
  #       date_month = format(date_this, format = "%m"),
  #       date_week = format(date_this, format = "%V"),
  #       date_day_year = format(date_this, format = "%j"),
  #       date_day_month = format(date_this, format = "%d"),
  #       date_day_week = format(date_this, format = "%u"),
  #       date_hour = format(date_this, format = "%H"),
  #       date_minute = format(date_this, format = "%M"),
  #       date_second = format(date_this, format = "%S"),
  #       stringsAsFactors = FALSE
  #     ), class = c("DateAtoms", "data.frame"))
  #   }

  # x <- c("2012-01-01 00:15", "2012-01-01 00:30", "2012-01-02 00:15", "2012-01-02 00:30",
  #   "2012-02-01 00:15", "2012-02-01 00:30", "2012-02-02 00:15", "2012-02-02 00:30",
  #   "2013-01-01 00:15", "2013-01-01 00:30", "2013-01-02 00:15", "2013-01-02 00:30"
  # )
  # date_this <- as.POSIXlt(x)
  ## IDs //
  #   x=out
  #   col=nms[[3]]
  if (with_ids) {
    nms <- names(out)

    for (col in nms) {
      out <- merge(out, getTimeId(x = out, col = col), sort = FALSE)
    }
    # head(out)
    # out <- out[ , c(nms, setdiff(names(out), nms))]

    #     class(out$date)
    #     class(date_this)
    #     idx <- match(as.character(date_this), out$date)
    idx <- match(date_this, out$date)
    # idx <- match(as.character(date_this), as.character(out$date))
    # table(diff(idx))
    # which(diff(idx) != 1)
    # idx <- which(is.na(x))
    out <- out[idx , c(nms, setdiff(names(out), nms))]
    row.names(out) <- NULL
    # head(out)
  }
  if (!include_date) {
    out <- out[ , -which(names(out) == "date")]
  } else {
    if (posix) {
      out$date <- as.POSIXlt(out$date)
    }
  }
  if (length(suffix)) {
    names(out) <- paste0(as.character(suffix), "_", names(out))
  }
  if (drop_date_in_name) {
    names(out) <- gsub("date_", "", names(out))
  }
  if (as_list) {
    out <- structure(as.list(out), class = c("DateAtoms", "list"))
  }
  out
}
