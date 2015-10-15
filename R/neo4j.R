#' @export
makeNeo4jProof <- function(data) {
  data_2 <- data
  classes <- lapply(data_2, class)
  classes_invalid <- c(
    "POSIXlt",
    "POSIXct"
  )
  # col=1
  for (col in 1:length(classes)) {
    if (any(classes[[col]] %in% classes_invalid)) {
      colname <- names(classes)[col]
      data_2[[colname]] <- as.character(data_2[[colname]])
    }
  }
  data_2
}



