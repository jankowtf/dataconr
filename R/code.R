
# Create list index -------------------------------------------------------

#' @export
createListIndex <- function(x, sep = c("[['", "']][['", "']]")) {
  paste0(sep[1], paste(x, collapse = sep[2]), sep[3])
}


