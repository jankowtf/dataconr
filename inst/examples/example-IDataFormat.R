
# Example formats ----------------------------------------------------------

format_1 <- list(
  function(x, pattern = "\\d{4}-\\d{2}-\\d{2}") {
    tmp <- lapply(x, function(ii) {
      if (any(grepl(pattern, ii))) {
        as.POSIXlt(ii)
      } else {
        ii
      }
    })
    as.data.frame(tmp, stringsAsFactors = FALSE)
  },
  function(x) {
    names(x) <- gsub("\\.", "_", names(x))
    x
  }
)
format_2 <- list(
  function(x, pattern = "\\d{4}-\\d{2}-\\d{2}") {
    tmp <- lapply(x, function(ii) {
      if (any(grepl(pattern, ii))) {
        as.character(ii)
      } else {
        ii
      }
    })
    as.data.frame(tmp, stringsAsFactors = FALSE)
  },
  function(x) {
    names(x) <- gsub("_", ".", names(x))
    x
  }
)

# Instantiate -------------------------------------------------------------

format_1_inst <- DataFormat$new(format = format_1)
format_2_inst <- DataFormat$new(format = format_2)

# Methods -----------------------------------------------------------------

format_1_inst$getFormat()
format_1_inst$setFormat(NULL)
format_1_inst$getFormat()
format_1_inst$setFormat(format_2)
format_1_inst$getFormat()

format_2_inst$getFormat()
format_2_inst$setFormat(NULL)
format_2_inst$getFormat()
format_2_inst$setFormat(format_1)
format_2_inst$getFormat()

