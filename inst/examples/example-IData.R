
# Example data ------------------------------------------------------------

data_r <- data.frame(
  x_1 = seq(10, 20, 5),
  x_2 = seq(as.POSIXct("2015-01-01"), length.out = 3, by = "1 days"),
  x_3 = TRUE,
  stringsAsFactors = FALSE
)
data_ext <- data.frame(
  x.1 = seq(10, 20, 5),
  x.2 = as.character(seq(as.POSIXlt("2015-01-01"), length.out = 3, by = "1 days")),
  x.3 = TRUE,
  stringsAsFactors = FALSE
)

# Example meta format -----------------------------------------------------

meta_format <- list(
  r = list(
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
  ),
  ext = list(
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
)

# Apply R format  ---------------------------------------------------------

inst <- Data$new(data = data_ext, meta_format = meta_format)
data_before <- inst$getData()
inst$applyMetaFormat(type = "r")
data_after <- inst$getData()

## Names comparison //
names(data_before)
names(data_after)

## Class comparison //
sapply(data_before, class)
sapply(data_after, class)

# Apply external format ---------------------------------------------------

inst <- Data$new(data = data_r, meta_format = meta_format)
data_before <- inst$getData()
inst$applyMetaFormat(type = "ext")
data_after <- inst$getData()

## Names comparison //
names(data_before)
names(data_after)

## Class comparison //
sapply(data_before, class)
sapply(data_after, class)
