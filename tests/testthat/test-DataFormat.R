
# Instantiate -------------------------------------------------------------

context("DataFormat: instantiate")

test_that("DataFormat: instantiate: empty", {
  expect_is(inst <- DataFormat$new(), "DataFormat")
  expect_true(inherits(inst, "IDataFormat"))
  expect_identical(inst$format, list())
})

test_that("DataFormat: instantiate: values", {
  target <- list(
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
  inst <- DataFormat$new(format = target)
  expect_identical(inst$format, target)
})

# Getters/setters ---------------------------------------------------------

context("DataFormat: getters/setters")

test_that("DataFormat: getters/setters", {
  target <- list(
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
  inst <- DataFormat$new(format = target)
  expect_identical(inst$getFormat(), target)
  expect_identical(inst$setFormat(NULL), NULL)
  expect_identical(inst$getFormat(), NULL)
  expect_identical(inst$setFormat(target), target)
  expect_identical(inst$getFormat(), target)
})

