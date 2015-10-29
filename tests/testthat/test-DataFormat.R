
# Instantiate -------------------------------------------------------------

context("DataFormat: instantiate")

test_that("DataFormat: instantiate: empty", {
  expect_is(inst <- DataFormat$new(), "DataFormat")
  expect_true(inherits(inst, "IDataFormat"))
  expect_identical(inst$format, list())
  expect_identical(inst$struc, list())
})

test_that("DataFormat: instantiate: values: format", {
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

test_that("DataFormat: instantiate: values: struc", {
  data <- data.frame(a = letters[1:5], b = letters[1:5])
#   target <- list(
#     rows = rownames(data),
#     columns = names(data),
#     structure = deparse(data)
#   )
  target <- getStructure(data)

  inst <- DataFormat$new(struc = target)
  expect_identical(inst$struc, target)
})


# Getters -----------------------------------------------------------------

context("DataFormat: getters/setters")

test_that("DataFormat: getters/setters", {
  ## Format //
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

  ## Structure //
  data <- data.frame(a = letters[1:5], b = letters[1:5])
  target <- getStructure(data)
  inst <- DataFormat$new(struc = target)
  expect_identical(inst$getStructure(), target)
  expect_identical(inst$setStructure(data), target)
  expect_identical(inst$getStructure(), target)
})

