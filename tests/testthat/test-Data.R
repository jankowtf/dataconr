
# Instantiate -------------------------------------------------------------

context("Data: instantiate")

test_that("Data: instantiate: empty", {
  expect_is(inst <- Data$new(), "Data")
  expect_true(inherits(inst, "IData"))
})

test_that("Data: instantiate: values", {
  format_dummy <- DataFormat$new()
  inst <- Data$new(
    data = data.frame(letters),
    r_format = format_dummy,
    ext_format = format_dummy
  )
  expect_identical(inst$data, data.frame(letters))
  expect_identical(inst$r_format, format_dummy)
  expect_identical(inst$ext_format, format_dummy)
})

# Apply format -------------------------------------------------------

context("Data: apply format")

test_that("Data: apply format", {
  data_r <- data.frame(
    x_1 = seq(10, 20, 5),
    x_2 = seq(as.POSIXct("2015-01-01"), length.out = 3, by = "1 days"),
    x_3 = TRUE
  )
  data_ext <- data.frame(
    x.1 = seq(10, 20, 5),
    x.2 = as.character(seq(as.POSIXlt("2015-01-01"), length.out = 3, by = "1 days")),
    x.3 = TRUE
  )

  r_format <- DataFormat$new(
    format = list(
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
  )
  ext_format <- DataFormat$new(
    format = list(
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
  #   Data$debug("applyRFormat")
  #   Data$undebug("applyRFormat")

  ## To R //
  inst <- Data$new(data = data_ext, r_format = r_format)
  expect_is(res <- inst$applyRFormat(), "data.frame")
  expect_true(nrow(res) > 0)
  expect_false(all(grepl("\\.", names(inst$data))))
  expect_true(inherits(inst$data$x_2, "POSIXct"))

  inst <- Data$new(data = data_ext, r_format = r_format)
  inst$getRFormat()$setStructure(inst$getData())
  # inst$getRFormat()$getStructure()
  res <- inst$applyRFormat()

  ## To external //
  inst <- Data$new(data = data_r, ext_format = ext_format)
  expect_is(res <- inst$applyExternalFormat(), "data.frame")
  expect_true(nrow(res) > 0)
  expect_false(all(grepl("_", names(inst$data))))
  expect_true(inherits(inst$data$x.2, "character"))

  inst <- Data$new(data = data_r, ext_format = ext_format)
  inst$getExternalFormat()$setStructure(inst$getData())
  # inst$getExternalFormat()$getStructure()
  res <- inst$applyExternalFormat()
})

# Apply injected format ---------------------------------------------------

context("Data: apply injected format")

test_that("Data: apply injected format: R", {
  skip("Not implemented yet")

  data <- data.frame(
    x_1 = 10,
    x_2 = as.POSIXlt("2015-01-01"),
    x_3 = TRUE
  )
  injected <- DataCon.IntelligentForecaster.Csv$new()

  Data$debug("applyRFormat")
  inst <- Data$new(data = data, injected = injected)
  inst$getInjected()
  inst$applyInjectedRFormat(type = "r")
})

