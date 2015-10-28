
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
    r_meta_format = format_dummy,
    ext_meta_format = format_dummy
  )
  expect_identical(inst$data, data.frame(letters))
  expect_identical(inst$r_meta_format, format_dummy)
  expect_identical(inst$ext_meta_format, format_dummy)
})

# Apply format -----------------------------------------------------------

context("Data: apply R format")

test_that("Data: apply R format", {
  skip("Not implemented yet")
  data <- data.frame(
    x_1 = 10,
    x_2 = as.POSIXlt("2015-01-01"),
    x_3 = TRUE
  )

  Data$debug("applyRFormat")
  inst <- Data$new(data = data, r_format = r_format)
  inst$applyFormat(type = "r")
})


# Apply meta format -------------------------------------------------------

context("Data: apply meta format")

test_that("Data: apply meta format", {
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

  r_meta_format <- DataFormat$new(
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
  ext_meta_format <- DataFormat$new(
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
  #   Data$debug("applyRMetaFormat")
  #   Data$undebug("applyRMetaFormat")

  ## To R //
  inst <- Data$new(data = data_ext, r_meta_format = r_meta_format)
  expect_is(res <- inst$applyRMetaFormat(), "data.frame")
  expect_true(nrow(res) > 0)
  expect_false(all(grepl("\\.", names(inst$data))))
  expect_true(inherits(inst$data$x_2, "POSIXct"))

  ## To external //
  inst <- Data$new(data = data_r, ext_meta_format = ext_meta_format)
  expect_is(res <- inst$applyExternalMetaFormat(), "data.frame")
  expect_true(nrow(res) > 0)
  expect_false(all(grepl("_", names(inst$data))))
  expect_true(inherits(inst$data$x.2, "character"))
})

# Structure -------------------------------------------------------------------

context("Data: cache structure")

test_that("Data: cache order", {
  data_r <- data.frame(
    x_1 = seq(10, 20, 5),
    x_2 = seq(as.POSIXct("2015-01-01"), length.out = 3, by = "1 days"),
    x_3 = TRUE,
    stringsAsFactors = FALSE
  )
# struc <- str(data_r)
# structure(data_r)
# eval(parse(text = deparse(data_r)))

  ## R structure //
  inst <- Data$new(data = data_r)
  expect_identical(inst$r_structure, list())
  expect_is(res <- inst$cacheRStructure(), "list")
  expect_identical(inst$r_structure, list(
    rows = c("1", "2", "3"),
    columns = c("x_1", "x_2", "x_3")
  ))

  ## External structure //
  inst <- Data$new(data = data_r)
  expect_identical(inst$ext_structure, list())
  expect_is(res <- inst$cacheExternalStructure(), "list")
  expect_identical(inst$ext_structure, list(
    rows = c("1", "2", "3"),
    columns = c("x_1", "x_2", "x_3")
  ))
})

context("Data: apply order")

test_that("Data: apply order", {
  data_r <- data.frame(
    x_1 = seq(10, 20, 5),
    x_2 = seq(as.POSIXct("2015-01-01"), length.out = 3, by = "1 days"),
    x_3 = TRUE
  )

  inst <- Data$new(data = data_r)
  inst$cacheRStructure()
  idx_r <- c("3", "1", "2")
  idx_c <- c("x_3", "x_1", "x_2")

  ## Scope = columns (default) //
  inst$data <- inst$data[idx_r , idx_c]
  expect_is(inst$applyRStructure(), "data.frame")
  expect_identical(inst$getData(), data_r[idx_r, ])
  inst$data <- inst$data[idx_r , idx_c]
  expect_is(inst$applyRStructure("columns"), "data.frame")
  expect_identical(inst$getData(), data_r[idx_r, ])

  ## Scope = rows //
  inst$data <- inst$data[idx_r , idx_c]
  expect_is(inst$applyRStructure("rows"), "data.frame")
  expect_identical(inst$getData(), data_r[, idx_c])

  ## Scope = both //
  inst$data <- inst$data[idx_r , idx_c]
  expect_is(inst$applyRStructure("both"), "data.frame")
  expect_identical(inst$getData(), data_r)
})
