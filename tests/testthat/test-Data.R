
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
    ext_format = format_dummy,
    r_meta_format = format_dummy,
    ext_meta_format = format_dummy
  )
  expect_identical(inst$data, data.frame(letters))
  expect_identical(inst$r_format, format_dummy)
  expect_identical(inst$ext_format, format_dummy)
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

  format <- list(
    x_1 = list(
      name_r = "x_1",
      name_r_handler = function(x) gsub("\\.", "_", x),
      class_r = "integer",
      class_r_handler = function(x) as.integer(x),
      position_r = 1,
      position_r_handler = NULL,
      name_ext = NULL,
      name_ext_handler = function(x) gsub("_", ".", x),
      class_ext = NULL,
      class_ext_handler = function(x) as.integer(x),
      position_ext = 1,
      position_ext_handler = NULL
    ),
    x_2 = list(
      name_r = "x_2",
      name_r_handler = function(x) gsub("\\.", "_", x),
      class_r = "POSIXlt",
      class_r_handler = function(x) as.POSIXlt(x),
      position_r = 2,
      position_r_handler = NULL,
      name_ext = NULL,
      name_ext_handler = function(x) gsub("_", ".", x),
      class_ext = "character",
      class_ext_handler = function(x) as.character(x),
      position_ext = 2,
      position_ext_handler = NULL
    ),
    x_3 = list(
      name_r = "x_3",
      name_r_handler = function(x) gsub("\\.", "_", x),
      class_r = "logical",
      class_r_handler = function(x) as.logical(x),
      position_r = 3,
      position_r_handler = NULL,
      name_ext = NULL,
      name_ext_handler = function(x) gsub("_", ".", x),
      class_ext = "integer",
      class_ext_handler = function(x) as.integer(x),
      position_ext = 3,
      position_ext_handler = NULL
    )
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

# Order -------------------------------------------------------------------

context("Data: cache order")

test_that("Data: cache order", {
  data_r <- data.frame(
    x_1 = seq(10, 20, 5),
    x_2 = seq(as.POSIXct("2015-01-01"), length.out = 3, by = "1 days"),
    x_3 = TRUE
  )

  inst <- Data$new(data = data_r)
  expect_identical(inst$order, list())
  expect_is(res <- inst$cacheOrder(), "list")
  expect_identical(inst$order, list(
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
  inst$cacheOrder()
  idx_r <- c("3", "1", "2")
  idx_c <- c("x_3", "x_1", "x_2")

  ## Scope = columns (default) //
  inst$data <- inst$data[idx_r , idx_c]
  expect_is(inst$applyOrder(), "data.frame")
  expect_identical(inst$getData(), data_r[idx_r, ])
  inst$data <- inst$data[idx_r , idx_c]
  expect_is(inst$applyOrder("columns"), "data.frame")
  expect_identical(inst$getData(), data_r[idx_r, ])

  ## Scope = rows //
  inst$data <- inst$data[idx_r , idx_c]
  expect_is(inst$applyOrder("rows"), "data.frame")
  expect_identical(inst$getData(), data_r[, idx_c])

  ## Scope = both //
  inst$data <- inst$data[idx_r , idx_c]
  expect_is(inst$applyOrder("both"), "data.frame")
  expect_identical(inst$getData(), data_r)
})
