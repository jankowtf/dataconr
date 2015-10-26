library(reltest)

context("DataCon.IntelligentForecaster.Csv")

# Initialize --------------------------------------------------------------

test_that("DataCon.IntelligentForecaster.Csv: plain", {
  expect_is(inst <- DataCon.IntelligentForecaster.Csv$new(),
    "DataCon.IntelligentForecaster.Csv")
  expect_true(inherits(inst, "DataCon.IntelligentForecaster"))
  expect_true(inherits(inst, "DataCon"))
  expect_true(inherits(inst, "IDataCon"))
})

test_that("DataCon.IntelligentForecaster.Csv: initialize", {
  expect_is(inst <- DataCon.IntelligentForecaster.Csv$new(con = tempdir()),
    "DataCon.IntelligentForecaster.Csv")
  expect_true(inherits(inst$con, "character"))
})

# Getters -----------------------------------------------------------------

test_that("DataCon.IntelligentForecaster.Csv: getter/setter", {
  path <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv/csv_1.csv")
  )
  expect_true(file.exists(path))
  inst <- DataCon.IntelligentForecaster.Csv$new(con = path)
  expect_identical(inst$getCached(), data.frame())
  expect_true(inherits(res <- inst$getCachedActive(), "data.frame"))
})

# Setters -----------------------------------------------------------------

test_that("DataCon.IntelligentForecaster.Csv: getter/setter", {
  inst <- DataCon.IntelligentForecaster.Csv$new()
  target <- data.frame(a = 1:3)
  expect_identical(inst$setCached(target), target)
  expect_identical(inst$setCachedActive(target), target)
})

# pull --------------------------------------------------------------------

test_that("DataCon.IntelligentForecaster.Csv :: pull(format = FALSE)", {
  inst <- DataCon.IntelligentForecaster.Csv$new()
  expect_error(res <- inst$pull(), "must be a character string or connection")

  path <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv/csv_1.csv")
  )
  expect_true(file.exists(path))
  inst <- DataCon.IntelligentForecaster.Csv$new(con = path)
  expect_is(res <- inst$pull(format = FALSE), "data.frame")
  expect_true(nrow(res) > 0)
  expect_true(nrow(inst$cached) > 0)
})

test_that("DataCon.IntelligentForecaster.Csv: pull", {
  inst <- DataCon.IntelligentForecaster.Csv$new()
  expect_error(res <- inst$pull(), "must be a character string or connection")

  path <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv/csv_1.csv")
  )
  expect_true(file.exists(path))
  inst <- DataCon.IntelligentForecaster.Csv$new(con = path)
  expect_is(res <- inst$pull(), "data.frame")
  expect_true(nrow(res) > 0)
  expect_true(nrow(inst$cached) > 0)
})

# toRFormat -----------------------------------------------------------

test_that("DataCon.IntelligentForecaster.Csv: toRFormat: empty", {
  inst <- DataCon.IntelligentForecaster.Csv$new()
  expect_error(res <- inst$toRFormat(), "no data available")
})

test_that("DataCon.IntelligentForecaster.Csv::toRFormat", {
  path <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv/csv_1.csv")
  )
  expect_true(file.exists(path))
  inst <- DataCon.IntelligentForecaster.Csv$new(con = path)
  tmp <- inst$pull(format = FALSE)
  expect_is(res <- inst$toRFormat(), "data.frame")
  expect_true(nrow(res) > 0)
  expect_true(nrow(inst$cached) > 0)
})

# toExternalFormat --------------------------------------------------

test_that("DataCon.IntelligentForecaster.Csv::toExternalFormat", {
  inst <- DataCon.IntelligentForecaster.Csv$new()
  # inst$cached
  expect_identical(res <- inst$toExternalFormat(), data.frame())

  path <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv/csv_1.csv")
  )
  expect_true(file.exists(path))
  inst <- DataCon.IntelligentForecaster.Csv$new(con = path)
  tmp <- inst$pull()
  tmp <- inst$toRFormat()
  expect_is(res <- inst$toExternalFormat(), "data.frame")
  expect_true(nrow(res) > 0)
})

# cached_active -----------------------------------------------------------

test_that("DataCon.IntelligentForecaster.Csv: cached_active: get", {
  path <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv/csv_1.csv")
  )
  expect_true(file.exists(path))
  inst <- DataCon.IntelligentForecaster.Csv$new(con = path)
  expect_is(res <- inst$cached_active, "data.frame")
  expect_true(nrow(res) > 0)
})

test_that("DataCon.IntelligentForecaster.Csv: cached_active: set", {
  inst <- DataCon.IntelligentForecaster.Csv$new()
  target <- data.frame(a = 1:3)
  expect_is(res <- inst$cached_active <- target, "data.frame")
  expect_true(nrow(res) > 0)
  expect_identical(res, target)
})


# Pull extended -----------------------------------------------------------

test_that("DataCon.IntelligentForecaster.Csv: pull: extended", {
  path <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv/csv_1.csv")
  )
  expect_true(file.exists(path))
  inst <- DataCon.IntelligentForecaster.Csv$new(con = path)
  expect_is(res <- inst$pull(extended = TRUE, with_ids = TRUE), "data.frame")
  expect_true(nrow(res) > 0)
  expect_true(nrow(inst$cached) > 0)
})

# Meta --------------------------------------------------------------------

test_that("DataCon.IntelligentForecaster.Csv: meta", {
  path <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv/csv_1.csv")
  )
  expect_true(file.exists(path))
  inst <- DataCon.IntelligentForecaster.Csv$new(con = path)
  expect_null(inst$meta$column_order)
  data <- inst$pull(extended = TRUE, with_ids = TRUE)
  expect_is(inst$meta$column_order, "character")
})
