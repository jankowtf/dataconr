library(reltest)
file_small_25 <- "csv_1_small_25.csv"

# DataCon: instantiate ----------------------------------------------------

context("DataCon: instantiate")

test_that("DataCon: instantiate", {
  expect_is(inst <- DataCon$new(), "DataCon")
  expect_true(inherits(inst, "IDataCon"))
})

test_that("DataCon: instantiate: values", {
  expect_is(inst <- DataCon$new(con = NA), "DataCon")
  expect_true(inherits(inst, "IDataCon"))
})

# DataCon: getters/setters ------------------------------------------------

context("DataCon: getters/setters")

test_that("DataCon: getters/setters", {
  inst <- DataCon$new()

  expect_is(inst$getCached(), "IData")
  expect_identical(inst$getConnection(), NULL)
  expect_error(inst$getCachedActive(), "you called the interface")

  con <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv",
      file_small_25)
  )
  expect_true(file.exists(con))

  inst <- DataCon$factories$test(con = con)

  expect_is(inst$getCached(), "IData")
  expect_identical(inst$getConnection(), con)
  expect_error(inst$getCachedActive(), "not implemented yet")

  expect_is(inst$setCached(Data$new()), "Data")
  expect_identical(inst$setConnection("abc"), "abc")
  expect_is(inst$setCachedActive(Data$new()), "Data")
})

# DataCon: apply format ---------------------------------------------------

context("DataCon: apply format")

test_that("DataCon: apply format", {
  expect_is(inst <- DataCon$new(), "DataCon")

  expect_error(inst$applyExternalFormat(),
    paste0(class(inst)[1], ": .*: not implemented yet"))
  expect_error(inst$applyRFormat(),
    paste0(class(inst)[1], ": .*: not implemented yet"))
})

# DataCon: pull/push ------------------------------------------------------

context("DataCon: pull/push")

test_that("DataCon: pull/push", {
  expect_is(inst <- DataCon$new(), "DataCon")

  expect_error(inst$pull(),
    paste0(class(inst)[1], ": .*: not implemented yet"))
  expect_error(inst$push(),
    paste0(class(inst)[1], ": .*: not implemented yet"))
})



