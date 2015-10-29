
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



