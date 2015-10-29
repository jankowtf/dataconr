
# IDataFormat -------------------------------------------------------------

context("IDataFormat")

test_that("IDataFormat", {
  expect_is(inst <- IDataFormat$new(), "IDataFormat")
  expect_error(inst$getFormat(),
    paste0(class(inst)[1], ": .*: you called the interface"))
  expect_error(inst$setFormat(),
    paste0(class(inst)[1], ": .*: you called the interface"))
  expect_error(inst$getStructure(),
    paste0(class(inst)[1], ": .*: you called the interface"))
  expect_error(inst$setStructure(),
    paste0(class(inst)[1], ": .*: you called the interface"))
})

# IData -------------------------------------------------------------------

context("IData")

test_that("IData", {
  expect_is(inst <- IData$new(), "IData")
  expect_error(inst$applyInjectedExternalFormat(),
    paste0(class(inst)[1], ": .*: you called the interface"))
  expect_error(inst$applyExternalFormat(),
    paste0(class(inst)[1], ": .*: you called the interface"))
  expect_error(inst$applyInjectedRFormat(),
    paste0(class(inst)[1], ": .*: you called the interface"))
  expect_error(inst$applyRFormat(),
    paste0(class(inst)[1], ": .*: you called the interface"))

  expect_error(inst$getData(),
    paste0(class(inst)[1], ": .*: you called the interface"))
  expect_error(inst$getInjected(),
    paste0(class(inst)[1], ": .*: you called the interface"))
  expect_error(inst$getRFormat(),
    paste0(class(inst)[1], ": .*: you called the interface"))
  expect_error(inst$getExternalFormat(),
    paste0(class(inst)[1], ": .*: you called the interface"))

  expect_error(inst$setData(),
    paste0(class(inst)[1], ": .*: you called the interface"))
  expect_error(inst$setInjected(),
    paste0(class(inst)[1], ": .*: you called the interface"))
  expect_error(inst$setExternalFormat(),
    paste0(class(inst)[1], ": .*: you called the interface"))
  expect_error(inst$setRFormat(),
    paste0(class(inst)[1], ": .*: you called the interface"))
})

# IDataCon ----------------------------------------------------------------

context("IDataCon")

test_that("IDataCon", {
  expect_is(inst <- IDataCon$new(), "IDataCon")
  expect_error(inst$applyExternalFormat(),
    paste0(class(inst)[1], ": .*: you called the interface"))
  expect_error(inst$applyRFormat(),
    paste0(class(inst)[1], ": .*: you called the interface"))

  expect_error(inst$getCached(),
    paste0(class(inst)[1], ": .*: you called the interface"))
  expect_error(inst$getCachedActive(),
    paste0(class(inst)[1], ": .*: you called the interface"))

  expect_error(inst$pull(),
    paste0(class(inst)[1], ": .*: you called the interface"))
  expect_error(inst$push(),
    paste0(class(inst)[1], ": .*: you called the interface"))

  expect_error(inst$setCached(),
    paste0(class(inst)[1], ": .*: you called the interface"))
  expect_error(inst$setCachedActive(),
    paste0(class(inst)[1], ": .*: you called the interface"))
})
