context("DataCon")

test_that("DataCon::plain", {
  expect_is(res <- DataCon$new(), "DataCon")
  expect_true(inherits(res, "IDataCon"))
})

test_that("DataCon::initialize", {
  expect_is(res <- DataCon$new(con = NA,
    hello_world = "hello world!"), "DataCon")
  expect_true(inherits(res, "IDataCon"))
})
