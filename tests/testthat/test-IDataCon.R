context("IDataCon")

test_that("IDataCon::plain", {
  expect_is(IDataCon$new(), "IDataCon")
})
