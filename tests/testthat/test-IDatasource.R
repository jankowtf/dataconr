context("IDatasource")

test_that("IDatasource::plain", {
  expect_is(IDatasource$new(), "IDatasource")
})

test_that("IDatasource::initialize", {
  expect_is(IDatasource$new(con = NA), "IDatasource")
})
