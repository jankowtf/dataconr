context("Datasource")

test_that("Datasource::plain", {
  expect_is(res <- Datasource$new(), "Datasource")
  expect_true(inherits(res, "IDatasource"))
})

test_that("Datasource::initialize", {
  expect_is(res <- Datasource$new(con = NA,
    hello_world = "hello world!"), "Datasource")
  expect_true(inherits(res, "IDatasource"))
})
