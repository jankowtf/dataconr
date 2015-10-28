context("Format")

test_that("Format: instantiate: empty", {
  expect_is(inst <- Format$new(), "Format")
  expect_true(inherits(inst, "IFormat"))
})
