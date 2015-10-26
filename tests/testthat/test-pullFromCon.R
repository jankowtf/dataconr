context("pullFromCon")

test_that("pullFromCon.DataCon.IntelligentForecaster.Csv", {
  inst <- DataCon.IntelligentForecaster.Csv$new(con = tempdir())
  expect_warning(
    expect_error(pullFromCon(inst), "cannot open the connection"),
    "Permission denied"
  )
})
