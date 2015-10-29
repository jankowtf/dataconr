
# handleSetStructure.default ----------------------------------------------

context("handleSetStructure.default")

test_that("handleSetStructure.default", {
  inst <- data.frame(a = 1, b = 2, c = 3)
  target <- getStructure(inst)
  expect_identical(res <- handleSetStructure(inst), target)
})

# handleSetStructure.Structure --------------------------------------------

context("handleSetStructure.Structure")

test_that("handleSetStructure.Structure.DataFrame", {
  data <- data.frame(a = 1, b = 2, c = 3)
  inst <- getStructure(data)

  expect_identical(res <- handleSetStructure(inst), inst)
})

