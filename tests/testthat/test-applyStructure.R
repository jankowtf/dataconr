
# applyStructure.DataFrame -------------------------------------------------

context("applyStructure.Structure.DataFrame")

test_that("applyStructure.Structure.DataFrame", {
  data <- data.frame(a = 1:3, b = letters[1:3], c = c(10, 20, 30))
  inst <- getStructure(data)

  idx_r <- c(2, 1, 3)
  idx_c <- c(2, 1, 3)

  data <- data[idx_r, idx_c]

  ## Columns //
  expect_is(res <- applyStructure(inst, data = data), class(data))
  expect_identical(res, data[, idx_c])

  ## Rows //
  expect_is(res <- applyStructure(inst, data = data, scope = "rows"),
    class(data))
  expect_identical(res, data[idx_r, ])

  ## Both //
  expect_is(res <- applyStructure(inst, data = data, scope = "both"),
    class(data))
  expect_identical(res, data[idx_r, idx_c])
})

# applyStructure.list -------------------------------------------------------

context("applyStructure.Structure.List")

test_that("applyStructure.Structure.List", {
  data <- list(a = 1, b = 2, c = 3)
  inst <- getStructure(data)

  idx <- c(2, 1, 3)

  data <- data[idx]

  ## Names //
  expect_is(res <- applyStructure(inst, data = data), class(data))
  expect_identical(res, data[idx])
})

# applyStructure.character --------------------------------------------------

context("applyStructure.Structure.Character")

test_that("applyStructure.Structure.Character", {
  data <- letters[1:3]
  names(data) <- data
  inst <- getStructure(data)

  idx <- c(2, 1, 3)

  data <- data[idx]

  ## Names //
  expect_is(res <- applyStructure(inst, data = data), class(data))
  expect_identical(res, data[idx])
})
