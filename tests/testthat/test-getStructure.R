
# getStructure.data.frame -------------------------------------------------

context("getStructure.data.frame")

test_that("getStructure.data.frame", {
  inst <- data.frame(a = 1, b = 2, c = 3)
  target <- list(
    names = c("a", "b", "c"),
    row.names = as.character(1),
    class = "data.frame"
  )
  expect_is(res <- getStructure(inst), "Structure.DataFrame")
  expect_true(inherits(res, "list"))
  expect_equivalent(res, target)
#   class(res$row.names)
#   class(target$row.names)
})

# getStructure.list -------------------------------------------------------

context("getStructure.list")

test_that("getStructure.list", {
  inst <- list(a = 1, b = 2, c = 3)
  target <- list(
    names = c("a", "b", "c")
  )
  expect_is(res <- getStructure(inst), "Structure.List")
  expect_true(inherits(res, "list"))
  expect_equivalent(res, target)
})

# getStructure.character --------------------------------------------------

context("getStructure.character")

test_that("getStructure.character", {
  inst <- letters
  target <- list(
    names = NULL,
    length = length(inst)
  )
  expect_is(res <- getStructure(inst), "Structure.Character")
  expect_true(inherits(res, "list"))
  expect_equivalent(res, target)

  names(inst) <- letters
  target <- list(
    names = letters,
    length = length(inst)
  )
  expect_is(res <- getStructure(inst), "Structure.Character")
  expect_true(inherits(res, "list"))
  expect_equivalent(res, target)
})
