library(RNeo4j)

context("Datasource.Neo4j")

test_that("Datasource.Neo4j::plain", {
  expect_is(Datasource.Neo4j$new(), "Datasource.Neo4j")
})

test_that("Datasource.Neo4j::initialize", {
  skip("Manual only")
  graph <- startGraph("http://localhost:7474/db/data/")
  # clear(graph, input = FALSE)
  # importSample(graph, "movies", input = FALSE)
  expect_is(res <- Datasource.Neo4j$new(con = graph), "Datasource.Neo4j")
  expect_true(inherits(res$con, "graph"))
})


# asExpectedInDatasource --------------------------------------------------

test_that("Datasource.Neo4j::asExpectedInDatasource", {
  inst <- Datasource.Neo4j$new()
  input <- data.frame(
    date = seq(as.POSIXlt("2015-01-01"), length.out = 3, by = "1 days"),
    value = 1:3
  )
  expect_is(res <- inst$asExpectedInDatasource(input = input), class(input))
  expect_is(res$date, "character")
})


# asExpectedInR -----------------------------------------------------------

test_that("Datasource.Neo4j::asExpectedInR", {
  inst <- Datasource.Neo4j$new()
  input <- data.frame(
    date = as.character(seq(as.POSIXlt("2015-01-01"), length.out = 3, by = "1 days")),
    value = 1:3
  )
  self=inst
  expect_is(res <- inst$asExpectedInR(input = input), class(input))
  expect_is(res$date, "POSIXlt")
})
