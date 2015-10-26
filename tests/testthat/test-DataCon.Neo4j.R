library(RNeo4j)

context("DataCon.Neo4j")

test_that("DataCon.Neo4j::plain", {
  expect_is(DataCon.Neo4j$new(), "DataCon.Neo4j")
})

test_that("DataCon.Neo4j::initialize", {
  skip("Manual only")
  graph <- startGraph("http://localhost:7474/db/data/")
  # clear(graph, data = FALSE)
  # importSample(graph, "movies", data = FALSE)
  expect_is(res <- DataCon.Neo4j$new(con = graph), "DataCon.Neo4j")
  expect_true(inherits(res$con, "graph"))
})


# toExternalFormat --------------------------------------------------

test_that("DataCon.Neo4j::toExternalFormat", {
  inst <- DataCon.Neo4j$new()
  data <- data.frame(
    date = seq(as.POSIXlt("2015-01-01"), length.out = 3, by = "1 days"),
    value = 1:3
  )
  expect_is(res <- inst$toExternalFormat(data = data), class(data))
  expect_is(res$date, "character")
})


# toRFormat -----------------------------------------------------------

test_that("DataCon.Neo4j::toRFormat", {
  inst <- DataCon.Neo4j$new()
  data <- data.frame(
    date = as.character(seq(as.POSIXlt("2015-01-01"), length.out = 3, by = "1 days")),
    value = 1:3
  )
  self=inst
  expect_is(res <- inst$toRFormat(data = data), class(data))
  expect_is(res$date, "POSIXlt")
})
