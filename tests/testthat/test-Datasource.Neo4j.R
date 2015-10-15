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
