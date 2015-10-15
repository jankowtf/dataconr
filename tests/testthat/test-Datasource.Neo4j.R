library(RNeo4j)

context("Datasource.Neo4j")

test_that("Datasource.Neo4j::plain", {
  expect_is(Datasource.Neo4j$new(), "Datasource.Neo4j")
})

test_that("Datasource.Neo4j::initialize", {
  skip("Manual only")
  graph <- startGraph("http://localhost:7474/db/data/")
  expect_is(Datasource.Neo4j$new(con = graph), "Datasource.Neo4j")
})

# test_that("Datasource.Neo4j::plain", {
#   importSample(graph, "movies", input = FALSE)
#   expect_is(Datasource.Neo4j$new(con = graph), "Datasource.Neo4j")
# })
