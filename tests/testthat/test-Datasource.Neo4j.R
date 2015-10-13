library(RNeo4j)
graph <- startGraph("http://localhost:7474/db/data/")

context("Datasource.Neo4j")

test_that("Datasource.Neo4j::plain", {
  expect_is(Datasource.Neo4j$new(), "Datasource.Neo4j")
})

test_that("Datasource.Neo4j::initialize", {
  expect_is(Datasource.Neo4j$new(con = graph), "Datasource.Neo4j")
})

# test_that("Datasource.Neo4j::plain", {
#   importSample(graph, "movies", input = FALSE)
#   expect_is(Datasource.Neo4j$new(con = graph), "Datasource.Neo4j")
# })
