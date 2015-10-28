library(RNeo4j)
filepath <- "csv_1_small_25.csv"

context("DataCon.Neo4j")

test_that("DataCon.Neo4j: plain", {
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

# applyExternalFormat --------------------------------------------------

test_that("DataCon.Neo4j: applyExternalFormat", {
  target <- Data$new(
    data = data.frame(
      date = seq(as.POSIXlt("2015-01-01"), length.out = 3, by = "1 days"),
      value = 1:3
    )
  )
  # inst$setCached(target)
  inst <- DataCon.Neo4j$new(cached = target)

  # inst$getCached()
  expect_is(res <- inst$applyExternalFormat(), class(target$getData()))
  expect_is(res$date, "character")
})

# applyRFormat -----------------------------------------------------------

test_that("DataCon.Neo4j: applyRFormat", {
  inst <- DataCon.Neo4j$new()
  target <- Data$new(
    data = data.frame(
      date = as.character(seq(as.POSIXlt("2015-01-01"), length.out = 3, by = "1 days")),
      value = 1:3
    )
  )
  inst$setCached(target)
  expect_is(res <- inst$applyRFormat(), class(target$getData()))
  expect_is(res$date, "POSIXlt")
})

# IntelligentForecaster and Neo4j -----------------------------------------

test_that("DataCon.IntelligentForecaster.Csv + DataCon.Neo4j: meta: column order", {
  path <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv",
      filepath)
  )
  expect_true(file.exists(path))
  inst <- DataCon.IntelligentForecaster.Csv$new(
    con = path,
    cached = Data$new()
  )
  data <- inst$pull(extended = TRUE, with_ids = TRUE)
  # inst$meta

  inst_2 <- DataCon.Neo4j$new(
    meta = inst$meta,
    cached = inst$getCached()
  )
  expect_identical(
    inst$getCached()$getRStructure(),
    inst_2$getCached()$getRStructure()
  )
  expect_is(res <- inst$applyRFormat(), class(inst$getCached()$getData()))
  expect_identical(order(names(inst$getCached()$getData())), order(names(res)))
})
