library(RNeo4j)
library(reltest)

filepath <- "csv_1_small_25.csv"

# DataCon.Neo4j: instantiate ----------------------------------------------

context("DataCon.Neo4j: instantiate")

test_that("DataCon.Neo4j: instantiate: empty", {
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


# DataCon.Neo4j: factories ------------------------------------------------

context("DataCon.Neo4j: factories")

test_that("DataCon.Neo4j: factories: production", {
  expect_is(inst <- DataCon.Neo4j$factories$production(), "DataCon.Neo4j")
  # inst$cached$r_format
})

test_that("DataCon.Neo4j: factories: test", {
  expect_is(DataCon.Neo4j$factories$test(con = tempdir()), "DataCon.Neo4j")
})

# DataCon.Neo4j: applyExternalFormat --------------------------------------

context("DataCon.Neo4j: applyExternalFormat")

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

# DataCon.Neo4j: applyRFormat ---------------------------------------------

context("DataCon.Neo4j: applyRFormat")

test_that("DataCon.Neo4j: applyRFormat", {
  # DataCon.Neo4j$debug("applyRFormat")
  inst <- DataCon.Neo4j$factories$production()
  data <- data.frame(
    date = as.character(seq(as.POSIXlt("2015-01-01"), length.out = 3, by = "1 days")),
    value = 1:3
  )
  inst$getCached()$setData(data)

  expect_is(res <- inst$applyRFormat(), class(inst$getCached()$getData()))
  expect_is(res$date, "POSIXlt")
})

# IntelligentForecaster and Neo4j -----------------------------------------

test_that("DataCon.IntelligentForecaster.Csv + DataCon.Neo4j: meta: column order", {
  con <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv",
      filepath)
  )
  expect_true(file.exists(con))

  inst <- DataCon.IntelligentForecaster.Csv$factories$test(con = con)
  data <- inst$pull(extended = TRUE, with_ids = TRUE)
  # inst$meta

  inst_2 <- DataCon.Neo4j$new(
    meta = inst$meta,
    cached = inst$getCached()
  )
  expect_identical(
    inst$getCached()$getRFormat()$getStructure(),
    inst_2$getCached()$getRFormat()$getStructure()
  )
  expect_is(res <- inst$applyRFormat(), class(inst$getCached()$getData()))
  expect_identical(order(names(inst$getCached()$getData())), order(names(res)))
})
