library(reltest)
file_small_1000 <- "csv_1_small_1000.csv"
file_small_25 <- "csv_1_small_25.csv"

# Instantiate -------------------------------------------------------------

context("DataCon.IntelligentForecaster.Csv: instantiate")

test_that("DataCon.IntelligentForecaster.Csv: empty", {
  expect_is(inst <- DataCon.IntelligentForecaster.Csv$new(),
    "DataCon.IntelligentForecaster.Csv")
  expect_true(inherits(inst, "DataCon.IntelligentForecaster"))
  expect_true(inherits(inst, "DataCon"))
  expect_true(inherits(inst, "IDataCon"))
})

test_that("DataCon.IntelligentForecaster.Csv: values", {
  expect_is(inst <- DataCon.IntelligentForecaster.Csv$new(con = tempdir()),
    "DataCon.IntelligentForecaster.Csv")
  expect_true(inherits(inst$con, "character"))
})

# Factory -----------------------------------------------------------------

context("DataCon.IntelligentForecaster.Csv: factory")

test_that("DataCon.IntelligentForecaster.Csv: factory: production", {
  expect_is(
    inst <- DataCon.IntelligentForecaster.Csv$factories$production(),
    "DataCon.IntelligentForecaster.Csv"
  )
  expect_true(inherits(inst$getCached(), "Data"))
  expect_true(inherits(inst$getCached()$getRFormat(), "DataFormat"))
  expect_true(inherits(inst$getCached()$getExternalFormat(), "DataFormat"))
})

test_that("DataCon.IntelligentForecaster.Csv: factory: test", {
  con <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv",
      file_small_25)
  )
  expect_is(
    inst <- DataCon.IntelligentForecaster.Csv$factories$test(con = con),
    "DataCon.IntelligentForecaster.Csv"
  )
  expect_true(inherits(inst$getCached(), "Data"))
  expect_true(inherits(inst$getCached()$getRFormat(), "DataFormat"))
  expect_true(inherits(inst$getCached()$getExternalFormat(), "DataFormat"))
})

# Getters/setters ----------------------------------------------------------

context("DataCon.IntelligentForecaster.Csv: getters/setters")

test_that("DataCon.IntelligentForecaster.Csv: getter/setter", {
  con <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv",
      file_small_1000)
  )
  expect_true(file.exists(con))

  inst <- DataCon.IntelligentForecaster.Csv$new(
    con = con
  )
  expect_equivalent(inst$getCached(), IData$new())

  expect_error(res <- inst$getCachedActive(),
    ".* you called the interface")

  inst <- DataCon.IntelligentForecaster.Csv$factories$test(con = con)
  expect_is(res <- inst$getCachedActive(), "data.frame")
})

# Setters -----------------------------------------------------------------

test_that("DataCon.IntelligentForecaster.Csv: getter/setter", {
  inst <- DataCon.IntelligentForecaster.Csv$new()
  target <- Data$new(data.frame(a = 1:3))
  expect_identical(inst$setCached(target), target)
  expect_identical(inst$setCachedActive(target), target)
})

# pull --------------------------------------------------------------------

test_that("DataCon.IntelligentForecaster.Csv: pull(format = FALSE)", {
  inst <- DataCon.IntelligentForecaster.Csv$new()
  expect_error(res <- inst$pull(), ".* you called the interface")

  con <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv",
      file_small_25)
  )
  expect_true(file.exists(con))
  # DataCon.IntelligentForecaster.Csv$debug("pull")
  inst <- DataCon.IntelligentForecaster.Csv$factories$test(con = con)
  expect_is(res <- inst$pull(format = FALSE), "data.frame")

  expect_true(nrow(res) > 0)
  expect_true(nrow(inst$getCached()$getData()) > 0)
})

test_that("DataCon.IntelligentForecaster.Csv: pull", {
  con <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv",
      file_small_1000)
  )
  expect_true(file.exists(con))

  inst <- DataCon.IntelligentForecaster.Csv$factories$test(con = con)
  expect_is(res <- inst$pull(), "data.frame")
  expect_true(nrow(res) > 0)
  expect_true(nrow(inst$getCached()$getData()) > 0)
})

test_that("DataCon.IntelligentForecaster.Csv: pull: overwrite", {
  con <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv",
      file_small_1000)
  )
  expect_true(file.exists(con))

  inst <- DataCon.IntelligentForecaster.Csv$factories$test(con = con)
  expect_is(res <- inst$pull(), "data.frame")
  expect_warning(expect_is(res <- inst$pull(), "data.frame"),
    "DataCon.IntelligentForecaster.Csv: pull: cached data exists \\(no overwrite\\)"
  )
  expect_true(nrow(res) > 0)
  expect_true(nrow(inst$getCached()$getData()) > 0)
  expect_is(res <- inst$pull(overwrite = TRUE), "data.frame")
})

# applyRFormat -----------------------------------------------------------

test_that("DataCon.IntelligentForecaster.Csv: applyRFormat: empty", {
  inst <- DataCon.IntelligentForecaster.Csv$factories$production()
  expect_error(res <- inst$applyRFormat(), ".* no data available")
})

test_that("DataCon.IntelligentForecaster.Csv: applyRFormat: test", {
  inst <- DataCon.IntelligentForecaster.Csv$new()
  expect_error(res <- inst$applyRFormat(), "you called the interface")
})

test_that("DataCon.IntelligentForecaster.Csv: applyRFormat", {
  con <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv",
      file_small_25)
  )
  expect_true(file.exists(con))

  inst <- DataCon.IntelligentForecaster.Csv$factories$test(con = con)
  tmp <- inst$pull(format = FALSE)
  expect_is(res <- inst$applyRFormat(), "data.frame")
  expect_is(res <- inst$getCached()$applyRFormat(), "data.frame")
  expect_true(nrow(res) > 0)
  expect_true(nrow(inst$getCached()$getData()) > 0)
})

# applyExternalFormat --------------------------------------------------

test_that("DataCon.IntelligentForecaster.Csv: applyExternalFormat", {
  inst <- DataCon.IntelligentForecaster.Csv$new(
    cached = Data$new()
  )
  # inst$cached
  expect_identical(res <- inst$applyExternalFormat(), data.frame())

  con <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv",
      file_small_25)
  )
  expect_true(file.exists(con))

  inst <- DataCon.IntelligentForecaster.Csv$factories$test(con = con)
  tmp <- inst$pull()
  tmp <- inst$applyRFormat()
  expect_is(res <- inst$applyExternalFormat(), "data.frame")
  expect_true(nrow(res) > 0)
})

# cached_active -----------------------------------------------------------

test_that("DataCon.IntelligentForecaster.Csv: cached_active: get", {
  con <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv",
      file_small_25)
  )
  expect_true(file.exists(con))

  inst <- DataCon.IntelligentForecaster.Csv$factories$test(con = con)
  expect_is(res <- inst$cached_active, "data.frame")
  expect_true(nrow(res) > 0)
})

test_that("DataCon.IntelligentForecaster.Csv: cached_active: set", {
  inst <- DataCon.IntelligentForecaster.Csv$new(
    cached = Data$new()
  )
  target <- data.frame(a = 1:3)
  expect_is(res <- inst$cached_active <- target, "data.frame")
  expect_true(nrow(res) > 0)
  expect_identical(res, target)
})

# Pull extended -----------------------------------------------------------

test_that("DataCon.IntelligentForecaster.Csv: pull: extended", {
  con <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv",
      file_small_25)
  )
  expect_true(file.exists(con))

  inst <- DataCon.IntelligentForecaster.Csv$factories$test(con = con)

  expect_is(res <- inst$pull(extended = TRUE), "data.frame")
  # sapply(res, class)
  expect_true(nrow(res) > 0)
  expect_true(nrow(inst$getCached()$getData()) > 0)
})

# Meta --------------------------------------------------------------------

test_that("DataCon.IntelligentForecaster.Csv: meta: extended", {
  con <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv",
      file_small_25)
  )
  expect_true(file.exists(con))

  # DataCon.IntelligentForecaster.Csv$debug("pull")
  inst <- DataCon.IntelligentForecaster.Csv$factories$test(con = con)

  ## Default meta value //
  expect_true(inst$meta$applyRFormat$extended == FALSE)
  data <- inst$pull()
  target_1 <- c("id", "date", "value", "comment", "note")
  expect_true(all(names(data) %in% target_1))

  ## Change meta value //
  inst$meta$applyRFormat$extended <- TRUE
  data <- inst$pull(overwrite = TRUE)
  target_2 <- c(target_1, "date_day", "date_year", "date_month", "date_week",
    "date_day_year", "date_day_month", "date_day_week", "date_hour", "date_minute",
    "date_second")
  expect_true(all(names(data) %in% target_2))

  ## Overwrite meta value //
  data <- inst$pull(extended = FALSE, overwrite = TRUE)
  expect_true(all(names(data) %in% target_1))
  expect_true(all(!names(data) %in% setdiff(target_2, target_1)))
})

test_that("DataCon.IntelligentForecaster.Csv: meta: with_ids", {
  con <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv",
      file_small_25)
  )
  expect_true(file.exists(con))

  # DataCon.IntelligentForecaster.Csv$debug("pull")
  inst <- DataCon.IntelligentForecaster.Csv$factories$test(con = con)

  ## Default meta value //
  expect_true(inst$meta$applyRFormat$with_ids == FALSE)
  data <- inst$pull()
  target_1 <- c("id", "date", "value", "comment", "note")
  expect_true(all(names(data) %in% target_1))

  ## Change meta value //
  inst$meta$applyRFormat$extended <- TRUE
  inst$meta$applyRFormat$with_ids <- TRUE
  data <- inst$pull(overwrite = TRUE)
  target_2 <- c("date_day", "date_year", "date_month", "date_week",
    "date_day_year", "date_day_month", "date_day_week", "date_hour", "date_minute",
    "date_second")
  target_3 <- paste0(target_2, "_id")
  target <- c(target_1, target_2, "date_id", target_3)
  expect_true(all(names(data) %in% target))
})

# Structure ------------------------------------------------------------------

context("DataCon.IntelligentForecaster.Csv: structure")

test_that("DataCon.IntelligentForecaster.Csv: structure", {
  con <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv",
      file_small_25)
  )
  expect_true(file.exists(con))
  inst <- DataCon.IntelligentForecaster.Csv$factories$test(con = con)
  inst$pull()
  expect_identical(
    inst$getCached()$getExternalFormat()$getStructure()$names,
    c("ID", "Moment", "Value", "Comment", "Note")
  )
  expect_identical(
    inst$getCached()$getRFormat()$getStructure()$names,
    c("id", "date", "value", "comment", "note")
  )
})

# Meta format -------------------------------------------------------------

context("DataCon.IntelligentForecaster.Csv: meta format")

test_that("DataCon.IntelligentForecaster.Csv: meta format", {
  con <- withCorrectWorkingDir(
    file.path(getwd(), "data/persistent/DataCon.IntelligentForecaster.Csv",
      file_small_25)
  )
  expect_true(file.exists(con))

  data_r <- data.frame(
    x_1 = seq(10, 20, 5),
    x_2 = seq(as.POSIXct("2015-01-01"), length.out = 3, by = "1 days"),
    x_3 = TRUE,
    stringsAsFactors = FALSE
  )
  data_ext <- data.frame(
    x.1 = seq(10, 20, 5),
    x.2 = as.character(seq(as.POSIXlt("2015-01-01"), length.out = 3, by = "1 days")),
    x.3 = TRUE,
    stringsAsFactors = FALSE
  )

  r_format <- list(
    function(x, pattern = "\\d{4}-\\d{2}-\\d{2}") {
      tmp <- lapply(x, function(ii) {
        if (any(grepl(pattern, ii))) {
          as.POSIXlt(ii)
        } else {
          ii
        }
      })
      as.data.frame(tmp, stringsAsFactors = FALSE)
    },
    function(x) {
      names(x) <- gsub("\\.", "_", names(x))
      x
    }
  )
  ext_format <- list(
    function(x, pattern = "\\d{4}-\\d{2}-\\d{2}") {
      tmp <- lapply(x, function(ii) {
        if (any(grepl(pattern, ii))) {
          as.character(ii)
        } else {
          ii
        }
      })
      as.data.frame(tmp, stringsAsFactors = FALSE)
    },
    function(x) {
      names(x) <- gsub("_", ".", names(x))
      x
    }
  )

  inst <- DataCon.IntelligentForecaster.Csv$new(
    con = con,
    cached = Data$new(
      r_format = DataFormat$new(format = r_format),
      ext_format = DataFormat$new(format = ext_format)
    )
  )
  inst$getCached()$setData(data_ext)
  res <- inst$getCached()$applyRFormat()
  expect_false(all(grepl("\\.", names(res))))
  expect_true(inherits(res$x_2, "POSIXct"))

  res <- inst$getCached()$applyExternalFormat()
  expect_false(all(grepl("_", names(res))))
  expect_true(inherits(res$x.2, "character"))
})
