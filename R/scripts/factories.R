# Factories ---------------------------------------------------------------

IDataCon <- R6Class(
  classname = "IDataCon",
  portable = TRUE,
  public = list(
    getData = function() {stop("Interface method")},
    setData = function(...) {stop("Interface method")}
  )
)

DataCon <- R6Class(
  classname = "IDataCon",
  inherit = IDataCon,
  portable = TRUE,
  public = list(
    data = "data.frame",
    initialize = function(
      data = data.frame()
    ) {
      self$data <- data
    },
    getData = function() {self$data},
    setData = function(value) {self$data <- value}
  )
)

Data <- R6Class(
  classname = "Data",
  portable = TRUE,
  public = list(
    con = "IDataCon",
    initialize = function(
      con = IDataCon$new()
    ) {
      self$con <- con
    }
  )
)

inst <- Data$new()
inst$con$getData()

Data2 <- R6Class(
  classname = "Data2",
  portable = TRUE,
  public = list(
    con = "IDataCon",
    initialize = function(
      con = IDataCon$new()
    ) {
      self$con <- con
    }
  ),
  private = list(
    factory = function(
      type = c("production", "test")
    ) {
      type <- match.arg(type, c("production", "test"))
      if (type == "production") {
        Data2$new(
          con = DataCon$new()
        )
      } else if (type == "test") {
        Data2$new(
          con = DataCon$new(data = data.frame(a = 1:3, b = letters[1:3]))
        )
      }
    }
  )
)

inst <- Data2$new()
inst$con$getData()

inst <- Data2$private_methods$factory()
inst$con$getData()

inst <- Data2$private_methods$factory("test")
inst$con$getData()

Data2$factory <- Data2$private_methods$factory
inst <- Data2$factory()
inst$con$getData()
inst <- Data2$factory("test")
inst$con$getData()

Data3 <- R6Class(
  classname = "Data3",
  portable = TRUE,
  public = list(
    con = "IDataCon",
    initialize = function(
      con = IDataCon$new()
    ) {
      self$con <- con
    }
  )
)

runFactory <- function(which, ...) {
  UseMethod("runFactory", which)
}

runFactory.Data3.production <- function(
  which
) {
  Data2$new(
    con = DataCon$new()
  )
}

runFactory.Data3.test <- function(
  which
) {
  Data2$new(
    con = DataCon$new(data = data.frame(a = 1:3, b = letters[1:3]))
  )
}

Data3$factory <- function(which) {
  runFactory(which)
}

inst <- Data3$factory(structure(NULL, class = "Data3.production"))
inst$con$getData()

inst <- Data3$factory(structure(NULL, class = "Data3.test"))
inst$con$getData()

inst <- Data3$factory(structure(NULL, class = "Data3.asdf"))

Data4 <- R6Class(
  classname = "Data4",
  portable = TRUE,
  public = list(
    con = "IDataCon",
    initialize = function(
      con = IDataCon$new()
    ) {
      self$con <- con
    }
  ),
  private = list(
    factories = list(
      production = function() {
        Data4$new(
          con = DataCon$new()
        )
      },
      test = function(
        which
      ) {
        Data4$new(
          con = DataCon$new(data = data.frame(a = 1:3, b = letters[1:3]))
        )
      }
    )
  )
)

inst <- Data4$private_fields$factories$production()
inst$con$getData()

inst <- Data4$private_fields$factories$test()
inst$con$getData()

Data4$factories <- Data4$private_fields$factories
Data4$factories$production()
