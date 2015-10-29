#' @title
#' Get structure of an R object
#'
#' @description
#' Designed for usage with implementations of the
#' \code{\link[idata]{IDataFormat}} interface and methods
#' \code{\link[idata]{IDataFormat$getStructure}} and
#' \code{\link[idata]{IDataFormat$setStructure}}
#'
#' @details
#' TODO
#'
#' @param inst A class instance.
#' @return See respective methods.
#' @example inst/examples/example-getStructure.R
#' @export
getStructure <- function(inst, ...) {
  UseMethod("getStructure", inst)
}

# getStructure.data.frame --------------------------------------------------

#' @title
#' Get structure of an R object
#'
#' @description
#' See generic: \code{\link[idata]{getStructure}}
#' Method for: \code{\link[base]{data.frame}}
#'
#' @details
#' TODO
#'
#' @param inst \code{\link[base]{data.frame}}.
#' @return \code{\link[base]{list}}. Results of calling \code{attributes(inst)}.
#' @example inst/examples/example-getStructure.R
#' @export
getStructure.data.frame <- function(inst) {
  value <- attributes(inst)
  value$row.names <- as.character(value$row.names)
  structure(value, class = c("Structure.DataFrame", "Structure", class(value)))
}

# getStructure.list -------------------------------------------------------

#' @title
#' Get structure of an R object
#'
#' @description
#' See generic: \code{\link[idata]{getStructure}}
#' Method for: \code{\link[base]{list}}
#'
#' @details
#' TODO
#'
#' @param inst \code{\link[base]{list}}.
#' @return \code{\link[base]{list}}. Results of calling \code{attributes(inst)}.
#' @example inst/examples/example-getStructure.R
#' @export
getStructure.list <- function(inst) {
  value <- attributes(inst)
  structure(value, class = c("Structure.List", "Structure", class(value)))
}

# getStructure.character --------------------------------------------------

#' @title
#' Get structure of an R object
#'
#' @description
#' See generic: \code{\link[idata]{getStructure}}
#' Method for: \code{\link[base]{character}}
#'
#' @details
#' TODO
#'
#' @param inst \code{\link[base]{character}}.
#' @return \code{\link[base]{list}}. Results of calling \code{attributes(inst)}.
#' @example inst/examples/example-getStructure.R
#' @export
getStructure.character <- function(inst) {
  value <- list(names = names(inst), length = length(inst))
  structure(value, class = c("Structure.Character", "Structure", class(value)))
}

# handleSetStructure ------------------------------------------------------

#' @title
#' Handler for setting structure information
#'
#' @description
#' Designed for usage with implementations of the
#' \code{\link[idata]{IDataFormat}} interface and method
#' \code{\link[idata]{IDataFormat$setStructure}}
#'
#' @details
#' TODO
#'
#' @param inst A class instance.
#' @return See respective methods.
#' @example inst/examples/example-handleSetStructure.R
#' @export
handleSetStructure <- function(inst, ...) {
  UseMethod("handleSetStructure", inst)
}

# handleSetStructure.default ----------------------------------------------

#' @title
#' Handler for setting structure information
#'
#' @description
#' See generic: \code{\link[idata]{handleSetStructure}}
#' Method for: \code{\link[base]{ANY}}
#'
#' @details
#' TODO
#'
#' @param inst \code{\link[base]{ANY}}.
#' @return \code{\link[base]{Structure}}. Return value of
#'  \code{\link[idata]{getStructure}}.
#' @example inst/examples/example-handleSetStructure.R
#' @export
handleSetStructure.default = function(
  inst
) {
  getStructure(inst)
}

# handleSetStructure.Structure --------------------------------------------

#' @title
#' Handler for setting structure information
#'
#' @description
#' See generic: \code{\link[idata]{handleSetStructure}}
#' Method for: \code{\link[base]{Structure}}
#'
#' @details
#' TODO
#'
#' @param inst \code{\link[base]{Structure}}.
#' @return \code{\link[base]{Structure}}. Just returns input.
#' @example inst/examples/example-handleSetStructure.R
#' @export
handleSetStructure.Structure = function(
  inst
) {
  inst
}

# applyStructure ----------------------------------------------------------

#' @title
#' Apply structure of an R object
#'
#' @description
#' Designed for usage with implementations of the
#' \code{\link[idata]{IDataFormat}} interface and methods
#' \code{\link[idata]{IDataFormat$getStructure}}
#'
#' @details
#' TODO
#'
#' @param inst A class instance.
#' @return An error. See respective methods.
#' @example inst/examples/example-getStructure.R
#' @export
applyStructure <- function(inst, data, scope, ...) {
  UseMethod("applyStructure", inst)
}

# applyStructure.Structure.DataFrame --------------------------------------

applyStructure.Structure.DataFrame = function(
  inst,
  data,
  scope = c("columns", "rows", "both")
) {
  data_0 <- data
  scope <- match.arg(scope, c("columns", "rows", "both"))
  if (scope == "columns" && !is.null(idx <- inst$names)) {
    data <- try(data[ , idx], silent = TRUE)
  } else if (scope == "rows" && !is.null(idx <- inst$row.names)) {
    data <- try(data[idx , ], silent = TRUE)
  } else if (
    scope == "both" &&
      !is.null(idx_r <- inst$row.names) &&
      !is.null(idx_c <- inst$names)
  ) {
    data <- try(data[idx_r , idx_c], silent = TRUE)
  }
  if (inherits(data, "try-error")) {
    cl <- gsub("\\(.*", "", as.character(sys.call()[1]))
    warning(paste0(cl, ": failed"))
    data <- data_0
  }
  data
}

# applyStructure.Structure.List -------------------------------------------

applyStructure.Structure.List = function(
  inst,
  data,
  scope = c("names")
) {
  scope <- match.arg(scope, c("names"))
  if (scope == "names" && !is.null(idx <- inst$names)) {
    data <- data[idx]
  }
  data
}

# applyStructure.Structure.Character --------------------------------------

applyStructure.Structure.Character = function(
  inst,
  data,
  scope = c("names")
) {
  scope <- match.arg(scope, c("names"))
  if (scope == "names" && !is.null(idx <- inst$names)) {
    data <- data[idx]
  }
  data
}
