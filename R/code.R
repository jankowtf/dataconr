#' @export
createListIndex <- function(x, sep = c("[['", "']][['", "']]")) {
  paste0(sep[1], paste(x, collapse = sep[2]), sep[3])
}

# File system -------------------------------------------------------------

#' @title
#' Map directory structure
#'
#' @description
#' Maps directory structure to a \code{\link{list}}.
#'
#' @details
#' Whether or not only directories or actual files should be included can
#' be controlled by \code{include_files}.
#'
#' @param dir \code{\link[base]{character}}.
#'  Directory path.
#' @param include_root \code{\link[base]{logical}}.
#'  \code{TRUE}: include root directory;
#'  \code{FALSE}: do not include root directory. See examples.
#' @param include_files \code{\link[base]{logical}}.
#'  \code{TRUE}: include actual files;
#'  \code{FALSE}: do not include files. See examples.
#' @param include_files \code{\link[base]{logical}}.
#'  \code{TRUE}: include actual files;
#'  \code{FALSE}: do not include files. See examples.
#' @return \code{\link[base]{list}}.
#' @example inst/examples/mapDirectory.R
#' @template authors
#' @template references
#' @export
mapDirectory <- function(
  dir = ".",
  include_root = TRUE,
  include_files = TRUE,
  relative = FALSE
) {
  if (length(dir) > 1) {
    stop("mapDirectory: only allowed for atomic directory input")
  }
  dir <- gsub("\\\\", "/", dir)

  if (relative) {
    wd <- setwd(dir)
    dir <- "."
    on.exit(setwd(wd))
  }

  dirs <- list.dirs(dir, full.names = TRUE, recursive = TRUE)
  files <- list.files(dir, full.names = TRUE, recursive = TRUE)
  files_dirs <- unique(dirname(files))

  dirs <- dirs[dirs != ""]
  files <- files[files != ""]
  files_dirs <- files_dirs[files_dirs != ""]

  splitted <- strsplit(dirs, "/")
  #   level_1=splitted[[3]]
  #   level_2=3
  out <- list()
  for(level_1 in splitted) {
    for(level_2 in 1:length(level_1)) {
      idx <- createListIndex(level_1[1:level_2])
      path <- createListIndex(level_1[1:level_2], sep = c("", "/", ""))
      expr_get <- sprintf("%s%s", "out", idx)
      expr_set <- if (path %in% files_dirs && include_files) {
        sprintf("%s%s <- as.list(list.files(path))", "out", idx)
      } else {
        sprintf("%s%s <- list()", "out", idx)
      }
      tmp <- eval(parse(text = expr_get))
      if (is.null(tmp)) {
        eval(parse(text = expr_set))
      }
    }
  }
  if (!include_root) {
    out <- out[[1]]
  }
  out
}


# Map datasource to list --------------------------------------------------

mapDatasourceToList <- function(con) {
  UseMethod("mapDatasourceToList", con)
}

#' @title
#' Map Neo4j database to list
#'
#' @description
#' Maps Neo4j database as provided by package \code{RNeo4j}
#' (\url{https://cran.rstudio.com/web/packages/RNeo4j/index.html})
#' to \code{\link{list}}.
#'
#' @details
#' TODO
#'
#' @param con \code{\link[RNeo4j]{graph}}.
#'  Neo4j graph.
#' @return \code{\link[base]{list}}.
#' @example inst/examples/mapDatasourceToList.graph.R
#' @template authors
#' @template references
#' @export
mapDatasourceToList.graph <- function(
  con
) {
  stop("mapDatasourceToList.graph::not refactored yet")
  sumry <- suppressMessages(summary(con))
  query <- sapply(1:nrow(sumry), function(row) {
    string <- "
      MATCH (this:%s)-[to:%s]->(that:%s)
      RETURN DISTINCT this as this, to AS to, that AS that
    "
    sprintf(string, sumry[row, 1], sumry[row, 2], sumry[row, 3])
  })
#   tmp <- cypherToList(graph, query[[1]])
  tmp[[1]]
  attributes(tmp[[1]]$this)
  as.list(tmp[[1]]$this)
  # tmp[[1]]$this[[1]]
#   cypherToList(graph, "MATCH (movie:Movie) RETURN movie")
#   cypherToList(graph, "MATCH (n) RETURN n")
#   cypherToList(graph, "MATCH (n)-->(rel:) RETURN rel")

  sumry$Query <- query

  ## Find out relationships
  # ii=6
  idx <- lapply(1:length(sumry$This), function(ii) {
    pos_this <- ii
    this <- sumry$This[pos_this]

    pos_that <- which(sumry$That == this)
  })
  idx_keep <- sapply(idx, length) == 0

  # q=query[[1]]
  paths <- lapply(query, function(q) {
    res <- cypherToList(con, q)
    # nms <- sapply(res, "[[", "this")
    # structure(lapply(res, function(ii) unname(c(unlist(ii)))), names = nms)
    lapply(res, function(ii) unname(c(unlist(ii))))
  })
  paths[[1]]

  ii=2
  for (ii in rev(1:length(paths))) {
    idx_this <- idx[[ii]]
    if (length(idx_this)) {
      path_2=paths[[ii]][[1]]
      ii_idx_this=idx_this[2]
      for (ii_idx_this in idx_this) {
        path_1=paths[[ii_idx_this]][[1]]
        tmp <- lapply(paths[[ii]], function(path_2) {
          tmp <- sapply(paths[[ii_idx_this]], function(path_1) {
            if (path_2[1] == path_1[length(path_1)]) {
              unique(c(path_1, path_2))
            }
          })
          is_null <- sapply(tmp, is.null)
          tmp[!is_null]
        })
        tmp <- unlist(tmp, recursive = FALSE)
        paths[[ii_idx_this]] <- tmp
      }
    }
  }
  splitted <- unlist(paths[idx_keep], recursive = FALSE)

  out <- list()
  for(level_1 in splitted) {
    for(level_2 in 1:length(level_1)) {
      idx <- createListIndex(level_1[1:level_2])
      # path <- createListIndex(level_1[1:level_2], sep = c("", "/", ""))
      expr_get <- sprintf("%s%s", "out", idx)
      expr_set <- sprintf("%s%s <- list()", "out", idx)
      tmp <- eval(parse(text = expr_get))
      if (is.null(tmp)) {
        eval(parse(text = expr_set))
      }
    }
  }
  out
}

#' @title
#' Map Neo4j database to list
#'
#' @description
#' Maps Neo4j database as provided by package \code{RNeo4j}
#' (\url{https://cran.rstudio.com/web/packages/RNeo4j/index.html}) and
#' wrapped by connector class \code{\link[datar]{Datasource.Neo4j}}
#' to a \code{\link{list}}.
#'
#' @details
#' TODO
#'
#' @param con \code{\link[RNeo4j]{Datasource.Neo4j}}.
#'  Neo4j graph wrapped by connector class.
#' @return \code{\link[base]{list}}.
#' @example inst/examples/mapDatasourceToList.Datasource.Neo4j.R
#' @template authors
#' @template references
#' @export
mapDatasourceToList.Datasource.Neo4j <- function(
  con
) {
  stop("mapDatasourceToList.Datasource.Neo4j::not implemented yet")
  sumry <- suppressMessages(summary(con))
  query <- sapply(1:nrow(sumry), function(row) {
    string <- "MATCH (x:%s)-[%s]->(y:%s) RETURN DISTINCT x.id as this, y.id AS that"
    # string <- "MATCH (x:%s)-[%s]->(y:%s) RETURN x.id, y.id"
    #     query <- structure(
    #       sprintf(string, sumry[row, 1], sumry[row, 2], sumry[row, 3]),
    #       names = sumry[row, 3]
    #     )
    sprintf(string, sumry[row, 1], sumry[row, 2], sumry[row, 3])
  })
  sumry$Query <- query
  idx <- lapply(sumry$This, function(ii) which(sumry$That == ii))
  idx_keep <- sapply(idx, length) == 0

  # q=query[1]
  paths <- lapply(query, function(q) {
    res <- cypherToList(con, q)
    # nms <- sapply(res, "[[", "this")
    # structure(lapply(res, function(ii) unname(c(unlist(ii)))), names = nms)
    lapply(res, function(ii) unname(c(unlist(ii))))
  })

  ii=2
  for (ii in rev(1:length(paths))) {
    idx_this <- idx[[ii]]
    if (length(idx_this)) {
      path_2=paths[[ii]][[1]]
      ii_idx_this=idx_this[2]
      for (ii_idx_this in idx_this) {
        path_1=paths[[ii_idx_this]][[1]]
        tmp <- lapply(paths[[ii]], function(path_2) {
          tmp <- sapply(paths[[ii_idx_this]], function(path_1) {
            if (path_2[1] == path_1[length(path_1)]) {
              unique(c(path_1, path_2))
            }
          })
          is_null <- sapply(tmp, is.null)
          tmp[!is_null]
        })
        tmp <- unlist(tmp, recursive = FALSE)
        paths[[ii_idx_this]] <- tmp
      }
    }
  }
  splitted <- unlist(paths[idx_keep], recursive = FALSE)

  out <- list()
  for(level_1 in splitted) {
    for(level_2 in 1:length(level_1)) {
      idx <- createListIndex(level_1[1:level_2])
      # path <- createListIndex(level_1[1:level_2], sep = c("", "/", ""))
      expr_get <- sprintf("%s%s", "out", idx)
      expr_set <- sprintf("%s%s <- list()", "out", idx)
      tmp <- eval(parse(text = expr_get))
      if (is.null(tmp)) {
        eval(parse(text = expr_set))
      }
    }
  }
  out
}

