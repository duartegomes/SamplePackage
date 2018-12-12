#' @title  Merge Chunks Objects
#' @description This function merges 2 datasets/ chunks.
#' @examples MergeChunks(TestEmailChunk, TestClientChunk, by = "uuid, timestamp")
#' @param x,y Tables to be merged
#' @param ... by Column names in x and y to merge on or
#' by.x, by.y Vectors of column names in x and y to merge on
#' @param join "inner", "outer", "left" or "right"
#' @author JTA - The Data Scientists
#' @export
#' @family Chunk Manipulators
#' @seealso \code{\link{TAPChunks}}
MergeChunks <- function(x, y, ..., join = "inner") {
  if (!is.data.frame(x)) {
    TAPChunks:::catError(sprintf("\nError: %s is not a table", deparse(substitute(x))))
  }
  if (!is.data.frame(y)) {
    TAPChunks:::catError(sprintf("\nError: %s is not a table", deparse(substitute(y))))
  }

  suffixes.x <- paste0(".", deparse(substitute(x)))
  suffixes.y <- paste0(".", deparse(substitute(y)))

  parameters <- c("by.x", "by.y", "by")

  list.parameters <- list(...)
  if (length(setdiff(names(list.parameters), parameters)) != 0) {
    TAPChunks:::catError("\nThe parameters are not well defined.")
  }

  parameters.list <- NULL

  for (i in parameters) {
    parameters.list <- c(parameters.list, sum(i == names(list.parameters)))
  }

  joinlist <- c("inner", "outer", "left", "right")

  if (!(join %in% joinlist)) TAPChunks:::catError(sprintf("The join command needs to be one of the following %s", paste0(joinlist, collapse = ", ")))

  join <- switch(tolower(join),
    "inner" = "inner",
    "outer" = "all",
    "left" = "all.x",
    "right" = "all.y"
  )

  if (sum(parameters.list == c(1, 1, 0)) == 3) {
    key.x <- gsub("\\s", "", (list.parameters$by.x))
    key.x <- strsplit(key.x, ",")[[1]]

    if (length(setdiff(key.x, names(x))) != 0) {
      TAPChunks:::catError("\nColumn name error.")
    }

    key.y <- gsub("\\s", "", (list.parameters$by.y))
    key.y <- strsplit(key.y, ",")[[1]]

    if (length(setdiff(key.y, names(y))) != 0) {
      TAPChunks:::catError("\nColumn name error.")
    }

    return(eval(parse(text = sprintf("merge(
                                      x, by.x = key.x,
                                      y, by.y = key.y,
                                      suffixes = c(suffixes.x, suffixes.y),
                                      allow.cartesian = T, %s = TRUE)", join))))
  } else {
    if (sum(parameters.list == c(0, 0, 1)) == 3) {
      key <- gsub("\\s", "", (list.parameters$by))
      key <- strsplit(key, ",")[[1]]

      if (length(setdiff(key, names(x))) != 0) {
        TAPChunks:::catError("\nColumn name error.")
      }

      if (length(setdiff(key, names(y))) != 0) {
        TAPChunks:::catError("\nColumn name error.")
      }

      return(eval(parse(text = sprintf("merge(
                                        x, y, by = key,
                                        suffixes = c(suffixes.x, suffixes.y),
                                        allow.cartesian = T, %s = TRUE)", join))))
    } else {
      if (sum(parameters.list == c(0, 0, 0)) == 3) {
        key <-
          intersect(names(x)[!sapply(x, is.integer)], names(y)[!sapply(y, is.integer)])
        key <- setdiff(key, "variable")

        if (length(key) == 0) {
          TAPChunks:::catError("There isn't connection between the tables.")
        }

        TAPChunks:::catInfo(sprintf("Default key is:\n\t-%s", paste(key, collapse = "\n\t-")))

        return(eval(parse(text = sprintf("merge(
                                          x, y, by = key,
                                          suffixes = c(suffixes.x, suffixes.y),
                                          allow.cartesian = T, %s = TRUE)", join))))
      } else {
        TAPChunks:::catError("\nEither select the 'by' or select 'by.x' and the 'by.y'.")
      }
    }
  }
}
