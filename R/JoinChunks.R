
#' @title Joining data chunks to form one
#' @description
#' This function joins chunks of data together to form one appended table.
#' @details
#' This function performs a row join of data chunks by appending the chunks
#' together. The resulting data chunk has all the data from all submitted chunks and
#' so is a full outer join. If the submitted chunks have the same column format then
#' the resulting chunk will also retain this format. If either of the submitted chunks
#' has a column not present in the other then this will not join.\cr\cr
#' The data is not reweighted after join.
#' @family Chunk Manipulators
#' @param ... Chunks to be combined
#' @export
#' @author JTA - The Data Scientists
#' @return Returns a chunk with the chunks provided combined together
#' @seealso \code{\link{TAPChunks}}
JoinChunks <- function(...) {
  tables <- list(...)
  names(tables) <- match.call(expand.dots = FALSE)$...

  if (length(tables) == 0) {
    TAPChunks:::catError("Chunk not provided.")
  }

  # If the dataset are NULL it just delete the dataset
  tables[lapply(tables, is.null) == T] <- NULL

  # if any of the datasets are not data.table, tries to set it as DT
  try(lapply(tables[!sapply(tables, data.table::is.data.table)], data.table::setDT), silent = T)

  # if the dataset is not a data.table it stops.
  if (!all(sapply(tables, function(x) {
    identical(class(x), c("data.table", "data.frame"))
  }))) {
    TAPChunks:::catError("Cannot join due to different classes, please provide chunks as data.table")
  }

  # remove all tables that dont have any rows
  tables[lapply(tables, nrow) == 0] <- NULL

  if (length(tables) == 1) {
    return(tables[[names(tables)]])
  } else if (length(tables) == 0) {
    TAPChunks:::catError("All Chunks provided are invalid")
  }

  # check if there is no missing columns
  if (all(sapply(tables, function(x) length(match(names(x), names(tables[[1]]))) == length(names(tables[[1]]))))) {
    if (sum(sapply(tables, nrow)) < 100000) {
      # we can add , fill = TRUE to improve the function.
      result <- data.table::rbindlist(tables, use.names = TRUE)
      return(result)
    }

    charList <-
      names(grep("character", lapply(tables[[1]], class), value = T))
    if (length(charList) != 0) {
      tables[[1]][, charList] <- lapply(tables[[1]][, charList, with = F], as.factor)
    }

    factorList <- names(grep("factor", lapply(tables[[1]], class), value = T))

    cols <- data.table::copy(names(tables[[1]]))
    result <- as.list(tables[[1]])

    tables[[1]] <- NULL

    while (length(tables) != 0) {
      b <- data.table::copy(tables[[1]])

      factDiff <- setdiff(factorList, names(grep("factor", lapply(tables[[1]], class), value = T)))
      if (length(factDiff) != 0) {
        b[, factDiff] <- lapply(b[, factDiff, with = F], as.factor)
      }

      for (j in 1:length(cols)) {
        col <- cols[j]

        if (col %in% factorList) {
          levels(result[[col]]) <-
            unique(c(levels(result[[col]]), levels(b[[col]])))
          result[[col]][(length(result[[col]]) + 1):(length(result[[col]]) + length(b[[col]]))] <-
            b[[col]]
        } else {
          result[[col]] <- append(result[[col]], b[[col]])
        }
        if (ncol(b) == 1) {
          tables[[1]] <- NULL
          rm(b)
        } else {
          b[, (col) := NULL]
        }
      }
    }
    rm(tables)
    data.table::setDT(result, F)
  } else {
    if (all(sapply(sapply(tables, function(x) match(names(x), names(tables[[1]])))[-1], function(x) {
      length(x[!is.na(x)])
    })) == 0) {
      TAPChunks:::catError("Name of columns don't match!")
    } else {
      diffColumns <- sapply(sapply(tables, function(x) match(names(x), names(tables[[1]]))), length)
      TAPChunks:::catError(sprintf(
        "Inconsistent number of columns in %s: %s",
        names(diffColumns), diffColumns
      ))
    }
  }
}
