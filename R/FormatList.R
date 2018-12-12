#' @title FormatList
#' @description Format the list in columns
#' @param ListElement List to be applied
#' @param Title List title
#' @param NumberCol Number the columns the list will have
#' @family Internal Utilities
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
FormatList <- function(ListElement,
                       Title = "Title:",
                       NumberCol = 3) {
  formatColumns <-
    unique(c(seq(
      from = 0, to = length(ListElement), NumberCol
    ), length(ListElement)))

  numberChar <- c()
  for (n in 0:NumberCol) {
    if (n == 0) {
      Index <- formatColumns[-1]
    } else {
      Index <- formatColumns
    }
    Index <- Index + n
    Index <- Index[which(Index <= max(formatColumns))]

    numberChar <- max(nchar(ListElement[Index]))

    for (i in Index) {
      test <- nchar(ListElement[i])
      ListElement[i] <-
        paste0(ListElement[i], paste0(rep(" ", numberChar - test), collapse = ""))
    }
  }

  ListElement[formatColumns] <-
    paste0(ListElement[formatColumns], "\n\t")

  if (length(ListElement) > 9) {
    numberTime <- as.character(1:length(ListElement))
    numberTime[1:9] <- paste0(" ", numberTime[1:9])
    numberTime[1] <- paste0("\t\t", numberTime[1])
  } else {
    numberTime <- as.character(1:length(ListElement))
    numberTime <- paste0(" ", numberTime)
    numberTime[1] <- paste0("\t\t", numberTime[1])
  }

  return(
    paste(
      sprintf("%s\n", Title),
      paste(
        numberTime,
        ListElement,
        sep = " - ",
        collapse = "\t"
      )
    )
  )
}
