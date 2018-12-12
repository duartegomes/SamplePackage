#' @title Filtering data chunks
#' @description This function allows users to apply a filter to a data chunk to receive
#' a subset of the input data.
#' @family Chunk Manipulators
#' @import data.table
#' @export
#' @author JTA - The Data Scientists
#' @param Data A data chunk
#' @param Filter A string defining the filter condition.  This is a calculation that must
#' return a boolean value of TRUE or FALSE. For a lot more information on writing calculations
#' please refer to \code{\link{CalculateColumn}}.
#' @examples FilterChunk(TestEmailChunk, Filter = variable == 'exchange_2010')
#' @seealso \code{\link{TAPChunks}}
FilterChunk <- function(Data, Filter = 1 == 1, ...) {
  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) TAPChunks:::catError("Data file not specified.")

  f1 <- substitute(Filter)
  if (typeof(f1) != "language") TAPChunks:::catError("Syntax error in filter definition.")
  if (deparse(f1) == "1 == 1") TAPChunks:::catError("Missing or incomplete filter definition")
  e <- try({
    ret <- Data[eval(f1)]
    ret <- droplevels(ret)
    return(ret)
  }
  ,
  silent = T
  )

  e <- strsplit(e, ":")[[1]][2]

  TAPChunks:::catError(e)
}
