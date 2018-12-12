#' @title  Select Columns in Chunk
#' @description This function allows to select which columns from the chunk to extract
#' @examples SelectColumns(Source = Chunk, Columns = c("uuid", "variable", "value"))
#' @param Source chunk
#' @param Columns List of name columns to extract
#' @author JTA - The Data Scientists
#' @export
#' @family Chunk Manipulators
#' @seealso \code{\link{TAPChunks}}
SelectColumns <- function(Source = NULL, Columns = NULL) {
  Columns <- gsub("\\s", "", Columns)

  if (is.null(Source)) {
    TAPChunks:::catError("\nPlease select a valid Source.")
  }

  if (is.null(Columns)) {
    TAPChunks:::catError("\nPlease select the list of columns to extract.")
  }

  if (length(intersect(names(Source), Columns)) == length(Columns)) {
    Source <- data.table::data.table(Source)
    chunk <- Source[, Columns, with = F]
  } else {
    diff.columns <- setdiff(Columns, names(Source))
    TAPChunks:::catError(sprintf("\n Column(s) not found: %s", diff.columns))
  }

  return(chunk)
}
