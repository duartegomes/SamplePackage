#' @title Documenting a data file's columns
#' @description A function to write the Roxygen code to describe the columns in a data file.
#' @details This function will produce text that can be copied into the roxygen comments
#' section of a datafile description.  The resulting text will add a table to the documentation with
#' two columns: Column Name and Description.  The first column is then filled with a list of
#' all of the columns in the data file that was passed to this function.
#' @param df a Data Table
#' @author JTA - The Data Scientists
#' @keywords internal
#' @examples cat(DocumentFileColumns(TestEmailChunk))
#' @family Documentation Support Tools
DocumentFileColumns <- function(df) {
  stopifnot(is.data.frame(df))
  head <- paste("\\emph{", names(df), "}", collapse = " \\tab \\cr \n#' ", sep = "")
  paste("#' \\tabular{ll}{\n#' ",
    "\\strong{Column Name} \\tab \\strong{Description} \\cr\n#' ",
    head,
    "\\tab \\cr}",
    sep = ""
  )
}
