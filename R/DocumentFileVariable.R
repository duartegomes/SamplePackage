#' @title Documenting a TAP Chunk's variables
#' @description A function to write the Roxygen code to describe the Variables in a Chunk
#' @details This function will produce text that can be copied into the roxgen comments
#' of a data file description.  It will produce a table listing all the possible values in the variable
#' column of the chunk passed to the function.
#' @param df a TAP Chunk (Data Table with a variable column)
#' @author JTA - The Data Scientists
#' @keywords internal
#' @examples cat(doc_variables(TestEmailChunk))
#' @family Documentation Support Tools
DocumentFileVariable <- function(df) {
  stopifnot(is.data.frame(df))
  if (intersect(names(df), "variable") == "variable") {
    rows <- paste("\\emph{", unique(df[order(variable)]$variable), "}", collapse = " \\tab \\cr \n#' ")
    paste("#' \\tabular{ll}{\n#' ",
      "\\strong{variable} \\tab \\strong{Notes}\\cr \n#' ",
      rows,
      "\\tab \\cr}",
      sep = ""
    )
  }
}
