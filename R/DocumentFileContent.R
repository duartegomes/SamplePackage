#' @title Documenting a Data File's Content
#' @description A function to write the Roxygen code to add an example of a file's content
#' @details When run this function will take the top 9 rows of the data table passed
#' as a parameter and convert the remailing content into a table using roxygen syntax.
#' @param df a Data Table to be documented
#' @author JTA - The Data Scientists
#' @keywords internal
#' @examples cat(doc_content(TestEmailChunk))
#' @family Documentation Support Tools
DocumentFileContent <- function(df) {
  stopifnot(is.data.frame(df))
  df <- head(df, 9)
  if (intersect(names(df), "uuid") == "uuid") {
    df$uuid <- paste0(substr(df$uuid, 7, 11), "...")
  }
  if (intersect(names(df), "row_id") == "row_id") {
    df[, row_id := NULL]
  }

  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))

  cols <- lapply(df, format)
  contents <- do.call(
    "paste",
    c(cols, list(sep = " \\tab ", collapse = "\\cr\n#' "))
  )

  head <- paste("\\strong{", names(col_align), collapse = "} \\tab ", sep = "")

  paste("#' \\tabular{",
    paste(col_align, collapse = ""), "}{\n#' ",
    head,
    "}\\cr\n#' ",
    contents,
    "\n#' }\n",
    sep = ""
  )
}
