
#' @title Preparing Static HTML Documentation for an R Package
#' @description A function to write the R manual content into a Hyperlinked HTML format.
#' @details When R builds a package it writes an index file as HTML in the package
#' directory and bundles the help text into a specific format.  This function unpacks
#' the help text and creates a set of HTML files, one for each manual entry. The HTML
#' is hyperlinked.
#' @param pkg Name of the package to be documented
#' @param links A vector with the HTML links to be used.  Defaults to findHTMLlinks.
#' @author JTA Associados Lda
#' @keywords internal
#' @examples \donttest{TAPChunks:::static_help("TAPChunks")}
#' @family Documentation Support Tools
static_help = function(pkg, links = tools::findHTMLlinks()) {
  wd <- getwd()
  helpdir <- system.file('html', package = pkg)
  setwd(helpdir)
  message("Generated help files will be placed in ", helpdir)
  pkgRdDB = tools:::fetchRdDB(file.path(find.package(pkg),
                                        'help', pkg))
  force(links); topics = names(pkgRdDB)
  for (p in topics) {
    try(tools::Rd2HTML(pkgRdDB[[p]],
                   paste(p, 'html', sep = '.'),
                   package = pkg,
                   Links = links,
                   no_links = is.null(links)))
    message(p)
  }
  setwd(wd) # Get back to the current working directory
}


#' @title Documenting a Data File's Content
#' @description A function to write the Roxygen code to add an example of a file's content
#' @details When run this function will take the top 9 rows of the data table passed
#' as a parameter and convert the remailing content into a table using roxygen syntax.
#' @param df a Data Table to be documented
#' @author JTA Associados Lda
#' @keywords internal
#' @examples cat(doc_content(TestEmailChunk))
#' @family Documentation Support Tools
doc_content <- function(df) {
  stopifnot(is.data.frame(df))
  df        <- head(df, 9)
  if (intersect(names(df), "uuid") == "uuid") {
    df$uuid <- paste0(substr(df$uuid, 7, 11),"...")
  }
  if (intersect(names(df), "row_id") == "row_id") {
    df[, row_id := NULL]
  }

  align     <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))

  cols     <- lapply(df, format)
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", collapse = "\\cr\n#' ")))

  head     <- paste("\\strong{", names(col_align), collapse = "} \\tab ", sep = "")

  paste("#' \\tabular{",
        paste(col_align, collapse = ""), "}{\n#' ",
        head,
        "}\\cr\n#' ",
        contents,
        "\n#' }\n", sep = "")
}

#' @title Documenting a TAP Chunk's variables
#' @description A function to write the Roxygen code to describe the Variables in a Chunk
#' @details This function will produce text that can be copied into the roxgen comments
#' of a data file description.  It will produce a table listing all the possible values in the variable
#' column of the chunk passed to the function.
#' @param df a TAP Chunk (Data Table with a variable column)
#' @author JTA Associados Lda
#' @keywords internal
#' @examples cat(doc_variables(TestEmailChunk))
#' @family Documentation Support Tools
doc_variables <- function(df){
  stopifnot(is.data.frame(df))
  if (intersect(names(df), "variable") == "variable") {
    rows <- paste("\\emph{", unique(df[order(variable)]$variable), "}", collapse = " \\tab \\cr \n#' ")
    paste("#' \\tabular{ll}{\n#' ",
          "\\strong{variable} \\tab \\strong{Notes}\\cr \n#' ",
          rows,
          "\\tab \\cr}",
          sep = "")
  }
}

#' @title Documenting a data file's columns
#' @description A function to write the Roxygen code to describe the columns in a data file.
#' @details This function will produce text that can be copied into the roxygen comments
#' section of a datafile description.  The resulting text will add a table to the documentation with
#' two columns: Column Name and Description.  The first column is then filled with a list of
#' all of the columns in the data file that was passed to this function.
#' @param df a Data Table
#' @author JTA Associados Lda
#' @keywords internal
#' @examples cat(doc_cols(TestEmailChunk))
#' @family Documentation Support Tools
doc_cols <- function(df) {
  stopifnot(is.data.frame(df))
  head <- paste("\\emph{", names(df), "}", collapse = " \\tab \\cr \n#' ", sep = "")
  paste("#' \\tabular{ll}{\n#' ",
        "\\strong{Column Name} \\tab \\strong{Description} \\cr\n#' ",
        head,
        "\\tab \\cr}",
        sep = "")
}

