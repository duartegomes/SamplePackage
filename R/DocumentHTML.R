#' @title Preparing Static HTML Documentation for an R Package
#' @description A function to write the R manual content into a Hyperlinked HTML format.
#' @details When R builds a package it writes an index file as HTML in the package
#' directory and bundles the help text into a specific format.  This function unpacks
#' the help text and creates a set of HTML files, one for each manual entry. The HTML
#' is hyperlinked.
#' @param pkg Name of the package to be documented
#' @param links A vector with the HTML links to be used.  Defaults to findHTMLlinks.
#' @author JTA - The Data Scientists
#' @keywords internal
#' @examples \donttest{TAPChunks:::static_help("TAPChunks")}
#' @family Documentation Support Tools
DocumentHTML <- function(pkg, links = tools::findHTMLlinks()) {
  wd <- getwd()
  helpdir <- system.file("html", package = pkg)
  setwd(helpdir)
  message("Generated help files will be placed in ", helpdir)
  pkgRdDB <- tools:::fetchRdDB(file.path(
    find.package(pkg),
    "help", pkg
  ))
  force(links)
  topics <- names(pkgRdDB)
  for (p in topics) {
    try(tools::Rd2HTML(pkgRdDB[[p]],
      paste(p, "html", sep = "."),
      package = pkg,
      Links = links,
      no_links = is.null(links)
    ))
    message(p)
  }
  setwd(wd) # Get back to the current working directory
}
