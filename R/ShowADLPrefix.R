#' @title Find the filename prefix associated with a source
#' @description The TAP Data Lake stores files with a unique filename.  The
#' name consists of a prefix (that depends on the source) and the timestamp.
#' This function tells you the prefix text that the system uses for the selected source.
#' @inheritParams ShowADLPath
#' @author JTA - The Data Scientists
#' @return Returns a string with the prefix for the given source
#' @keywords internal
#' @family Internal Utilities
#' @examples TAPChunks:::ShowADLPrefix("Email")
#' @seealso \code{\link{TAPChunks}}
ShowADLPrefix <- function(Source = "") {
  if (!exists("Configuration_env", envir = .GlobalEnv)) {
    TAPChunks:::GetConfiguration()
  }
  source_available <- names(Configuration_env$Source)

  if (length(intersect(tolower(Source), tolower(source_available))) == 0) {
    Index_source <- menu(
      choices = c(source_available), title =
        "The requested source does not exist, choose one of the following or press 0 to cancel the process:"
    )

    if (Index_source == 0) {
      TAPChunks:::catError("No source selected.")
    }
  } else {
    Index_source <- which(tolower(source_available) == tolower(Source))
  }

  Source <- source_available[Index_source]

  prefix <- Configuration_env$Source[[Source]]$prefix
  if (is.null(prefix)) TAPChunks:::catWarning("Source is unknown", "unknown")

  return(prefix)
}
