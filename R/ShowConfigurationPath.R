#' @title Establishing and pointing to the configuration settings
#' @description This is an internal function that will help to locate the directory
#' with configuration settings. The function is usually called without a parameter
#' and it returns the path to the configuration directory
#' (which is called TAPConfiguration and is stored in the user's home directory).
#' @param Directory When not given the function returns the path to the user's home
#' directory with \emph{TAPConfiguration} appended. If the user supplies a string
#' that refers to a directory that does not yet exist, a directory will be created.
#' @author JTA - The Data Scientists
#' @examples TAPChunks:::ShowConfigurationPath()
#' @family Internal Utilities
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
#' @return A string contining the configuration path
ShowConfigurationPath <- function(Directory) {
  if (missing(Directory)) {
    Configuration.path <- path.expand("~")
    Configuration.path <-
      paste0(Configuration.path, "/TAPConfiguration")
  } else {
    Configuration.path <- path.expand(Directory)
  }
  if (!file.exists(Configuration.path)) {
    dir.create(Configuration.path)
  }
  return(Configuration.path)
}
