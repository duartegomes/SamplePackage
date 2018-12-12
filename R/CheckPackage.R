
#' @title  Check for package dependencies
#' @description Check if the package exists and install in case doesn't exist.
#' @examples TAPChunks:::CheckPackage(x)
#' @param lbr Library
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
#' @keywords internal
#' @family Internal Utilities
CheckPackage <- function(lbr) {
  if (!require(lbr, character.only = TRUE)) {
    install.packages(lbr, dependencies = TRUE)
    if (!require(lbr, character.only = TRUE)) stop("Package not found")
  }
}
