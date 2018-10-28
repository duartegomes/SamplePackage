
#' @title  CheckPackage
#' @description Check if the package exists and install in case doesn't exist.
#' @examples CheckPackage(x)
#' @param lbr Library
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
CheckPackage <- function(lbr)
{
  if (!require(lbr, character.only = TRUE))
  {
    install.packages(lbr, dependencies = TRUE)
    if(!require(lbr, character.only = TRUE)) stop("Package not found")
  }
}
