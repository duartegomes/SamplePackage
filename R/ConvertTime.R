#' @title  Format seconds into a string with minutes and seconds
#' @description This routine accepts a numeric value, which can be passed as a numeric value or as a string,
#' and will return a formatted string.
#' @examples TAPChunks:::ConvertTime(25)
#' TAPChunks:::ConvertTime(3600)
#' @param TimeSec A numeric value containing the number of seconds to be converted.
#' @author JTA - The Data Scientists
#' @family Internal Functions
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
ConvertTime <- function(TimeSec) {
  if (missing(TimeSec)) {
    TAPChunks:::catError("Argument 'TimeSec' is missing, with no default.")
  } else {
    TimeSec <- round(as.numeric(TimeSec), 0)
    if (as.integer(TimeSec / 60) < 1) {
      return(paste(TimeSec, "secs", sep = " "))
    } else {
      secs <- round((TimeSec / 60 - as.integer(TimeSec / 60)) * 60, 0)
      mins <- as.integer(TimeSec / 60)
      return(paste(mins, "mins", secs, "secs", sep = " "))
    }
  }
}
