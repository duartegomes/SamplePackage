#' @title Preparing a range of timestamps
#' @description
#' A function that will accept the start and end timestamps and turn this information
#' into a vector with has all of the timestamps from the first until the last exclusively.
#' @details This function may be used to conserve memory when dealing with large files.
#' The function returns a vector with the range of timestamps so that a script may process
#' the chunks on a month by month basis, clearing the memory between each month.
#' @export
#' @examples TimeStamps(from = "2017M01", to = "2017M12")
#' @param From A timestamp to indicate the beginning of the range
#' @param To A timestamp to indicate the end of the range
#' @family Internal Utilities
#' @return Returns a vector of all posible timestamps between from and to
#' @author JTA - The Data Scientists
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
TimeStamps <- function(From = "", To = "") {
  if (!TAPChunks:::CheckDemogDB()) TAPChunks:::LoadDemogDB()

  if (To == "") To <- From

  From <- gsub("_", "M", From)
  To <- gsub("_", "M", To)

  months <- c("M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", "M09", "M10", "M11", "M12")
  years <- as.character(2000:2030)
  choices <- data.table::CJ(month = months, year = years)
  choices[, version := paste0(year, month) ]
  choices[version <= Configuration_env$Version$Max, available := "yes"]
  choices[is.na(available), available := "no"]
  choices <- choices[version >= From & version <= To][order(version)]

  noAvailable <- choices[available == "no"]$version
  if (length(noAvailable) > 0) {
    if (length(noAvailable) == 1) {
      TAPChunks:::catWarning(sprintf("The following month is not available:\n\t\t-%s", paste0(noAvailable, collapse = "\n\t\t-")), "month is not available")
    } else {
      TAPChunks:::catWarning(sprintf("The following months are not available:\n\t\t-%s", paste0(noAvailable, collapse = "\n\t\t-")), "month are not available")
    }
  }
  timestamp <- choices[available == "yes"]$version

  return(timestamp)
}
