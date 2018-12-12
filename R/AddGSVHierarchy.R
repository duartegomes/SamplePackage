#' @title Adding Firmographics (Geo, Segment and Vertical)
#' @description This function is a shorthand that allows you to add all three
#' firmographic fields at once.  The function will call the following three functions:
#' \enumerate{
#' \item{\code{\link{AddGeoHierarchy}}}
#' \item{\code{\link{AddSegmentHierarchy}}}
#' \item{\code{\link{AddVerticalHierarchy}}}
#' }
#' The functions are called without hierarchy selections and so the default hierarchy will be
#' added. If you wish to select the hierarchy that will be added then you may call the required
#' functions manually and add an option to select the hierarchy.
#' @param Data A TAP data chunk that must have a uuid field.
#' @export
#' @author JTA - The Data Scientists
#' @family RDR Integration Tools
#' @seealso \code{\link{TAPChunks}}
AddGSVHierarchy <- function(Data) {
  # Add Data from the Demogs File
  Data <- TAPChunks:::AddFirmographics(Data)
  # Now add the default hierarchies
  Data <-
    AddGeoHierarchy(
      AddSegmentHierarchy(
        AddVerticalHierarchy(Data)
      )
    )
  Data
}
