#' @title Add Segment hierarchy to a data chunk
#' @description This function allows you to add a Segment hierarchy to the chunk.
#' @section Segment Hierarchy:
#' The function accesses the RDR and retrieves the standard geography
#' hierarchy that is matched to the segment names that the file has.
#' @section Hierarchy available:
#' \tabular{rllll}{
#' \strong{Hierarchy Name}   \tab \strong{Level1} \tab \strong{Level2} \tab \strong{Level3} \tab \strong{Level4}\cr
#' \strong{Org Segment (default)}      \tab Sector          \tab SubSector       \tab Segment         \tab Segment_Code}
#' @section Columns fields description:
#' \tabular{ll}{
#' \strong{Sector}      \tab Consumer, Commercial or Public\cr
#' \strong{SubSector}   \tab An aggregation of the segment\cr
#' \strong{Segment}     \tab The segment name\cr
#' \strong{Segment_Code}\tab A three character mnemonic}
#' @section AddGeoHierarchy Reference Data Repository
#' @inheritParams AddGeoHierarchy
#' @family RDR Integration Tools
#' @export
#' @author JTA - The Data Scientists
#' @examples AddSegmentHierarchy(TestEmailChunk)
#' @seealso \code{\link{TAPChunks}}
AddSegmentHierarchy <- function(Data, SelectHierarchy = NULL) {
  if (!"Segment_Code" %in% names(Data)) Data <- TAPChunks:::AddFirmographics(Data, Dimension = "S")

  if (is.null(SelectHierarchy)) {
    SelectHierarchy <- "OrgSegment"
  }
  TAPChunks:::AddDimension(Data = Data, Dimension = "S", SelectHierarchy = SelectHierarchy)
}
