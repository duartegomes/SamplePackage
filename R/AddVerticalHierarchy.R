#' @title Add Vertical hierarchy to a data chunk
#' @description This function allows you to add a Vertical hierarchy to the chunk.
#' @section Vertical Hierarchy:
#' The function accesses the RDR and retrieves the standard vertical
#' hierarchy that is matched to the vertical names that the file has.
#' @section Hierarchy available:
#' \tabular{llll}{
#' \strong{Hierarchy Name}         \tab \strong{Level1} \tab \strong{Level2} \tab \strong{Level3}    \cr
#' \strong{Org Vertical (default)} \tab Vertical        \tab Industry        \tab IndustrySector}
#' @family RDR Integration Tools
#' @inheritSection AddGeoHierarchy Reference Data Repository
#' @export
#' @import data.table
#' @inheritParams AddGeoHierarchy
#' @author JTA - The Data Scientists
#' @examples AddVerticalHierarchy(TestEmailChunk)
#' @seealso \code{\link{TAPChunks}}
AddVerticalHierarchy <- function(Data, SelectHierarchy = NULL) {
  if (!"Vertical" %in% names(Data)) Data <- TAPChunks:::AddFirmographics(Data, Dimension = "V")
  Data
}
