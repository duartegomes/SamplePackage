#' @title Stripping Spiceworks chunks of unclean records
#' @description When we load Spiceworks demographics data we allocate taxonomy data
#' for Geography, Segment and Vertical.  When an org identifier (UUID) has all three
#' of these it is marked as being clean.  This function allows us to easily remove
#' all the orgs that are not clean from a data set.
#' @details
#' The function has a dependency in that the demographics data base needs to be loaded
#' to the R environment. A user may check this by running CheckDemogDB().  The demographics
#' data base is established by running LoadDemogDB ().
#' \cr
#' When this function is called it first validates that it has been sent a valid chunk.
#' Next it uses the uuid column to locate all rows in the input file that have an org identifier
#' that is listed as not being clean in the demographics database.  These rows are
#' filtered from the data before it is returned.
#' @examples  email <- CleanChunk(TestEmailChunk)
#' @author JTA - The Data Scientists
#' @param Data Chunk to be cleaned
#' @return Returns a data table containing only the rows in the original file that are
#' associated with clean organizations. If the routine was not able to run then the
#' original data set is returned unaltered.
#' @export
#' @family Weighting and cleaning functions
#' @seealso \code{\link{TAPChunks}}
CleanChunk <- function(Data) {
  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) TAPChunks:::catError("Data file not specified.")

  if (TAPChunks:::CheckDemogDB()) {
    if (TAPChunks:::ValidateChunk(Data)$valid) {
      Data <- subset(Data, uuid %in% TAP_env$demog_helper$uuid) # remove uuid not available in Demogs
      Data <- subset(Data, !(uuid %in% TAP_env$unclean))
      Data <- droplevels(Data)
      return(Data)
    } else {
      return(Data)
    }
  } else {
    return(Data)
  }
}
