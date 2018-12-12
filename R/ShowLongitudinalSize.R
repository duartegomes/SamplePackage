#' @title Show the Size of a Longitudinal Set
#' @export
#' @description This function considers each organisation identifier that exists in
#' the submitted chunk and looks to see the number of distinct time periods in which
#' each organization appears. The function then reports the size of the logitudinal
#' cohort for both Clean and not Clean orgs.
#' @param Data A data chunk that must have an org identifier and a timestamp column.
#' @examples ShowLongitudinalSize(TestEmailChunk)
#' @author JTA - The Data Scientists
#' @family Weighting and cleaning functions
#' @seealso \code{\link{TAPChunks}}
ShowLongitudinalSize <- function(Data) {
  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) TAPChunks:::catError("Data file not specified.")

  data_profile <- TAPChunks:::ValidateChunk(Data)
  if (data_profile$valid) {
    if (!TAPChunks:::CheckDemogDB()) TAPChunks:::LoadDemogDB()

    ls <- Data[!uuid %in% TAP_env$unclean, data.table::uniqueN(timestamp),
      by = uuid
    ][
      V1 == max(V1),
      data.table::uniqueN(uuid)
    ]
    ds <- Data[ uuid %in% TAP_env$unclean, data.table::uniqueN(timestamp),
      by = uuid
    ][
      V1 == max(V1),
      data.table::uniqueN(uuid)
    ]
    TAPChunks:::catInfo(paste("Clean == 'Y' organizations in longitudinal:", ls))
    TAPChunks:::catInfo(paste("Clean == 'N' organizations in longitudinal:", ds))
    return(ls)
  } else {
    TAPChunks:::catError("This function needs to have a valid chunk passed.")
  }
}
