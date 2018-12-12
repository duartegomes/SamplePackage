#' @title Forming the Longitudinal Data Set
#' @export
#' @description This function considers each organisation identifier that exists in
#' the submitted chunk and looks to see the number of distinct time periods in which
#' each organization appears. Then the data is filtered so that any organizations that
#' don't have data for all of the periods in the submitted chunk are removed.
#' The resulting chunk will have the same cohort of organizations in each period.
#' @param Data A data chunk that must have an org identifier and a timestamp column.
#' @return The data chunk with just the organization in the longitudinal cohort.
#' @examples ApplyLongitudinalSet(TestEmailChunk)
#' @import data.table
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
#' @family Weighting and cleaning functions
ApplyLongitudinalSet <- function(Data) {
  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) TAPChunks:::catError("Data file not specified.")

  profile <- TAPChunks:::ValidateChunk(Data)
  if (profile$valid) {
    if (bitwAnd(profile$status, 0x02) == 0x02) {
      max_periods <- Data[, data.table::uniqueN(timestamp)]
      long_set <-
        Data[, .(periods = data.table::uniqueN(timestamp)), by = "uuid"][periods == max_periods]$uuid
      orig_set <- Data[, data.table::uniqueN(uuid)]
      TAPChunks:::catInfo(paste(
        "Submitted data set has ",
        orig_set,
        " organizations covering ",
        max_periods,
        " periods."
      ))
      TAPChunks:::catInfo(paste(
        "The longitudinal set has ",
        length(long_set),
        " oranizations which appear in all periods."
      ))
      TAPChunks:::catInfo("This new data should be reweighted.")
      return(Data[uuid %in% long_set])
    } else {
      TAPChunks:::catInfo("No timestamp detected so we can't form the longitudinal set.")
    }
  } else {
    TAPChunks:::catInfo("Longitudinal process not run due to previous validation error.")
  }
}
