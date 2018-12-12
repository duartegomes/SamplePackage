#' @title Summarizing a file to see the sample size
#' @description This function will tell the user the sample size in a file.
#' The sample size is defined as the number of distinct orgs in the data and is
#' split between the number of unclean orgs, unweighted clean orgs and weighted clean
#' orgs.
#' @param Data A TAP Chunk to be summarised by the function.
#' @param By The name of a column in the chunk by which to segment the sample size.
#' @export
#' @family Chunk Manipulators
#' @examples ShowSampleSize(TestEmailChunk)
#' ShowSampleSize(ApplyWeightCalc(TestEmailChunk))
#' ShowSampleSize(ApplyWeightCalc(TestEmailChunk), By = "email_delivery")
#' @author JTA - The Data Scientists

ShowSampleSize <- function(Data, By = NULL) {
  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) TAPChunks:::catError("Data file not specified.")

  data_profile <- TAPChunks:::ValidateChunk(Data)
  if (data_profile$valid) {
    Data [, None := 1] # We need a default column to use to join when there are no other columns selected
    byv <- c(By, "None") # We automatically add the default column to the by
    tov <- "None"
    # We load the demog database if it is not available so we can see the unclean orgs
    if (!TAPChunks:::CheckDemogDB()) TAPChunks:::LoadDemogDB()

    option <- bitwAnd(data_profile$status, 0x03) # Extract the weight and timestamp status
    if (option == 2 | option == 3) {
      if (length(intersect(By, "timestamp")) == 0) {
        byv <- c("timestamp", byv)
        tov <- c("timestamp", tov) # Having a timestamp we add it to the by unless the user did
      }
    }

    # The dirty sample size (dss) and unweighted sample size (uss) can now be calculated
    dss <- Data[ (uuid %in% TAP_env$unclean), .(dirty_sample = data.table::uniqueN(uuid)), by = byv]
    uss <- Data[!(uuid %in% TAP_env$unclean), .(unweighted_sample = data.table::uniqueN(uuid)), by = byv]

    # We also add the dirty sample total and unweighted sample total
    dst <- Data[ (uuid %in% TAP_env$unclean), .(dst = data.table::uniqueN(uuid)), by = tov]
    ust <- Data[!(uuid %in% TAP_env$unclean), .(ust = data.table::uniqueN(uuid)), by = tov]

    # Weighted sample size may now be run
    if (option == 0 | option == 2) { # No weight so we just prepare a blank column
      wss <- Data[, .(weighted_sample = "***"), by = byv]
    }

    if (option == 1 | option == 3) { # There is a weight so we need to calculate the wss.
      # A two step calculation where we count orgs but by weight
      # We then prepare a weighted average
      wss <- Data[, .(uss = data.table::uniqueN(uuid)), by = c(byv, "weight")][, # Run the first stage where we get raw org counts by weight
        .(weighted_sample = round(sum(uss * weight), 1)),
        by = byv
      ]

      wst <- Data[, .(uss = data.table::uniqueN(uuid)), by = c(tov, "weight")][, # Run the first stage where we get raw org counts by weight
        .(wst = round(sum(uss * weight), 1)),
        by = tov
      ]
      if (!is.null(By)) {
        wss <- merge(wss, wst, by = tov)
        wss[, weighted_pc := round(weighted_sample / wst, 6)]
        data.table::setnames(wss, old = "wst", new = "weighted_total")
      }
    }

    if (!is.null(By)) {
      uss <- merge(uss, ust, by = tov)
      uss[, unweighted_pc := round(unweighted_sample / ust, 6)]
      data.table::setnames(uss, old = "ust", new = "unweighted_total")
    }

    if (nrow(dss) == 0) {
      return(merge(wss, uss, by = byv)                        [, -c("None")])
    } else {
      return(merge(merge(wss, uss, by = byv, sort = TRUE), dss, all = T, by = byv, sort = TRUE)[, -c("None")])
    }
  }
}
