#' @title Applying weights to data
#' @description
#' This function adds dynamically calculated weights to the submitted data.
#' @details
#' The weight calculation is designed to alter the distribution of the sample to match
#' the distribution of the population data that we get from the Market Model. The function offers two
#' types of weighting which can be chosen using the WType parameter. The weighting types are as follows:
#' \tabular{ll}{
#' \strong{Shape}\tab This is the default weighting type. The size of the weighted sample is normally
#' unchanged by the operation so the weighted and unweighted sample sizes of the entire table are unchanged.
#' Each individual org will be weighted so that the distribution of the sample orgs has the same shape as
#' the distribution of the population orgs.  \cr
#' \strong{Scale}\tab If the user sets the scale parameter the weighted sample size becomes equal to
#' the population as set in the market model.\cr}
#' \cr
#' The calculation is performed over a variety of dimensions, with the default being GSV
#' (by all combinations of Geography, Segment and Vertical).  Using the Granularity parameter
#' this may be changed to any combination of G S and V.\cr
#'
#' @inheritSection ShowWeightCalc Example Weighting Calculation
#' @inheritSection ShowWeightCalc Example Calculation
#' @export
#' @param Data Chunk to be weighted
#' @param Granularity Dimensions to weight. Default being GSV (by all combinations of Geography, Segment and Vertical)
#' @param WType Define the type of weighting as either \emph{Shape} (which is the default option) or \emph{Scale}
#' @examples ApplyWeightCalc(TestEmailChunk, Granularity = "S")
#' @author JTA - The Data Scientists
#' @family Weighting and cleaning functions
#' @return Returns the original data table with an additional column \emph{weight}.
#' @seealso \code{\link{TAPChunks}}
ApplyWeightCalc <- function(Data, Granularity = "gsv", WType = "Shape") {
  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) TAPChunks:::catError("Data file not specified.")

  if (TAPChunks:::CheckDemogDB()) {
    # Define the granularity for weighting
    Weight_levl <- 0L

    choices <-
      grep(
        paste0("[", Granularity, "]"),
        c("G", "S", "V"),
        ignore.case = T,
        value = T
      )
    if (length(choices) != nchar(Granularity)) {
      TAPChunks:::catError("Granularity parameter should only contain the characters G, S or V")
    }

    # According to the selection of weight type we construct a bit mask that will extract only
    # the meta data that we require

    if (length(intersect(choices, "G")) == 1) {
      Weight_levl <- bitwOr(Weight_levl, 0x003F)
    }
    if (length(intersect(choices, "S")) == 1) {
      Weight_levl <- bitwOr(Weight_levl, 0x03C0)
    }
    if (length(intersect(choices, "V")) == 1) {
      Weight_levl <- bitwOr(Weight_levl, 0x3C00)
    }

    if (Weight_levl != 0L) {
      cols <- names(Data)

      # Some sources have many repeating rows because they list devices.  This is not relevant
      # to weighting schemas so we first summarise the input to just have orgs

      orgs <- Data[, .N, by = c("uuid", "timestamp")][, N := NULL]


      firmogs <- TAP_env$demog_helper[, .(uuid,
        gsv,
        WClean = clean
      )]

      orgs <- firmogs[orgs, on = "uuid"]

      orgs[, gsv := bitwAnd(gsv, Weight_levl)]

      sample_dist <-
        orgs[, .(orgSample = data.table::uniqueN(uuid)), by = c("gsv", "WClean", "timestamp")]


      target <- TAP_env$target_helper[, .(gsv, entity_count)]

      target[, gsv := bitwAnd(gsv, Weight_levl)]

      populn_dist <- target [, .(
        orgPop = sum(entity_count),
        WClean = "Y"
      ), by = gsv]
      Weight_Calc <-
        merge(
          sample_dist,
          populn_dist,
          all.x = T,
          by = c("gsv", "WClean")
        )

      Weight_Calc[is.na(orgSample), orgSample := 0]
      Weight_Calc[is.na(orgPop), orgPop := 0]

      Weight_Calc[WClean == "Y", cs := sum(orgSample, na.rm = T), by = "timestamp"]
      Weight_Calc[, orgTarget := ifelse(orgSample == 0, 0, orgPop)]
      Weight_Calc[, distTarget := orgTarget / sum(orgTarget, na.rm = T), by = "timestamp"]
      Weight_Calc[, orgExpect := distTarget * cs]
      Weight_Calc[is.na(orgExpect), orgExpect := 0]
      Weight_Calc[, shapeW := ifelse(orgSample == 0, 0, orgExpect / orgSample)]
      Weight_Calc[, scaleW := ifelse(orgSample == 0, 0, orgTarget / orgSample)]

      if (WType == "Shape") {
        Weight_Calc[, weight := shapeW]
      } else {
        if (WType == "Scale") {
          Weight_Calc[, weight := scaleW]
        } else {
          TAPChunks:::catWarning("Unknown WeightType.")
          Weight_Calc[, weight := 0]
        }
      }

      orgs <- orgs[Weight_Calc[, c("gsv", "WClean", "timestamp", "weight")], on = c("gsv", "WClean", "timestamp")]

      return(Data[orgs, on = c("uuid", "timestamp")])
    }
    TAPChunks:::catInfo("No granularity selected")
  }
  TAPChunks:::catInfo("This function requires both demogs and targets to be loaded to the environment")
}
