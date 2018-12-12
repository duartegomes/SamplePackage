#' @title Reviewing a weight calculation
#' @description
#' This function shows the weight calculation resulting from the submitted data.
#' @details
#' The weight calculation is designed to alter the distribution of the sample to match
#' the distribution of the population data that we get from the Market Model.  The size of the
#' unweighted sample is normally unchanged by the operation.  If the user sets the scale parameter
#' this default weighting style may be changed so that the weighted sample size becomes equal to
#' the population.\cr\cr
#' The calculation is performed over a variety of dimensions, with the default being GSV
#' (by all combinations of Geography, Segment and Vertical).  Using the Granularity parameter
#' this may be changed to any combination of G S and V.\cr
#'
#' @section Example Weighting Calculation:
#' Once the Granularity parameter is set there will be a weight for each of the distinct
#' values.  In this example we assume Granularity = "S" which results in 9 different categories.
#' If you wish to repeat this example you may run the command shown in the examples section. \cr
#' \strong{Step 1: Separate Unclean Orgs} \cr
#' Unclean orgs will not be counted and so Target Orgs, Target Distribution, Expected Orgs and the final
#' Weight will all be zero. \cr
#' \strong{Step 2: Count the orgs in the sample} \cr
#' The sample submitted to the function via the data parameter is counted to give the number of
#' orgs in each category or \emph{Sample Orgs}. \cr
#' \strong{Step 3: Count the orgs in the population} \cr
#' The population from the market model is counted to give orgs in each category or \emph{Population Orgs} \cr
#' \strong{Step 4: Establish target orgs} \cr
#' We establish targets \emph{Target Orgs}.  These are usually equal to the population except where our sample is zero
#' in which case it is also zero.  If we don't make this adjustment the total weighted sample will
#' not equal the total unweighted sample because there is no possible weight that can scale a zero to a value. \cr
#' \strong{Step 5: Express the \emph{Target Orgs} as ratios that sum to unity} \cr
#' The \emph{Target Distribution} is the value of \emph{Target Orgs} divided by the total \emph{Target Orgs}. \cr
#' \strong{Step 6: Calculate the number of orgs expected to give the population distribution} \cr
#' The expected sample size \emph{Expected Orgs} that will respect the proportions in the population are
#' calculated by multiplying the number of clean \emph{Sample Orgs} by the \emph{Target Distribution}. \cr
#' \strong{Step 7: Calculate the weight} \cr
#' The system calculates two types of weight:
#' The \emph{Shape Weight} is the \emph{Expected Orgs} divided by the \emph{Sample Orgs}.
#' The \emph{Scale Weight} is the \emph{Target Orgs}   divided by the \emph{Sample Orgs}.
#' If the sample orgs is zero then the either weight is also zero. \cr
#'
#' @section Example Calculation:
#' \tabular{llrrrrrrr}{
#' Segment \tab Clean \tab Sample Orgs \tab Population Orgs \tab Target Orgs \tab Target Distribution \tab Expected Orgs \tab Scale Weight \tab Shape Weight\cr
#' LSB \tab Y \tab 87 \tab 44,289,935 \tab  44,289,935 \tab  0.73233\tab  298.061\tab 509,079 \tab  3.4344\cr
#' CSB \tab Y \tab 80 \tab 10,273,280 \tab  10,273,280 \tab  0.16986\tab   69.136\tab 128,416 \tab  0.8663\cr
#' LMM \tab Y \tab 72 \tab    841,880 \tab     841,880 \tab  0.01392\tab    5.665\tab  11,692 \tab  0.0788\cr
#' CMM \tab Y \tab 85 \tab    573,335 \tab     573,335 \tab  0.00948\tab    3.858\tab   6,745 \tab  0.0455\cr
#' UMM \tab Y \tab 13 \tab    199,535 \tab     199,535 \tab  0.00329\tab    1.342\tab  15,348 \tab  0.1035\cr
#' ENT \tab Y \tab  2 \tab    111,200 \tab     111,200 \tab  0.00183\tab    0.748\tab  55,600 \tab  0.3750\cr
#' Edu \tab Y \tab 41 \tab  3,011,555 \tab   3,011,555 \tab  0.04979\tab   20.267\tab  73,452 \tab  0.4955\cr
#' Gov \tab Y \tab 28 \tab  1,176,720 \tab   1,176,720 \tab  0.01945\tab    7.919\tab  42,025 \tab  0.2835\cr
#' NoPC\tab Y \tab  0 \tab 54,034,370 \tab           0 \tab  0.00000\tab    0.000\tab       0 \tab  0.0000\cr
#' }
#' @export
#' @param Data Chunk to be weighted
#' @param Granularity Dimensions to weight. Default being GSV (by all combinations of Geography, Segment and Vertical)
#' @examples ShowWeightCalc(TestEmailChunk, Granularity = "S")
#' @author JTA - The Data Scientists
#' @family Weighting and cleaning functions
#' @return Returns a data table with the calculation detailed.
#' @seealso \code{\link{TAPChunks}}
ShowWeightCalc <- function(Data, Granularity = "gsv") {
  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) TAPChunks:::catError("Data file not specified.")

  if (TAPChunks:::CheckDemogDB()) {
    # Define the granularity for weighting
    Weight_levl <- c()

    choices <-
      grep(
        paste0("[", Granularity, "]"),
        c("G", "S", "V"),
        ignore.case = T,
        value = T
      )
    if (length(choices) != nchar(Granularity)) {
      TAPChunks:::catInfo("Granularity parameter should only contain the characters G, S or V.")
    }

    if (length(intersect(choices, "G")) == 1) {
      Weight_levl <- c(Weight_levl, "WCountry")
    }
    if (length(intersect(choices, "S")) == 1) {
      Weight_levl <- c(Weight_levl, "Segment")
    }
    if (length(intersect(choices, "V")) == 1) {
      Weight_levl <- c(Weight_levl, "Vertical")
    }

    if (length(Weight_levl) != 0) {
      if (length(unique(Data$timestamp)) > 1) {
        choice <- min(as.character(unique(Data$timestamp)))
        TAPChunks:::catInfo("The data submitted has more than one timestamp.")
        TAPChunks:::catInfo("Weights are calculated independently for each timestamp.")
        TAPChunks:::catInfo("For the purposes of this illustration we have chosen the first.")
        TAPChunks:::catInfo(paste("period in your data:", choice))
        Data <- Data[timestamp == choice]
      }

      # Get the set of uuid's from the data sent to us
      sample_set <- unique(Data$uuid)

      sample_meta <-
        TAP_env$demog_helper  [uuid %in% sample_set, c(
          "uuid",
          "WeightingChoice_Code",
          "Dim_OrgSeg_3Segment_Code",
          "Sector_Code",
          "clean"
        ), with = F]

      sample_meta <-
        sample_meta[
          ,
          .(uuid,
            WCountry = WeightingChoice_Code,
            Segment = Dim_OrgSeg_3Segment_Code, Vertical = Sector_Code, clean
          )
        ]

      sample_dist <-
        sample_meta   [,
          .(`Sample Orgs` = data.table::uniqueN(uuid)),
          by = c(Weight_levl, "clean")
        ]

      target <- TAP_env$target_helper[
        ,
        .(
          Vertical = vertical_sector, Segment = segment_code,
          WCountry = w_country, entity_count
        )
      ]

      populn_dist <- target [, .(
        `Population Orgs` = sum(entity_count),
        clean = "Y"
      ), by = Weight_levl]

      Weight_Calc <-
        merge(
          sample_dist,
          populn_dist,
          all = T,
          by = c(Weight_levl, "clean")
        )

      Weight_Calc[is.na(`Sample Orgs`), `Sample Orgs` := 0]
      total_clean_sample <- Weight_Calc[clean == "Y", sum(`Sample Orgs`, na.rm = T)]
      Weight_Calc[is.na(`Population Orgs`), `Population Orgs` := 0]
      Weight_Calc[, `Target Orgs` := ifelse(`Sample Orgs` == 0, 0, `Population Orgs`)]
      Weight_Calc[, `Target Distribution` := `Target Orgs` / sum(`Target Orgs`, na.rm = T)]
      Weight_Calc[, `Expected Orgs` := `Target Distribution` * total_clean_sample]
      Weight_Calc[, `Shape Weight` := ifelse(`Sample Orgs` == 0, 0, `Expected Orgs` / `Sample Orgs`)]
      Weight_Calc[, `Scale Weight` := ifelse(`Sample Orgs` == 0, 0, `Target Orgs` / `Sample Orgs`)]

      data.table::setkeyv(Weight_Calc, c("clean", Weight_levl))

      Weight_Calc[clean == "N", c(Weight_levl) := "***"]
      Weight_Calc <- Weight_Calc[, .(
        `Sample Orgs` = sum(`Sample Orgs`),
        `Population Orgs` = sum(`Population Orgs`),
        `Target Orgs` = sum(`Target Orgs`),
        `Target Distribution` = sum(`Target Distribution`),
        `Expected Orgs` = sum(`Expected Orgs`),
        `Scale Weight` = sum(`Scale Weight`),
        `Shape Weight` = sum(`Shape Weight`)
      ),
      by = c(Weight_levl, "clean")
      ]

      new <- gsub(
        "segment_code", "Segment",
        gsub(
          "vertical_sector", "Vertical",
          gsub("WCountry", "Country", Weight_levl)
        )
      )

      data.table::setnames(Weight_Calc, old = Weight_levl, new = new)

      return(Weight_Calc[`Sample Orgs` != 0])
    }
    TAPChunks:::catInfo("No granularity selected.")
  }
  TAPChunks:::catInfo("This function requires both demogs and targets to be loaded to the environment.")
}
