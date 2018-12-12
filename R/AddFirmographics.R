#' @title Add Demographic Data
#' @description A standard telemetry chunk contains an org identifier
#' called the uuid (Universally Unique Identifier).  As a separate process
#' we analize the uuids and attribute certain firmographics to them.  Examples include
#' the country name, segment and vertical market. These attributes are stored in a
#' separate database having all the demographic information.  This function
#' helps user to easily attach the firmographic attributes to a chunk.
#' @details
#' The function checks to see if the demographics database has been loaded
#' to the environment.If not it will first load the demographics database.
#' The function joins the submitted chunk to the database using the uuid and returns the
#' original data with the following extra columns added:
#' \tabular{ll}{\strong{Column}\tab\strong{Description}\cr
#' \strong{Clean}   \tab The clean flag to show if the org has a full set of firmographics. \cr
#' \strong{Country} \tab The country name as defined by the source provider.\cr
#' \strong{Segment} \tab The segment as calculated by our load process (typically from PC and employee sizes).\cr
#' \strong{Vertical}\tab The vertical market as defined by SIC codes.\cr}
#' The function is internal and is used to bring in firmographic tags that can then be extended with
#' hierarchical data from the RDR.
#' @family Internal Utilities
#' @import data.table
#' @export
#' @author JTA - The Data Scientists
#' @param Data A data chunk
#' @return A data chunk with additional columns
#' @examples TAPChunks:::AddFirmographics(TestServerRolesChunk)
#' @seealso \code{\link{TAPChunks}}
AddFirmographics <- function(Data, Dimension = "All") {
  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) TAPChunks:::catError("Data file not specified.")

  if (TAPChunks:::TestNet() == F) {
    TAPChunks:::catError("Off-Line")
  }

  TAPChunks:::CheckDemogDB()

  TAPChunks:::catInfo(sprintf(
    "Firmographics has been loaded: \n\t- Number of Organizations: %s",
    nrow(TAP_env$demog_helper)
  ))

  Firmographics <-
    TAP_env$demog_helper[
      ,
      .(uuid,
        Country = ParentCountry_Name,
        Vertical = Sector_Code, Segment_Code = Dim_OrgSeg_3Segment_Code, clean
      )
    ]


  if (Dimension != "All") {
    if (!(Dimension %in% c("G", "S", "V"))) TAPChunks:::catError("The dimension is unknown")

    Firmographics <- switch(Dimension,
      "G" = Firmographics[, .(uuid, Country, clean)],
      "S" = Firmographics[, .(uuid, Segment_Code, clean)],
      "V" = Firmographics[, .(uuid, Vertical, clean)]
    )
  }

  cols <- setdiff(intersect(names(Data), names(Firmographics)), "uuid")

  if (length(cols) > 0) {
    Data <- Data[, setdiff(names(Data), cols), with = F]
  }

  Data <- merge(Data, Firmographics, all.x = T, by = "uuid")

  if (length(Data[is.na(clean), uuid]) != 0) {
    numberRow <- length(Data[is.na(clean), uuid])
    TAPChunks:::catWarning(
      sprintf("There are %s orgs without firmographic info. They will be removed.", numberRow),
      numberRow
    )
    Data <- Data[!is.na(clean)]
  }

  Data <- Data[,
    TAPChunks:::OrderColumn(Data),
    with = F
  ]

  return(Data)
}
