#' @title Load the demographics database
#' @description
#' The TAP system uses a database of demographics to perform a variety of tasks:
#'  Filter Unclean orgs from datasets
#'  Compute distributions of firmographics to calculate weights
#'  Add firmographic data to other datasets to be used as slicers
#'  Define hierarchies in reports
#'  \cr
#' This function loads the database.
#' @details
#' The database is stored in a separate environment called TAP_env.
#' The function tests that this environment exists and if it doesn't it will create the environment.
#' The function then tests for the presence of these three files:
#'  demog_helper  : This is a data table with all of the known Spiceworks organizations
#'  target_helper : This is a data table with the population targets that are provided by the Market Model
#'  unclean       : This is a vector of unclean organisation UUIDs
#' If they are all present then the routine end and returns a TRUE. If any of these files is missing
#' the system attempts to create the files by accessing from the Data Lake. If this is successful then it returns TRUE
#'
#' @author JTA - The Data Scientists
#' @keywords internal
#' @return A boolean status flag where TRUE shows the operation was successful
#' @family Internal Utilities
#' @seealso \code{\link{TAPChunks}}
LoadDemogDB <- function() {
  Data_Lake <- TAPChunks:::ConnectToADL()
  if (!exists("TAP_env", envir = .GlobalEnv)) {
    TAP_env <<- new.env()
  }

  c_load <- NULL

  # if(length(ls(envir = TAP_env, all.names = all.names,pattern = "demog_helper"))==0) c_load<- c(c_load,"demog_helper.Rdata")
  if (length(ls(
    envir = TAP_env,
    all.names = all.names,
    pattern = "target_helper"
  )) == 0) {
    c_load <- c(c_load, "target_helper.Rdata")
  }
  if (length(ls(
    envir = TAP_env,
    all.names = all.names,
    pattern = "demog_helper"
  )) == 0) {
    c_load <- c(c_load, "demog_helper.Rdata")
  }


  if (length(c_load) != 0) {
    data.lake.path <- "Source_Data/Demog/rdata"
    operation <- "?op=OPEN&read=true"


    for (fetch in c_load) {
      if (fetch == "demog_helper.Rdata") {
        operation <- "?op=LISTSTATUS"
        versionTime <- httr::GET(
          paste0(
            "https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
            file.path(data.lake.path, "demog_helper.Rdata"),
            operation
          ),
          httr::add_headers(Authorization = paste("Bearer", Data_Lake))
        )
        operation <- "?op=OPEN&read=true"

        versionTime <-
          httr::content(versionTime, as = "parsed")$FileStatuses$FileStatus[[1]]$modificationTime

        if ("demog_helper.Rdata" %in% list.files(ShowCachePath())) {
          if (as.integer(file.info(file.path(ShowCachePath(), "demog_helper.Rdata"))$mtime) <
            as.integer(substr(versionTime, 1, 10))) {
            unlink(file.path(ShowCachePath(), "demog_helper.Rdata"))
          }
        }

        versionAge <-
          round((.Internal(Sys.time()) - as.integer(substr(
            versionTime, 1, 10
          ))) / 86400)

        if (as.integer(versionAge) >= 30) {
          catSuperUser(sprintf("The lastest version was updated %s days ago.", versionAge), "lastest version")
        } else {
          if (as.integer(versionAge) == 0) {
            TAPChunks:::catInfo("The lastest version was updated today.")
          } else {
            TAPChunks:::catInfo(sprintf(
              "The lastest version was updated %s days ago.",
              versionAge
            ))
          }
        }
      }

      TAPChunks:::catInfo(paste("Fetching", fetch, "from the data lake to cache."))

      assign(gsub(".Rdata", "", fetch), ReadADLFile(SpecificPath = data.lake.path, FileName = fetch, SaveChunk = T, ShowInfo = F))
      if (fetch == "target_helper.Rdata") {
        TAP_env$target_helper <- target_helper
        remove(target_helper)
      }
      if (fetch == "demog_helper.Rdata") {
        TAP_env$demog_helper <- demog_helper
        remove(demog_helper)
      }
    }

    Configuration_env$Version$Max <- max(levels(TAP_env$demog_helper$UUID_Version_Gen))
  } else {
    catSuperUser("Demographic files already exist in the TAP_env environment.")
  }

  # We replace any mentions of segment with a factor code which is ordered
  # This helps us to interpret the data but it also allows us to generate a small
  # integer value to describe the segment
  TAP_env$demog_helper [, Dim_OrgSeg_3Segment_Code := factor(Dim_OrgSeg_3Segment_Code,
    levels = c(
      "LSB", "CSB",
      "LMM", "CMM", "UMM",
      "ENT",
      "Edu", "Gov", "Unk"
    ),
    ordered = T
  )]
  TAP_env$target_helper[, segment_code := factor(segment_code,
    levels = c(
      "LSB", "CSB",
      "LMM", "CMM", "UMM",
      "ENT",
      "Edu", "Gov", "Unk", "NoPC"
    ),
    ordered = T
  )]
  # We do the same with the vertical market
  TAP_env$demog_helper [, Sector_Code := factor(Sector_Code,
    levels = c(
      "DistSvc",
      "Edu",
      "Fin",
      "Gov",
      "Health",
      "Infra",
      "IT",
      "MfrRes",
      "Unk"
    ),
    ordered = T
  )]

  TAP_env$target_helper[, vertical_sector := factor(vertical_sector,
    levels = c(
      "DistSvc",
      "Edu",
      "Fin",
      "Gov",
      "Health",
      "Infra",
      "IT",
      "MfrRes",
      "Unk"
    ),
    ordered = T
  )]

  # And finally with weighting geography
  w_countries <- c(
    "AUS", "AUT", "BEL", "BRA", "CAN", "CHE",
    "CHN", "DEU", "DNK", "ESP", "FIN", "FRA",
    "GBR", "IDN", "IND", "IRL", "ITA", "JPN",
    "KOR", "MEX", "MYS", "NLD", "NOR", "POL",
    "PRT", "RUS", "SWE", "TUR", "UNK", "USA",
    "ZAF", "roAPAC", "roCEE", "roGCR", "roLATAM", "roMEA"
  )

  TAP_env$demog_helper [, WeightingChoice_Code := factor(WeightingChoice_Code,
    levels = w_countries,
    ordered = T
  )]

  TAP_env$target_helper[, w_country := factor(w_country,
    levels = w_countries,
    ordered = T
  )]

  # The value for weighting geography ranges from 1 to 36 {This needs 6 bits to store}
  # The value for segment ranges from  1 to 10            {This needs 4 bits to store}
  # The value for vertical ranges from 1 to  9            {This needs 4 bits to store}
  # When we are weighting the data we need to mark the large chunks with the geography, segment and vertical
  # and this will occupy a great deal of memory.  The solution is to pack these three characteristics into
  # one integer:
  #
  # |_8192_|_4096_|_2048_|_1024_|__512_|__256_|__128_|___64_|___32_|___16_|____8_|____4_|____2_|____1_|
  # |___________________________|___________________________|_________________________________________|
  # |          VERTICAL         |           SEGMENT         |              WEIGHTING GEO              |
  #
  # As an example let us consider a Core Mid Market Health company in Canada
  # Health has index 5 (0101 in binary), CMM has index 4 (0100 in binary) and Canada has index 5.
  # The compressed value will be:
  # |.....0|.....1|.....0|.....1|.....0|.....1|.....0|.....0|.....0|.....0|.....0|.....1|.....0|.....1|
  #
  # 4096+1024+256+4+1 = 5381
  # We add this gsv coordinate to both the demog and target data:

  TAP_env$demog_helper [, gsv := bitwOr(
    bitwOr(
      as.integer(WeightingChoice_Code),
      bitwShiftL(as.integer(Dim_OrgSeg_3Segment_Code), 6)
    ), # Seg shifted left  6 bits
    bitwShiftL(as.integer(Sector_Code), 10)
  )] # Ver shifted left 10 bits

  TAP_env$target_helper[, gsv := bitwOr(
    bitwOr(
      as.integer(w_country),
      bitwShiftL(as.integer(segment_code), 6)
    ),
    bitwShiftL(as.integer(vertical_sector), 10)
  )]

  # Recover the weighting geo from a GSV :            bitwAnd(gsv, 0x003F)
  # Recover the segment from a gsv       : bitwShiftR(bitwAnd(gsv, 0x03C0),  6)
  # Recover the vertical from a gsv      : bitwShiftR(bitwAnd(gsv, 0x3C00), 10)
  # We know that if one of the items is unknown then the clean flag is "N". This allows us to
  # also use GSV to find dirty data. This is equivalent to finding a clean == "N":
  # bitwShiftR(bitwAnd(gsv, 0x03C0), 6) == 9L | bitwShiftR(bitwAnd(gsv, 0x3C00), 10) == 9L | bitwAnd(gsv, 0x003F) == 29L


  assign("unclean",
    TAP_env$demog_helper[clean == "N"]$uuid,
    envir = TAP_env
  )

  return(TRUE)
}
