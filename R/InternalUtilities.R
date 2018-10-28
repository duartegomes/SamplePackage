#' @title  Test For Internet Connectivity
#'
#' @description
#' This is an internal function that can be used by other tools
#' to detect web connectivity.  This is so the load processes may
#' continue to work (via the cache files) even when the user is
#' offline.
#'
#' @details
#' This function only works on windows operating systems and
#' functions by sending a single ping request to www.microsoft.com.
#' @export
#' @examples ifelse (TestNet(), "We have net connectivity.", "Offline")
#' @family Internal Utilities
#' @keywords internal
#' @author Jonathan Tooley Associados Lda
#' @return Returns a logical TRUE when the ping was successful else FALSE.
#' @seealso \code{\link{TAPChunks}}
# Backlog #120
TestNet <- function() {
  res <- !as.logical(tryCatch(
    system2(
      command = "ping",
      args    = "-n 1 www.microsoft.com",
      stdout  = NULL,
      stderr  = NULL
    ),
    warning = function(e) {
    },
    error   = function(e) {
    }
  ))
  return(ifelse(res == FALSE ||length(res) == 0, FALSE, TRUE))
}

#' @title  Test For Internet Connectivity
#'
#' @description
#' This is an internal function that can be used by other tools
#' to detect web connectivity.  This is so the load processes may
#' continue to work (via the cache files) even when the user is
#' offline.
#'
#' @details
#' This function only works on windows operating systems and
#' functions by sending a single ping request to "cmr-jta"
#' @export
#' @examples ifelse (TestVPN(), "We have net connectivity.", "Offline")
#' @family Internal Utilities
#' @keywords internal
#' @author Jonathan Tooley Associados Lda
#' @return Returns a logical TRUE when the ping was successful else FALSE.
#' @seealso \code{\link{TAPChunks}}

TestVPN <- function() {
  res <- !as.logical(tryCatch(
    system2(
      command = "ping",
      args    = "idweb",
      stdout  = NULL,
      stderr  = NULL
    ),
    warning = function(e) {
    },
    error   = function(e) {
    }
  ))
  return(ifelse(res == FALSE ||length(res) == 0, FALSE, TRUE))
}



#' @title Preparing a range of timestamps
#' @description
#' A function that will accept the start and end timestamps and turn this information
#' into a vector with has all of the timestamps from the first until the last exclusively.
#' @details This function may be used to conserve memory when dealing with large files.
#' The function returns a vector with the range of timestamps so that a script may process
#' the chunks on a month by month basis, clearing the memory between each month.
#' @export
#' @examples TimeStamps(from = "2017M01", to = "2017M12")
#' @param From A timestamp to indicate the beginning of the range
#' @param To A timestamp to indicate the end of the range
#' @family Internal Utilities
#' @return Returns a vector of all posible timestamps between from and to
#' @author Jonathan Tooley Associados Lda
#' @seealso \code{\link{TAPChunks}}
# Backlog #138
TimeStamps <- function(From = "", To = "") {
  if(!CheckDemogDB()) LoadDemogDB()

  if (To == "") To = From

  From <- gsub("_","M", From)
  To   <- gsub("_","M", To)

  months  <- c("M01","M02", "M03", "M04", "M05", "M06", "M07", "M08", "M09", "M10", "M11", "M12")
  years   <- as.character(2000:2030)
  choices <- data.table::CJ(month = months, year = years)
  choices[, version := paste0(year,month) ]
  choices[version <= Configuration_env$Version$Max, available := "yes"]
  choices[is.na(available), available := "no"]
  choices  <- choices[version >= From & version <= To][order(version)]

  noAvailable  <- choices[available == "no"]$version
  if(length(noAvailable) != 0)catWarning(sprintf("The following versions are not available:\n\t\t-%s", paste0(noAvailable, collapse = "\n\t\t-" )),"versions are not available")

  timestamp <- choices[available == "yes"]$version

  if(length(timestamp) <= 5){
    catSuperUser(paste0("Request\n\t\t-", paste0(timestamp, collapse = "\n\t\t-")))}else{
      catSuperUser(FormatList(timestamp, Title = "Request Timestamp:", 5))
    }

  return(timestamp)
}

#' @title Testing that the demographics database is loaded
#' @description
#' The TAP system uses a database of demographics to perform a variety of tasks:
#'  Filter Unclean orgs from datasets
#'  Compute distributions of firmographics to calculate weights
#'  Add firmographic data to other datasets to be used as slicers
#'  Define hierarchies in reports
#'
#' This function checks that the system has the database loaded.
#' @details
#' The database is stored in a separate environment called TAP_env.
#' The function tests that this environment exists and that it has the
#' following three objects:
#'  demog_helper  : This is a data table with all of the known Spiceworks organizations
#'  target_helper : This is a data table with the population targets that are provided by the Market Model
#'  unclean       : This is a vector of unclean organisation UUIDs
#'
#' If the environment does not exist it calls LoadDemogDB() to establish the environment
#' and returns the status of that operation.
#' @author Jonathan Tooley Associados Lda
#' @keywords internal
#' @export
#' @family Internal Utilities
#' @return Returns a logical TRUE if the database is loaded and FALSE if not.
#' @seealso \code{\link{TAPChunks}}
# Backlog 148
CheckDemogDB <- function () {
  if (exists("TAP_env"))
  {
    # Environment exists, but we had better check some more
    if ((!exists("demog_helper" , envir = TAP_env)) |
        (!exists("target_helper", envir = TAP_env)) |
        (!exists("unclean"      , envir = TAP_env)))
    {
      catInfo ("TAP environment exists but not loaded. ")
      catInfo ("Setting up environment for you now...")
      st <- LoadDemogDB()
      return(st)
    } else
      return(TRUE)
  }
  catInfo ("TAP environment not yet established. ")
  catInfo ("Setting up environment for you now...")
  st <- LoadDemogDB()
  return(st)
}


#' @title Load the demographics database
#' @description
#' The TAP system uses a database of demographics to perform a variety of tasks:
#'  Filter Unclean orgs from datasets
#'  Compute distributions of firmographics to calculate weights
#'  Add firmographic data to other datasets to be used as slicers
#'  Define hierarchies in reports
#'
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
#' @author Jonathan Tooley Associados Lda
#' @keywords internal
#' @export
#' @return A boolean status flag where TRUE shows the operation was successful
#' @family Internal Utilities
#' @seealso \code{\link{TAPChunks}}
# Backlog 144
LoadDemogDB <- function() {
  Data_Lake <- ConnectToADL()
  if (!exists("TAP_env", envir = .GlobalEnv)) {
    TAP_env <<- new.env()

  }

  c_load <- NULL

  # if(length(ls(envir = TAP_env, all.names = all.names,pattern = "demog_helper"))==0) c_load<- c(c_load,"demog_helper.Rdata")
  if (length(ls(
    envir = TAP_env,
    all.names = all.names,
    pattern = "target_helper"
  )) == 0)
    c_load <- c(c_load, "target_helper.Rdata")
  if (length(ls(
    envir = TAP_env,
    all.names = all.names,
    pattern = "demog_helper"
  )) == 0)
    c_load <- c(c_load, "demog_helper.Rdata")


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
          catSuperUser(sprintf("The lastest version was updated %s days ago.",versionAge),"lastest version")
        } else {
          if (as.integer(versionAge) == 0) {
            catInfo("The lastest version was updated today.")
          } else {
            catInfo(sprintf(
              "The lastest version was updated %s days ago.",
              versionAge
            ))
          }
        }
      }

      catInfo(paste("Fetching", fetch, "from the data lake to cache."))

      assign(gsub(".Rdata", "", fetch), ReadADLFile(SpecificPath = data.lake.path, FileName = fetch, SaveChunk = T, ShowInfo = F))
      if(fetch == "target_helper.Rdata"){
        TAP_env$target_helper <- target_helper
        remove(target_helper)
      }
      if(fetch == "demog_helper.Rdata"){
        TAP_env$demog_helper <- demog_helper
        remove(demog_helper)
      }

    }

    Configuration_env$Version$Max <- max(levels(TAP_env$demog_helper$UUID_Version_Gen))

  }else{
    catSuperUser("Demographic files already exist in the TAP_env environment.")
  }

  # We replace any mentions of segment with a factor code which is ordered
  # This helps us to interpret the data but it also allows us to generate a small
  # integer value to describe the segment
  TAP_env$demog_helper [, Dim_OrgSeg_3Segment_Code := factor(Dim_OrgSeg_3Segment_Code,
                                                             levels = c("LSB", "CSB",
                                                                        "LMM", "CMM", "UMM",
                                                                        "ENT",
                                                                        "Edu", "Gov", "Unk"),
                                                             ordered = T)]
  TAP_env$target_helper[, segment_code             := factor(segment_code,
                                                             levels = c("LSB", "CSB",
                                                                        "LMM", "CMM", "UMM",
                                                                        "ENT",
                                                                        "Edu", "Gov", "Unk", "NoPC"),
                                                             ordered = T)]
  # We do the same with the vertical market
  TAP_env$demog_helper [, Sector_Code     := factor(Sector_Code,
                                                    levels = c("DistSvc",
                                                               "Edu",
                                                               "Fin",
                                                               "Gov",
                                                               "Health",
                                                               "Infra",
                                                               "IT",
                                                               "MfrRes",
                                                               "Unk"),
                                                    ordered = T)]

  TAP_env$target_helper[, vertical_sector := factor(vertical_sector,
                                                    levels = c("DistSvc",
                                                               "Edu",
                                                               "Fin",
                                                               "Gov",
                                                               "Health",
                                                               "Infra",
                                                               "IT",
                                                               "MfrRes",
                                                               "Unk"),
                                                    ordered = T)]

  # And finally with weighting geography
  w_countries <- c("AUS", "AUT"   , "BEL"  , "BRA"  , "CAN"    , "CHE",
                   "CHN", "DEU"   , "DNK"  , "ESP"  , "FIN"    , "FRA",
                   "GBR", "IDN"   , "IND"  , "IRL"  , "ITA"    , "JPN",
                   "KOR", "MEX"   , "MYS"  , "NLD"  , "NOR"    , "POL",
                   "PRT", "RUS"   , "SWE"  , "TUR"  , "UNK"    , "USA",
                   "ZAF", "roAPAC", "roCEE", "roGCR", "roLATAM","roMEA")

  TAP_env$demog_helper [, WeightingChoice_Code := factor(WeightingChoice_Code,
                                                         levels  = w_countries,
                                                         ordered = T)]

  TAP_env$target_helper[, w_country            := factor(w_country,
                                                         levels = w_countries,
                                                         ordered = T)]

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
    bitwOr(            as.integer(WeightingChoice_Code),
                       bitwShiftL(as.integer(Dim_OrgSeg_3Segment_Code), 6)), #Seg shifted left  6 bits
    bitwShiftL(as.integer(Sector_Code), 10))]                     #Ver shifted left 10 bits

  TAP_env$target_helper[, gsv := bitwOr(
    bitwOr(            as.integer(w_country),
                       bitwShiftL(as.integer(segment_code), 6)),
    bitwShiftL(as.integer(vertical_sector), 10))]

  # Recover the weighting geo from a GSV :            bitwAnd(gsv, 0x003F)
  # Recover the segment from a gsv       : bitwShiftR(bitwAnd(gsv, 0x03C0),  6)
  # Recover the vertical from a gsv      : bitwShiftR(bitwAnd(gsv, 0x3C00), 10)
  # We know that if one of the items is unknown then the clean flag is "N". This allows us to
  # also use GSV to find dirty data. This is equivalent to finding a clean == "N":
  # bitwShiftR(bitwAnd(gsv, 0x03C0), 6) == 9L | bitwShiftR(bitwAnd(gsv, 0x3C00), 10) == 9L | bitwAnd(gsv, 0x003F) == 29L


  assign("unclean",
         TAP_env$demog_helper[clean  == "N"]$uuid,
         envir = TAP_env)

  return(TRUE)
}

#' @title  Decode the Http Error codes into messages
#' @description
#' This function decode the Http Error codes into messages
#' @param ErrorCode The client identifier
#' @family Internal Utilities
#' @examples httpCode(200)
#' @author Jonathan Tooley Associados Lda
#' @return Returns a message as a string
#' @keywords internal
#' @export
#' @seealso \code{\link{TAPChunks}}
# Backlog #182
HttpCode <- function(ErrorCode = 0) {

  errorMsg  <- switch(paste(ErrorCode),
                    "200" = "OK - The ADL fetch reported success.",
                    "201" = "Created - The request has been fulfilled, resulting in the creation of a new resource.",
                    "202" = "Accepted - The request has been accepted for processing, but the processing has not been completed.",
                    "204" = "No Content - The server successfully processed the request and is not returning any content.",
                    "205" = "Reset Content - The server successfully processed the request, but is not returning any content.",
                    "400" = "Bad Request - The server cannot or will not process the request due to an apparent client error.",
                    "401" = "Unauthorized - Similar to 403 Forbidden, but specifically for use when authentication is required and has failed or has not yet been provided.",
                    "403" = "Forbidden - The request was valid, but the server is refusing action. The user might not have the necessary permissions for a resource.",
                    "404" = "Not Found - The requested resource could not be found but may be available in the future.",
                    "405" = "Method Not Allowed - A request method is not supported for the requested resource.",
                    "500" = "Internal Server Error.",
                    "502" = "Bad Gateway - The server was acting as a gateway or proxy and received an invalid response from the upstream server.",
                    "503" = "Service Unavailable - The server is currently unavailable.")
  if(is.null(errorMsg)) errorMsg <- "Error unknown."
  return(errorMsg)
}

#' @title  Making a connection to the data lake
#' @description
#' This function allows us to establish a connection to the Data Lake.  It is intended to
#' be used internally by other functions in this package.  If there is no network access or
#' the system cannot access the Data Lake the function returns NULL.  If a connection can be
#' made it returns an access token.
#' @param AdlClientId The client identifier
#' @param AdlClientSecret The client secret
#' @param AdlTenantId The tenant identifier
#' @import curl
#' @import jsonlite
#' @family Internal Utilities
#' @examples ConnectToADL(AdlClientId     = "e20ebcde-ec2d-47ab-805a-9b157638bf04",
#'                        AdlClientSecret = "9LDqQfc0U+4+S7JsosPQ3HD9eLVvw9PiQYQEXU55y00=",
#'                        AdlTenantId     = "72f988bf-86f1-41af-91ab-2d7cd011db47")
#' @author Jonathan Tooley Associados Lda
#' @return Returns an access token (or NULL on error)
#' @export
#' @seealso \code{\link{TAPChunks}}
# Backlog #123
ConnectToADL<-
  function( AdlClientId     = NULL,
            AdlClientSecret = NULL,
            AdlTenantId     = NULL) {


    if(is.null(AdlClientId) & is.null(AdlClientSecret) & is.null(AdlTenantId)){
      catSuperUser("Connection will be created from the yaml file information","yaml file")
      if(is.null(Configuration_env$Source)) GetConfiguration()
      catSuperUser("Should run the GetConfiguration function before starting the tasks","GetConfiguration")

      AdlClientId     = Configuration_env$Key$ADL$Client
      AdlClientSecret = Configuration_env$Key$ADL$Secret
      AdlTenantId     = Configuration_env$Key$ADL$Tenant

      if(is.null(AdlClientId) & is.null(AdlClientSecret) & is.null(AdlTenantId)) catError("The Key configuration not found ", "Key")
    }else{
      catSuperUser("Connection was manually created","yaml file")
    }


    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    h <- curl::new_handle()
    success <-try(  curl::handle_setform(
      h,
      "grant_type"    = "client_credentials",
      "resource"      = "https://management.core.windows.net/",
      "client_id"     = AdlClientId,
      "client_secret" = AdlClientSecret
    ))

    if(class(success) == "try-error") catError("The keys to open the ADL connection are invalid.","keys")

    req <- try(
      curl::curl_fetch_memory(paste0(
        "https://login.windows.net/",
        AdlTenantId,
        "/oauth2/token"
      ),
      handle = h), silent = T)

    if(class(req) == "try-error") TAPChunks:::catError("The login to open the ADL failed.", "login")

    if(req$status_code == "401") TAPChunks:::catError("The keys to open the ADL connection are invalid.","keys")

    res <- jsonlite::fromJSON(rawToChar(req$content))

    catSuperUser("Connection to the ADL was successfully done.","ADL")
   return(res$access_token)
  }


#' @title  Validating data chunks
#' @description
#' This routine will validate data to ensure that it is a valid data chunk.  It is used
#' by many of the other functions before they start work on altering the chunk that
#' was presented.
#' @details In order to be valid the data presented must be a data table with a uuid column.
#' @examples ValidateChunk(email)
#' @param Data A data chunk to be validated.
#' @family Internal Utilities
#' @author Jonathan Tooley Associados Lda
#' @keywords internal
#' @export
#' @return Returns a logical TRUE if the chunk is valid else it returns FALSE.
#' @seealso \code{\link{TAPChunks}}
# Backlog #143
ValidateChunk  <- function(Data) {
  # Check that the file is a data.table with a uuid field
  valid  <- TRUE
  # The status code is a bitwise indicator where each bit of the integer
  # represents a logical element of the data
  status <- 0L
  # Bit 0: when set to 1 the file has a weight column present
  # Bit 1: when set to 1 the file has a timestamp column present
  # Bit 2: when set to 1 the file contains unclean organizations
  cols   <- names(Data)

  if (!("data.table" %in% intersect(class(Data), "data.table"))) {
    valid <- FALSE
    catInfo("Validation Failed: Not a data table.")
  } else
    if (!("uuid" %in% cols)) {
      valid  <- FALSE
      catInfo("Validation Failed: No org identifier found.")
    } else {
      if (!("weight" %in% cols)) {
        catInfo("This data chunk does not have weights.")
        status <- bitwAnd(status, 0xFE)
      } else
        status <- bitwOr (status, 0x01)
      if (!("timestamp" %in% cols)) {
        catInfo("This data chunk does not have a timestamp.")
        status <- bitwAnd(status, 0xFD)
      } else
        status <- bitwOr (status, 0x02)

      if (CheckDemogDB()) {
        if (length(intersect(Data$uuid, TAP_env$unclean)) == 0) {
          catInfo("The data file only has clean organizations.")
          status <- bitwAnd(status, 0xFB)
        } else{
          catInfo("The data file contains unclean organizations.")
          status <- bitwOr (status, 0x04)
        }
      } else {
        catInfo("Unable to check the clean status as ")
        catInfo("the demog database is not loaded.")
      }
    }
  return(list(valid = valid, status = status))
}

#' @title  Check Memory
#' @description
#' This function checks the available memory and estimates the
#' memory required to open the chunk files.
#' @details The function will lookup the size of the file requested and compare to
#' the memory available.  If the memory is not sufficient the function will switch
#' to an interactive mode where it will offer options to clear the memory.
#' @examples CheckMemory("SWWorkloadWL2017M12.Rdata")
#' @param Request chunk files required
#' @family Internal Utilities
#' @author Jonathan Tooley Associados Lda
#' @export
#' @seealso \code{\link{TAPChunks}}
#' #174
CheckMemory <- function (Request = NULL) {


  if (is.null(Request)) {
    catError("No chunk passed in parameters")
  }
  RAM_machine <- memory.limit()
  RAM_use_R <- memory.size()
  RAM_use_PC <- data.table::data.table(shell("tasklist", intern = T))
  RAM_use_PC <- apply(RAM_use_PC[!1:4], 1, function(x) gsub("\\s*\\s",
                                                            "#", gsub("\\sK$", "", x)))
  RAM_use_PC <- gsub("\\D.+#", "", RAM_use_PC)
  RAM_use_PC <- sum(as.numeric(gsub("\\D", "", RAM_use_PC)),
                    na.rm = T)
  RAM_use_PC <- as.integer(RAM_use_PC/1024)
  space_estimate <- as.integer(TAP_env$InfoChunk[file_name_rdata %in%
                                           Request | file_name_csv %in% Request, sum(ram_bytes)]/1024^2)
  catSuperUser(sprintf("Memory available to OS: %s GB", round(RAM_machine/1024,
                                                              2)))
  catSuperUser(sprintf("Memory used by OS: %s GB", round(RAM_use_PC/1024,
                                                         2)))
  catSuperUser(sprintf("Memory used in R Session: %s GB", round(RAM_use_R/1024,
                                                                2)))

  if (RAM_machine - RAM_use_PC < space_estimate * 1.25) {
    catError(sprintf("RAM: %s GB (Available: %s GB) Failed",
                     round(space_estimate * 1.25/1024, 2), round(RAM_machine/1024,
                                                                 2) - round(RAM_use_PC/1024, 2)),"Failed")
  }
  else {
    if ((RAM_machine - RAM_use_PC) * 0.75 > space_estimate *
        1.25) {
      catSuccess(sprintf("RAM: %s GB (Available: %s GB) OK",
                         round(space_estimate * 1.25/1024, 2), round(RAM_machine/1024,
                                                                     2) - round(RAM_use_PC/1024, 2)),"OK")

    }
    else {
      if ((RAM_machine - RAM_use_PC) > space_estimate *
          1.25) {
        answer <- menu(choices = c("Yes", "No"), title = "Memory is scarce, Do you want to continue?")
        if (answer != 1)
          catError("Check task.")
        if (answer == 1)
          answer2 <- menu(choices = c("Yes", "No"), title = "Would you like to clear the R memory?")
        if (answer2 == 1)
          CleanMemory()
      }
      else {
        if (RAM_machine - RAM_use_PC + space_estimate >
            space_estimate) {

          catWarning(sprintf("RAM: %s GB (Available: %s GB) Warning",
                             round(space_estimate * 1.25/1024, 2), round(RAM_machine/1024,
                                                                         2) - round(RAM_use_PC/1024, 2)),"Warning")
          catWarning("You may be able to open the requested file if you clear the memory.")
          answer2 <- menu(choices = c("Yes", "No"), title = "Would you like to clear the R memory?")
          if (answer2 == 1) {
            CleanMemory()
          }
          RAM_use_R <- memory.size()
          if ((RAM_machine - RAM_use_PC) * 0.75 > space_estimate *
              1.25) {
            catSuccess("Memory check: OK.", "OK")
          }
          else {
            catError(sprintf("RAM: %s GB (Available: %s GB) Failed",
                             round(space_estimate * 1.25/1024, 2), round(RAM_machine/1024,
                                                                         2) - round(RAM_use_PC/1024, 2)),"Failed")
          }
        }
        else {
          catError(sprintf("RAM: %s GB (Available: %s GB) Failed",
                           round(space_estimate * 1.25/1024, 2), round(RAM_machine/1024,
                                                                       2) - round(RAM_use_PC/1024, 2)),"Failed")
        }
      }
    }
  }
}



#' @title Check Disk
#' @description
#' TODO
#' TODO
#' @details TODO
#' TODO
#' @examples CheckSpaceDisk("SWWorkloadWL2017M12.Rdata")
#' @param Request chunk files required
#' @family Internal Utilities
#' @author Jonathan Tooley Associados Lda
#' @seealso \code{\link{TAPChunks}}
#' @export
CheckSpaceDisk <- function(Request){

  savePath <- ShowCachePath()

  DiskPath <- tolower(gsub("\\s", "", savePath))
  if (grepl("^\\w:", DiskPath)) {
    DiskPath <- substr(DiskPath, 1, 2)
  } else{
    catError("Cache Folder is incorrect")
  }

  SpaceDisk <-
    shell(sprintf("fsutil volume diskfree %s", DiskPath), intern = T)[3]

  SpaceDisk <-
    as.numeric(substr(SpaceDisk, gregexpr(":", SpaceDisk)[[1]] + 1, nchar(SpaceDisk)))

  requestBytes <-
    sum(
      sum(as.numeric(TAP_env$InfoChunk[file_name_csv %in%  Request & rsession_size_bytes!= "LoadError"]$csv_size_bytes)),
      sum(as.numeric(TAP_env$InfoChunk[file_name_rdata %in%  Request & rsession_size_bytes!= "LoadError"]$rdata_size_bytes))
    )

  SpaceDiskUnits <-
    utils:::format.object_size(as.numeric(SpaceDisk), units = "auto")
  requestBytesUnits <-
    utils:::format.object_size(as.numeric(requestBytes), units = "auto")

  if (0.75 * SpaceDisk > requestBytes) {

    catSuccess(sprintf("Disk: %s (Available: %s) OK",requestBytesUnits,SpaceDiskUnits), "OK")

  } else{
    if (SpaceDisk > requestBytes) {
      catWarning(sprintf("Disk: %s (Available: %s) Warning",requestBytesUnits,SpaceDiskUnits), "Warning")

    } else{
      catError(sprintf("Disk: %s (Available: %s) Failed",requestBytesUnits,SpaceDiskUnits), "Failed")
    }
  }
}

#' @title  CheckRequirements
#' @description
#' TODO
#' TODO
#' @details TODO
#' TODO
#' @param Request chunk files required
#' @family Internal Utilities
#' @author Jonathan Tooley Associados Lda
#' @seealso \code{\link{TAPChunks}}
#' @export
CheckRequirements <- function(Request) {
  catInfo("Requested chunk requires:")
  if (!exists("TAP_env", envir = .GlobalEnv)) {
    TAP_env <<- new.env()
  }

  if (length(ls(
    envir = TAP_env,
    all.names = all.names,
    pattern = "^InfoChunk$"
  )) == 0) {
    assign("InfoChunk", ReadADLFile("/Source_Data/Demog/rdata", "InfoChunk.Rdata"))
    TAP_env$InfoChunk <- InfoChunk
    remove(InfoChunk)

  }

  CheckMemory(Request)

  Request <- setdiff(Request,
                     list.files(ShowCachePath()))

  if (length(Request) != 0) {
    CheckSpaceDisk(Request)
  }

}




#' @title Open SQL Connection
#' @description This function can open connection with SQL without create a odbc connection
#' @param Server name of server SQL
#' @param Database name of database SQL
#' @param Uid user identification of server SQL
#' @param Pwd password of server SQL
#' @import RODBC
#' @return Connection with SQL
#' @export
OpenSqlConnection  <- function(Server, Database, Uid = NULL, Pwd = NULL) {

  if (missing(Server)) {

    catError("Argument 'server' is missing, with no default")
  }else if (missing(Database)) {

    catError("Argument 'database' is missing, with no default")
  }
  #create the connection path
  name.connection <- paste0("driver={SQL Server};server=", Server, ";database=", Database)

  #if have uid and pwd, we add into the connection path
  if (!is.null(Uid)) {

    if (!is.null(Pwd)) {

      name.connection <- paste0(name.connection, ";Uid=", Uid, ";Pwd=", Pwd)
    }
  }else {

    #add trusted connection to path
    name.connection <- paste0(name.connection, ";trusted_connection=true")
  }

  #Connect with the path
  sql.connection <- RODBC::odbcDriverConnect(name.connection)

  #log message
  if (class(sql.connection) == "RODBC") {

    catSuperUser(paste0("Connection successful: \n\t\t-Server  : ", Server,"\n\t\t-Database: ", Database, "\n"))
  }else {

    catError("Connection failed")
  }

  return(sql.connection)
}

#' @title Get Mapping table from RDR
#'
#' @description This function returns the Mapping table from RDR
#' @family Internal Utilities
#' @author Jonathan Tooley Associados Lda
#' @param Dimension name the dimension
#' @return The mapping table
#' @seealso \code{\link{TAPChunks}}
#' @export
# Backlog #176
GetMapping <- function(Dimension = NULL) {

  if(is.null(Dimension)){
    catError("Argument 'Dimension' is missing, with no default. Please use 'S','P','G' or 'O'.")
  }

  if (!TestNet()) catError("\nPlease check your network connection!")

  Dimension <- toupper(Dimension)

  SQLTable <- switch (Dimension,
                      "S" = "[svw_DimH_DemogsSegment]",
                      "P" = "[svw_Custom_ProductTaxonomy]",
                      "G" = "[DSSTGeo] t1 FULL OUTER JOIN [MDSDB].[mdm].[svw_DimH_DemogsGeo] t2 ON t1.Dim_OrgGeo_5Country_Name = t2.ParentCountry_Name;",
                      "O" = "[svw_DimH_PlatOS]",
                      "H" = "[svw_Custom_HierarchyMetadata]")

  if (is.null(SQLTable))
    catError("Dimension does not found. Please use 'S','P','G','H' or 'O'.")

  connection <- OpenSqlConnection("CAPENGSQL2\\TAP,51567", "MDSDB")
  if(Dimension == "O"){
    query <- sprintf("SELECT * FROM [mdm].%s ORDER BY matchPattern_ID", SQLTable)

  }else{

    query <- sprintf("SELECT * FROM [mdm].%s", SQLTable)
  }

  hierarchy  <- data.table::data.table(RODBC::sqlQuery(connection, query))

  close(connection)

  if(Dimension != "H"){
    column_name <- grep("_Name", names(hierarchy), value = T)
    column_name <- setdiff(column_name, "Variant_Name")
    keep        <- intersect(names(hierarchy), "Segment_Code")

    hierarchy <- hierarchy[, c(column_name, keep), with = F]
    hierarchy <- hierarchy[, sapply(hierarchy, class) != "logical", with = F]
    hierarchy <- hierarchy[, lapply(.SD, as.factor)]
    hierarchy <- unique(hierarchy)

    names(hierarchy) <- gsub("(Parent)|(_Name)", "", names(hierarchy))
  }

  if(Dimension == "G") {
    names(hierarchy)[1] <- "Area_DSST"
    hierarchy$Dim_OrgGeo_5Country <- NULL
  }

  return(hierarchy)
}

#' @title Sort a data chunk's columns to a defined order
#' @description Sort the columns in the data chunk
#' @param DT Chunk where the sort order will be applied
#' @export
#' @family Internal Utilities
#' @keywords internal
#' @author Jonathan Tooley Associados Lda
#' @seealso \code{\link{TAPChunks}}
# Backlog
OrderColumn <- function(DT) {

  DT <- names(DT)

  begin   <- c("row_id"         , "uuid"             ,"server_roles_uuid"  , "timestamp" )

  dimGeo  <- c("Area"           , "Region"           , "SubReg",
               "Subsidiary"     , "Country"          , "WCountry" )

  dimSeg  <- c("Sector"         , "SubSector"        , "Segment",
               "Segment_Code"   , "Vertical")

  dimOS   <- c("Platform"       , "Architecture"     , "Ecosystem",
               "OS"             , "OS_Name"          , "MatchPattern_Name")

  dimProd <- c("ProductCategory", "ProductType"      , "CloudProductDetail",
               "Product2"       , "Product1"         , "Product0" )

  end     <- c("variable"       , "value")

  all.dim <- c(dimGeo, dimSeg, dimOS, dimProd)
  part1   <- intersect(c(begin, all.dim), DT)
  part2   <- setdiff(DT, c(c(begin, all.dim, end)))
  part3   <- intersect(end, DT)

  return(c(part1, part2, part3))
}

#' @title Check Chunk Files information
#' @description TODO
#' @param Source TODO
#' @param From The timestamp of the first month of data.  If this is not provided this defaults
#' to 2016M01 but in general the user should select a timestamp.
#' @param To The timestamp of the last month of data needed when the user needs to get a range
#' of timestamps.  If this is left blank then the system returns just the single timestamp provided
#' in the to parameter.
#' @family Internal Utilities
#' @import utils
#' @keywords internal
#' @author Jonathan Tooley Associados Lda
#' @seealso \code{\link{TAPChunks}}
# Backlog
CheckChunkInfo  <- function(Source = "ALL", From = "" , To = ""){

  if(Source == "ALL"){

    GetConfiguration()
    source_available <- names(Configuration_env$Source)

  }else{

    source_available <- ShowADLPath(Source)
    source_available <- attributes(source_available)$source
  }

  DT.csv   <- NULL
  DT.rdata <- NULL

  for(source.name in source_available){

    adl.account.name  <- ConnectToADL()

    for( folder.name in c("csv", "rdata")){

      dir <- httr::GET(
        paste0(
          "https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
          sprintf("Source_Data/SW/%s/%s", source.name, folder.name), "?op=LISTSTATUS"
        ),
        httr::add_headers(Authorization = paste("Bearer",  adl.account.name))
      )


      fname       <- c()
      fsize       <- c()
      formatsize  <- c()

      if (dir$status_code == 200) {
        dir <- httr::content(dir, as = "parsed")
        dir <- dir$FileStatuses$FileStatus
        num <- length(dir)
        for (i in (1:num)){
          fname <- c(fname, dir[[i]]$pathSuffix)
          fsize <- c(fsize, dir[[i]]$length )
          formatsize  <- c(formatsize, utils:::format.object_size(as.numeric(dir[[i]]$length), "auto"))

        }
      }


      if(folder.name == "csv"){
        DT <- data.table::data.table(
          key.file = gsub(".csv","",fname), file_name_csv = fname,
          csv_size_bytes = fsize, csv_size = formatsize, source = source.name)
        DT.csv  <- rbind(DT.csv, DT)
      }

      if(folder.name == "rdata"){
        DT <- data.table::data.table(
          key.file = gsub(".Rdata", "", fname), file_name_rdata = fname,
          rdata_size_bytes = fsize, rdata_size = formatsize, source = source.name)
        DT.rdata  <- rbind(DT.rdata, DT)
      }
    }
  }

  infoChunk  <- merge(DT.csv, DT.rdata, all = T, by = c("key.file","source"))
  infoChunk[, time := substr(key.file, nchar(key.file) - 6, nchar(key.file))]


  request        <- TimeStamps(From = From, To = To)

  if( length(request) != 0){
    infoChunk <- infoChunk[time %in% request]
  }

  DT.rsession <- NULL

  for(i in infoChunk$file_name_rdata){

    catInfo(i)
    SpecificPath <- ShowADLPath(infoChunk[file_name_rdata == i, source])
    FileName <- i

    download.error <- try(assign(substr(i, 1, gregexpr("\\.", i)[[1]][1] - 1), ReadADLFile(SpecificPath, FileName, SaveChunk = F, ShowInfo = T)) == 0)

    read.error     <- try(data.table::is.data.table(get(gsub(".Rdata", "", i))))

    if(download.error[1] != T) {DT <- "LoadError"}
    if(read.error[1] != T & download.error[1] == T) {DT <- "ReadError"}

    if(read.error[1] == T & download.error[1] == T) DT <- object.size(get(gsub(".Rdata", "", i)))

    DT.rsession <- c(DT.rsession, DT)
    remove( list = gsub(".Rdata", "", i), envir = .GlobalEnv )

  }

  infoChunk$rsession_size_bytes <- DT.rsession


  infoChunk[, rsession_size := "undefined"]
  infoChunk[rsession_size_bytes %in% c("LoadError" , "ReadError"), rsession_size :=
              rsession_size_bytes]
  infoChunk[rsession_size == "undefined", rsession_size := utils:::format.object_size(as.numeric(rsession_size_bytes), "auto"), by =
              "key.file"]

  infoChunk  <- infoChunk[,.(time, source, chunk = key.file,
                            file_name_csv, csv_size_bytes, csv_size,
                            file_name_rdata, rdata_size_bytes, rdata_size,
                            rsession_size_bytes, rsession_size
  )]

  return(infoChunk)
}

#' @title Unzip files
#' @description TODO
#' @param File TODO
#' @param Savepath TODO
#' @family Internal Utilities
#' @keywords internal
#' @author Jonathan Tooley Associados Lda
#' @seealso \code{\link{TAPChunks}}
Unzip  <- function(File = "", SavePath){

  if(missing(SavePath)){

    SavePath <- ShowCachePath()

  }else{

    SavePath <- ShowCachePath(SavePath)
  }

  if (length(grep(".zip", File)) != 0){

    if(length(grep("/", File)) == 1){

      folder <- substr(File, 1, gregexpr("/", File)[[1]][1] - 1)

      unzip (file.path(SavePath, File),
             exdir = file.path(SavePath, folder))

      #delete .zip file after extracting files
      unlink(file.path(SavePath, File))

    } else{

      #unzip compressed file
      unzip (file.path(SavePath, File),
             #export to temp folder
             exdir = file.path(SavePath))

      unlink(file.path(SavePath, File))
    }

  }else{

    catError("Different extension file.")
  }
}


#' @title  WriteFmt (Internal Function used in ExportToSQL)
#' @description TODO
#' @examples TODO
#' @param InputData TODO
#' @param ServerPath TODO
#' @author Jonathan Tooley Associados Lda
#' @export
#' @seealso \code{\link{TAPChunks}}
WriteFmt <- function(InputData, ServerPath = ".") {

  start.time <- Sys.time()
  table.name <- "fmt_file"

  if (missing(InputData)) {

    catError("Argument 'InputData' is missing, with no default.")

  }else if (missing(table.name)) {

    catError("Argument 'table.name' is missing, with no default.")

  }
  #create temporary folder with FMT file
  if (file.exists(paste(ServerPath, "WasteFile", sep = "/")) == F) {

    dir.create(paste(ServerPath, "WasteFile", sep = "/"), showWarnings = FALSE)

  }
  #set up FMT file
  sql.server.version   <- "12.0"
  col.names            <- names(InputData)
  header               <- c(sql.server.version, length(col.names))
  tab                  <- "\\t"
  newline              <- "\\r\\n"
  character.collaction <- "SQL_Latin1_General_CP1_CI_AS"

  is.text  <- sapply(InputData, is.character)
  body     <- data.table::data.table(host_field_order    = seq_along(col.names),
                                     host_data_type      = "SQLCHAR",
                                     prefix_length       = 0,
                                     data_length         = 0,
                                     terminator          = paste0("\"", c(rep(tab, length(col.names) - 1), newline), "\""),
                                     server_column_order = seq_along(col.names),
                                     server_column_name  = paste0("\"", col.names, "\""),
                                     collation           = ifelse(is.text, character.collaction, "\"\""))

  file.name <- sprintf(paste(ServerPath, "WasteFile/%s.fmt", sep = "/"), table.name)

  writeLines(header, file.name)
  write.table(body, file.name,
              append    = TRUE,
              quote     = FALSE,
              sep       = "\t",
              na        = "",
              row.names = FALSE,
              col.names = FALSE)

  catSuccess(paste("The ", table.name, ".FMT has been created successfully:\n\t. Processed in: ",
                   table.name, ConvertTime(difftime(Sys.time(), start.time, units = "secs"))))

}

#' @title  ConvertTime (Internal Function used in ExportToSQL)
#' @description TODO
#' @examples TODO
#' @param TimeSec TODO
#' @author Jonathan Tooley Associados Lda
#' @export
#' @seealso \code{\link{TAPChunks}}
ConvertTime  <- function(TimeSec) {

  if (missing(TimeSec)) {

    catError("Argument 'TimeSec' is missing, with no default.")
  }else {

    TimeSec  <- round(as.numeric(TimeSec), 0)
    if (as.integer(TimeSec / 60) < 1) {

      return(paste(TimeSec, "secs", sep = " "))
    } else {

      secs <- round( (TimeSec / 60 - as.integer(TimeSec / 60)) * 60, 0)
      mins <- as.integer(TimeSec / 60)
      return(paste(mins, "mins", secs, "secs", sep = " "))
    }
  }
}

#' @title  ConvertTime (Internal Function used in ExportToSQL)
#' @description TODO
#' @examples TODO
#' @param Connection TODO
#' @param InputData TODO
#' @param DbSchema TODO
#' @param DbTableName TODO
#' @author Jonathan Tooley Associados Lda
#' @export
#' @seealso \code{\link{TAPChunks}}
CreateTable  <- function(Connection, InputData, DbSchema, DbTableName){

  if (missing(Connection) || class(Connection) != "RODBC") {

    catError("Argument 'Connection' is missing, with no default or is not a RODBC Connection.")

  } else if (missing(InputData)) {

    catError("Argument 'InputData' is missing, with no default.")

  } else if (missing(DbSchema)) {

    catError("Argument 'DB Schema' is missing, with no default.")

  } else if (missing(DbTableName)) {

    catError("Argument 'DB table name' is missing, with no default.")

  } else {

    create.schema  <- gsub("\n", "",  sprintf("IF NOT EXISTS (
                                              SELECT  SCHEMA_NAME
                                              FROM    INFORMATION_SCHEMA.SCHEMATA
                                              WHERE   SCHEMA_NAME = '%s' )
                                              BEGIN
                                              EXEC sp_executesql N'CREATE SCHEMA %s'
                                              END", DbSchema, DbSchema))
    DT       <- data.table::data.table(InputData)

    name.col <- paste0("[", names(head(DT)), "]")

    tryCatch({

      DT[is.na(DT)] <- "na"
      size.class <- c(sapply(data.table::data.table(DT[, sapply(head(DT), is.character), with = F]), function(x) {
        max(nchar(x)) + 15
      }),
      sapply(data.table::data.table(DT[, sapply(head(DT), is.factor), with = F]), function(x) {
        max(nchar(levels(x))) + 15
      }))
      size.class[is.na(size.class)] <- 50
    }
    , error = function(err) {

      size.class <- 50
    }
    )

    if (!is.null(attributes(size.class))){

      names(size.class) <- paste0("[", names(size.class), "]")
    }

    class.DT <- NULL
    n        <- 1
    for (i in  sapply( DT, class)) {

      i <- i[1][[1]]
      if (i == "numeric") {

        new.class <- "FLOAT"
      } else if (i == "integer") {

        new.class <- "INT"
      } else if (i == "logical") {

        new.class <- "BIT"
      } else if (i == "date") {

        new.class <- " DATE"
      } else if (i == "factor" | i == "character" | i == "ordered") {

        new.class <- sprintf("VARCHAR(%s)", size.class[name.col[n]])
      } else {

        new.class <- "VARCHAR(50)"
      }
      n <- n + 1

      class.DT     <- c(class.DT, new.class)
    }
    table.name   <- paste(DbSchema, DbTableName, sep = ".")
    create.table <- sprintf("IF OBJECT_ID (N'%s', N'U') IS NOT NULL DROP TABLE %s; CREATE TABLE %s (%s);",
                            table.name, table.name, table.name, paste(paste(name.col, class.DT), collapse = ", "))

    result <- RODBC::sqlQuery(Connection, create.schema)
    if (length(result) == 2){
      catInfo(sprintf("ERROR-> %s", result[1])); catError("Process Break: error creating schema.")
    }
    result <- RODBC::sqlQuery(Connection, create.table)
    if (length(result) == 2){
      catInfo(sprintf("ERROR-> %s", result[1])); catError("Process Break: error creating table.")
    }
    return(catSuccess(sprintf("Table %s has been created successfully!", paste(DbSchema, DbTableName, sep = "."))))
  }
}


#' @title  Rbind function recreated to have a better perform in RAM memory
#' @description This function recreates the base rbind from dta.table library. Keep in mind that it will remove the DT that you insert,
#' since it tries to reduce the memory usage.
#' @examples TODO
#' @param a first data table to append
#' @param b second data table to append
#' @author Jonathan Tooley Associados Lda
#' @export
#' @seealso \code{\link{TAPChunks}}
rbindImp <- function(a, b){

  if (all(names(a) == names(b))) {

    result <- list()

    cols <- copy(names(a))

    factorList <- names(grep("factor", lapply(a, class), value = T))

    for (j in 1:length(cols)) {

      col <- cols[j]

      if(col %in% factorList){

        result[[col]]         <- a[[col]]
        levels(result[[col]]) <- unique(c(as.vector.factor(a[[col]]), as.vector.factor(b[[col]])))
        result[[col]][(length(result[[col]]) + 1):(length(result[[col]]) + length(b[[col]]))] <- b[[col]]


      }else{
        result[[col]] <- append(a[[col]], b[[col]])
      }

      a[, (col) := NULL]
      b[, (col) := NULL]


    }

    setDT(result, F)
    return(result)

  }else{
    catError("Columns don't have the same column names")
  }


}

