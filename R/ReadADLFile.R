#' @title  Reading user data from the Data Lake
#' @description This function allows users to read in a file that has been previously
#' stored on the Azure Data Lake. Chunks of telemetry data may be more easily read by
#' using \code{\link{ReadADLChunk}} which simplifies the process for pre-prepared chunks.
#' This function allows users more flexibility to select the file they wish and they may
#' also get added file metadata too.
#' @author JTA - The Data Scientists
#' @examples TODO
#' @return Prints a status message.
#' @param SpecificPath Specific folder path where the file is located
#' @param FileName Name of the file
#' @param SaveChunk Save chunk in your local machine
#' @param ShowInfo Show extra information
#' @export
#' @family Data Lake Tools
#' @seealso \code{\link{TAPChunks}}
ReadADLFile <- function(SpecificPath, FileName, SaveChunk = T, ShowInfo = T) {
  if (length(grep("csv$", tolower(FileName))) + length(grep("rdata$", tolower(FileName))) == 0) {
    TAPChunks:::catError("Only works with Rdata or CSV files.", "Rdata or CSV")
  }
  Data_Lake <- TAPChunks:::ConnectToADL()
  operation <- "?op=LISTSTATUS"
  dir <- httr::GET(paste0(
    "https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
    SpecificPath, operation
  ), httr::add_headers(Authorization = paste(
    "Bearer",
    Data_Lake
  )))
  if (dir$status_code == 200) {
    fname <- c()
    dir <- httr::content(dir, as = "parsed")
    dir <- dir$FileStatuses$FileStatus
    num <- length(dir)
    for (i in (1:num)) {
      fname <- c(fname, dir[[i]]$pathSuffix)
    }
    if (length(fname) >= 5) {
      catSuperUser(FormatList(
        c(fname, fname, fname, fname),
        "List Files:", 5
      ))
    }
    else {
      catSuperUser(sprintf("List Files:\n\t\t- %s", paste(fname,
        collapse = "\n\t\t- "
      )))
    }
  }
  else {
    TAPChunks:::catError(TAPChunks:::HttpCode(dir$status_code))
  }
  FileName <- fname[which(tolower(fname) == tolower(FileName))]
  if (length(FileName) == 0) {
    TAPChunks:::catError(
      "The file name was not found, please check the name.",
      "not found"
    )
  }
  info <- httr::GET(paste0(
    "https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
    file.path(SpecificPath, FileName), operation
  ), httr::add_headers(Authorization = paste(
    "Bearer",
    Data_Lake
  )))
  if (info$status_code != 200) {
    TAPChunks:::catError(TAPChunks:::HttpCode(info$status_code))
  }
  info <- httr::content(info, as = "parsed")
  if (ShowInfo == T) {
    versionTime <- info$FileStatuses$FileStatus[[1]]$modificationTime
    versionTime <- as.integer(substr(versionTime, 1, 10))
    versionTime <- format(.POSIXct(versionTime), "%d/%b/%Y ")
    length.file <- info$FileStatuses$FileStatus[[1]]$length
    length.file <- round(length.file / 1024 / 1024, 2)
    TAPChunks:::catInfo(sprintf("Last Modification: %s", versionTime))
    TAPChunks:::catInfo(sprintf("Size: %sMB", length.file))
  }
  temp.folder <- ShowCachePath()
  if (sum(list.files(temp.folder) == FileName) > 0) {
    TAPChunks:::catInfo("The file already exists in cache.", "in cache")
  }
  else {
    operation <- "?op=OPEN&read=true"
    r <- httr::GET(paste0(
      "https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
      file.path(SpecificPath, FileName), operation
    ), httr::add_headers(Authorization = paste(
      "Bearer",
      Data_Lake
    )))
    if (r$status_code != 200) {
      TAPChunks:::catError(TAPChunks:::HttpCode(r$status_code))
    }
    else {
      writeBin(httr::content(r), file.path(
        temp.folder,
        FileName
      ))
      TAPChunks:::catSuccess(sprintf(
        "The ADL fetch (path:%s) reported success.",
        file.path(SpecificPath, FileName)
      ), "success")
    }
  }
  if (length(grep("csv$", FileName)) == 1) {
    DT <- suppressWarnings(assign(gsub(".csv", "", FileName), data.table::fread(file.path(
      temp.folder,
      FileName
    ))))
    Data <- "DT"
  }
  else {
    Data <- load(file.path(temp.folder, FileName))
  }
  if (SaveChunk != T) {
    unlink(file.path(temp.folder, FileName))
  }

  return(get(Data))
}
