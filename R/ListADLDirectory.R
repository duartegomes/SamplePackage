#' @title List files in the Data Lake
#' @description
#' A function to return a directory of files in the data lake for the specificied source.
#' @details Telemetry data is stored in an Azure Data Lake and is organized by data source.
#' This function allows the user to retrieve a directory listing of all available files
#' relating to a particular data source.
#' @inheritParams ShowADLPath
#' @author JTA - The Data Scientists
#' @return Returns a data table with a list of available filenames or the message off-line
#' if the user does not have net connectivity.
#' @param Directory Specific directory in an Azure Data Lake
#' @param Info if true return the modification time
#' @export
#' @examples ListADLDirectory("Email")
#' @family Data Lake Tools
#' @seealso \code{\link{TAPChunks}}
ListADLDirectory <- function(Directory = "", Info = F) {
  if (!exists("Configuration_env", envir = .GlobalEnv)) {
    TAPChunks:::GetConfiguration()
  }
  if (Directory == "") {
    data.lake.path <- "Source_Data/SW"
  } else {
    data.lake.path <- Directory
  }

  if (TAPChunks:::TestNet()) {
    Data_Lake <-
      TAPChunks:::ConnectToADL()

    operation <- "?op=LISTSTATUS"
    dirFolder <-
      httr::GET(
        paste0(
          "https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
          data.lake.path,
          operation
        ),
        httr::add_headers(Authorization = paste("Bearer", Data_Lake))
      )

    folderName <- c()
    ftime <- c()

    if (dirFolder$status_code == 200) {
      dirFolder <- httr::content(dirFolder, as = "parsed")
      dirFolder <- dirFolder$FileStatuses$FileStatus
      num <- length(dirFolder)
      for (i in (1:num)) {
        folderName <- c(folderName, dirFolder[[i]]$pathSuffix)
        ftime <- c(ftime, dirFolder[[i]]$modificationTime)
      }
    } else {
      TAPChunks:::catError("The directory was not found, please check the directory name.", "not found")
    }

    ftime <- substr(ftime, 1, 10)
    summary <- data.table::data.table(file = folderName, ftime)
    data.table::set(summary, i = NULL, j = "TimeADL", .POSIXct(summary$ftime))
    data.table::set(summary, i = NULL, j = "ftime", NULL)

    filesName <- grep("\\.", folderName, value = T)

    folderName <- setdiff(folderName, filesName)

    if (Directory != "") {
      if (length(filesName) != 0) {
        catSuperUser(paste(
          "There are this files:\n\t\t-",
          paste(filesName, collapse = "\n\t\t- ")
        ), "files")
      }
      if (length(folderName) != 0) {
        catSuperUser(paste(
          "There are this folders:\n\t\t-",
          paste(folderName, collapse = "\n\t\t- ")
        ), "folders")
      }


      if (Info) {
        return(data.table::data.table(summary))
      } else {
        return(data.table::data.table(Directory = summary$file))
      }
    } else {
      oldFolder <- names(Configuration_env[["Source"]])


      missingFolder <- setdiff(oldFolder, folderName)
      newFolder <- setdiff(folderName, oldFolder)

      if (length(missingFolder)) catSuperUser(sprintf("There are sources that disappeared from the ADL:\n\t\t-%s", paste(missingFolder, collapse = "\n\t\t-")))
      if (length(newFolder)) catSuperUser(sprintf("There are sources that must be added to the source.yaml file:\n\t\t-%s", paste(newFolder, collapse = "\n\t\t-")), "source.yaml")


      Index_source <- menu(
        choices = folderName, title =
          "The requested source does not exist, choose one folder of the following or press 0 to cancel the process:"
      )
      if (Index_source == 0) {
        TAPChunks:::catError("No source selected.")
      }
      source <- folderName[Index_source]

      if (Directory == "") {
        data.lake.path <- sprintf(paste0(data.lake.path, "/%s/rdata"), source)
      }

      else {
        data.lake.path <- sprintf(paste0(data.lake.path, "/%s"), source)
      }

      dir <-
        httr::GET(
          paste0(
            "https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
            data.lake.path,
            operation
          ),
          httr::add_headers(Authorization = paste("Bearer", Data_Lake))
        )


      fname <- c()
      ftime <- c()

      if (dir$status_code == 200) {
        dir <- httr::content(dir, as = "parsed")
        dir <- dir$FileStatuses$FileStatus
        num <- length(dir)
        for (i in (1:num)) {
          fname <- c(fname, dir[[i]]$pathSuffix)
          ftime <- c(ftime, dir[[i]]$modificationTime)
        }
      }

      ftime <- substr(ftime, 1, 10)
      summary <- data.table::data.table(file = fname, ftime)
      data.table::set(summary, i = NULL, j = "TimeADL", .POSIXct(summary$ftime))
      data.table::set(summary, i = NULL, j = "ftime", NULL)
    }
  } else {
    TAPChunks:::catError("Off-line\n", "Off-line")
  }

  if (Directory == "") {
    catSuperUser(paste(
      "There are this files:\n\t\t-",
      paste(fname, collapse = "\n\t\t- ")
    ))
    if (Info) {
      return(data.table::data.table(summary))
    } else {
      return(data.table::data.table(Directory = fname))
    }
  }
}
