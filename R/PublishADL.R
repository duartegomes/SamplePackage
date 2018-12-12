#' @title Publish File into the Data Lake
#' @description
#' A function to save files in the Publication folder in the data lake. This function is similar to SaveADL() function, the difference is the chunk is saved in the Publish area.
#' By default saves a CSV file but also supports Rdata files.
#' @author JTA - The Data Scientists
#' @examples PublishADL(TestEmailChunk, Project="Emailtracker", ChunkName = "FactFile", FileExtension="csv", Replace = T)
#' @inheritParams SaveADL
#' @param Data A TAP Chunk to be saved to the data lake
#' @param Project Name of the project. If the project doesn't exist a new folder will be created in the ADL.
#' @param ChunkName The name that is given to the data on the lake.
#' @param FileExtension A string containing either 'csv' or 'rdata' used to select the format in which the file is saved.
#' @param Replace Replace in case exists a file with same name. By default is FALSE.
#' @export
#' @family Reporting and saving tools
#' @seealso \code{\link{TAPChunks}}
PublishADL <- function(Data,
                       Project,
                       ChunkName,
                       FileExtension = "csv",
                       Replace = F) {
  if (!tolower(FileExtension) %in% c("rdata", "csv")) {
    TAPChunks:::catError("The operation was interrupted. Parameter 'FileExtension' should be 'Rdata' or 'csv'.")
  }

  if (missing(Data)) {
    TAPChunks:::catError("The operation was interrupted. Missing parameter 'Data'.")
  }

  if (!is.data.frame(Data)) {
    TAPChunks:::catWarning("The parameter Data isn't a data.table.")
  }

  if (missing(ChunkName)) {
    ChunkName <- substitute(Data)
  }

  operation <- "?op=LISTSTATUS"

  listProject <-
    httr::GET(
      paste0(
        "https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
        "Publication",
        operation
      ),
      httr::add_headers(Authorization = paste(
        "Bearer",
        TAPChunks:::ConnectToADL()
      ))
    )

  fname <- c()

  if (listProject$status_code == 200) {
    dir <- httr::content(listProject, as = "parsed")$FileStatuses$FileStatus
    num <- length(dir)

    for (i in (1:num)) {
      fname <- c(fname, dir[[i]]$pathSuffix)
    }
  }

  Path <-
    file.path("Publication", Project)

  listFiles <-
    httr::GET(
      paste0(
        "https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
        Path,
        operation
      ),
      httr::add_headers(Authorization = paste(
        "Bearer",
        TAPChunks:::ConnectToADL()
      ))
    )
  fname <- c()
  if (listFiles$status_code == 200) {
    dir <-
      httr::content(listFiles, as = "parsed")$FileStatuses$FileStatus
    num <- length(dir)
    for (i in (1:num)) {
      fname <- c(fname, dir[[i]]$pathSuffix)
    }
  }

  if (tolower(paste0(ChunkName, paste0(".", FileExtension))) %in% tolower(fname)) {
    if (Replace == T) {
      SaveADL(
        Data,
        FileExtension = FileExtension,
        ADLDirectory = Path,
        ChunkName = ChunkName,
        SaveLocal = F
      )
    } else {
      TAPChunks:::catError(
        sprintf(
          "The %s file already exists in ALD. If you want to replace, set the Replace parameter as TRUE",
          paste0(ChunkName, paste0(".", FileExtension))
        ),
        paste0(ChunkName, paste0(".", FileExtension))
      )
    }
  } else {
    SaveADL(
      Data,
      FileExtension = FileExtension,
      ADLDirectory = Path,
      ChunkName = ChunkName,
      SaveLocal = F
    )
  }
}
