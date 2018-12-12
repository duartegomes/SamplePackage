#' @title Save file in the Data Lake
#' @description
#' A function to save files in the data lake. By default saves a CSV file but also supports Rdata files.
#' By default saves in a folder named with user alias under user directory in the Data Lake.
#' @author JTA - The Data Scientists
#' @return Returns a status message.
#' @examples SaveADL(Data = TestEmailChunk, FileExtension = "csv", ADLDirectory = "Publication/", ChunkName = "EmailSample", SaveLocal=F)
#' @param Data This is a required field and is usually a TAP chunk but it can be any item of class "data.table" or "data.frame".
#' If the user submits a data frame then this will first be converted to a data table before the calculated column is added.
#' This is because data tables are far more efficient in R than a data frame.
#' The resulting table that is returned by this function will also be a data table even if a data frame was submitted.
#' @param FileExtension Extension supports csv and Rdata files. By default is csv.
#' @param ADLDirectory Directory where the file will be stored. By default is a folder
#' with the user's alias.
#' @param ChunkName Optional name to be given to the file when saved in the ADL. If null will aquire the object's name.
#' @param SavePath Path to save the chunk locally. By default is TAP cache folder.
#' @param SaveLocal To save the chunk locally set as TRUE, otherwise set as FALSE. By default is TRUE.
#' @export
#' @family Reporting and saving tools
#' @seealso \code{\link{TAPChunks}}
SaveADL <- function(Data, FileExtension = "csv", ADLDirectory, ChunkName, SavePath, SaveLocal = T) {
  FileExtension <- tolower(FileExtension)

  if (!(FileExtension == "csv" |
    FileExtension == "rdata")) {
    TAPChunks:::catError("Extension Format incorrect.")
  } else {
    if (FileExtension == "rdata") FileExtension <- "Rdata"
  }

  if (missing(ChunkName)) {
    ChunkName <- deparse(substitute(Data))
    FileName <- paste0(deparse(substitute(Data)), ".", FileExtension)
  } else {
    FileName <- paste0(ChunkName, ".", FileExtension)
  }

  if (missing(ADLDirectory)) {
    data.lake.path <- file.path(paste("user", Sys.getenv("USERNAME"), sep = "/"), FileName)
  } else {
    data.lake.path <- file.path(ADLDirectory, FileName)
  }
  if (missing(SavePath)) {
    path <- ShowCachePath()
  } else {
    path <- ShowCachePath(SavePath)
  }

  if (TAPChunks:::TestNet()) {
    Data_Lake <- TAPChunks:::ConnectToADL()

    adl.account.name <- "capdevtapdl"


    if (tolower(FileExtension) == "csv") {
      write.csv(Data, paste0(path, "/", FileName), row.names = FALSE)
      upload.file <- httr::upload_file(paste0(path, "/", FileName))
    }

    if (tolower(FileExtension) == "rdata") {
      assign(ChunkName, Data)
      save(list = ChunkName, file = paste0(path, "/", FileName))
      upload.file <- httr::upload_file(paste0(path, "/", FileName))
    }

    r <- httr::PUT(paste0(
      "https://", adl.account.name, ".azuredatalakestore.net/webhdfs/v1/",
      data.lake.path, "?op=CREATE&overwrite=true&write=true"
    ),
    body = upload.file,
    httr::add_headers(
      Authorization = paste("Bearer", Data_Lake),
      "Transfer-Encoding" = "chunked"
    )
    )

    result.load <- switch(toString(r$status_code),
      "201" = TAPChunks:::HttpCode(201),
      "400" = TAPChunks:::HttpCode(400),
      "403" = TAPChunks:::HttpCode(403),
      "404" = TAPChunks:::HttpCode(404)
    )

    if (!SaveLocal) {
      unlink(paste0(path, "/", FileName))
    }


    return(result.load)
  }
}
