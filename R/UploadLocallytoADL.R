#' @title Save file in the Data Lake
#' @description
#' A function to save files in the data lake.
#' @author JTA - The Data Scientists
#' @return Prints a status message.
#' @examples TODO
#' @param FileName Name file
#' @param FileExtension Extension file
#' @param ADLDirectory Directory where the file will be stored.
#' @param FileDirectory TODO
#' @export
#' @family Data Lake Tools
#' @seealso \code{\link{TAPChunks}}
UploadLocallyToADL <- function(FileName, FileExtension = "", ADLDirectory, FileDirectory = "") {
  FileExtension <- tolower(FileExtension)

  if (FileExtension == "rdata") {
    FileExtension <- "Rdata"
  }


  FileName <- paste0(FileName, ".", FileExtension)


  if (missing(ADLDirectory)) {
    data.lake.path <-
      file.path(paste("user", Sys.getenv("USERNAME"), sep = "/"), FileName)
  } else {
    data.lake.path <- file.path(ADLDirectory, FileName)
  }


  if (TAPChunks:::TestNet()) {
    Data_Lake <- TAPChinks:::ConnectToADL()

    adl.account.name <- "capdevtapdl"

    upload.file <-
      httr::upload_file(paste0(FileDirectory, "/", FileName))

    r <-
      httr::PUT(
        paste0(
          "https://",
          adl.account.name,
          ".azuredatalakestore.net/webhdfs/v1/",
          data.lake.path,
          "?op=CREATE&overwrite=true&write=true"
        ),
        body = upload.file,
        httr::add_headers(
          Authorization = paste("Bearer", Data_Lake),
          "Transfer-Encoding" = "chunked"
        )
      )

    result.load <- switch(
      toString(r$status_code),
      "201" = TAPChunks:::HttpCode(201),
      "400" = TAPChunks:::HttpCode(400),
      "403" = TAPChunks:::HttpCode(403),
      "404" = TAPChunks:::HttpCode(404)
    )

    return(result.load)
  }
}
