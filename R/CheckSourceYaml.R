#' @title Read Configuration Files
#' @description Check the YAML file to see if there is any source modification
#' @family Internal Utilities
#' @seealso \code{\link{TAPChunks}}
#' @keywords internal
CheckSourceYaml <- function() {
  if (sum(list.files(TAPChunks:::ShowConfigurationPath()) == "Source.yaml") == 1) {
    operation <- "?op=LISTSTATUS"
    data.lake.path <- "Source_Data/Configuration/Source.yaml"
    Data_Lake <- TAPChunks:::ConnectToADL(
      Configuration_env$Key$ADL$Client,
      Configuration_env$Key$ADL$Secret,
      Configuration_env$Key$ADL$Tenant
    )

    dir <-
      httr::GET(
        paste0(
          "https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
          data.lake.path,
          operation
        ),
        httr::add_headers(Authorization = paste("Bearer", Data_Lake))
      )

    dir <- httr::content(dir, as = "parsed")
    ftime <-
      as.integer(substr(dir$FileStatuses$FileStatus[[1]]$modificationTime, 1, 10))

    fileTime <-
      as.integer(file.info(file.path(
        TAPChunks:::ShowConfigurationPath(), "Source.yaml"
      ))$mtime)

    if (fileTime < ftime) {
      unlink(file.path(TAPChunks:::ShowConfigurationPath(), "Source.yaml"))
      TAPChunks:::catSuperUser("The file Source.yaml is outdated.")
    }
  }
}
