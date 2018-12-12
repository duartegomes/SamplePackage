#' @title Get configuration settings
#' @description Get the file configurations like Keys to access ADL and ABS and also the source YAML file
#' @param Directory Directory where Configuration files are stored
#' @importFrom httr GET add_headers content upload_file PUT
#' @author JTA - The Data Scientists
#' @keywords internal
#' @importFrom  yaml yaml.load_file
#' @examples TAPChunks:::GetConfiguration()
#' @family Internal Utilities
#' @seealso \code{\link{TAPChunks}}
GetConfiguration <- function(Directory = NULL) {
  if (is.null(Directory)) {
    Directory <- TAPChunks:::ShowConfigurationPath()
  }

  TAPChunks:::OptionsMessage()

  remoteDirectory <- Directory

  RStudioVersion <- Configuration_env$Message$Configuration$RStudioVersion

  if (!is.numeric(RStudioVersion)) RStudioVersion <- 1.0

  if (RStudioVersion < 1.1) {
    cat(
      "You should update the version of RStudio in order to take advantage of all the functionalities in package TAPChunk.
      \t-https://www.rstudio.com/products/rstudio/download/#download\n"
    )
  }

  configurationFiles <- c("Key.yaml", "Message.yaml", "Source.yaml")

  for (i in configurationFiles) {
    subEnv <- gsub(".yaml", "", i)

    if (subEnv == "Key") {
      if (length(intersect(list.files(Directory), "Key.yaml")) != 1 || file.info(file.path(Directory, "Key.yaml"))$mtime < file.info(file.path(remoteDirectory, "Key.yaml"))$mtime) {
        if (TAPChunks:::TestVPN()) {
          if (length(intersect(list.files(remoteDirectory), "Key.yaml")) == 1) {
            file.info(file.path(Directory, "Key.yaml"))

            file.copy(
              from = file.path(remoteDirectory, "Key.yaml"),
              to = file.path(Directory, "Key.yaml"),
              overwrite = TRUE,
              recursive = FALSE,
              copy.mode = TRUE
            )
          }
        } else {
          TAPChunks:::catError(
            "The Keys Configuration File is not found. If you don't have the file please ask TAPsupport@microsoft.com for more information.",
            "TAPsupport@microsoft.com"
          )
        }
      } else {
        nameFile <- intersect(list.files(Directory), i)
        listConf <-
          yaml::yaml.load_file(file.path(Directory, nameFile))
        for (n in names(listConf)) {
          Configuration_env[[subEnv]][[n]] <- listConf[[n]]
        }
      }
    }
    if (subEnv == "Message") {
      if (length(intersect(list.files(Directory), "Message.yaml")) != 1) {
        TAPChunks:::OptionsMessage()
      } else {
        nameFile <- intersect(list.files(Directory), i)
        listConf <-
          yaml::yaml.load_file(file.path(Directory, nameFile))
        for (n in names(listConf)) {
          Configuration_env[[subEnv]][[n]] <- listConf[[n]]
        }
        Configuration_env$Message$Configuration$StatusMessage <-
          "default"
        Configuration_env$Message$Configuration$RStudioVersion <-
          RStudioVersion
      }
    }
    if (subEnv == "Source") {
      TAPChunks:::CheckSourceYaml()
      if (length(intersect(list.files(Directory), "Source.yaml")) != 1) {
        filesYaml <- "Source.yaml"
        Data_Lake <- TAPChunks:::ConnectToADL(
          Configuration_env$Key$ADL$Client,
          Configuration_env$Key$ADL$Secret,
          Configuration_env$Key$ADL$Tenant
        )
        operation <- "?op=OPEN&read=true"
        r <-
          httr::GET(
            paste0(
              "https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
              file.path("Source_Data/Configuration", filesYaml),
              operation
            ),
            httr::add_headers(Authorization = paste("Bearer", Data_Lake))
          )

        writeBin(httr::content(r), file.path(Directory, filesYaml))
      }

      nameFile <- intersect(list.files(Directory), i)
      listConf <- yaml::yaml.load_file(file.path(Directory, nameFile))
      for (n in names(listConf)) {
        Configuration_env[[subEnv]][[n]] <- listConf[[n]]
      }
    }
  }
}
