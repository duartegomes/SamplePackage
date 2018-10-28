

#' @title FormatList
#' @description Format the list in columns
#' @param ListElement List to be applied
#' @param Title List title
#' @param NumberCol Number the columns the list will have
#' @family Internal Utilities
FormatList <- function(ListElement,
                       Title = "Title:" ,
                       NumberCol = 3) {
  formatColumns <-
    unique(c(seq(
      from = 0, to = length(ListElement), NumberCol
    ), length(ListElement)))

  numberChar <- c()
  for (n in 0:NumberCol) {
    if (n == 0) {
      Index <- formatColumns[-1]
    } else{
      Index <- formatColumns
    }
    Index <- Index + n
    Index <- Index[which(Index <= max(formatColumns))]

    numberChar <- max(nchar(ListElement[Index]))

    for (i in Index) {
      test <- nchar(ListElement[i])
      ListElement[i] <-
        paste0(ListElement[i], paste0(rep(" ", numberChar - test), collapse = ""))
    }
  }

  ListElement[formatColumns] <-
    paste0(ListElement[formatColumns], "\n\t")

  if (length(ListElement) > 9) {
    numberTime      <- as.character(1:length(ListElement))
    numberTime[1:9] <- paste0(" ", numberTime[1:9])
    numberTime[1]   <- paste0("\t\t", numberTime[1])
  } else{
    numberTime      <- as.character(1:length(ListElement))
    numberTime <- paste0(" ", numberTime)
    numberTime[1]   <- paste0("\t\t", numberTime[1])
  }

  return(
    paste(
      sprintf("%s\n", Title),
      paste(
        numberTime,
        ListElement,
        sep = " - ",
        collapse = "\t"
      )
    )
  )

}


#' @title Establishing and pointing to the configuration settings
#' @description TODO
#' @param Directory TODO
#' @author Jonathan Tooley Associados Lda
#' @export
#' @examples ShowConfigurationPath()
#' @family Cache Controls
#' @seealso \code{\link{TAPChunks}}
# Backlog ???
ShowConfigurationPath <- function(Directory) {
  if (missing(Directory)) {
    Configuration.path <- path.expand("~")
    Configuration.path <-
      paste0(Configuration.path, "/TAPConfiguration")
  } else{
    Configuration.path <- path.expand(Directory)
  }
  if (!file.exists(Configuration.path)) {
    dir.create(Configuration.path)
  }
  return(Configuration.path)
}

#' @title CheckSourceYaml
#' @description Check the YAML file to see if there is any source modification
#' @family Internal Utilities
CheckSourceYaml <- function() {

  if(TestNet()){

    if (sum(list.files(ShowConfigurationPath()) == "Source.yaml") == 1) {
      operation <- "?op=LISTSTATUS"
      data.lake.path <- "Source_Data/Configuration/Source.yaml"
      Data_Lake <- ConnectToADL( Configuration_env$Key$ADL$Client,
                                 Configuration_env$Key$ADL$Secret,
                                 Configuration_env$Key$ADL$Tenant)

      dir <-
        httr::GET(
          paste0(
            "https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
            data.lake.path,
            operation
            ),
          httr::add_headers(Authorization = paste("Bearer",  Data_Lake))
          )
      dir   <- httr::content(dir, as = "parsed")
      ftime <- as.integer(substr(dir$FileStatuses$FileStatus[[1]]$modificationTime, 1, 10))

      fileTime <- as.integer(file.info(file.path(ShowConfigurationPath(), "Source.yaml"))$mtime)

      if (fileTime < ftime) {
        unlink(file.path(ShowConfigurationPath(), "Source.yaml"))
        catSuperUser("The file Source.yaml is outdated.")
      }
    }
  }
}


#' @title Get configuration settings
#' @description Get the file configurations like Keys to access ADL and ABS and also the source YAML file
#' @param Directory Directory where Configuration files are stored
#' @import httr
#' @author Jonathan Tooley Associados Lda
#' @export
#' @import yaml
#' @examples GetConfiguration()
#' @family Internal Utilities
#' @seealso \code{\link{TAPChunks}}
# Backlog ???
GetConfiguration <- function(Directory = NULL) {

  if (is.null(Directory)) {
    Directory <- ShowConfigurationPath()
  }

  TAPChunks:::OptionsMessage()

  if(TestVPN()) {

    UpdateTAPChunks()
  }

  RStudioVersion <- Configuration_env$Message$Configuration$RStudioVersion

  if(!is.numeric(RStudioVersion)) RStudioVersion<-1.0

  if (RStudioVersion < 1.1) {
    cat(
      "You should update the version of RStudio in order to take advantage of all the functionalities in package TAPChunk.
      \t-https://www.rstudio.com/products/rstudio/download/#download\n"
    )
  }


  configurationFiles <- c("Key.yaml", "Message.yaml", "Source.yaml")
  remoteDirectory <- "//capengsharedev/JTAPackages/TAPConfiguration"

  for (i in configurationFiles) {
    subEnv <- gsub(".yaml", "", i)

    if (subEnv == "Key") {
      if (length(intersect(list.files(Directory), "Key.yaml")) != 1 || file.info(file.path(Directory,"Key.yaml"))$mtime < file.info(file.path(remoteDirectory,"Key.yaml"))$mtime) {

        if(TestVPN()) {

          if(length(intersect(list.files(remoteDirectory), "Key.yaml")) == 1){

            file.copy(from=file.path(remoteDirectory,"Key.yaml"), to=file.path(Directory,"Key.yaml"),
                      overwrite = TRUE, recursive = FALSE,
                      copy.mode = TRUE)

          }else {
            catError( "The Keys Configuration File is not found. If you don't have the file please ask TAPsupport@microsoft.com for more information.",
                           "TAPsupport@microsoft.com")
                           }
        }else {

          catError( "The Keys Configuration File is not found. If you don't have the file please ask TAPsupport@microsoft.com for more information.",
                    "TAPsupport@microsoft.com"
          )
          }
      }else {
        nameFile <- intersect(list.files(Directory), i)
        listConf <- yaml::yaml.load_file(file.path(Directory, nameFile))

        for (n in names(listConf)) {
          Configuration_env[[subEnv]][[n]] <- listConf[[n]]
        }
      }
    }

    if (subEnv == "Message") {
      if (length(intersect(list.files(Directory), "Message.yaml")) != 1) {
        TAPChunks:::OptionsMessage()
      } else{
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
        Data_Lake <-  ConnectToADL( Configuration_env$Key$ADL$Client,
                                    Configuration_env$Key$ADL$Secret,
                                    Configuration_env$Key$ADL$Tenant)
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
        writeBin(httr::content(r), file.path(Directory,  filesYaml))
      }

      nameFile <- intersect(list.files(Directory), i)
      listConf <- yaml::yaml.load_file(file.path(Directory, nameFile))
      for (n in names(listConf)) {
        Configuration_env[[subEnv]][[n]] <- listConf[[n]]
      }
    }
  }
}


#' @title OptionsMessage
#' @description To define which message type the users want to see.
#' @param Error Want to get Error messages?
#' @param Warning Want to get warning messages?
#' @param Info Want to get info messages?
#' @param Success Want to get success messages?
#' @family Internal Utilities
#'
OptionsMessage <- function(Error = T,
                           Warning = T,
                           Info = T,
                           Success = T) {

  if (!exists("Configuration_env", envir = .GlobalEnv)){
    Configuration_env <<- new.env()
  }

  Configuration_env$Message$Configuration$Error     <- Error
  Configuration_env$Message$Configuration$Warning   <- Warning
  Configuration_env$Message$Configuration$Info      <- Info
  Configuration_env$Message$Configuration$Success   <- Success
  Configuration_env$Message$Configuration$SuperUser <- FALSE
  Configuration_env$Message$Configuration$StatusMessage <- "default"


  RStudioVersion<-try(RStudio.Version(), silent = TRUE)


  if(is.null(attributes(RStudioVersion)$class)){
    RStudioVersion <- paste(RStudioVersion$version)
    RStudioVersion <- as.numeric(substr(RStudioVersion, 1, 3))
  }else{
    RStudioVersion <- 1.0
  }

  Configuration_env$Message$Configuration$RStudioVersion <-  RStudioVersion

}



#' @title catError
#'
#' @description Print Error message with Highlighed text
#' @param Text TODO
#' @param Highlight TODO
#' @export
#'
catError <- function(Text, Highlight = NULL) {
  if (!exists("Configuration_env", envir = .GlobalEnv))
    OptionsMessage()

  RstudioVersion <-
    Configuration_env$Message$Configuration$RStudioVersion
  errorMessage <- Text
  Text <- paste0(Text, "\n")

  if (Configuration_env$Message$Configuration$Error) {
    if (!is.null(Highlight)) {
      if (RstudioVersion >= 1.1)

        Text <- gsub(Highlight, BoldText(Highlight), Text)
    }

    if (RstudioVersion >= 1.1) {
      Text <- RedText(paste0("\t- ", Text))

    } else{
      Text <- paste0("\t- ", Text)
    }


    status <- Configuration_env$Message$Configuration$StatusMessage


    if (status != "error") {
      if (RstudioVersion >= 1.1) {
        cat(RedText(paste0(BoldText("Error:\n"))))
      }else{cat("Error:\n")}
    }

    cat(Text)
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    Configuration_env$Message$Configuration$StatusMessage <- "error"
    stop(errorMessage)
  }else{stop(errorMessage)}

}

#' @title catWarn
#' @description Print Warning message with Highlighed text
#' @param Text TODO
#' @param Highlight TODO
#' @export
#'
catWarning <- function(Text, Highlight = NULL) {
  if (!exists("Configuration_env", envir = .GlobalEnv))
    OptionsMessage()
  RstudioVersion <-
    Configuration_env$Message$Configuration$RStudioVersion
  warnMessage <- Text
  Text <- paste0(Text, "\n")
  if (Configuration_env$Message$Configuration$Warning) {
    if (!is.null(Highlight)) {
      if (RstudioVersion >= 1.1)
        Text <- gsub(Highlight, YellowText(BoldText(Highlight)), Text)
    }

    Text <- paste0("\t- ", Text)

    status <- Configuration_env$Message$Configuration$StatusMessage

    if (status != "warn") {
      if (RstudioVersion >= 1.1) {
        cat(paste0(YellowText(BoldText(
          "Warning:\n"
        ))))
      }else{cat("Warning:\n")}
    }
    cat(Text)
    options(warn = -1)
    warning(warnMessage)
  }
  else{
    options(warn = -1)
    warning(warnMessage)
  }
  Configuration_env$Message$Configuration$StatusMessage <- "warn"

}

#' @title catSuccess
#' @description Print Success message with Highlighed text
#' @param Text TODO
#' @param Highlight TODO
#' @export
#'
catSuccess <- function(Text, Highlight = NULL) {
  if (!exists("Configuration_env", envir = .GlobalEnv))
    OptionsMessage()
  RstudioVersion <-
    Configuration_env$Message$Configuration$RStudioVersion

  Text <- paste0(Text, "\n")
  if (Configuration_env$Message$Configuration$Success) {
    if (!is.null(Highlight)) {
      if (RstudioVersion >= 1.1)
        Text <- gsub(Highlight, GreenText(BoldText(Highlight)), Text)
    }


    Text <- paste0("\t- ", Text)

    status <- Configuration_env$Message$Configuration$StatusMessage


    if (status != "success") {
      if (RstudioVersion >= 1.1) {
        cat(paste0(GreenText(BoldText(
          "Success:\n"
        ))))
      }
      else{
        cat("Success:\n")
      }
    }

    cat(Text)

    Configuration_env$Message$Configuration$StatusMessage <-
      "success"
  }
}

#' @title catSuperUser
#' @description Print Super User messages with Highlighed text
#' @param Text TODO
#' @param Highlight TODO
#' @export
catSuperUser <- function(Text, Highlight = NULL) {
  if (!exists("Configuration_env", envir = .GlobalEnv))
    OptionsMessage()
  RstudioVersion <-
    Configuration_env$Message$Configuration$RStudioVersion
  Text <- paste0(Text, "\n")
  if (Configuration_env$Message$Configuration$SuperUser) {
    if (!is.null(Highlight)) {
      if (RstudioVersion >= 1.1)
        Text <- gsub(Highlight, BoldText(Highlight), Text)
    }

    if (RstudioVersion >= 1.1) {
      Text <- BlueText(paste0("\t- ", Text))
    } else{
      Text <- paste0("\t- ", Text)
    }

    status <- Configuration_env$Message$Configuration$StatusMessage


    if (status != "superUser") {
      if (RstudioVersion >= 1.1) {
        cat(paste0(BlueText(BoldText(
          "SuperUser:\n"
        ))))
      } else{
        cat("SuperUser:\n")
      }
    }

    cat(Text)

    Configuration_env$Message$Configuration$StatusMessage <-
      "superUser"
  }
}


#' @title UpdateTAPChunks
#' @description Update TAPChunks version in case there is a more recent one.
#' @import desc
#' @examples UpdatePackage()
#' @author Jonathan Tooley Associados Lda
#' @return None
#' @export
#' @seealso \code{\link{TAPChunks}}
UpdateTAPChunks <-  function(){

  if(TestVPN()){
    Built        <- installed.packages(lib.loc = .libPaths())[grep("TAPChunks", installed.packages(lib.loc = .libPaths())), "Built"]
    Installed    <- installed.packages(lib.loc = .libPaths())[grep("TAPChunks", installed.packages(lib.loc = .libPaths())), "Version"]

    temp.folder  <- "//capengsharedev/JTAPackages/"
    files        <- list.files(temp.folder)
    if(length(files)==0){

      catWarning( "In folder'//capengsharedev/JTAPackages', no files were found. Please check that you have access.")
      catWarning( "Can't update TAPChunks.")

      return(invisible)
    }

    package      <- grep("tar.gz", files, value = T)
    package      <- max(package)
    version      <- substr(package, gregexpr(".tar", package)[[1]][1] - 5, gregexpr(".tar", package)[[1]][1] - 1)

    directory <- getwd()
    setwd(paste0(.libPaths()[1], "/TAPChunks"))
    desc     <- desc::description$new()
    Deps     <- data.table::data.table(desc$get_deps())
    Imports  <- Deps[type == "Imports"]$package
    Imports  <- paste(Imports, collapse = ",")
    Imports  <- paste0(Imports, "\n(>= 1.3.4)")
    Suggests <- Deps[type == "Suggests"]$package
    Suggests <- paste(Suggests, collapse = ",")
    DependsP <- Deps[type == "Depends"]$package
    DependsV <- Deps[type == "Depends"]$version
    Depends  <- paste(DependsP, DependsV)


    availableP           <- matrix(c("TAPChunks", version,  Depends, Imports, Suggests,  "file LICENSE",
                                     package, temp.folder), nrow = 1, ncol = 8, dimnames = list("TAPChunks"))

    colnames(availableP) <- c("Package" , "Version" , "Depends", "Imports",  "Suggests", "License","File" ,
                              "Repository")
    setwd(directory)

    if(Installed < version){
      catInfo("Library TAPChunks:")
      catInfo(sprintf("Installed version %s in %s", Installed, .libPaths()[1]))

      catInfo(sprintf("New version %s in %s", version, temp.folder))
      answer <- readline("Do you want Update TAPChunks? (1 - Yes, 2 - No, 0 - Cancel)\n")
      answer <- tolower(answer)

      if(tolower(answer) == 0 | tolower(answer) == 2 | tolower(answer) == "no"){
        return("Cancelled")
      }

      if(tolower(answer) == 1 | tolower(answer) == "yes"){

        packages <- c('httr', 'curl', 'jsonlite', 'data.table', 'RODBC', 'yaml', 'desc')

        for(n in packages){

          CheckPackage(n)
        }

        install.packages(pkgs = paste0(temp.folder, package), repos = NULL,
                         type = "source", available = availableP, dependencies = c("Depends","Imports"))

        #unload and re-load TAPChunks
        unloadNamespace("TAPChunks")
        library(TAPChunks)

        }
    }

  }else{
    catWarning( "Can't update TAPChunks.")
    return(invisible)
  }

}
