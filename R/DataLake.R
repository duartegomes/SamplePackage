#' @title List files in the Data Lake
#' @description
#' A function to return a directory of files in the data lake for the specificied source.
#' @details Telemetry data is stored in an Azure Data Lake and is organized by data source.
#' This function allows the user to retrieve a directory listing of all available files
#' relating to a particular data source.
#' @inheritParams ShowADLPath
#' @author Jonathan Tooley Associados Lda
#' @return Returns a data table with a list of available filenames or the message off-line
#' if the user does not have net connectivity.
#' @param Directory Specific directory in an Azure Data Lake
#' @param Info if true return the modification time
#' @export
#' @examples ADLDirectory("Email")
#' @family Data Lake
#' @seealso \code{\link{TAPChunks}}
ListADLDirectory <- function (Directory = "", Info = F) {

  if (!exists("Configuration_env", envir = .GlobalEnv)){
    GetConfiguration()
  }
  if(Directory == ""){

    data.lake.path <- "Source_Data/SW"

  } else {

    data.lake.path <- Directory
  }

  if (TestNet()) {
    Data_Lake <-
      ConnectToADL()

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
    ftime      <- c()

    if (dirFolder$status_code == 200) {
      dirFolder <- httr::content(dirFolder, as = "parsed")
      dirFolder <- dirFolder$FileStatuses$FileStatus
      num <- length(dirFolder)
      for (i in (1:num)) {
        folderName <- c(folderName, dirFolder[[i]]$pathSuffix)
        ftime      <- c(ftime, dirFolder[[i]]$modificationTime)
      }
    }else{
      catError("The directory was not found, please check the directory name.", "not found")
    }

    ftime       <- substr(ftime, 1, 10)
    summary     <- data.table::data.table(file = folderName, ftime)
    data.table::set(summary, i = NULL, j = "TimeADL",   .POSIXct(summary$ftime))
    data.table::set(summary, i = NULL, j = "ftime",     NULL)

    filesName <- grep("\\.", folderName, value = T)

    folderName <- setdiff(folderName, filesName)

    if(Directory != ""){

      if (length(filesName) != 0)
        catSuperUser(paste("There are this files:\n\t\t-",
                           paste(filesName, collapse = "\n\t\t- ")), "files")
      if (length(folderName) != 0)
        catSuperUser(paste("There are this folders:\n\t\t-",
                           paste(folderName, collapse = "\n\t\t- ")),"folders")


      if(Info){

        return(data.table::data.table(summary))

      }else{

        return(data.table::data.table(Directory = summary$file))
      }
    } else {

      oldFolder  <- names(Configuration_env[["Source"]])


      missingFolder <- setdiff(oldFolder, folderName)
      newFolder     <- setdiff(folderName, oldFolder)

      if(length(missingFolder)) catSuperUser(sprintf("There are sources that disappeared from the ADL:\n\t\t-%s", paste(missingFolder, collapse = "\n\t\t-" )))
      if(length(newFolder)) catSuperUser(sprintf("There are sources that must be added to the source.yaml file:\n\t\t-%s", paste(newFolder, collapse = "\n\t\t-" )),"source.yaml")


      Index_source <- menu(choices = folderName, title =
                             "The requested source does not exist, choose one folder of the following or press 0 to cancel the process:")
      if( Index_source == 0){
        catError("No source selected.")
      }
      source <- folderName[Index_source]

      if(Directory == ""){

        data.lake.path  <- sprintf(paste0(data.lake.path, "/%s/rdata"), source)
      }

      else{

        data.lake.path  <- sprintf(paste0(data.lake.path, "/%s"), source)
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


      fname  <- c()
      ftime  <- c()

      if (dir$status_code == 200) {
        dir <- httr::content(dir, as = "parsed")
        dir <- dir$FileStatuses$FileStatus
        num <- length(dir)
        for (i in (1:num)) {
          fname  <- c(fname, dir[[i]]$pathSuffix)
          ftime  <- c(ftime, dir[[i]]$modificationTime)
        }

      }

      ftime       <- substr(ftime, 1, 10)
      summary     <- data.table::data.table(file = fname, ftime)
      data.table::set(summary, i = NULL, j = "TimeADL",   .POSIXct(summary$ftime))
      data.table::set(summary, i = NULL, j = "ftime",     NULL)


    }} else {
      catError("Off-line\n","Off-line")
    }

  if(Directory == ""){
    catSuperUser(paste("There are this files:\n\t\t-",
                  paste(fname, collapse = "\n\t\t- ")))
    if(Info){

      return(data.table::data.table(summary))

    }else{

      return(data.table::data.table(Directory = fname))
    }
  }
}


#' @title List files in the data lake
#' @details This function has been renamed and is deprecated. See \code{\link{ListADLDirectory}}
#' @keywords internal
#' @export
ADLDirectory <- function(Directory = "", Info = F){
  catWarning("This function has been renamed ListADLDirectory and has been deprecated.", "ListADLDirectory")
  ListADLDirectory(Directory, Info)
}

#' @title  Reading user data from the Data Lake
#' @description This function allows users to read in a file that has been previously
#' stored on the Azure Data Lake. Chunks of telemetry data may be more easily read by
#' using \code{\link{ReadADLChunk}} which simplifies the process for pre-prepared chunks.
#' @author Jonathan Tooley Associados Lda
#' @examples TODO
#' @return Prints a status message.
#' @param SpecificPath Specific folder path where the file is located
#' @param FileName Name of the file
#' @param SaveChunk Save chunk in your local machine
#' @param ShowInfo Show extra information
#' @export
ReadADLFile  <- function (SpecificPath, FileName, SaveChunk = T, ShowInfo = T) {

  if (length(grep("csv$", tolower(FileName))) + length(grep("rdata$", tolower(FileName))) == 0)
    catError("Only works with Rdata or CSV files.", "Rdata or CSV")
  Data_Lake <- ConnectToADL()
  operation <- "?op=LISTSTATUS"
  dir <- httr::GET(paste0("https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
                          SpecificPath, operation), httr::add_headers(Authorization = paste("Bearer",
                                                                                            Data_Lake)))
  if (dir$status_code == 200) {
    fname <- c()
    dir <- httr::content(dir, as = "parsed")
    dir <- dir$FileStatuses$FileStatus
    num <- length(dir)
    for (i in (1:num)) {
      fname <- c(fname, dir[[i]]$pathSuffix)
    }
    if (length(fname) >= 5) {
      catSuperUser(FormatList(c(fname, fname, fname, fname),
                              "List Files:", 5))
    }
    else {
      catSuperUser(sprintf("List Files:\n\t\t- %s", paste(fname,
                                                          collapse = "\n\t\t- ")))
    }
  }
  else {
    catError(HttpCode(dir$status_code))
  }
  FileName <- fname[which(tolower(fname) == tolower(FileName))]
  if (length(FileName) == 0) {
    catError("The file name was not found, please check the name.",
             "not found")
  }
  info <- httr::GET(paste0("https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
                           file.path(SpecificPath, FileName), operation), httr::add_headers(Authorization = paste("Bearer",
                                                                                                                  Data_Lake)))
  if (info$status_code != 200) {
    catError(HttpCode(info$status_code))
  }
  info <- httr::content(info, as = "parsed")
  if (ShowInfo == T) {
    versionTime <- info$FileStatuses$FileStatus[[1]]$modificationTime
    versionTime <- as.integer(substr(versionTime, 1, 10))
    versionTime <- format(.POSIXct(versionTime), "%d/%b/%Y ")
    length.file <- info$FileStatuses$FileStatus[[1]]$length
    length.file <- round(length.file/1024/1024, 2)
    catInfo(sprintf("Last Modification: %s", versionTime))
    catInfo(sprintf("Size: %sMB", length.file))
  }
  temp.folder <- ShowCachePath()
  if (sum(list.files(temp.folder) == FileName) > 0) {
    catInfo("The file already exists in cache.", "in cache")
  }
  else {
    operation <- "?op=OPEN&read=true"
    r <- httr::GET(paste0("https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
                          file.path(SpecificPath, FileName), operation), httr::add_headers(Authorization = paste("Bearer",
                                                                                                                 Data_Lake)))
    if (r$status_code != 200) {
      catError(HttpCode(r$status_code))
    }
    else {
      writeBin(httr::content(r), file.path(temp.folder,
                                           FileName))
      catSuccess(sprintf("The ADL fetch (path:%s) reported success.",
                         file.path(SpecificPath, FileName)), "success")
    }
  }
  if (length(grep("csv$", FileName)) == 1) {
    DT <- suppressWarnings(assign(gsub(".csv", "", FileName), data.table::fread(file.path(temp.folder,
                                                                                          FileName))))
    Data <- "DT"
  }
  else {
    Data <- load(file.path(temp.folder, FileName))
  }
  if (SaveChunk != T)
    unlink(file.path(temp.folder, FileName))

  return(get(Data))
}

#' @title Reading files from the Azure Data Lake
#' @export
#' @details This function has been renamed. See \code{\link{ReadADLFile}}
#' @keywords internal
ADLRead  <- function(SpecificPath, FileName, SaveChunk = T, ShowInfo = T){
  catWarning("This function has been renamed ReadADLFile and has been deprecated.", "ReadADLFile")
  ReadADLFile(SpecificPath = SpecificPath, FileName = FileName,
              SaveChunk = SaveChunk, ShowInfo = ShowInfo)
}




#' @title Save file in the Data Lake
#' @description
#' A function to save files in the data lake. By default saves a CSV file but also supports Rdata files.
#' By default saves in a folder named with user alias under user directory in the Data Lake.
#' @author Jonathan Tooley Associados Lda
#' @return Prints a status message.
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
#' @family Data Lake
#' @seealso \code{\link{TAPChunks}}
SaveADL <- function(Data, FileExtension = "csv", ADLDirectory, ChunkName, SavePath, SaveLocal = T){

  FileExtension <- tolower(FileExtension)

  if (!(FileExtension == "csv" |
        FileExtension == "rdata")) {
    catError("Extension Format incorrect.")

  } else{

    if (FileExtension == "rdata") FileExtension <- "Rdata"
  }

  if(missing(ChunkName)) {

    ChunkName <- deparse(substitute(Data))
    FileName  <- paste0(deparse(substitute(Data)),".", FileExtension)

  }else{

    FileName <- paste0(ChunkName, ".", FileExtension)
  }

  if (missing(ADLDirectory)) {

    data.lake.path <- file.path(paste("user", Sys.getenv("USERNAME"), sep = "/"), FileName)

  }else {

    data.lake.path <- file.path(ADLDirectory, FileName)

  }
  if(missing(SavePath)){

    path <- ShowCachePath()

  }else{

    path <- ShowCachePath(SavePath)
  }

  if (TestNet()){

    Data_Lake <- ConnectToADL()

    adl.account.name <- "capdevtapdl"


    if(tolower(FileExtension) == "csv") {

      write.csv(Data, paste0(path, "/", FileName), row.names = FALSE)
      upload.file <- httr::upload_file(paste0(path, "/", FileName))
    }

    if(tolower(FileExtension) == "rdata"){

      assign(ChunkName, Data)
      save(list = ChunkName, file = paste0(path, "/", FileName))
      upload.file <- httr::upload_file(paste0(path, "/", FileName))
    }

    r <- httr::PUT(paste0("https://", adl.account.name, ".azuredatalakestore.net/webhdfs/v1/",
                          data.lake.path, "?op=CREATE&overwrite=true&write=true"),
                   body = upload.file,
                   httr::add_headers(Authorization = paste("Bearer", Data_Lake),
                                     "Transfer-Encoding" = "chunked"))

    result.load <-  switch(toString(r$status_code),
                           "201" = HttpCode(201),
                           "400" = HttpCode(400),
                           "403" = HttpCode(403),
                           "404" = HttpCode(404))

    if(!SaveLocal){

      unlink(paste0(path, "/", FileName))
    }


    return(result.load)
  }
}


#' @title Download Files from Blob Storage
#' @description
#' A function to download files from the Blob Storage in Azure Data Lake.
#' @details The raw tememtry data that we receive from SpiceWorks and other providers
#' is loaded to a Blob (\strong{B}inary \strong{L}arge \strong{Ob}ject) Store.
#' This data is then loaded via a managed process to our Azure data Lake in the \emph{chunk} format.
#' This function is used internally to process raw data from the Blob Store to the Data Lake.
#' It can, however, be used by anyone who wishes to access an original file.
#'
#' The function is dependent of having the Azure Command Line tools installed on the host machine.
#'
#' Be aware that the chunk format is much more efficient in terms of its use of both
#' disk and memory resources and is the preffered file format for telemetry analysis.
#'
#' The requested file is downloaded as a csv to the users cache directory. It
#' is stored in a sub directory with the name of the month.
#'
#' Any file may be downloaded by specifying its name in the \emph{FileSpecific}
#' parameter. If the user wishes to retrieve a telemetry file then there is a simpler
#' syntax where the function only requires the source and month to be specified
#' in the \emph{Source} and \emph{Timestamp} parameters.
#'
#' If the user does not give parameters then the system will operate in an
#' interactive mode, offering a choice of options to the user.
#'
#' @author Jonathan Tooley Associados Lda
#' @return Prints a status message.
#' @param Source source to download
#' @param Timestamp month to download
#' @param FileSpecific Path to the file to be downloaded
#' @param SavePath TODO
#' @param Container Container of Blob Storage. By default is where spiceworks data is laying
#' @family Data Lake
#' @examples \donttest{ReadABSFile("Email", "2015M01")}
#' @export
#' @seealso \code{\link{TAPChunks}}
#' #171
ReadABSFile <- function( Source       = ""                  , Timestamp = ""   ,
                         FileSpecific = ""                  , SavePath  = ""   ,
                         Container    = "spiceworks-tapdata") {


  SavePath    <- ifelse(SavePath == "", ShowCachePath(), SavePath)
  Timestamp   <- gsub("_", "M", Timestamp)
  check.azure <- tryCatch(
    system("azure", intern = T),
    error = function(e) {return("Azure CLI not found")}
  )

  if (length(check.azure) == 1) {
    catError(
      "Azure CLI not found in your system. Please go to wiki and check 'How to Install Azure CLI':\n\t\thttps://github.com/CAPInfrastructure/TAP/wiki/How-to-Install-Azure-CLI",
      "https://github.com/CAPInfrastructure/TAP/wiki/How-to-Install-Azure-CLI"
    )
  } else {

    if (exists("Configuration_env", envir = .GlobalEnv)){
      if(length(names(Configuration_env)) == 1){
        rm(Configuration_env, envir = .GlobalEnv)
      }}
    if (!exists("Configuration_env", envir = .GlobalEnv)){
      GetConfiguration()
    }

    blob.name    <- Configuration_env$Key$ABS$Name
    account.key  <- Configuration_env$Key$ABS$Key


    if (FileSpecific == "") {

      if(Container == "spiceworks-tapdata"){

        source_available <- NULL
        for (i  in names(Configuration_env$Source)){
          source_available <-
            unique(c(source_available, Configuration_env$Source[[i]]$blobsource
            )
            )
        }
        source_available <- c(source_available, "Demographics")

        Source <- tolower(Source)

        if (length(intersect(tolower(Source), tolower(source_available))) == 0) {
          Index_source <- menu(choices = c(source_available),
                               title   = "The requested source does not exist, choose one of the following or press 0 to cancel the process:")

          if (Index_source == 0) catError("The operation was interrupted.", "interrupted")

          Source <- source_available[Index_source]
          Source <- tolower(Source)
        }


        Source <-
          ifelse(Source == "demographics",
                 "Demographics",
                 names(Configuration_env$Source)[grep(paste0("\\b", tolower(Source)), tolower(names(Configuration_env$Source)))[1]])
        blob.storage.map  <-
          ifelse(Source == "Demographics",
                 "demographics",
                 Configuration_env$Source[[Source]]$blobpattern)


        s <- system(
          paste(
            "azure storage blob list -a",
            blob.name,
            "-k"          ,
            account.key,
            "--container" ,
            Container
          ),
          intern = T
        )

        s <- unlist(lapply(s[-(1:4)],
                           function(x) {
                             start <- gregexpr("\\d\\d\\s[A-z][A-z][A-z]\\s\\d\\d\\d\\d", x)[[1]]
                             part1 <- substr(x, 6, gregexpr("BlockBlob", x)[[1]][1] -  1)
                             part2 <- substr(x, start, start + 10)

                             paste(trimws(part1), part2, sep = "/")
                           }
        )
        )

        s <- grep("^\\d\\d\\d\\d_\\d\\d", s, value = T)

        s <- s[sapply(s, function(x) length(gregexpr("/", x)[[1]]) < 3)]

        s <- grep(blob.storage.map, s, value = T)

        timeDT  <- NULL
        nameDT  <- NULL
        timeMod <- NULL
        for (i in s) {
          x <- strsplit(i, "/")[[1]]

          timeDT <- c(timeDT, gsub("_", "M", x[1]))

          nameDT <- c(nameDT, x[2])

          timeMod <- c(timeMod, x[3])

        }

        catSuperUser(TAPChunks:::FormatList(nameDT, "File available:",3))

        s <- data.table::data.table(timeDT, nameDT, timeMod)

        time_available <- timeDT

        if (length(intersect(Timestamp, time_available)) == 0) {
          Index_source <- menu(choices = c(time_available),
                               title   = "Please select a timestamp, choose one of the following or press 0 to cancel the process:")
          if (Index_source == 0) {
            catError("The operation was interrupted.", "interrupted")
          }

          Timestamp <- time_available[Index_source]
        }

        s <- s[timeDT == Timestamp]

        catSuperUser(paste0("Files found:\n\t\t-",(paste(s$nameDT, collapse = "\n\t\t-"))))


        if (nrow(s) > 1) {
          s$timeMod <- as.integer(as.Date(s$timeMod, format = "%d %b %Y"))
          s <- s[timeMod == max(timeMod)]
          catWarning(sprintf(
            "There are several files for the required period, the file used was:\n\t\t-%s",
            s$nameDT
          ),
          s$nameDT
          )

          if (nrow(s) > 1) {
            Index_source <-
              menu(s$nameDT, "Please choose one of the following.")
            if (Index_source == 0) {
              catError("The operation was interrupted.", "interrupted")
            }
            s <- s[Index_source]
          }}

        path.blob     <- file.path(gsub("M", "_", s$timeDT), s$nameDT)
        FileSpecific  <- s$nameDT

      }
      if(Container == "data"){

        s <- system(
          paste(
            "azure storage blob list -a",
            blob.name,
            "-k"          ,
            account.key,
            "--container" ,
            Container
          ),
          intern = T
        )

        s <- unlist(lapply(s[-(1:4)],
                           function(x) {
                             start <- gregexpr("\\d\\d\\s[A-z][A-z][A-z]\\s\\d\\d\\d\\d", x)[[1]]
                             part1 <- substr(x, 6, gregexpr("BlockBlob", x)[[1]][1] -  1)
                             part2 <- substr(x, start, start + 10)

                             paste(trimws(part1), part2, sep = "/")
                           }
        )
        )

        s <- grep("Sources", s, value = T)

        source_available  <- NULL
        for (i in s) {
          x <- strsplit(i, "/")[[1]]
          source_available <- unique(c(source_available, x[2]))
        }


        if (length(intersect(Source, source_available)) == 0) {
          Index_source <- menu(choices = c(source_available),
                               title   = "The requested source does not exist, choose one of the following or press 0 to cancel the process:")

          if (Index_source == 0) catError("The operation was interrupted.", "interrupted")

          Source <- source_available[Index_source]
        }

        s <- grep(Source, s, value = T)

        timeDT  <- NULL
        nameDT  <- NULL
        timeMod <- NULL
        for (i in s) {
          x <- strsplit(i, "/")[[1]]

          timeDT <- c(timeDT, gsub("_", "M", x[3]))

          nameDT <- c(nameDT, x[4])

          timeMod <- c(timeMod, x[5])

        }

        catSuperUser(TAPChunks:::FormatList(nameDT, "File available:",3))


        timeDT <- gsub("[A-Za-z]+$", "", timeDT)

        s <- data.table::data.table(timeDT, nameDT, timeMod)[timeDT != ""]

        time_available <- unique(s$timeDT)

        if (length(intersect(Timestamp, time_available)) == 0) {
          Index_source <- menu(choices = c(time_available),
                               title   = "Please select a timestamp, choose one of the following or press 0 to cancel the process:")
          if (Index_source == 0) {
            catError("The operation was interrupted.", "interrupted")
          }

          Timestamp <- time_available[Index_source]
        }

        s <- s[timeDT == Timestamp]

        catSuperUser(paste0("Files found:\n\t\t-",(paste(s$nameDT, collapse = "\n\t\t-"))))

        if (nrow(s) > 1) {
          catWarning("There are several files for the required period, choose one the following:")

          Index_source <-
            menu(s$nameDT, "Please choose one of the following.")
          if (Index_source == 0) {
            catError("The operation was interrupted.", "interrupted")
          }
          s <- s[Index_source]
        }


        path.blob     <- paste0("Sources/", Source, "/", file.path(gsub("M", "_", s$timeDT), s$nameDT))
        FileSpecific  <- s$nameDT


      }}else{

        path.blob  <- FileSpecific
        test       <- gregexpr("/",path.blob)[[1]][1]

        if(test != - 1) FileSpecific <-  gsub(".*/", "", substr(path.blob, max(test) + 1, nchar(path.blob)))

      }

    system(
      paste(
        "azure storage blob download -a",     blob.name,
        " -k"         ,          account.key,
        "--container" ,          Container,
        "--blob"      ,          path.blob,
        "-d"          ,          SavePath
      )
    )

    if (length(grep(".zip$", tolower(path.blob))) == 1) {
      unzip(file.path(SavePath, path.blob), exdir = SavePath)
      FileSpecific <- gsub(".zip", ".csv", FileSpecific)
    } else{
      file.copy(
        from = file.path(SavePath, path.blob),
        to = SavePath,
        overwrite = T
      )
    }


    if(length(grep("/",path.blob )) != 0){
      unlink(file.path(SavePath, substr(path.blob, 1, gregexpr("/",path.blob)[[1]][1]-1)), recursive = T)
    }else{
      unlink(file.path(SavePath, path.blob), recursive = T)
    }
    return(data.table::fread(file.path(SavePath, FileSpecific)))

  }
}


#' @title Reading files from BLOB storage
#' @details DEPRECATED: See \code{\link{ReadABSFile}}
#' @export
#' @keywords internal
ABSRead <- function(Source = "", Timestamp = "", SpecificSource = F, FileSpecific = "") {
  warning("This function has been renamed ReadABSFile and has been deprecated.")
  ReadABSFile(Source, Timestamp, SpecificSource, FileSpecific)
}


#' @title Save file in the Data Lake
#' @description
#' A function to save files in the data lake.
#' @author Jonathan Tooley Associados Lda
#' @return Prints a status message.
#' @examples TODO
#' @param FileName Name file
#' @param FileExtension Extension file
#' @param ADLDirectory Directory where the file will be stored.
#' @param FileDirectory TODO
#' @export
#' @family Data Lake
#' @seealso \code{\link{TAPChunks}}
UploadLocallyToADL <-  function(FileName, FileExtension = "", ADLDirectory, FileDirectory = "") {

  FileExtension <- tolower(FileExtension)

  if (FileExtension == "rdata")
    FileExtension <- "Rdata"


  FileName <- paste0(FileName, ".", FileExtension)


  if (missing(ADLDirectory)) {
    data.lake.path <-
      file.path(paste("user", Sys.getenv("USERNAME"), sep = "/"), FileName)

  } else {
    data.lake.path <- file.path(ADLDirectory, FileName)

  }


  if (TestNet()) {
    Data_Lake <- ConnectToADL()

    adl.account.name <- "capdevtapdl"

    upload.file      <-
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

    result.load <-  switch(
      toString(r$status_code),
      "201" = HttpCode(201),
      "400" = HttpCode(400),
      "403" = HttpCode(403),
      "404" = HttpCode(404)
    )

    return(result.load)
  }
}

#' @title Publish File into the Data Lake
#' @description
#' A function to save files in the Publication folder in the data lake. This function is similar to SaveADL() function, the difference is the chunk is saved in the Publish area.
#' By default saves a CSV file but also supports Rdata files.
#' @author Jonathan Tooley Associados Lda
#' @examples PublishADL(TestEmailChunk, Project="Emailtracker", ChunkName = "FactFile", FileExtension="csv", Replace = T)
#' @inheritParams SaveADL
#' @param Project Name of the project. If the project doesn't exist a new folder will be created in the ADL.
#' @param Replace Replace in case exists a file with same name. By default is FALSE.
#' @export
#' @family Data Lake
#' @seealso \code{\link{TAPChunks}}
PublishADL <- function(Data,
                       Project,
                       ChunkName,
                       FileExtension = "csv",
                       Replace = F) {

  if (!tolower(FileExtension) %in% c("rdata", "csv"))
    catError("The operation was interrupted. Parameter 'FileExtension' should be 'Rdata' or 'csv'.")

  if (missing(Data))
    catError("The operation was interrupted. Missing parameter 'Data'.")

  if (!is.data.frame(Data))
    catWarning("The parameter Data isn't a data.table.")

  if (missing(ChunkName))
    ChunkName <- substitute(Data)

  operation <- "?op=LISTSTATUS"

  listProject <-
    httr::GET(
      paste0(
        "https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
        "Publication",
        operation
      ),
      httr::add_headers(Authorization = paste("Bearer",
                                              ConnectToADL()))
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
    file.path("Publication", tolower(Project))

  listFiles <-
    httr::GET(
      paste0(
        "https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
        Path,
        operation
      ),
      httr::add_headers(Authorization = paste("Bearer",
                                              ConnectToADL()))
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
    } else{

      catError(sprintf("The %s file already exists in ALD. If you want to replace, set the Replace parameter as TRUE",
                       paste0(ChunkName, paste0(".", FileExtension))),
               paste0(ChunkName, paste0(".", FileExtension)))
    }
  } else{

    SaveADL(
      Data,
      FileExtension = FileExtension,
      ADLDirectory = Path,
      ChunkName = ChunkName,
      SaveLocal = F
    )}
}
