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
#' @author JTA - The Data Scientists
#' @return Prints a status message.
#' @param Source source to download
#' @param Timestamp month to download
#' @param FileSpecific Path to the file to be downloaded
#' @param SavePath TODO
#' @param Container Container of Blob Storage. By default is where spiceworks data is laying
#' @family Data Lake Tools
#' @examples \donttest{ReadABSFile("Email", "2015M01")}
#' @export
#' @importFrom utils unzip
#' @seealso \code{\link{TAPChunks}}
#' #171
ReadABSFile <- function(Source = "", Timestamp = "",
                        FileSpecific = "", SavePath = "",
                        Container = "spiceworks-tapdata") {
  SavePath <- ifelse(SavePath == "", ShowCachePath(), SavePath)
  Timestamp <- gsub("_", "M", Timestamp)
  check.azure <- tryCatch(
    system("azure", intern = T),
    error = function(e) {
      return("Azure CLI not found")
    }
  )

  if (length(check.azure) == 1) {
    TAPChunks:::catError(
      "Azure CLI not found in your system. Please go to wiki and check 'How to Install Azure CLI':\n\t\thttps://github.com/CAPInfrastructure/TAP/wiki/How-to-Install-Azure-CLI",
      "https://github.com/CAPInfrastructure/TAP/wiki/How-to-Install-Azure-CLI"
    )
  } else {
    if (exists("Configuration_env", envir = .GlobalEnv)) {
      if (length(names(Configuration_env)) == 1) {
        rm(Configuration_env, envir = .GlobalEnv)
      }
    }
    if (!exists("Configuration_env", envir = .GlobalEnv)) {
      TAPChunks:::GetConfiguration()
    }

    blob.name <- Configuration_env$Key$ABS$Name
    account.key <- Configuration_env$Key$ABS$Key


    if (FileSpecific == "") {
      if (Container == "spiceworks-tapdata") {
        source_available <- NULL
        for (i in names(Configuration_env$Source)) {
          source_available <-
            unique(c(source_available, Configuration_env$Source[[i]]$blobsource))
        }
        source_available <- c(source_available, "Demographics")

        Source <- tolower(Source)

        if (length(intersect(tolower(Source), tolower(source_available))) == 0) {
          Index_source <- menu(
            choices = c(source_available),
            title = "The requested source does not exist, choose one of the following or press 0 to cancel the process:"
          )

          if (Index_source == 0) TAPChunks:::catError("The operation was interrupted.", "interrupted")

          Source <- source_available[Index_source]
          Source <- tolower(Source)
        }


        Source <-
          ifelse(Source == "demographics",
            "Demographics",
            names(Configuration_env$Source)[grep(paste0("\\b", tolower(Source)), tolower(names(Configuration_env$Source)))[1]]
          )
        blob.storage.map <-
          ifelse(Source == "Demographics",
            "demographics",
            Configuration_env$Source[[Source]]$blobpattern
          )


        s <- system(
          paste(
            "azure storage blob list -a",
            blob.name,
            "-k",
            account.key,
            "--container",
            Container
          ),
          intern = T
        )

        s <- unlist(lapply(
          s[-(1:4)],
          function(x) {
            start <- gregexpr("\\d\\d\\s[A-z][A-z][A-z]\\s\\d\\d\\d\\d", x)[[1]]
            part1 <- substr(x, 6, gregexpr("BlockBlob", x)[[1]][1] - 1)
            part2 <- substr(x, start, start + 10)

            paste(trimws(part1), part2, sep = "/")
          }
        ))

        s <- grep("^\\d\\d\\d\\d_\\d\\d", s, value = T)

        s <- s[sapply(s, function(x) length(gregexpr("/", x)[[1]]) < 3)]

        s <- grep(blob.storage.map, s, value = T)

        timeDT <- NULL
        nameDT <- NULL
        timeMod <- NULL
        for (i in s) {
          x <- strsplit(i, "/")[[1]]

          timeDT <- c(timeDT, gsub("_", "M", x[1]))

          nameDT <- c(nameDT, x[2])

          timeMod <- c(timeMod, x[3])
        }

        catSuperUser(TAPChunks:::FormatList(nameDT, "File available:", 3))

        s <- data.table::data.table(timeDT, nameDT, timeMod)

        time_available <- timeDT

        if (length(intersect(Timestamp, time_available)) == 0) {
          Index_source <- menu(
            choices = c(time_available),
            title = "Please select a timestamp, choose one of the following or press 0 to cancel the process:"
          )
          if (Index_source == 0) {
            TAPChunks:::catError("The operation was interrupted.", "interrupted")
          }

          Timestamp <- time_available[Index_source]
        }

        s <- s[timeDT == Timestamp]

        catSuperUser(paste0("Files found:\n\t\t-", (paste(s$nameDT, collapse = "\n\t\t-"))))


        if (nrow(s) > 1) {
          s$timeMod <- as.integer(as.Date(s$timeMod, format = "%d %b %Y"))
          s <- s[timeMod == max(timeMod)]
          TAPChunks:::catWarning(
            sprintf(
              "There are several files for the required period, the file used was:\n\t\t-%s",
              s$nameDT
            ),
            s$nameDT
          )

          if (nrow(s) > 1) {
            Index_source <-
              menu(s$nameDT, "Please choose one of the following.")
            if (Index_source == 0) {
              TAPChunks:::catError("The operation was interrupted.", "interrupted")
            }
            s <- s[Index_source]
          }
        }

        path.blob <- file.path(gsub("M", "_", s$timeDT), s$nameDT)
        FileSpecific <- s$nameDT
      }
      if (Container == "data") {
        s <- system(
          paste(
            "azure storage blob list -a",
            blob.name,
            "-k",
            account.key,
            "--container",
            Container
          ),
          intern = T
        )

        s <- unlist(lapply(
          s[-(1:4)],
          function(x) {
            start <- gregexpr("\\d\\d\\s[A-z][A-z][A-z]\\s\\d\\d\\d\\d", x)[[1]]
            part1 <- substr(x, 6, gregexpr("BlockBlob", x)[[1]][1] - 1)
            part2 <- substr(x, start, start + 10)

            paste(trimws(part1), part2, sep = "/")
          }
        ))

        s <- grep("Sources", s, value = T)

        source_available <- NULL
        for (i in s) {
          x <- strsplit(i, "/")[[1]]
          source_available <- unique(c(source_available, x[2]))
        }


        if (length(intersect(Source, source_available)) == 0) {
          Index_source <- menu(
            choices = c(source_available),
            title = "The requested source does not exist, choose one of the following or press 0 to cancel the process:"
          )

          if (Index_source == 0) TAPChunks:::catError("The operation was interrupted.", "interrupted")

          Source <- source_available[Index_source]
        }

        s <- grep(Source, s, value = T)

        timeDT <- NULL
        nameDT <- NULL
        timeMod <- NULL
        for (i in s) {
          x <- strsplit(i, "/")[[1]]

          timeDT <- c(timeDT, gsub("_", "M", x[3]))

          nameDT <- c(nameDT, x[4])

          timeMod <- c(timeMod, x[5])
        }

        catSuperUser(TAPChunks:::FormatList(nameDT, "File available:", 3))


        timeDT <- gsub("[A-Za-z]+$", "", timeDT)

        s <- data.table::data.table(timeDT, nameDT, timeMod)[timeDT != ""]

        time_available <- unique(s$timeDT)

        if (length(intersect(Timestamp, time_available)) == 0) {
          Index_source <- menu(
            choices = c(time_available),
            title = "Please select a timestamp, choose one of the following or press 0 to cancel the process:"
          )
          if (Index_source == 0) {
            TAPChunks:::catError("The operation was interrupted.", "interrupted")
          }

          Timestamp <- time_available[Index_source]
        }

        s <- s[timeDT == Timestamp]

        catSuperUser(paste0("Files found:\n\t\t-", (paste(s$nameDT, collapse = "\n\t\t-"))))

        if (nrow(s) > 1) {
          TAPChunks:::catWarning("There are several files for the required period, choose one the following:")

          Index_source <-
            menu(s$nameDT, "Please choose one of the following.")
          if (Index_source == 0) {
            TAPChunks:::catError("The operation was interrupted.", "interrupted")
          }
          s <- s[Index_source]
        }


        path.blob <- paste0("Sources/", Source, "/", file.path(gsub("M", "_", s$timeDT), s$nameDT))
        FileSpecific <- s$nameDT
      }
    } else {
      path.blob <- FileSpecific
      test <- gregexpr("/", path.blob)[[1]][1]

      if (test != -1) FileSpecific <- gsub(".*/", "", substr(path.blob, max(test) + 1, nchar(path.blob)))
    }

    system(
      paste(
        "azure storage blob download -a", blob.name,
        " -k", account.key,
        "--container", Container,
        "--blob", path.blob,
        "-d", SavePath
      )
    )

    if (length(grep(".zip$", tolower(path.blob))) == 1) {
      unzip(file.path(SavePath, path.blob), exdir = SavePath)
      FileSpecific <- gsub(".zip", ".csv", FileSpecific)
    } else {
      file.copy(
        from = file.path(SavePath, path.blob),
        to = SavePath,
        overwrite = T
      )
    }


    if (length(grep("/", path.blob)) != 0) {
      unlink(file.path(SavePath, substr(path.blob, 1, gregexpr("/", path.blob)[[1]][1] - 1)), recursive = T)
    } else {
      unlink(file.path(SavePath, path.blob), recursive = T)
    }
    return(data.table::fread(file.path(SavePath, FileSpecific)))
  }
}
