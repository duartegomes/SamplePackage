#' @title  Reading Source Chunks from the Data Lake
#' @description
#' This function can be used to pull data chunks from the Azure Data Lake where
#' the TAP data is stored.  It uses a two stage process where the data is downloaded
#' to a local cache and then read to the working environment.  This means that after
#' the first call the function will be much faster as it can use the cached data.
#' It also means that users can analyse data without access to any other resource
#' such as when travelling.
#' @details
#' The function requires the user to select a Spiceworks source and either an individual or
#' a range of timestamps for which data is required.
#'
#' The system then checks for net connectivity.  If we have net connectivity
#' the function requests a directory listing from the Data Lake and compares it to a
#' directory listing of the local cache.
#'
#' The system then considers the set of requested data and then generates these sets:
#'  1. Requested data that can be loaded directly from the Cache
#'  2. Requested data that is not in the cache but can be copied down from the Data Lake
#'  3. Requested data that is not available
#'
#'  After reporting the results the system pulls the files identified in set 2 from the Data
#'  Lake to the cache and then loads all the available data from the cache.
#' @inheritParams ShowADLPath
#' @param From The timestamp of the first month of data.  If this is not provided this defaults
#' to 2016M01 but in general the user should select a timestamp.
#' @param To The timestamp of the last month of data needed when the user needs to get a range
#' of timestamps.  If this is left blank then the system returns just the single timestamp provided
#' in the to parameter.
#' @param FileExtension "csv" or "rdata"
#' @param Directory If set to TRUE the function returns a data table with a directory
#' of contents of both the Data Lake and the Cache.  This defaults to FALSE.
#' @param Iterate This can be ommitted or used to control what happens during a script run in the IterateScript
#' function.  If this is called with Iterate = F then when the script is run as part of an iteration this command
#' will be kept as it is.  When this is called with Iterate = T then when the script is run under iteration the command
#' will be modified to read just one month of data which will be updated by the iteration.
#' @examples ReadADLChunk(Source = "Email", From = "2015M01")
#' ReadADLChunk(Source = "Email", Directory = TRUE)
#' @family Data Lake Tools
#' @author JTA - The Data Scientists
#' @return Returns a data table:
#' If the parameter directory = TRUE the table has a directory
#' of the Data Lake and the local cache.
#' If directory = FALSE (or was not specified) the table has the requested data.
#' If the request spanned more than one timestamp all the available
#' timestamps are returned in one concatenated file.
#'
#' As a by-product the function leaves copies of the requested
#' chunks in the local Cache
#' @export
#' @seealso \code{\link{TAPChunks}}
ReadADLChunk <- function(Source = "", From = "", To = "", FileExtension = "rdata", Directory = F, Iterate = F) {
  Iterate <- Iterate
  Data_Lake <- TAPChunks:::ConnectToADL()
  FileExtension <- tolower(FileExtension)
  if (FileExtension != "rdata" & FileExtension != "csv") {
    TAPChunks:::catError("Only works with Rdata or CSV files.")
  }
  request <- TimeStamps(From = From, To = To)
  if (length(grep("rdata$", FileExtension)) == 1) {
    data.lake.path <- TAPChunks:::ShowADLPath(Source)
  }
  else {
    data.lake.path <- TAPChunks:::ShowADLPath(Source)
    data.lake.path <- gsub("rdata", "csv", data.lake.path)
  }
  Source <- attributes(data.lake.path)$source
  if (is.null(data.lake.path)) {
    TAPChunks:::catError("Unknown Source.")
  }
  operation <- "?op=LISTSTATUS"
  if (!TAPChunks:::TestNet()) {
    TAPChunks:::catError("Off-line")
  }
  dir <- httr::GET(paste0(
    "https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
    data.lake.path, operation
  ), httr::add_headers(Authorization = paste(
    "Bearer",
    Data_Lake
  )))
  fname <- c()
  if (dir$status_code == 200) {
    dir <- httr::content(dir, as = "parsed")
    dir <- dir$FileStatuses$FileStatus
    num <- length(dir)
    for (i in (1:num)) {
      fname <- c(fname, dir[[i]]$pathSuffix)
    }
  }
  temp.folder <- ShowCachePath()
  cache <- list.files(temp.folder)
  prefix <- TAPChunks:::ShowADLPrefix(Source)
  cache <- grep(prefix, cache, value = TRUE)
  if (Directory == FALSE) {
    if (length(request) == 0) {
      timeAvailable <- gsub(
        paste(prefix, ".Rdata", sep = "|"),
        "", fname
      )
      formatColumns <- unique(c(seq(
        from = 0, to = length(fname),
        5
      ), length(fname)))
      timeAvailable[formatColumns] <- paste0(
        timeAvailable[formatColumns],
        "\n"
      )

      numberTime <- as.character(1:length(fname))

      if (length(fname) != 1) {
        if (length(fname) <= 9) {
          numberTime[2:length(fname)] <- paste0(
            " ",
            numberTime[2:length(fname)]
          )
        }
        else {
          numberTime[2:9] <- paste0(" ", numberTime[2:9])
        }
      }


      if (From == "" || length(request)==0) {
        TAPChunks:::catError(paste("The requested month is not available. The available months are:\n\t", paste("", timeAvailable, sep = "- ", collapse = "\t")))
      }
    }
    request <- sapply(request, function(x) grep(
        x, fname,
        value = T
      ))
    typefile <- substr(
      request[1], gregexpr("\\.", request[1])[[1]][1],
      nchar(request[1])
    )
    r <- length(request)
    rdesc <- ifelse(r == 1, "chunk", "chunks")
  }
  else {
    summary <- merge(data.table::data.table(
      ind = fname,
      ADL = fname
    ), data.table::data.table(
      ind = request,
      Req = request
    ), by = "ind", all = T)
    summary <- merge(summary, data.table::data.table(
      ind = cache,
      cache = cache
    ), by = "ind", all = T)
    summary <- summary[
      , setdiff(names(summary), "ind"),
      with = F
    ]
    return(summary[, .(`In Data Lake` = ADL, `In Local Cache` = cache)])
  }
  TAPChunks:::catInfo(paste("You have requested ", r, rdesc))
  a <- length(intersect(request, fname))
  if (a == 0 & r == 1) {
    TAPChunks:::catInfo("That chunk is not in the data lake.")
  } else if (a == 0 & r > 1) {
    TAPChunks:::catInfo("None of the requested chunks are in the data lake.")
  } else if (a == 1 & r == 1) {
    TAPChunks:::catInfo("The chunk is available in the data lake.")
  } else if (a == 1 & r > 1) {
    TAPChunks:::catInfo(paste("Of the", r, "chunks requested,", a, "is available in the data lake."))
  } else if (a == r & r > 1) {
    TAPChunks:::catInfo("All the chunks are available in the data lake.")
  } else if (a > 1 & r > 1) {
    TAPChunks:::catInfo(paste("Of the", r, "chunks requested,", a, "are available in the data lake."))
  }
  c <- length(intersect(request, cache))
  if (c == 0) {
    TAPChunks:::catInfo("None of your requested chunks are in your cache.")
  } else if (a == 1 & c == 1) {
    TAPChunks:::catInfo("Your chunk has been located in the cache.")
  } else if (a > 1 & c != 0) {
    TAPChunks:::catInfo(paste(
      "Of the", a, "chunks to be loaded,", c,
      "will be loaded from cache."
    ))
  }
  TAPChunks:::CheckRequirements(request)
  result <- NULL
  tempDir <- tempdir()
  result <- list()

  checkRequest <- 1
  for (i in request) {
    gc()
    TAPChunks:::catInfo(sprintf("Loading chunk %s", i))
    assign(gsub(typefile, "", i), ReadADLFile(
      SpecificPath = data.lake.path, FileName = i,
      SaveChunk = T, ShowInfo = F
    ))
    gc()

    # alternative to rbind: iterates throw columns and remove them as it goes along the chunk, to remove any aditional copy

    # save list of factors to perform different action
    factorList <- names(get(gsub(typefile, "", i))[, grep("factor", lapply(get(gsub(typefile, "", i)), class), value = T)])

    # get names of columns
    cols <- data.table::copy(names(get(gsub(typefile, "", i))))

    # loop for each column
    for (j in 1:length(cols)) {
      col <- cols[j]

      if (col %in% factorList) {

        # in the first chunk, it will only save the column, since there is nothing to bind
        if (checkRequest == 1) {
          result[[col]] <- get(gsub(typefile, "", i))[[col]]
        } else {

          # save new levels of factors
          data.table::setattr(result[[col]], "levels", unique(c(levels(result[[col]]), levels(get(gsub(typefile, "", i))[[col]]))))

          # append to end of file
          result[[col]][(length(result[[col]]) + 1):(length(result[[col]]) + length(get(gsub(typefile, "", i))[[col]]))] <- get(gsub(typefile, "", i))[[col]]
        }
      } else {

        # if its not factor, can append normally
        result[[col]] <- append(result[[col]], get(gsub(typefile, "", i))[[col]])
      }


      # remove column from RAM
      get(gsub(typefile, "", i))[, (col) := NULL]
      gc()
    }

    # add another source
    checkRequest <- checkRequest + 1
    remove(list = gsub(typefile, "", i))
    gc()
    if (attributes(data.lake.path)$source == "Cosmos") {
      TAPChunks:::CheckDemogDB()
      if (!"cosmos_demog_helper" %in% names(TAP_env)) {
        assign("cosmos_demog_helper", ReadADLFile("Source_Data/Cosmos", "Cosmos_demog.Rdata"))
        TAP_env$demog_helper <- rbind(
          TAP_env$demog_helper,
          Cosmos_demog,
          fill = T
        )
        TAP_env$cosmos_demog_helper <- T
        remove(Cosmos_demog, envir = .GlobalEnv)
      }
    }
  }

  data.table::setDT(result, F)
  data.table::setattr(result, "class", append("telemetry", attr(result, "class")))
  return(result)
}
