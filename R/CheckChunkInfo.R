#' @title Check Chunk Files information
#' @description TODO
#' @param Source TODO
#' @param From The timestamp of the first month of data.  If this is not provided this defaults
#' to 2016M01 but in general the user should select a timestamp.
#' @param To The timestamp of the last month of data needed when the user needs to get a range
#' of timestamps.  If this is left blank then the system returns just the single timestamp provided
#' in the to parameter.
#' @family Internal Utilities
#' @import utils
#' @keywords internal
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
CheckChunkInfo <- function(Source = "ALL", From = "", To = "") {
  if (Source == "ALL") {
    TAPChunks:::GetConfiguration()
    source_available <- names(Configuration_env$Source)
  } else {
    source_available <- TAPChunks:::ShowADLPath(Source)
    source_available <- attributes(source_available)$source
  }

  DT.csv <- NULL
  DT.rdata <- NULL

  for (source.name in source_available) {
    adl.account.name <- TAPChunks:::ConnectToADL()

    for (folder.name in c("csv", "rdata")) {
      dir <- httr::GET(
        paste0(
          "https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
          sprintf("Source_Data/SW/%s/%s", source.name, folder.name), "?op=LISTSTATUS"
        ),
        httr::add_headers(Authorization = paste("Bearer", adl.account.name))
      )


      fname <- c()
      fsize <- c()
      formatsize <- c()

      if (dir$status_code == 200) {
        dir <- httr::content(dir, as = "parsed")
        dir <- dir$FileStatuses$FileStatus
        num <- length(dir)
        for (i in (1:num)) {
          fname <- c(fname, dir[[i]]$pathSuffix)
          fsize <- c(fsize, dir[[i]]$length)
          formatsize <- c(formatsize, utils:::format.object_size(as.numeric(dir[[i]]$length), "auto"))
        }
      }


      if (folder.name == "csv") {
        DT <- data.table::data.table(
          key.file = gsub(".csv", "", fname), file_name_csv = fname,
          csv_size_bytes = fsize, csv_size = formatsize, source = source.name
        )
        DT.csv <- JoinChunks(DT.csv, DT)
      }

      if (folder.name == "rdata") {
        DT <- data.table::data.table(
          key.file = gsub(".Rdata", "", fname), file_name_rdata = fname,
          rdata_size_bytes = fsize, rdata_size = formatsize, source = source.name
        )
        DT.rdata <- JoinChunks(DT.rdata, DT)
      }
    }
  }

  infoChunk <- merge(DT.csv, DT.rdata, all = T, by = c("key.file", "source"))
  infoChunk[, time := substr(key.file, nchar(key.file) - 6, nchar(key.file))]


  request <- TimeStamps(From = From, To = To)

  if (length(request) != 0) {
    infoChunk <- infoChunk[time %in% request]
  }

  DT.rsession <- NULL

  for (i in infoChunk$file_name_rdata) {
    # TODo is this message useful?
    TAPChunks:::catInfo(i)
    SpecificPath <- TAPChunks:::ShowADLPath(infoChunk[file_name_rdata == i, source])
    FileName <- i

    download.error <- try(assign(substr(i, 1, gregexpr("\\.", i)[[1]][1] - 1), ReadADLFile(SpecificPath, FileName, SaveChunk = F, ShowInfo = T)) == 0)

    read.error <- try(data.table::is.data.table(get(gsub(".Rdata", "", i))))

    if (download.error[1] != T) {
      DT <- "LoadError"
    }
    if (read.error[1] != T & download.error[1] == T) {
      DT <- "ReadError"
    }

    if (read.error[1] == T & download.error[1] == T) DT <- object.size(get(gsub(".Rdata", "", i)))

    DT.rsession <- c(DT.rsession, DT)
    remove(list = gsub(".Rdata", "", i), envir = .GlobalEnv)
  }

  infoChunk$rsession_size_bytes <- DT.rsession


  infoChunk[, rsession_size := "undefined"]
  infoChunk[rsession_size_bytes %in% c("LoadError", "ReadError"), rsession_size :=
    rsession_size_bytes]
  infoChunk[rsession_size == "undefined", rsession_size := utils:::format.object_size(as.numeric(rsession_size_bytes), "auto"),
    by =
      "key.file"
  ]

  infoChunk <- infoChunk[, .(time, source,
    chunk = key.file,
    file_name_csv, csv_size_bytes, csv_size,
    file_name_rdata, rdata_size_bytes, rdata_size,
    rsession_size_bytes, rsession_size
  )]

  return(infoChunk)
}
