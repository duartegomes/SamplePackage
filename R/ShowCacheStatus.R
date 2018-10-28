#' @title  Detecting when data on the Data Lake has been updated
#' @description Compare the local files in the cache with the files on the
#' Data Lake and warn the user if any have been superseded.
#' @details
#' There are often times when our data provider reissues files after discovering
#' errors in the feed.  This then requires us to re process the loading of that data.
#' The issue is that, for the convenience of our users, we allow the local system to
#' keep a cached copy of the original chunk.  This command allows the user to detect
#' outdated files. \cr \cr
#' The data lake is separated into distinct areas for each data source.  Because of this
#' the user must specify the source for which they wish to get a status update.
#' @inheritParams ShowADLPath
#' @examples ShowCacheStatus("Email")
#' @family Cache Controls
#' @author JTA - The Data Scientists
#' @return Returns an error status to show if the command completed succesfully or not
#' @export
#' @seealso \code{\link{TAPChunks}}
ShowCacheStatus <- function(Source = ""){
  data.lake.path <- ShowADLPath(Source)
  Source <- attributes(data.lake.path)$source
  prefix         <- ShowADLPrefix(attributes(data.lake.path)$source)

  if (is.null(data.lake.path) | is.null(prefix)) catError("Unknown Source.")

  operation <- "?op=LISTSTATUS"

  if (TestNet()) {

    Data_Lake <- ConnectToADL()

    dir.rdata <- httr::GET(paste0("https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
                                  data.lake.path, operation), httr::add_headers(Authorization = paste("Bearer",
                                                                                                      Data_Lake)))
    dir.csv   <- httr::GET(paste0("https://capdevtapdl.azuredatalakestore.net/webhdfs/v1/",
                                  gsub("rdata$","csv",data.lake.path), operation), httr::add_headers(Authorization = paste("Bearer",
                                                                                                                           Data_Lake)))

    fname <- c()
    ftime <- c()

    if (dir.rdata$status_code == 200 & dir.csv$status_code == 200) {
      dir <- c(httr::content(dir.rdata, as = "parsed")$FileStatuses$FileStatus,
               httr::content(dir.csv, as = "parsed")$FileStatuses$FileStatus)
      num <- length(dir)
      for (i in (1:num)){
        fname <- c(fname, dir[[i]]$pathSuffix)
        ftime <- c(ftime, dir[[i]]$modificationTime )
      }
    }
  }else{

    catError("Off-line","Off-line")
  }

  ftime       <- substr(ftime, 1, 10)
  temp.folder <- ShowCachePath()
  cache       <- list.files(temp.folder)
  cache       <- grep(prefix, cache, value = TRUE)

  if (length(cache) != 0){
    cachetime   <- NULL

    for (i in cache){
      cachetime <- c(cachetime,
                     as.integer(
                       file.info(
                         paste0(temp.folder, "/", i))$mtime))
    }

    summary     <- merge(data.table::data.table(file = fname, ftime),
                         data.table::data.table(file = cache, cachetime),
                         all.y=T,
                         by="file")

    data.table::set(summary, i = NULL, j = "TimeADL",   .POSIXct(summary$ftime))
    data.table::set(summary, i = NULL, j = "TimeCache", .POSIXct(summary$cachetime))
    data.table::set(summary, i = NULL, j = "Status",    ifelse(summary$ftime < summary$cachetime,
                                                               "Last Version",
                                                               "Need Update"))
    data.table::set(summary, i = NULL, j = "ftime",     NULL)
    data.table::set(summary, i = NULL, j = "cachetime", NULL)

    if( "Need Update" %in% unique(summary$Status)){
      catWarning(sprintf("%s files aren´t update.", Source ),Source)
    }else{
      catInfo(sprintf("%s files are update.", Source ),Source)
    }
    return(summary)
  }
  catWarning ("No files in the cache for this source.")
}
