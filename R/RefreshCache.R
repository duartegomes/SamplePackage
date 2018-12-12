#' @title  Detecting when data on the Data Lake has been updated
#' @description Delete and fetch superseded files
#' @details There are often times when our data provider reissues files after discovering
#' errors in the feed.  This then requires us to re process the loading of that data.
#' The issue is that, for the convenience of our users, we allow the local system to
#' keep a cached copy of the original chunk.  This command allows the user to detect
#' outdated files and to refresh the cache.
#' @examples RefreshCache()
#' @family Cache Controls
#' @author JTA - The Data Scientists
#' @return Returns an error status to show if the command completed succesfully or not
#' @export
#' @seealso \code{\link{TAPChunks}}
RefreshCache <- function() {
  source_available <- names(Configuration_env[["Source"]])

  files.cache <- list.files(ShowCachePath())

  for (i in source_available) {
    if (sum(grepl(TAPChunks:::ShowADLPath(i), files.cache)) != 0) {
      info.cache <- ShowCacheStatus(i)
      info.cache <- info.cache[Status == "Need Update"]

      if (nrow(info.cache) != 0) {
        if (nrow(info.cache) == 1) {
          TAPChunks:::catWarning(sprintf("%s: %s file is outdated", i, nrow(info.cache)))
        }
        if (nrow(info.cache) != 1) {
          TAPChunks:::catWarning(sprintf("%s: %s files are outdated", i, nrow(info.cache)))
        }

        for (n in info.cache[, file]) {
          unlink(file.path(ShowCachePath(), n), recursive = T)
          timestamp <- gsub(paste0(ADLPrefix(i), "|.Rdata$", "|.csv$"), "", n)
          if (grepl(".Rdata$", n)) ReadADLChunk(i, timestamp)
          if (grepl(".csv$", n)) ReadADLChunk(i, timestamp, FileExtension = "csv")
          if (grepl(".csv$|.Rdata$", n) == F) TAPChunks:::catWarning("wrong format.")

          TAPChunks:::catSuccess(sprintf("%s file was well updated update", n, nrow(info.cache)))
        }
      }
    }
  }
}
