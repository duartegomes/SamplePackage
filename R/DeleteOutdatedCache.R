#' @title Delete outdated Rdata files in cache folder
#' @description Delete Rdata files with more 30 days in cache folder
#' @author JTA - The Data Scientists
#' @export
#' @examples DeleteOutdatedCache()
#' @family Cache Controls
#' @seealso \code{\link{TAPChunks}}

DeleteOutdatedCache <- function() {
  path <- ShowCachePath()
  files.name <- ListCacheDirectory(ShowAttributes = T)

  oldfiles <- files.name[ `File Created` <= Sys.Date() - as.difftime(tim = 30, units = "days") & grepl(".Rdata", `File Name`)]

  for (i in oldfiles[, `File Name`]) {
    unlink(file.path(path, i), recursive = T)
  }
}
