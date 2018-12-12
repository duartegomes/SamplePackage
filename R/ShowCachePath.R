#' @title Defining where the cache folder is stored
#' @description
#' This function allocates a path where the cache will be stored, if Directory parameter
#' is NULL then it will return the path where the cache has been established (usually in the user's
#' home directory).
#' @details
#' When users run TAPChunks on their local machines it is normal for the system to establish the cache
#' in the user's home directory.  An example would be C:/Users/USERNAME/Documents/TAPCache.  If you call
#' this function without a parameter the system will return the path name of the cache directory.  When
#' you are using TAPChunks on a shared server you might need to change the location of your cache to
#' place it on a disk with more space. In this case you can establish a new location by passing it to the
#' function
#' @param Directory Optional Path to set the Cache location
#' @author JTA - The Data Scientists
#' @export
#' @examples ShowCachePath()
#' \dontrun{ShowCachePath("F:")}
#' @family Cache Controls
#' @return A string with the path to the cache folder.
#' @seealso \code{\link{TAPChunks}}
ShowCachePath <- function(Directory) {
  if (!exists("Configuration_env")) {
    TAPChunks:::GetConfiguration()
  }

  if (missing(Directory)) {
    if (!is.null(Configuration_env$Message$Configuration$cache.path)) {
      cache.path <- Configuration_env$Message$Configuration$cache.path
    } else {
      cache.path <- path.expand("~")
      cache.path <- paste0(cache.path, "/TAPCache")
    }
  } else {
    cache.path <- path.expand(Directory)
  }

  if (!file.exists(cache.path)) {
    dir.create(cache.path)
  }

  Configuration_env$Message$Configuration$cache.path <- cache.path

  cache <- list.files(cache.path, recursive = T)
  file.names <- list.files(cache.path)
  if (sum(grepl("\\/", cache)) >= 1) {
    list.folders <- paste(sapply(setdiff(file.names, cache), paste0, collapse = ", "), collapse = ", ")

    TAPChunks:::catWarning(sprintf("The subdirectory %s has been found and any files in them will not be recognized by TAPChunks", list.folders), list.folders)
  }
  return(cache.path)
}
