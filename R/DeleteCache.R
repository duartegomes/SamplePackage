#' @title  Deleting files from the local cache
#' @description
#' Users will need to maintain their local cache. This function allows all the cache
#' to be cleared or just selected files.
#' @details The filename can either be an explicit name or can use
#' wildcards.  DeleteCache("*") will remove all files. The function
#' will confirm the choice before actually removing files.
#' @param PatternFile Pattern name of the files to be deleted. Default is "*".
#' @param ForceDelete If True will also delete folders.
#' @examples DeleteCache("*")
#' @family Cache Controls
#' @author JTA - The Data Scientists
#' @return Returns an error status to show if the command completed succesfully or not
#' @export
#' @seealso \code{\link{TAPChunks}}
DeleteCache <-
  function(PatternFile = "**", ForceDelete = F) {
    path <- ShowCachePath()
    directory <- ListCacheDirectory()
    if (data.table::is.data.table(directory)) {
      directory[, all_tolower := tolower(`File Name`)]
      PatternFile <- tolower(PatternFile)

      if (substr(PatternFile, 1, 1) != "*") {
        PatternFile <- paste0("^", PatternFile)
      }

      if (substr(PatternFile, nchar(PatternFile), nchar(PatternFile)) != "*") {
        PatternFile <- paste0(PatternFile, "$")
      }

      PatternFile <- gsub("\\*", ".*", PatternFile)
      files <- directory[grep(PatternFile, all_tolower), `File Name`]
      if (length(files) == 0) {
        TAPChunks:::catInfo("No files available with the pattern.")
      } else {
        TAPChunks:::catInfo(sprintf(
          "Files list: \n\t\t%s\n",
          paste(1:length(files), files, sep = " - ", collapse = "\n\t\t")
        ))

        answer <- "NO"
        if (ForceDelete == F) {
          answer <-
            readline(cat("Do you want to delete ALL chunks listed?\n\tY-Yes\n\tN-No\n\tRange of chunks - Insert the number separated by commas:\n"))
          answer <- tolower(answer)
        }


        if (answer == "y" | ForceDelete == T) {
          for (i in files) {
            unlink(file.path(path, i), recursive = T)
          }
          TAPChunks:::catSuccess(sprintf("%s files have been deleted", length(files)), length(files))
        } else {
          if (answer == "n" & ForceDelete == F) {
            TAPChunks:::catWarning("Operation Cancelled!", "Cancelled")
          } else {
            files <- files[as.numeric(strsplit(gsub("\\s", "", answer), ",")[[1]])]
            for (i in files) {
              unlink(file.path(path, i), recursive = T)
            }
            TAPChunks:::catSuccess(sprintf("%s files have been deleted", length(files)), length(files))
          }
        }
      }
    }
  }
