#' @title List files in the local cache
#' @description Show a list of files that we have in the local cache
#' @details
#' This function will establish if the user has a local cache directory installed
#' in their machine.  If it doesn't exist it will be automatically created.
#' The function then returns a data table that has a list of chunks that are stored
#' in the cache.  \cr\cr
#' The user may select an optional filter using the source parameter
#' that will filter the results to show only chunks that relate to the source.\cr\cr
#' If the user selects showattributes = TRUE then the returned data table has
#' a series of properties listed that will help the user understand the formation of the
#' chunk. \cr
#' @inheritParams ShowADLPath
#' @param ShowAttributes When TRUE a detailed listing of the files attributes is shown.
#' @examples ListCacheDirectory()
#' ListCacheDirectory(ShowAttributes = TRUE)
#' ListCacheDirectory("Client", ShowAttributes = TRUE)
#' @family Cache Controls
#' @author JTA - The Data Scientists
#' @return Returns a data table with a columns called 'File Name' holding strings with
#' the name of each file found.  If the user selected ShowAttributes = TRUE then the data
#' table will have additional columns for 'File Size' and 'File Created'
#' @export
#' @seealso \code{\link{TAPChunks}}
ListCacheDirectory <- function(Source = "",
                               ShowAttributes = FALSE) {
  # Set the path to where the cache will be stored and create the folder if needed
  temp.folder <- ShowCachePath()
  cache <- list.files(temp.folder)

  if (length(cache) == 0) {
    TAPChunks:::catInfo("The file is empty.")
  } else {
    file.sizes <- c()
    file.dates <- c()
    file.names <- list.files(temp.folder, recursive = T)
    file.names <- c(file.names, setdiff(cache, file.names))

    for (i in file.names) {
      fi <- file.info(paste0(temp.folder, "/", i))
      file.sizes <-
        c(file.sizes, utils:::format.object_size(fi$size, "MB"))
      file.dates <- c(file.dates, as.character(fi$mtime))
    }
    if (Source != "") {
      prefix <- TAPChunks:::ShowADLPrefix(Source)
    } else {
      prefix <- NULL
    }

    if (is.null(prefix)) {
      prefix <- ""
    }

    info <-
      data.table::data.table(
        `File Name` = file.names[grep(prefix, file.names)],
        `File Size` = file.sizes[grep(prefix, file.names)],
        `File Created` = file.dates[grep(prefix, file.names)]
      )

    total.size <- sum(as.numeric(gsub(" Mb", "", file.sizes[grep(prefix, file.names)])))

    if (prefix == "") {
      TAPChunks:::catInfo(
        sprintf(
          "There are %s files (total size = %s Mb)",
          length(file.sizes),
          total.size
        ),
        paste(length(file.sizes), "files")
      )
    } else {
      TAPChunks:::catInfo(
        sprintf(
          "There are %s %s files (total size = %s Mb)",
          length(file.sizes[grep(prefix, file.names)]), Source,
          total.size
        ),
        paste(length(file.sizes[grep(prefix, file.names)]), Source)
      )
    }

    if (!ShowAttributes) {
      info <- data.table::data.table(`File Name` = info$`File Name`)
    }

    if (nrow(info) == 0) info <- NULL

    return(info)
  }
}
