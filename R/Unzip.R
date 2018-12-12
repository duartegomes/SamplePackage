#' @title Unzip files
#' @description TODO
#' @param File TODO
#' @param Savepath TODO
#' @family Internal Utilities
#' @keywords internal
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
Unzip <- function(File = "", SavePath) {
  if (missing(SavePath)) {
    SavePath <- ShowCachePath()
  } else {
    SavePath <- ShowCachePath(SavePath)
  }

  if (length(grep(".zip", File)) != 0) {
    if (length(grep("/", File)) == 1) {
      folder <- substr(File, 1, gregexpr("/", File)[[1]][1] - 1)

      unzip(file.path(SavePath, File),
        exdir = file.path(SavePath, folder)
      )

      # delete .zip file after extracting files
      unlink(file.path(SavePath, File))
    } else {

      # unzip compressed file
      unzip(file.path(SavePath, File),
        # export to temp folder
        exdir = file.path(SavePath)
      )

      unlink(file.path(SavePath, File))
    }
  } else {
    TAPChunks:::catError("Different extension file.")
  }
}
