#' @title  Save Objects
#' @description This function allows to save R environment objects into local machine. Formats available csv and rdata
#' @examples TAPChunks:::SaveLocal(TestClientChunk, TestEmailChunk, path = getwd());
#' TAPChunks:::SaveLocal("TestClientChunk", "TestEmailChunk", FileExtension = "rdata", path = getwd())
#' @param ... Objects to be saved
#' @param FileExtension A string with the required file extension. This can be one of two types, csv or rdata.
#' @param Path Destination of the files with information of the R session objects
#' @family Internal Utilities
#' @author JTA - The Data Scientists
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
SaveLocal <- function(..., FileExtension = "csv", Path = NULL) {
  FileExtension <- tolower(FileExtension)

  if (FileExtension != c("csv") & FileExtension != c("rdata")) {
    TAPChunks:::catError("The file extension is unknown.")
  }

  if (is.null(Path)) {
    Path <- ShowCachePath()
  }

  if (!file.exists(Path)) {
    TAPChunks:::catError("The path doesn't exist.")
  }

  list.obj <- match.call(expand.dots = FALSE)$...
  list.data <- list(...)
  names(list.data) <- file.path(Path, paste0(list.obj, paste0(".", FileExtension)))
  class_obj <- sapply(list.data, class)

  for (i in 1:length(list.data)) {
    if (class_obj[i] == "character") {
      list.data[[i]] <- get(list.data[[i]])
    }
  }

  n <- 1
  for (file.name in names(list.data)) {
    if (FileExtension == "csv") {
      data.table::fwrite(list.data[[file.name]], file = file.name)
    }
    if (FileExtension == "rdata") {
      assign(paste(list.obj[[n]]), list.data[[file.name]])
      save(list = paste(list.obj[[n]]), file = file.name)
    }
    n <- n + 1
  }
  TAPChunks:::catInfo(paste("Saved successfully. Path:", Path))
}
