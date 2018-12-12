#' @title Check The Space on the User's Disk
#' @description
#' When the TAPChunks system writes a chunk to disk this function is used to see if there is
#' sufficient space to store the file.
#' @details The function makes an operating system call to the WMI (Windows Management Instrumentation)
#' command line utility to see disk space available.  This is then compared with the expected size of
#' the chunk.  If there is space available then a success message is printed. If the size of the disk is
#' close to the requirment a warning message is printed.  When there is no space an error condition is
#' triggered which will serve a message and halt execution.
#' @examples CheckSpaceDisk("SWWorkloadWL2017M12.Rdata")
#' @param Request chunk files required
#' @family Internal Utilities
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
#' @keywords internal
CheckSpaceDisk <- function(Request) {
  savePath <- ShowCachePath()

  DiskPath <- tolower(gsub("\\s", "", savePath))
  if (grepl("^\\w:", DiskPath)) {
    DiskPath <- substr(DiskPath, 1, 2)
  } else {
    TAPChunks:::catError("Cache Folder is incorrect")
  }

  SpaceDisk <- shell("wmic logicaldisk get size,freespace,caption", intern = T)

  SpaceDisk <- as.numeric(utils:::read.fwf(textConnection(SpaceDisk[1:(length(SpaceDisk) - 1)]),
    widths = c(9, 13, 13), strip.white = TRUE, stringsAsFactors = FALSE
  )$V2[2:2])

  requestBytes <-
    sum(
      sum(as.numeric(TAP_env$InfoChunk[file_name_csv %in% Request & rsession_size_bytes != "LoadError"]$csv_size_bytes)),
      sum(as.numeric(TAP_env$InfoChunk[file_name_rdata %in% Request & rsession_size_bytes != "LoadError"]$rdata_size_bytes))
    )

  SpaceDiskUnits <- utils:::format.object_size(as.numeric(SpaceDisk), units = "auto")
  requestBytesUnits <- utils:::format.object_size(as.numeric(requestBytes), units = "auto")

  if (0.75 * SpaceDisk > requestBytes) {
    TAPChunks:::catSuccess(sprintf("Disk: %s (Available: %s) OK", requestBytesUnits, SpaceDiskUnits), "OK")
  } else {
    if (SpaceDisk > requestBytes) {
      TAPChunks:::catWarning(sprintf("Disk: %s (Available: %s) Warning", requestBytesUnits, SpaceDiskUnits), "Warning")
    } else {
      TAPChunks:::catError(sprintf("Disk: %s (Available: %s) Failed", requestBytesUnits, SpaceDiskUnits), "Failed")
    }
  }
}
