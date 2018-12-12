#' @title  Check Memory
#' @description
#' This function checks the available memory and estimates the
#' memory required to open the chunk files.
#' @details The function will lookup the size of the file requested and compare to
#' the memory available.  If the memory is not sufficient the function will switch
#' to an interactive mode where it will offer options to clear the memory.
#' @examples TAPChunks:::CheckMemory("SWWorkloadWL2017M12.Rdata")
#' @param Request chunk files required
#' @family Internal Utilities
#' @author JTA - The Data Scientists
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
CheckMemory <- function(Request = NULL) {
  if (is.null(Request)) {
    TAPChunks:::catError("No chunk passed in parameters")
  }
  RAM_machine <- memory.limit()
  RAM_use_R <- memory.size()
  RAM_use_PC <- data.table::data.table(shell("tasklist", intern = T))
  RAM_use_PC <- apply(RAM_use_PC[!1:4], 1, function(x) gsub(
      "\\s*\\s",
      "#", gsub("\\sK$", "", x)
    ))
  RAM_use_PC <- gsub("\\D.+#", "", RAM_use_PC)
  RAM_use_PC <- sum(
    as.numeric(gsub("\\D", "", RAM_use_PC)),
    na.rm = T
  )
  RAM_use_PC <- as.integer(RAM_use_PC / 1024)
  space_estimate <- as.integer(TAP_env$InfoChunk[file_name_rdata %in%
    Request | file_name_csv %in% Request, sum(ram_bytes)] / 1024 ^ 2)
  catSuperUser(sprintf("Memory available in OS: %s GB", round(
    RAM_machine / 1024,
    2
  )))
  catSuperUser(sprintf("Memory used by OS: %s GB", round(
    RAM_use_PC / 1024,
    2
  )))
  catSuperUser(sprintf("Memory used in R Session: %s GB", round(
    RAM_use_R / 1024,
    2
  )))

  if (RAM_machine - RAM_use_PC < space_estimate * 1.25) {
    TAPChunks:::catError(sprintf(
      "RAM: %s GB (Available: %s GB) Failed",
      round(space_estimate * 1.25 / 1024, 2), round(
        RAM_machine / 1024,
        2
      ) - round(RAM_use_PC / 1024, 2)
    ), "Failed")
  }
  else {
    if ((RAM_machine - RAM_use_PC) * 0.75 > space_estimate * 1.25) {
      TAPChunks:::catSuccess(sprintf(
        "RAM: %s GB (Available: %s GB) OK",
        round(space_estimate * 1.25 / 1024, 2), round(
          RAM_machine / 1024,
          2
        ) - round(RAM_use_PC / 1024, 2)
      ), "OK")
    }
    else {
      TAPChunks:::catWarning(sprintf(
        "RAM: %s GB (Available: %s GB) Warning - Memory is scarce",
        round(space_estimate * 1.25 / 1024, 2), round(
          RAM_machine / 1024,
          2
        ) - round(RAM_use_PC / 1024, 2)
      ), "Warning")
      # Apply gc to release some space.
      gc()
    }
  }
}
