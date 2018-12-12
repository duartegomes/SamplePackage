#' @title Clean large tables from the memory
#' @description This function removes all the objects that
#' are using more than the specified number of Mbytes in ram
#' @details Per default the is size cut-off is set as 10MB so all
#' tables above this size will be cleared from memory. The function
#' will not remove any tables that have had their protect attribut set.
#' See \code{\link{Protect}} and \code{\link{Unprotect}} for more information
#' on protecting tables in memory.
#' @author JTA - The Data Scientists
#' @export
#' @param LimitMB Limit value in MB.
#' @examples CleanMemory(20)
#' @family Compute Tools
#' @return Cleans the memory, no return.
#' @seealso \code{\link{TAPChunks}}
CleanMemory <- function(LimitMB = 10) {
  remove_obj <- NULL
  for (i in ls(envir = .GlobalEnv)) {
    obj.size <- format(object.size(get(i)), units = "Mb")
    obj.size <- as.numeric(gsub(" Mb", "", obj.size))

    if (is.null(attributes(get(i))$Protect) & obj.size >= LimitMB) {
      remove_obj <- c(remove_obj, i)
    }
  }

  if (!is.null(remove_obj)) {
    rm(list = remove_obj, envir = .GlobalEnv)
    TAPChunks:::catInfo(paste0("Removed Objects:\n\t\t-", paste0(remove_obj, collapse = "\n\t\t-")))
  }

  startRAM <- memory.size()
  lowRAM <- startRAM * .1
  while (startRAM != lowRAM) {
    startRAM <- lowRAM
    gc()
    lowRAM <- memory.size()
  }
}
