#' @title  Validating data chunks
#' @description
#' This routine will validate data to ensure that it is a valid data chunk.  It is used
#' by many of the other functions before they start work on altering the chunk that
#' was presented.
#' @details In order to be valid the data presented must be a data table with a uuid column.
#' @examples TAPChunks:::ValidateChunk(email)
#' @param Data A data chunk to be validated.
#' @family Internal Utilities
#' @author JTA - The Data Scientists
#' @keywords internal
#' @return Returns a logical TRUE if the chunk is valid else it returns FALSE.
#' @seealso \code{\link{TAPChunks}}
ValidateChunk <- function(Data) {
  # Check that the file is a data.table with a uuid field
  valid <- TRUE
  # The status code is a bitwise indicator where each bit of the integer
  # represents a logical element of the data
  status <- 0L
  # Bit 0: when set to 1 the file has a weight column present
  # Bit 1: when set to 1 the file has a timestamp column present
  # Bit 2: when set to 1 the file contains unclean organizations
  cols <- names(Data)

  if (!("data.table" %in% intersect(class(Data), "data.table"))) {
    valid <- FALSE
    TAPChunks:::catInfo("Validation Failed: Not a data table.")
  } else
  if (!("uuid" %in% cols)) {
    valid <- FALSE
    TAPChunks:::catInfo("Validation Failed: No org identifier found.")
  } else {
    if (!("weight" %in% cols)) {
      TAPChunks:::catInfo("This data chunk does not have weights.")
      status <- bitwAnd(status, 0xFE)
    } else {
      status <- bitwOr(status, 0x01)
    }
    if (!("timestamp" %in% cols)) {
      TAPChunks:::catInfo("This data chunk does not have a timestamp.")
      status <- bitwAnd(status, 0xFD)
    } else {
      status <- bitwOr(status, 0x02)
    }

    if (TAPChunks:::CheckDemogDB()) {
      if (length(intersect(Data$uuid, TAP_env$unclean)) == 0) {
        TAPChunks:::catInfo("The data file only has clean organizations.")
        status <- bitwAnd(status, 0xFB)
      } else {
        TAPChunks:::catInfo("The data file contains unclean organizations.")
        status <- bitwOr(status, 0x04)
      }
    } else {
      TAPChunks:::catInfo("Unable to check the clean status as ")
      TAPChunks:::catInfo("the demog database is not loaded.")
    }
  }
  return(list(valid = valid, status = status))
}
