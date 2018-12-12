#' @title  Check we can load a chunk
#' @description
#' TODO
#' TODO
#' @details TODO
#' TODO
#' @param Request chunk files required
#' @family Internal Utilities
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
#' @keywords internal
CheckRequirements <- function(Request) {
  TAPChunks:::catInfo("Requested chunk requires:")
  if (!exists("TAP_env", envir = .GlobalEnv)) {
    TAP_env <<- new.env()
  }

  if (length(ls(
    envir = TAP_env,
    all.names = all.names,
    pattern = "^InfoChunk$"
  )) == 0) {
    assign("InfoChunk", ReadADLFile("/Source_Data/Demog/rdata", "InfoChunk.Rdata"))
    TAP_env$InfoChunk <- InfoChunk
    remove(InfoChunk)
  }

  TAPChunks:::CheckMemory(Request)

  Request <- setdiff(
    Request,
    list.files(ShowCachePath())
  )

  if (length(Request) != 0) {
    TAPChunks:::CheckSpaceDisk(Request)
  }
}
