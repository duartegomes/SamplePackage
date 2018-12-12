#' @title  Test For Internet Connectivity
#' @description
#' This is an internal function that can be used by other tools
#' to detect web connectivity.  This is so the load processes may
#' continue to work (via the cache files) even when the user is
#' offline.
#' @details
#' This function only works on windows operating systems and
#' functions by sending a single ping request to www.microsoft.com.
#' @examples ifelse (TAPChunks:::TestNet(), "We have net connectivity.", "Offline")
#' @family Internal Utilities
#' @keywords internal
#' @author JTA - The Data Scientists
#' @return Returns a logical TRUE when the ping was successful else FALSE.
#' @seealso \code{\link{TAPChunks}}
TestNet <- function() {
  res <- !as.logical(tryCatch(
    system2(
      command = "ping",
      args = "-n 1 www.microsoft.com",
      stdout = NULL,
      stderr = NULL
    ),
    warning = function(e) {
    },
    error = function(e) {
    }
  ))
  return(ifelse(res == FALSE || length(res) == 0, FALSE, TRUE))
}
