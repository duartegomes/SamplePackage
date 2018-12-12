#' @title Testing that the demographics database is loaded
#' @description
#' The TAP system uses a database of demographics to perform a variety of tasks:
#'  Filter Unclean orgs from datasets
#'  Compute distributions of firmographics to calculate weights
#'  Add firmographic data to other datasets to be used as slicers
#'  Define hierarchies in reports
#'
#' This function checks that the system has the database loaded.
#' @details
#' The database is stored in a separate environment called TAP_env.
#' The function tests that this environment exists and that it has the
#' following three objects:
#'  demog_helper  : This is a data table with all of the known Spiceworks organizations
#'  target_helper : This is a data table with the population targets that are provided by the Market Model
#'  unclean       : This is a vector of unclean organisation UUIDs
#'
#' If the environment does not exist it calls LoadDemogDB() to establish the environment
#' and returns the status of that operation.
#' @author JTA - The Data Scientists
#' @keywords internal
#' @family Internal Utilities
#' @return Returns a logical TRUE if the database is loaded and FALSE if not.
#' @seealso \code{\link{TAPChunks}}
CheckDemogDB <- function() {
  if (exists("TAP_env")) {
    # Environment exists, but we had better check some more
    if ((!exists("demog_helper", envir = TAP_env)) |
      (!exists("target_helper", envir = TAP_env)) |
      (!exists("unclean", envir = TAP_env))) {
      TAPChunks:::catInfo("TAP environment exists but not loaded. ")
      TAPChunks:::catInfo("Setting up environment for you now...")
      st <- TAPChunks:::LoadDemogDB()
      return(st)
    } else {
      return(TRUE)
    }
  }
  TAPChunks:::catInfo("TAP environment not yet established. ")
  TAPChunks:::catInfo("Setting up environment for you now...")
  st <- TAPChunks:::LoadDemogDB()
  return(st)
}
