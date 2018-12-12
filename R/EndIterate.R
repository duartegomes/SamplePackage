#' @title Delimiter for use when setting up Iterations
#' @description TAP scripts can be run in the usual fashion from within your development environment
#' or under the control of a compute tool, such as the IterateScript function.  In order to be run
#' under iteration a script must use deliminators to show where an iterate block begins and where it
#' ends.  The EndIterate function marks the end of an iterate block and must always be paired with
#' a \code{\link{BeginIterate}} function.
#' @param Delete This is a boolean option (which defaults to TRUE) and will control whether the function
#' deletes any new files that where generated within the block.  If the function is called with Delete = FALSE
#' then the steps to compare the assets lists and delete the files are skipped.
#' @section The Iterate Control Block:
#' This function will refer to the Iter_env environment that was established by the \code{\link{BeginIterate}}
#' function. The environment has a list of objects that existed at the begining of the iterate block.
#' @section Asset Comparison:
#' After refering to the list of objects that existed at the start of the block the function will compare this
#' with the list of objects that exist post block execution.
#' @section File Deletion and Memory Cleanup:
#' Any new objects which were generated during the execution of the block will be deleted and the memory
#' is returned to the user's R environment.
#' @author JTA - The Data Scientists
#' @export
#' @family Compute Tools
#' @seealso \code{\link{TAPChunks}}
EndIterate <- function(Delete = T) {
  catIterate <- function(x) {
    TAPChunks:::catStyle(
      Header = "Iterate:",
      Text = x,
      Style = "i",
      Color = "blue"
    )
  }
  catIterate("Ending iterate block.")
  pf <- parent.frame()
  if (Delete) {
    if (exists("Iter_env", envir = .GlobalEnv)) {
      assets_then <- .GlobalEnv$Iter_env$assets
      assets_now <- ls(envir = pf)
      assets_new <- setdiff(assets_now, assets_then)
      if (length(assets_new) > 0) {
        catIterate(sprintf("Memory used at the end of the iterate block before asset deletion is %6.2f", memory.size()))
        catIterate("Deleting the following assets:")
        catIterate(assets_new)
        rm(list = assets_new, envir = pf)
        gc()
        catIterate(sprintf("Memory used at the end of the iterate block after asset deletion is %6.2f", memory.size()))
      }
    }
  } else {
    catIterate("User requested not to delete files. ")
  }
}
