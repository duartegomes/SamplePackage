
#' @title Delimiter for use when setting up Iterations
#' @description TAP scripts can be run in the usual fashion from within your development environment
#' or under the control of a compute tool, such as the IterateScript function.  In order to be run
#' under iteration a script must use deliminators to show where an iterate block begins and where it
#' ends.  The BeginIterate function marks the start of an iterate block and must always be paired with
#' an \code{\link{EndIterate}} function, which will show where the block ends.
#' @section The Iterate Control Block:
#' When this function is called it will first check to see if an iterate control block has been
#' established.  If not, then this is created in the user's environment that called the BeginIterate
#' function.  The environment is named \strong{Iter_env} and is used to store the objects that exist
#' in the user's environment when the BeginIterate function is called.
#' @section Asset List:
#' The function takes an inventory of objects that exist in the user's environment and this inventory
#' is stored in the Iterate Control Block. This will allow the \code{\link{EndIterate}} function to
#' clean the objects created within the block to keep memory requirements under control.
#' @author JTA - The Data Scientists
#' @family Compute Tools
#' @seealso \code{\link{TAPChunks}}
#' @export
# For the moment these functions are NULL but they can be used as place holders for controlling memory
# and other ideas.
BeginIterate <- function() {
  catIterate <- function(x) {
    TAPChunks:::catStyle(
      Header = "Iterate:",
      Text = x,
      Style = "i",
      Color = "blue"
    )
  }
  pf <- parent.frame()
  if (!exists("Iter_env", envir = .GlobalEnv)) {
    catIterate("Establishing the Iteration Control Environment.")
    Iter_env <<- new.env(parent = emptyenv())
  }
  assets <- ls(envir = pf)
  catIterate("Recording the assets in use at the start of the iteration block.")
  assign(
    x = "assets",
    value = assets,
    envir = .GlobalEnv$Iter_env
  )
  gc()
  catIterate(sprintf("Memory used at the start of iterate block %6.2f Mbytes.", memory.size()))
}
