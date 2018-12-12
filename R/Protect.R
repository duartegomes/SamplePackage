#' @title Protect a table in memory from deletion
#' @description
#' This function has two main uses:
#' \enumerate{
#' \item Protect an existing chunk from being cleaned from memory
#' \item Create an empty protected chunk}
#' adds an attribute to a TAP Chunk that protects the object from being deleted
#' when the user uses the CleanMemory function.
#' @details
#' The function receives a list of objects.  If the object already exists
#' then it will be protected.  If the object is not defined then it will be
#' created and protected.
#' @param ... A list of objects that the user wishes to protect or to have created as empty protected chunks.
#' @author JTA - The Data Scientists
#' @export
#' @family Compute Tools
#' @return A object with attribute set as protect.
#' @seealso \code{\link{TAPChunks}}
Protect <- function(...) {
  objs <- as.character(match.call(expand.dots = FALSE)$...)
  env <- parent.frame()
  for (obj in objs) {
    res <- try(eval(parse(text = sprintf("attr(%s, 'Protect') <- TRUE", obj)), envir = env),
      silent = T
    )
    if (class(res) == "try-error") {
      TAPChunks:::catInfo(sprintf("An empty protected object will be created for %s.", obj))
      eval(parse(text = sprintf("%s <- data.table::data.table()", obj)), envir = env)
    } else {
      TAPChunks:::catInfo(sprintf("Object %s protected.", obj))
    }
  }
}
