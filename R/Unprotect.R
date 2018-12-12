#' @title Unprotect the object that has been assigned as Protected
#' @description
#' This function removes the Protect attribute from the list of objects passed in the parameter.
#' When the user uses the CleanMemory function the object will be deleted.
#' @param ... A series of object names to have the protection removed.
#' @author JTA - The Data Scientists
#' @export
#' @family Cache Controls
#' @return A object without attribute set as protect.
#' @seealso \code{\link{TAPChunks}}
Unprotect <- function(...) {
  objs <- as.character(match.call(expand.dots = FALSE)$...)
  env <- parent.frame()
  for (obj in objs) {
    res <- try(eval(parse(text = sprintf("attr(%s, 'Protect') <- NULL", obj)), envir = env),
      silent = T
    )
    if (class(res) == "try-error") {
      TAPChunks:::catInfo(sprintf("Object %s not recognised.", obj))
    } else {
      TAPChunks:::catInfo(sprintf("Object %s unprotected.", obj))
    }
  }
}
