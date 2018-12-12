#' @title Printing Struck Text
#' @description This function uses escape sequences to add codes to the string that
#' is passed to this function so that string appears to be struck out.
#' @return Returns a string.
#' @param Text String to apply Strike codes.
#' @examples cat(TAPChunks:::StrikeText("Hello World!"))
#' message(TAPChunks:::StrikeText("Status100"))
#' @family Text Formatters
#' @keywords internal
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
StrikeText <- function(Text)
  paste0("\033[9m", Text, "\033[29m")
