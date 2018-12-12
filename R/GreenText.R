#' @title Printing Green Text
#' @description This function applies green font color to the text submitted by wrapping the text
#' with ANSI escape sequences.
#' @return Returns a string.
#' @param Text String to apply font color.
#' @examples   cat(TAPChunks:::GreenText("Hello World!"))
#' message(TAPChunks:::GreenText("Status100"))
#' @family Text Formatters
#' @keywords internal
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
GreenText <- function(Text)
  paste0("\033[32m", Text, "\033[39m")
