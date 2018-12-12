#' @title Printing Yellow Text
#' @description This function applies yellow font color to the text submitted by wrapping the text
#' with ANSI escape sequences.
#' @return Returns a string.
#' @param Text String to apply font color.
#' @examples   cat(TAPChunks:::YellowText("Hello World!"))
#' message(TAPChunks:::YellowText("Status100"))
#' @family Text Formatters
#' @keywords internal
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
YellowText <- function(Text)
  paste0("\033[33m", Text, "\033[39m")
