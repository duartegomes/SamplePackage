#' @title Printing Bold Text
#' @description This function bolds the text submitted by wrapping the text
#' with ANSI escape sequences.
#' @return Returns a string
#' @param Text String to apply bold codes.
#' @examples   cat(TAPChunks:::BoldText("Hello World!"))
#' message(TAPChunks:::BoldText("Status100"))
#' @family Text Formatters
#' @keywords internal
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
BoldText <- function(Text)
  paste0("\033[1m", Text, "\033[22m")
