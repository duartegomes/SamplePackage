#' @title Printing UnderLined Text
#' @description This function underlines the text submitted by wrapping the text
#' with ANSI escape sequences.
#' @return Returns a string.
#' @param Text String to apply underline codes.
#' @examples   cat(TAPChunks:::UnderLineText("Hello World!"))
#' message(TAPChunks:::UnderLineText("Status 100"))
#' @family Text Formatters
#' @keywords internal
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
UnderLineText <- function(Text = "")
  paste0("\033[4m", Text, "\033[24m")
