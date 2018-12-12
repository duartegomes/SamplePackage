#' @title Printing Red Text
#' @description This function applies red font color to the text submitted by wrapping the text
#' with ANSI escape sequences.
#' @return Returns a string.
#' @param Text String to add font color.
#' @examples   cat(TAPChunks:::RedText("Hello World!"))
#' message(TAPChunks:::RedText("Status100"))
#' @family Text Formatters
#' @keywords internal
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
RedText <- function(Text)
  paste0("\033[31m", Text, "\033[39m")
