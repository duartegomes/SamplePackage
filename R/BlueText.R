#' @title Printing Blue Text
#' @description This function applies blue font color to the text submitted by wrapping the text
#' with ANSI escape sequences.
#' @return Returns a string
#' @param Text String to apply font color.
#' @examples   cat(TAPChunks:::BlueText("Hello World!"))
#' message(TAPChunks:::BlueText("Status100"))
#' @family Text Formatters
#' @keywords internal
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
BlueText <- function(Text)
  paste0("\033[34m", Text, "\033[39m")
