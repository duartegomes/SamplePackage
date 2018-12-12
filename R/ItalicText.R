#' @title Printing Italic Text
#' @description This function applies escape sequences to the sting that is passed to make
#' that string appear in italics.
#' @return Returns a string.
#' @param Text String to apply Italic codes.
#' @examples   cat(TAPChunks:::ItalicText("Hello World!"))
#' message(TAPChunks:::ItalicText("Status100"))
#' @family Text Formatters
#' @keywords internal
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
ItalicText <- function(Text)
  paste0("\033[3m", Text, "\033[23m")
