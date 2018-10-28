#' @title BoldText
#' @author JTA - The Data Scientists
#' @description Apply underline ANSI codes arount the submitted string
#' @return Returns a string preceded by Esc[1m and closed with Esc[22m
#' @param Text String to apply bold codes
#' @examples   cat(BoldText("Hello World!"))
#' message(BoldText("Status100"))
#' @family Text Formatters
#' @keywords internal
BoldText <- function(Text)
  paste0("\033[1m", Text, "\033[22m")
