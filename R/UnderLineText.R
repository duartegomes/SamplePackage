#' @title UnderLineText
#' @author JTA - The Data Scientists
#' @description Apply underline ANSI codes arount the submitted string
#' @return Returns a string preceded by Esc[4m and closed with Esc[24m
#' @param Text String to apply underline codes
#' @keywords internal
#' @examples   cat(UnderLineText("Hello World!"))
#' message(UnderLineText("Status 100"))
#' @family Text Formatters
UnderLineText <- function(Text = "")
  paste0("\033[4m", Text, "\033[24m")
