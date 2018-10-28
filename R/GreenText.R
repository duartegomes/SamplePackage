#' @title GreenText
#' @description Add Green font color
#' @param Text String to apply font color
#' @examples   cat(GreenText("Hello World!"))
#' message(GreenText("Status100"))
#' @family Text Formatters
#' @keywords internal
GreenText <- function(Text)
  paste0("\033[32m", Text, "\033[39m")
