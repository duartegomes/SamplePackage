#' @title YellowText
#' @description Apply yellow font color
#' @param Text String to apply font color
#' @examples   cat(YellowText("Hello World!"))
#' message(YellowText("Status100"))
#' @family Text Formatters
#' @keywords internal
YellowText <- function(Text)
  paste0("\033[33m", Text, "\033[39m")
