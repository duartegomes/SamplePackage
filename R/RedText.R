#' @title RedText
#' @description Apply Red font color
#' @param Text String to add font color
#' @examples   cat(RedText("Hello World!"))
#' message(RedText("Status100"))
#' @family Text Formatters
#' @keywords internal
RedText <- function(Text)
  paste0("\033[31m", Text, "\033[39m")
