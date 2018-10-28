#' @title BlueText
#' @description Add Blue font color
#' @param Text String to apply font color
#' @examples   cat(BlueText("Hello World!"))
#' message(BlueText("Status100"))
#' @family Text Formatters
#' @keywords internal
BlueText <- function(Text)
  paste0("\033[34m", Text, "\033[39m")
