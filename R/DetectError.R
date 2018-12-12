#' @title Check and detect Error Syntax on iterate script
#' @description Checks for syntax errors before start to work on the iteration.
#' @param sf Error Object
#' @param FirstExp First Expression, the error message
#' @param LastExp Last Expression, usually the line Number
#' @param Env Environment
#' @keywords internal
#' @author JTA - The Data Scientists
#' @family Internal Utilities
#' @seealso \code{\link{TAPChunks}}
DetectError <- function(Sf, FirstExp, LastExp, Env) {
  catIterate <- function(x) {
    TAPChunks:::catStyle(
      Header = "Iterate:",
      Text = x,
      Style = "i",
      Color = "blue"
    )
  }

  t <- FirstExp
  test <- NULL
  Result <- NULL

  TAPChunks:::OptionsMessage(Error = F)
  while (t <= LastExp & class(test)[1] != "try-error") {
    test <- try(eval(Sf[t], envir = Env), silent = T)
    t <- t + 1
  }
  TAPChunks:::OptionsMessage(Error = T)


  if (class(test)[1] == "try-error") {
    t <- t - 1
    endLine <- getSrcLocation(Sf, first = F)[t]
    startline <- getSrcLocation(Sf, first = T)[t]

    text <- if (startline == endLine) {
      sprintf("Line %s", startline)
    } else {
      sprintf("Lines %s to %s", startline, endLine)
    }

    text <- sprintf(
      "%s \n\t- %s ",
      attr(test, "condition")$message,
      text
    )

    assets_new <- setdiff(ls(Env), .GlobalEnv$Iter_env$assets)
    catIterate("Ending iterate block and deleting the following assets:")
    catIterate(assets_new)
    rm(list = assets_new, envir = Env)
    catError(text)
  }
}
