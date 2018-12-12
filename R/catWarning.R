#' @title Print Warning Messages
#' @description Print Warning message with Highlighed text.
#' @param Text A string with the message that we wish to report to the user
#' @param Highlight A string that can be found in the string that was passed to the Text parameter
#' and which will be highlighted in bold text.
#' Defaults to NULL which results in no highlighting.
#' @author JTA - The Data Scientists
#' @keywords internal
#' @family Console Messaging Functions
#' @seealso \code{\link{TAPChunks}}
catWarning <- function(Text, Highlight = NULL) {
  if (!exists("Configuration_env", envir = .GlobalEnv)) {
    OptionsMessage()
  }
  RstudioVersion <-
    Configuration_env$Message$Configuration$RStudioVersion
  warnMessage <- Text
  Text <- paste0(Text, "\n")
  if (Configuration_env$Message$Configuration$Warning) {
    if (!is.null(Highlight)) {
      if (RstudioVersion >= 1.1) {
        Text <- gsub(Highlight, TAPChunks:::YellowText(TAPChunks:::BoldText(Highlight)), Text)
      }
    }

    Text <- paste0("\t- ", Text)

    status <- Configuration_env$Message$Configuration$StatusMessage

    if (status != "warn") {
      if (RstudioVersion >= 1.1) {
        cat(paste0(TAPChunks:::YellowText(TAPChunks:::BoldText(
          "Warning:\n"
        ))))
      } else {
        cat("Warning:\n")
      }
    }
    cat(Text)
    options(warn = -1)
    warning(warnMessage)
  }
  else {
    options(warn = -1)
    warning(warnMessage)
  }
  Configuration_env$Message$Configuration$StatusMessage <- "warn"
}
