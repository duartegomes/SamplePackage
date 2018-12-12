#' @title Print Error Messages
#' @description Print Error message with Highlighed text
#' @param Text A string with the message that we wish to report to the user
#' @param Highlight A string that can be found in the string that was passed to the Text parameter
#' and which will be highlighted in bold text.
#' Defaults to NULL which results in no highlighting.
#' @author JTA - The Data Scientists
#' @family Console Messaging Functions
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
catError <- function(Text, Highlight = NULL) {
  if (!exists("Configuration_env", envir = .GlobalEnv)) {
    OptionsMessage()
  }

  RstudioVersion <-
    Configuration_env$Message$Configuration$RStudioVersion
  errorMessage <- Text
  Text <- paste0(Text, "\n")

  if (Configuration_env$Message$Configuration$Error) {
    if (!is.null(Highlight)) {
      if (RstudioVersion >= 1.1) {
        Text <- gsub(Highlight, TAPChunks:::BoldText(Highlight), Text)
      }
    }

    if (RstudioVersion >= 1.1) {
      Text <- TAPChunks:::RedText(paste0("\t- ", Text))
    } else {
      Text <- paste0("\t- ", Text)
    }


    status <- Configuration_env$Message$Configuration$StatusMessage


    if (status != "error") {
      if (RstudioVersion >= 1.1) {
        cat(TAPChunks:::RedText(paste0(TAPChunks:::BoldText("Error:\n"))))
      } else {
        cat("Error:\n")
      }
    }

    cat(Text)
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    Configuration_env$Message$Configuration$StatusMessage <- "error"
    stop(errorMessage)
  } else {
    stop(errorMessage)
  }
}
