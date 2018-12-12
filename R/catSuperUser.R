#' @title Print Console Messages
#' @description Print Super User messages with Highlighed text.
#' @param Text A string with the message that we wish to report to the user
#' @param Highlight A string that can be found in the string that was passed to the Text parameter
#' and which will be highlighted in bold text.
#' Defaults to NULL which results in no highlighting.
#' @author JTA - The Data Scientists
#' @keywords internal
#' @family Console Messaging Functions
#' @seealso \code{\link{TAPChunks}}
catSuperUser <- function(Text, Highlight = NULL) {
  if (!exists("Configuration_env", envir = .GlobalEnv)) {
    OptionsMessage()
  }
  RstudioVersion <-
    Configuration_env$Message$Configuration$RStudioVersion
  Text <- paste0(Text, "\n")
  if (Configuration_env$Message$Configuration$SuperUser) {
    if (!is.null(Highlight)) {
      if (RstudioVersion >= 1.1) {
        Text <- gsub(Highlight, TAPChunks:::BoldText(Highlight), Text)
      }
    }

    if (RstudioVersion >= 1.1) {
      Text <- TAPChunks:::BlueText(paste0("\t- ", Text))
    } else {
      Text <- paste0("\t- ", Text)
    }

    status <- Configuration_env$Message$Configuration$StatusMessage


    if (status != "superUser") {
      if (RstudioVersion >= 1.1) {
        cat(paste0(TAPChunks:::BlueText(TAPChunks:::BoldText(
          "SuperUser:\n"
        ))))
      } else {
        cat("SuperUser:\n")
      }
    }

    cat(Text)

    Configuration_env$Message$Configuration$StatusMessage <-
      "superUser"
  }
}
