#' @title Print Information Messages
#' @description Print Information message with Highlighed text
#' @param Text A string with the message that we wish to report to the user
#' @param Highlight A string that can be found in the string that was passed to the Text parameter
#' and which will be highlighted in bold text.
#' Defaults to NULL which results in no highlighting.
#' @details This is best shown by some examples:
#' \preformatted{
#' TAPChunks:::catInfo("Hello user.", "user")}
#'  - Hello \strong{user}.
#' \preformatted{
#' TAPChunks:::catInfo("Hello user.", "Name")}
#'  - Hello user.
#' @keywords internal
#' @family Console Messaging Functions
#' @seealso \code{\link{TAPChunks}}
#' @author JTA - The Data Scientists
catInfo <- function(Text, Highlight = NULL) {
  if (!exists("Configuration_env", envir = .GlobalEnv)) {
    OptionsMessage()
  }
  RstudioVersion <-
    Configuration_env$Message$Configuration$RStudioVersion
  Text <- paste0(Text, "\n")

  if (Configuration_env$Message$Configuration$Info) {
    if (!is.null(Highlight)) {
      if (RstudioVersion >= 1.1) {
        Text <- gsub(Highlight, TAPChunks:::BoldText(Highlight), Text)
      }
    }
    Text <- paste0("\t- ", Text)
    status <- Configuration_env$Message$Configuration$StatusMessage


    if (status != "info") {
      if (RstudioVersion >= 1.1) {
        cat(paste0(TAPChunks:::BoldText("Info:\n")))
      } else {
        cat(paste0("Info:\n"))
      }
    }
    cat(Text)
    Configuration_env$Message$Configuration$StatusMessage <- "info"
  }
}
