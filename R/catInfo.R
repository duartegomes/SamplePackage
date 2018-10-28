#' @title catInfo
#' @description Print Info message with Highlighed text
#' @param Text A string with the message that we wish to report to the user
#' @param Highlight A string that can be found in the string that was passed to the Text parameter and which will be highlighted in bold text.  Defaults to NULL
#' which results in no highlighting.
#' @description This function is used to pass a message to the console.  The message is indented with a tab character and a new line character will be appended.
#' The message is printed in black.
#' @details This is best shown by some examples:
#' \preformatted{
#' catInfo("Hello user.", "user")}
#'  - Hello \strong{user}.
#' \preformatted{
#' catInfo("Hello user.", "Name")}
#'  - Hello user.
#' @export
#' @keywords internal
#' @family Text Formatters
#' @seealso \code{\link{TAPChunks}}
#'
catInfo <- function(Text, Highlight = NULL) {
  if (!exists("Configuration_env", envir = .GlobalEnv))
    OptionsMessage()
  RstudioVersion <-
    Configuration_env$Message$Configuration$RStudioVersion
  Text <- paste0(Text, "\n")

  if (Configuration_env$Message$Configuration$Info) {
    if (!is.null(Highlight)) {
      if (RstudioVersion >= 1.1)
        Text <- gsub(Highlight, BoldText(Highlight), Text)
    }
    Text <- paste0("\t- ", Text)
    status <- Configuration_env$Message$Configuration$StatusMessage


    if (status != "info") {
      if (RstudioVersion >= 1.1) {
        cat(paste0(BoldText("Info:\n")))
      } else{
        cat(paste0("Info:\n"))
      }
    }
    cat(Text)
    Configuration_env$Message$Configuration$StatusMessage <- "info"
  }

}
