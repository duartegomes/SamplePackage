#' @title Defining Message Style
#' @description This function allows us to customize the console messages by applying styles.
#' @param Header String to define the chapter
#' @param Text A string with the message that we wish to report to the user
#' @param Style The initial of the style pretended. (b-bold, i-italic, u-underline, s-strike)
#' @param Color The color to be aplied.  (red, green, yellow, blue)
#' @examples  TAPChunks:::catStyle(Header="Header:", Text="Text", Style = "ui", Color = "blue")
#' @author JTA - The Data Scientists
#' @seealso \code{\link{TAPChunks}}
#' @family Console Messaging Functions
#' @keywords internal
catStyle <-
  function(Header, Text,
             Style, Color = "black") {
    if (!exists("Configuration_env", envir = .GlobalEnv)) {
      OptionsMessage()
    }

    if (missing(Style)) Style <- ""

    styleFormat <- tolower(sapply(1:nchar(Style), function(x) {
      substr(Style, x, x)
    }))

    if ("b" %in% styleFormat) {
      Text <- TAPChunks:::BoldText(Text)
    }
    if ("i" %in% styleFormat) {
      Text <- TAPChunks:::ItalicText(Text)
    }
    if ("u" %in% styleFormat) {
      Text <- TAPChunks:::UnderLineText(Text)
    }
    if ("s" %in% styleFormat) {
      Text <- TAPChunks:::StrikeText(Text)
    }

    textHeader <- TAPChunks:::BoldText(Header)

    Text <- paste0(Text, "\n")
    Text <- paste0("\t- ", Text)

    switch(tolower(Color),
      red = {
        textHeader <- TAPChunks:::RedText(textHeader)
        Text <- TAPChunks:::RedText(Text)
      },
      green = {
        textHeader <- TAPChunks:::GreenText(textHeader)
        Text <- TAPChunks:::GreenText(Text)
      },
      yellow = {
        textHeader <- TAPChunks:::YellowText(textHeader)
        Text <- TAPChunks:::YellowText(Text)
      },
      blue = {
        textHeader <- TAPChunks:::BlueText(textHeader)
        Text <- TAPChunks:::BlueText(Text)
      }
    )

    status <- Configuration_env$Message$Configuration$StatusMessage

    if (status != Header) {
      cat(paste0(textHeader, "\n"))
    }

    cat(Text)
    Configuration_env$Message$Configuration$StatusMessage <- Header
  }
