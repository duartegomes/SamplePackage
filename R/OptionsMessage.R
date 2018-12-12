#' @title OptionsMessage
#' @description To define which message type the users want to see.
#' @param Error Want to get Error messages?
#' @param Warning Want to get warning messages?
#' @param Info Want to get info messages?
#' @param Success Want to get success messages?
#' @family Internal Utilities
#' @keywords internal
OptionsMessage <- function(Error = T,
                           Warning = T,
                           Info = T,
                           Success = T) {
  if (!exists("Configuration_env", envir = .GlobalEnv)) {
    Configuration_env <<- new.env()
  }

  Configuration_env$Message$Configuration$Error <- Error
  Configuration_env$Message$Configuration$Warning <- Warning
  Configuration_env$Message$Configuration$Info <- Info
  Configuration_env$Message$Configuration$Success <- Success
  Configuration_env$Message$Configuration$SuperUser <- FALSE
  Configuration_env$Message$Configuration$StatusMessage <- "default"


  RStudioVersion <- try(RStudio.Version(), silent = TRUE)


  if (is.null(attributes(RStudioVersion)$class)) {
    RStudioVersion <- paste(RStudioVersion$version)
    RStudioVersion <- as.numeric(substr(RStudioVersion, 1, 3))
  } else {
    RStudioVersion <- 1.0
  }

  Configuration_env$Message$Configuration$RStudioVersion <- RStudioVersion
}
