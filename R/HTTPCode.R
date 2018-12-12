#' @title  Decode the Http Error codes into messages
#' @description
#' This function decode the Http Error codes into messages
#' @param ErrorCode The client identifier
#' @family Internal Utilities
#' @examples TAPChunks:::httpCode(200)
#' @author JTA - The Data Scientists
#' @return Returns a message as a string
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
HttpCode <- function(ErrorCode = 0) {
  errorMsg <- switch(paste(ErrorCode),
    "200" = "OK - The ADL fetch reported success.",
    "201" = "Created - The request has been fulfilled, resulting in the creation of a new resource.",
    "202" = "Accepted - The request has been accepted for processing, but the processing has not been completed.",
    "204" = "No Content - The server successfully processed the request and is not returning any content.",
    "205" = "Reset Content - The server successfully processed the request, but is not returning any content.",
    "400" = "Bad Request - The server cannot or will not process the request due to an apparent client error.",
    "401" = "Unauthorized - Similar to 403 Forbidden, but specifically for use when authentication is required and has failed or has not yet been provided.",
    "403" = "Forbidden - The request was valid, but the server is refusing action. The user might not have the necessary permissions for a resource.",
    "404" = "Not Found - The requested resource could not be found but may be available in the future.",
    "405" = "Method Not Allowed - A request method is not supported for the requested resource.",
    "500" = "Internal Server Error.",
    "502" = "Bad Gateway - The server was acting as a gateway or proxy and received an invalid response from the upstream server.",
    "503" = "Service Unavailable - The server is currently unavailable."
  )
  if (is.null(errorMsg)) errorMsg <- "Error unknown."
  return(errorMsg)
}
