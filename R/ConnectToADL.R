#' @title  Making a connection to the data lake
#' @description
#' This function allows us to establish a connection to the Data Lake.  It is intended to
#' be used internally by other functions in this package.  If there is no network access or
#' the system cannot access the Data Lake the function returns NULL.  If a connection can be
#' made it returns an access token.
#' @param AdlClientId The client identifier
#' @param AdlClientSecret The client secret
#' @param AdlTenantId The tenant identifier
#' @importFrom curl new_handle handle_setform curl_fetch_memory
#' @import jsonlite
#' @family Internal Utilities
#' @examples TAPChunks:::ConnectToADL(AdlClientId     = "e20ebcde-ec2d-47ab-805a-9b157638bf04",
#'                                    AdlClientSecret = "9LDqQfc0U+4+S7JsosPQ3HD9eLVvw9PiQYQEXU55y00=",
#'                                    AdlTenantId     = "72f988bf-86f1-41af-91ab-2d7cd011db47")
#' @author JTA - The Data Scientists
#' @return Returns an access token (or NULL on error)
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
ConnectToADL <-
  function(AdlClientId = NULL,
             AdlClientSecret = NULL,
             AdlTenantId = NULL) {
    if (is.null(AdlClientId) & is.null(AdlClientSecret) & is.null(AdlTenantId)) {
      catSuperUser("Connection will be created from the yaml file information", "yaml file")
      if (is.null(Configuration_env$Source)) TAPChunks:::GetConfiguration()
      catSuperUser("Should run the GetConfiguration function before starting the tasks", "GetConfiguration")

      AdlClientId <- Configuration_env$Key$ADL$Client
      AdlClientSecret <- Configuration_env$Key$ADL$Secret
      AdlTenantId <- Configuration_env$Key$ADL$Tenant

      if (is.null(AdlClientId) & is.null(AdlClientSecret) & is.null(AdlTenantId)) TAPChunks:::catError("The Key configuration not found ", "Key")
    } else {
      catSuperUser("Connection was manually created", "yaml file")
    }


    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    h <- curl::new_handle()
    success <- try(curl::handle_setform(
      h,
      "grant_type" = "client_credentials",
      "resource" = "https://management.core.windows.net/",
      "client_id" = AdlClientId,
      "client_secret" = AdlClientSecret
    ))

    if (class(success) == "try-error") TAPChunks:::catError("The keys to open the ADL connection are invalid.", "keys")

    req <- try(
      curl::curl_fetch_memory(paste0(
        "https://login.windows.net/",
        AdlTenantId,
        "/oauth2/token"
      ),
      handle = h
      ),
      silent = T
    )

    if (class(req) == "try-error") TAPChunks:::catError("The login to open the ADL failed.", "login")

    if (req$status_code == "401") TAPChunks:::catError("The keys to open the ADL connection are invalid.", "keys")

    res <- jsonlite::fromJSON(rawToChar(req$content))

    catSuperUser("Connection to the ADL was successfully done.", "ADL")
    return(res$access_token)
  }
