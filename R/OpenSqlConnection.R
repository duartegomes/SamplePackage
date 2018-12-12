#' @title Open SQL Connection
#' @description This function can open connection with SQL without create a odbc connection
#' @param Server name of server SQL
#' @param Database name of database SQL
#' @param Uid user identification of server SQL
#' @param Pwd password of server SQL
#' @import RODBC
#' @return Connection with SQL
#' @author JTA - The Data Scientists
#' @family Internal Utilities
#' @keywords internal
OpenSqlConnection <- function(Server, Database, Uid = NULL, Pwd = NULL) {
  if (missing(Server)) {
    TAPChunks:::catError("Argument 'server' is missing, with no default")
  } else if (missing(Database)) {
    TAPChunks:::catError("Argument 'database' is missing, with no default")
  }
  # create the connection path
  name.connection <- paste0("driver={SQL Server};server=", Server, ";database=", Database)

  # if have uid and pwd, we add into the connection path
  if (!is.null(Uid)) {
    if (!is.null(Pwd)) {
      name.connection <- paste0(name.connection, ";Uid=", Uid, ";Pwd=", Pwd)
    }
  } else {

    # add trusted connection to path
    name.connection <- paste0(name.connection, ";trusted_connection=true")
  }

  # Connect with the path
  sql.connection <- RODBC::odbcDriverConnect(name.connection)

  # log message
  if (class(sql.connection) == "RODBC") {
    catSuperUser(paste0("Connection successful: \n\t\t-Server  : ", Server, "\n\t\t-Database: ", Database, "\n"))
  } else {
    TAPChunks:::catError("Connection failed")
  }

  return(sql.connection)
}
