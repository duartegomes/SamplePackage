#' @title  Create Table in SQL
#' @description TODO
#' @examples TODO
#' @param Connection TODO
#' @param InputData TODO
#' @param DbSchema TODO
#' @param DbTableName TODO
#' @author JTA - The Data Scientists
#' @family Internal Utilities
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
CreateTable <- function(Connection, InputData, DbSchema, DbTableName) {
  if (missing(Connection) || class(Connection) != "RODBC") {
    TAPChunks:::catError("Argument 'Connection' is missing, with no default or is not a RODBC Connection.")
  } else if (missing(InputData)) {
    TAPChunks:::catError("Argument 'InputData' is missing, with no default.")
  } else if (missing(DbSchema)) {
    TAPChunks:::catError("Argument 'DB Schema' is missing, with no default.")
  } else if (missing(DbTableName)) {
    TAPChunks:::catError("Argument 'DB table name' is missing, with no default.")
  } else {
    create.schema <- gsub("\n", "", sprintf("IF NOT EXISTS (
                                            SELECT  SCHEMA_NAME
                                            FROM    INFORMATION_SCHEMA.SCHEMATA
                                            WHERE   SCHEMA_NAME = '%s' )
                                            BEGIN
                                            EXEC sp_executesql N'CREATE SCHEMA %s'
                                            END", DbSchema, DbSchema))
    DT <- data.table::data.table(InputData)

    name.col <- paste0("[", names(head(DT)), "]")

    tryCatch({
      DT[is.na(DT)] <- "na"
      size.class <- c(
        sapply(data.table::data.table(DT[, sapply(head(DT), is.character), with = F]), function(x) {
          max(nchar(x)) + 15
        }),
        sapply(data.table::data.table(DT[, sapply(head(DT), is.factor), with = F]), function(x) {
          max(nchar(levels(x))) + 15
        })
      )
      size.class[is.na(size.class)] <- 50
    }
    ,
    error = function(err) {
      size.class <- 50
    }
    )

    if (!is.null(attributes(size.class))) {
      names(size.class) <- paste0("[", names(size.class), "]")
    }

    class.DT <- NULL
    n <- 1
    for (i in sapply(DT, class)) {
      i <- i[1][[1]]
      if (i == "numeric") {
        new.class <- "FLOAT"
      } else if (i == "integer") {
        new.class <- "INT"
      } else if (i == "logical") {
        new.class <- "BIT"
      } else if (i == "date") {
        new.class <- " DATE"
      } else if (i == "factor" | i == "character" | i == "ordered") {
        new.class <- sprintf("VARCHAR(%s)", size.class[name.col[n]])
      } else {
        new.class <- "VARCHAR(50)"
      }
      n <- n + 1

      class.DT <- c(class.DT, new.class)
    }
    table.name <- paste(DbSchema, DbTableName, sep = ".")
    create.table <- sprintf(
      "IF OBJECT_ID (N'%s', N'U') IS NOT NULL DROP TABLE %s; CREATE TABLE %s (%s);",
      table.name, table.name, table.name, paste(paste(name.col, class.DT), collapse = ", ")
    )

    result <- RODBC::sqlQuery(Connection, create.schema)
    if (length(result) == 2) {
      TAPChunks:::catInfo(sprintf("ERROR-> %s", result[1]))
      TAPChunks:::catError("Process Break: error creating schema.")
    }
    result <- RODBC::sqlQuery(Connection, create.table)
    if (length(result) == 2) {
      TAPChunks:::catInfo(sprintf("ERROR-> %s", result[1]))
      TAPChunks:::catError("Process Break: error creating table.")
    }
    return(TAPChunks:::catSuccess(sprintf("Table %s has been created successfully!", paste(DbSchema, DbTableName, sep = "."))))
  }
}
