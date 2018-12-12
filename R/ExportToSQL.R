#' @title Export to SQL
#' @description This function inserts a R table into SQL
#' @param Connection OpenSqlConnection(Connection, DataBase) TODO
#' @param InputData class = Data.frame | Data.table, table in r
#' @param DbSchema class = string, name of schema in SQL
#' @param DbTableName class = string, name for table in SQl
#' @param ServerPath path where the FMT and TSV file
#' @param Append default = FALSE, When Append = F it creates a new table. If not, adds new information to an exist table in SQL
#' @return None
#' @family Reporting and saving tools
#' @export
ExportToSQL <- function(Connection, InputData, DbSchema, DbTableName, ServerPath = ".", Append = FALSE) {
  if (missing(Connection) || class(Connection) != "RODBC") {
    TAPChunks:::catError("Argument 'Connection' is missing, with no default or is not a RODBC Connection.")
  } else if (missing(InputData)) {
    TAPChunks:::catError("Argument 'InputData' is missing, with no default.")
  } else if (missing(DbSchema)) {
    TAPChunks:::catError("Argument 'DbSchema' is missing, with no default.")
  } else if (missing(DbTableName)) {
    TAPChunks:::catError("Argument 'DbTableName' is missing, with no default.")
  }

  start.time <- Sys.time()

  if (ServerPath == ".") {
    ServerPath <- ShowCachePath()
  }
  TAPChunks:::WriteFmt(InputData, ServerPath)
  name.file <- sprintf(paste(ServerPath, "WasteFile/%s.tsv", sep = "/"), "tsv_file")

  write.table(InputData, file = name.file, quote = FALSE, sep = "\t", row.names = FALSE, col.names = T, na = "")

  # data.table::fwrite(InputData, file = name.file, quote = FALSE, sep = "\t", row.names = FALSE, col.names = T, na = "")

  DbSchema <- gsub("\\[|\\]", "", DbSchema)
  DbSchema <- sprintf("[%s]", DbSchema)
  DbTableName <- gsub("\\[|\\]", "", DbTableName)
  DbTableName <- sprintf("[%s]", DbTableName)


  if (Append == FALSE) {
    TAPChunks:::CreateTable(Connection, InputData, DbSchema, DbTableName)
    bulk.query <- paste0(
      paste("BULK INSERT", paste(DbSchema, DbTableName, sep = ".")),
      paste(" FROM", paste0("'", name.file, "'")),
      paste(
        " WITH (CODEPAGE = 1252, FORMATFILE = ",
        paste0("'", paste(gsub("tsv", "fmt", name.file), sep = "/"), "'"), ","
      ),
      paste(" FIELDTERMINATOR = '\t',  FIRSTROW = 2, ROWTERMINATOR = '\n', TABLOCK ,KEEPNULLS)")
    )

    result <- RODBC::sqlQuery(Connection, bulk.query)
    if (length(result) == 2) {
      TAPChunks:::catInfo(sprintf("ERROR-> %s", result[1]))
      catError("Bulk ERROR")
    }
  } else {
    TAPChunks:::CreateTable(Connection, InputData, DbSchema = "temporary", DbTableName = "tab")
    bulk.query <- paste0(
      "BULK INSERT temporary.tab",
      paste(" FROM", paste0("'", name.file, "'")),
      paste(
        " WITH (CODEPAGE = 1252, FORMATFILE = ",
        paste0("'", paste(gsub("tsv", "fmt", name.file), sep = "/"), "'"), ","
      ),
      paste(" FIELDTERMINATOR = '\t',  FIRSTROW = 2, ROWTERMINATOR = '\n', TABLOCK,KEEPNULLS )")
    )

    result <- RODBC::sqlQuery(Connection, bulk.query)
    if (length(result) == 2) {
      TAPChunks:::catInfo(sprintf("ERROR-> %s", result[1]))
      catError("Bulk ERROR")
    }

    query <- sprintf(
      "SELECT COUNT(*) as Rows FROM sys.tables T INNER JOIN sys.schemas S ON T.schema_id=S.schema_id WHERE S.name='%s' and T.name='%s'",
      gsub("\\[|\\]", "", DbSchema), gsub("\\[|\\]", "", DbTableName)
    )

    exist.table <- data.table::data.table(RODBC::sqlQuery(Connection, query))

    if (exist.table$Rows == 0) {
      TAPChunks:::CreateTable(Connection, InputData, DbSchema, DbTableName)
      TAPChunks:::catInfo(sprintf("The table %s was created because was not found in the DB.", DbTableName))
    }

    columns <- sprintf("[%s]", paste(names(InputData), collapse = "],["))
    append_query <- sprintf(
      "INSERT INTO %s.%s (%s) SELECT %s FROM [temporary].[tab]",
      DbSchema, DbTableName, columns, columns
    )
    result <- RODBC::sqlQuery(Connection, append_query)
    if (length(result) == 2) {
      TAPChunks:::catInfo(sprintf("ERROR-> %s", result[1]))
      catError("ERROR inserting data")
    }

    result <- RODBC::sqlQuery(Connection, "DROP TABLE [temporary].[tab]")
    if (length(result) == 2) {
      TAPChunks:::catInfo(sprintf("ERROR-> %s", result[1]))
      catError("ERROR Dropping data")
    }
  }
  unlink(list.files(paste(ServerPath, "WasteFile", sep = "/"), full.names = T, include.dirs = F, ignore.case = T))
  TAPChunks:::catSuccess(sprintf("Success! Processed in: %s ", TAPChunks:::ConvertTime(difftime(Sys.time(), start.time, units = "secs"))))
}
