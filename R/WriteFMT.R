#' @title  WriteFmt (Internal Function used in ExportToSQL)
#' @description TODO
#' @examples TODO
#' @param InputData TODO
#' @param ServerPath TODO
#' @author JTA - The Data Scientists
#' @keywords internal
#' @family Internal Utilities
#' @seealso \code{\link{TAPChunks}}
WriteFmt <- function(InputData, ServerPath = ".") {
  start.time <- Sys.time()
  table.name <- "fmt_file"

  if (missing(InputData)) {
    TAPChunks:::catError("Argument 'InputData' is missing, with no default.")
  } else if (missing(table.name)) {
    TAPChunks:::catError("Argument 'table.name' is missing, with no default.")
  }
  # create temporary folder with FMT file
  if (file.exists(paste(ServerPath, "WasteFile", sep = "/")) == F) {
    dir.create(paste(ServerPath, "WasteFile", sep = "/"), showWarnings = FALSE)
  }
  # set up FMT file
  sql.server.version <- "12.0"
  col.names <- names(InputData)
  header <- c(sql.server.version, length(col.names))
  tab <- "\\t"
  newline <- "\\r\\n"
  character.collaction <- "SQL_Latin1_General_CP1_CI_AS"

  is.text <- sapply(InputData, is.character)
  body <- data.table::data.table(
    host_field_order = seq_along(col.names),
    host_data_type = "SQLCHAR",
    prefix_length = 0,
    data_length = 0,
    terminator = paste0("\"", c(rep(tab, length(col.names) - 1), newline), "\""),
    server_column_order = seq_along(col.names),
    server_column_name = paste0("\"", col.names, "\""),
    collation = ifelse(is.text, character.collaction, "\"\"")
  )

  file.name <- sprintf(paste(ServerPath, "WasteFile/%s.fmt", sep = "/"), table.name)

  writeLines(header, file.name)
  write.table(body, file.name,
    append = TRUE,
    quote = FALSE,
    sep = "\t",
    na = "",
    row.names = FALSE,
    col.names = FALSE
  )

  TAPChunks:::catSuccess(paste(
    "The ", table.name, ".FMT has been created successfully:\n\t. Processed in: ",
    table.name, TAPChunks:::ConvertTime(difftime(Sys.time(), start.time, units = "secs"))
  ))
}
