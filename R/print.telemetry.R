#' @title S3 print handler for telemetry data
print.telemetry <- function(tel){
  rows <- format(nrow(p), big.mark = ",")
  if (any(names(p)=="uuid")) orgs <- format(length(levels(p$uuid)), big.mark = ",") else orgs <- "unknown"
  cat(sprintf("Telemetry data with %s rows from %s organizations.\n\n", rows, orgs))
  tel <- head(tel,3)
  if (any(names(tel)=="uuid")) data.table::set(tel, j = "uuid", value = paste0(substr(tel$uuid,1,6),"..."))
  if (any(names(tel)=="row_id")) data.table::set(tel, j = "row_id", value = NULL)
  if (any(names(tel)=="born_on_year")) data.table::set(tel, j = "born_on_year", value = paste0(substr(tel$born_on_year,1,4),"..."))
  # dist[, pos := regexpr("-", born_on_year) - 1]
  # dist <- dist[pos > 1]
  # dist[, born := as.numeric(substr(born_on_year, 1, pos))]

  data.table:::print.data.table(tel)
}
