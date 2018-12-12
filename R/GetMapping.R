#' @title Get Mapping table from RDR
#' @description This function returns the Mapping table from RDR
#' @family Internal Utilities
#' @author JTA - The Data Scientists
#' @param Dimension name the dimension
#' @return The mapping table
#' @seealso \code{\link{TAPChunks}}
#' @keywords internal
GetMapping <- function(Dimension = NULL) {
  if (is.null(Dimension)) {
    TAPChunks:::catError("Argument 'Dimension' is missing, with no default. Please use 'S','P','G' or 'O'.")
  }

  if (!TAPChunks:::TestNet()) TAPChunks:::catError("\nPlease check your network connection!")

  Dimension <- toupper(Dimension)

  SQLTable <- switch(Dimension,
    "S" = "[svw_DimH_DemogsSegment]",
    "P" = "[svw_Custom_ProductTaxonomy]",
    "G" = "[DSSTGeo] t1 FULL OUTER JOIN [MDSDB].[mdm].[svw_DimH_DemogsGeo] t2 ON t1.Dim_OrgGeo_5Country_Name = t2.ParentCountry_Name;",
    "O" = "[svw_DimH_PlatOS]",
    "H" = "[svw_Custom_HierarchyMetadata]"
  )

  if (is.null(SQLTable)) {
    TAPChunks:::catError("Dimension does not found. Please use 'S','P','G','H' or 'O'.")
  }

  connection <- TAPChunks:::OpenSqlConnection("CAPENGSQL2\\TAP,51567", "MDSDB")
  if (Dimension == "O") {
    query <- sprintf("SELECT * FROM [mdm].%s ORDER BY matchPattern_ID", SQLTable)
  } else {
    query <- sprintf("SELECT * FROM [mdm].%s", SQLTable)
  }

  hierarchy <- data.table::data.table(RODBC::sqlQuery(connection, query))

  close(connection)

  if (Dimension != "H") {
    column_name <- grep("_Name", names(hierarchy), value = T)
    column_name <- setdiff(column_name, "Variant_Name")
    keep <- intersect(names(hierarchy), "Segment_Code")

    hierarchy <- hierarchy[, c(column_name, keep), with = F]
    hierarchy <- hierarchy[, sapply(hierarchy, class) != "logical", with = F]
    hierarchy <- hierarchy[, lapply(.SD, as.factor)]
    hierarchy <- unique(hierarchy)

    names(hierarchy) <- gsub("(Parent)|(_Name)", "", names(hierarchy))
  }

  if (Dimension == "G") {
    names(hierarchy)[1] <- "Area_DSST"
    hierarchy$Dim_OrgGeo_5Country <- NULL
  }

  return(hierarchy)
}
