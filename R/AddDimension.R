#' @title ADD Dimension
#' @description This is an internal function called by other functions to add Geog, Product or Segment
#' hierarchies.
#' @details The function requires MS Corp access to access the RDR sql database. The function will allow
#' the user to chose the particular hierarchy required.
#' @examples TAPChunks:::AddDimension(Data = Data, Dimension = "P")
#' @author JTA - The Data Scientists
#' @return Returns the submitted data chunk with the additional dimensional columns added.
#' @param Data A data chunk
#' @param Dimension A single character used to select the dimension to be added.
#' \itemize{
#' \item "G" For Geography
#' \item "S" For Segment
#' \item "P" For Product
#' }
#' @param SelectHierarchy The name of the hierarchy required.
#' @keywords internal
#' @family Internal Utilities
AddDimension <- function(Data, Dimension = "G", SelectHierarchy = NULL) {
  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) TAPChunks:::catError("Data file not specified.")

  ExpDimension <- switch(Dimension,
    "G" = "Geography",
    "P" = "Product",
    "S" = "Segment"
  )


  MapHierarchy <-
    TAPChunks:::GetMapping("H")[DimensionCategory == ExpDimension, -"Name", with = F]

  if (ExpDimension == "Product") {
    MapHierarchy <- JoinChunks(
      MapHierarchy[HierarchyName == "DimH_Product"],
      MapHierarchy[HierarchyName != "DimH_Product",

        JoinChunks(
          data.table::data.table(
            DisplayName = "Product0_Variant",
            Level_ID = 1,
            IsVariant = "Y"
          )
          ,
          .SD[, .(DisplayName, Level_ID = Level_ID + 1, IsVariant)]
        )
        ,
        by = c("DimensionCategory", "HierarchyName", "FriendlyName")
      ]
    )
    MapHierarchy[DisplayName == "Product0_Variant", DisplayName := "variable"]
  }

  if (ExpDimension == "Segment") {
    MapHierarchy$Level_ID <- MapHierarchy$Level_ID + 1
    MapHierarchy <- JoinChunks(
      MapHierarchy,
      MapHierarchy[, data.table::data.table(
        DisplayName = "Segment_Code",
        Level_ID = 1,
        IsVariant = "Y"
      ), by = c("DimensionCategory", "HierarchyName", "FriendlyName")]
    )
  }
  if (ExpDimension == "Geography") {
    MapHierarchy[DisplayName == "Dim_OrgGeo_5Country", DisplayName := "Country"]
    MapHierarchy[HierarchyName == "DimH_DSSTGeo" & DisplayName == "Area", DisplayName := "Area_DSST"]
  }

  MapHierarchy$DisplayName <-
    gsub("Parent", "", MapHierarchy$DisplayName)

  if (length(intersect(MapHierarchy$DisplayName, names(Data))) == 0) Data <- TAPChunks:::AddFirmographics(Data)

  MapHierarchyDIM <-
    MapHierarchy[DisplayName %in% intersect(MapHierarchy$DisplayName, names(Data)), .SD[Level_ID ==
      min(Level_ID)], by = c("DimensionCategory", "HierarchyName")]

  MapHierarchyDIM[, DimInData := "Y"]

  MapHierarchy <-
    merge(MapHierarchyDIM,
      MapHierarchy,
      by = names(MapHierarchy),
      all = T
    )
  MapHierarchy[order(Level_ID), DimLevel := .SD[DimInData == "Y", Level_ID], by = c("DimensionCategory", "HierarchyName")]
  MapHierarchy <-
    MapHierarchy[!is.na(DimLevel) & DimLevel <= Level_ID]

  MAP <- TAPChunks:::GetMapping(Dimension)

  if (Dimension == "P") {
    data.table::setnames(MAP, "Product0_Variant", "variable")
  }

  if (Dimension == "S") {
    MAP$Segment_Code <- gsub("^\\d\\d\\d_", "", MAP$Segment_Code)
  }

  if (is.null(SelectHierarchy)) {
    Question <-
      MapHierarchy[IsVariant == "N", .(FriendlyName, DisplayName, DimInData)]
    Question[DimInData == "Y", DisplayName := paste0(DisplayName)]

    Question <-
      Question[, .(string = paste0(FriendlyName, sprintf(
        "(%s)", paste(DisplayName, collapse = ", ")
      ))),
      by = "FriendlyName"
      ]$string

    Question <- c(Question, "Select the columns")

    Question <-
      paste(1:length(Question),
        Question,
        sep = "-",
        collapse = "\n\t"
      )
    format_right <- F
    while (format_right == F) {
      answer <-
        readline(cat(
          paste0(
            "Choose Hierarchy: please, insert numbers separated by commas or press 0 to cancel the process: \n\t",
            Question
          )
        ))

      if (answer == 0) {
        TAPChunks:::catError("No source selected.")
      }

      if (is.na(sum(as.numeric(strsplit(answer, ",")[[1]])))) {
        TAPChunks:::catWarning("You should put number separate by commas.")
        format_right <- F
      } else {
        answer <- as.numeric(strsplit(answer, ",")[[1]])
        format_right <- T
      }
    }
  } else {
    SelectHierarchy <- gsub("\\s", "", SelectHierarchy)

    SelectHierarchy <- strsplit(SelectHierarchy, ",")[[1]]
    selectOptions <-
      gsub("\\s", "", unique(MapHierarchy$FriendlyName))


    answer <- NULL
    for (i in selectOptions) {
      answer <- c(answer, i %in% SelectHierarchy)
    }

    answer <- which(answer == T)

    if (length(answer) != length(SelectHierarchy)) {
      TAPChunks:::catError("Required hierarchies don't exist. ")
    }
  }

  select_column <- NULL
  if (length(intersect(answer, length(unique(MapHierarchy$HierarchyName)) + 1)) == 1) {
    columns <-
      sort(unique(MapHierarchy[IsVariant != "Y"]$DisplayName))
    Question <-
      paste(1:length(columns),
        columns,
        sep = "-",
        collapse = "\n\t"
      )

    format_right <- F
    while (format_right == F) {
      answer2 <-
        readline(cat(
          paste0("Choose the columns, separated by commas:\n\t", Question)
        ))

      if (is.na(sum(as.numeric(strsplit(answer2, ",")[[1]])))) {
        TAPChunks:::catWarning("You should put number separate by comas.")
        format_right <- F
      } else {
        answer2 <- as.numeric(strsplit(answer2, ",")[[1]])
        format_right <- T
      }
    }

    select_column <- c(select_column, paste(columns[answer2]))
    answer <-
      setdiff(answer, length(unique(MapHierarchy$HierarchyName)) + 1)
  }

  select_column <-
    c(select_column, paste(unique(MapHierarchy[FriendlyName %in% unique(MapHierarchy$FriendlyName)[answer], DisplayName])))
  select_column <- unique(select_column)

  if (length(setdiff(select_column, names(Data))) == 0) {
    TAPChunks:::catInfo("Already exist dimension.")
    return(Data)
  }

  MapHierarchy <- merge(
    MapHierarchy,
    data.table::data.table(
      DisplayName = select_column, request =
        "Y"
    ),
    all = T,
    by = "DisplayName"
  )

  MapHierarchyADD <-
    MapHierarchy[is.na(DimInData) &
      request == "Y", .SD[1], by = "DisplayName"]


  for (i in unique(MapHierarchy$HierarchyName)) {
    temp.Product <- MapHierarchy[HierarchyName == i]

    temp.Product <-
      temp.Product[DimInData == "Y" |
        DisplayName %in% MapHierarchyADD[HierarchyName == i, DisplayName]]

    MapHierarchy[HierarchyName == i, DisplayName]

    temp.map <-
      unique(MAP[, temp.Product[!(is.na(DimInData) &
        is.na(request))]$DisplayName, with = F])

    Data <-
      merge(Data, unique(temp.map), by = temp.Product[DimInData == "Y", DisplayName])

    if (nrow(temp.map) != nrow(unique(temp.map))) {
      doubles <- temp.map[, .N, by = "Product0_Variant"][N > 1]
      doubles <- doubles[!Product0_Variant %in% c("[SW CCM-Cost multi-label product mapping]", "[SW CCM-Usage multi-label product mapping]")][, I := .I]
      text <- NULL
      for (i in doubles$I) {
        text <- c(text, paste(doubles[i]$Product0_Variant, doubles[i]$N, "\n"))
      }
      TAPChunks:::catError(paste("Need check the mappings, have doubles:\n", text, collapse = "\t- "))
    }
  }

  removeCol <- unique(MapHierarchy[is.na(request), DisplayName])
  removeCol <- intersect(removeCol, names(Data))

  if (length(removeCol) != 0) {
    Data <- Data[, .SD, .SDcols = !removeCol]
  }

  Data <- Data[, TAPChunks:::OrderColumn(Data), with = F]
  return(Data)
}
