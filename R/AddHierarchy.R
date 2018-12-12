#' @title Add Hierarchy
#' @description
#' This function allows adding all hierarchies at same time. The user can define
#' which dimension wants to add to the dataset.  This is an internal function that
#' is no longer used and should be removed in a future release.
#' @family Internal Utilities
#' @inheritSection AddGeoHierarchy Reference Data Repository
#' @inheritSection AddGeoHierarchy Geo Hierarchy
#' @inheritSection AddSegmentHierarchy Segment Hierarchy
#' @inheritSection AddProductHierarchy Product Hierarchy
#' @import data.table
#' @keywords internal
#' @author JTA - The Data Scientists
#' @param Data A data chunk
#' @param Dimension Dimension to be added to the dataset. i.e "G" "S" "O" "P"
#' @param Hierarchy TODO
#' @seealso \code{\link{TAPChunks}}
AddHierarchy <- function(Data,
                         Dimension = NULL,
                         Hierarchy = NULL) {
  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) TAPChunks:::catError("Data file not specified.")

  if (!is.null(Hierarchy)) Hierarchy <- gsub("\\s", "", strsplit(Hierarchy, ",")[[1]])
  Data <- TAPChunks:::AddFirmographics(Data)

  if (!is.null(Dimension)) {
    Dimension <- toupper(Dimension)
    order <- NULL
    for (i in 1:nchar(Dimension)) {
      order <- c(order, substr(Dimension, i, i))
    }
    if (length(unique(order)) != nchar(Dimension)) {
      TAPChunks:::catError("Invalid format:\n\tDimensions were repeated.")
    }

    order <- gregexpr("G|S|P|O|V", Dimension)[[1]]
    if (length(order) != nchar(Dimension)) {
      TAPChunks:::catError("Invalid format:\n\tThere are unknown dimensions.")
    }

    answer <- NULL
    for (i in c("G", "S", "O", "P")) {
      new.answer <-
        substr(
          Dimension,
          gregexpr(i, Dimension)[[1]],
          gregexpr(i, Dimension)[[1]]
        )

      if (new.answer != "") {
        answer <- c(answer, new.answer)
      }
    }
  }

  if (is.null(Dimension)) {
    DimAvailable <- c("Geo", "Segment", "OS", "Product", "Vertical")
    Question <-
      paste(1:length(DimAvailable),
        DimAvailable,
        sep = "-",
        collapse = "\n\t"
      )
    format_right <- F
    while (format_right == F) {
      answer <-
        readline(cat(
          paste0(
            "Choose Hierarchy: please, insert numbers separated by commas\n\t",
            paste0(Question, "\n")
          )
        ))
      if (is.na(sum(as.numeric(strsplit(answer, ",")[[1]])))) {
        TAPChunks:::catWarning("You should put number separate by comas.")
        format_right <- F
      } else {
        answer <- as.numeric(strsplit(answer, ",")[[1]])
        format_right <- T
      }
    }
    answer <- substr(DimAvailable[answer], 1, 1)
  }
  H <-
    unique(TAPChunks:::GetMapping("H")[, .(dim = substr(DimensionCategory, 1, 1), FriendlyName)])
  H <- H[dim %in% answer ]
  H$FriendlyName <- gsub("\\s", "", H$FriendlyName)
  Hierarchy <- gsub("\\s", "", Hierarchy)

  checkHierarchy <- setdiff(Hierarchy, H$FriendlyName)
  if (length(checkHierarchy) != 0) catWarning(sprintf("The following hierarchies weren't detected:\n\t\t-%s", paste(checkHierarchy, collapse = "\n\t\t-")))

  Hierarchy <- intersect(Hierarchy, H$FriendlyName)
  Hierarchy <- unique(Hierarchy)

  for (i in answer) {
    Data <- switch(
      i,
      "G" = AddGeoHierarchy(
        Data,
        if (nrow(H[dim == "G" &
          FriendlyName %in% Hierarchy]) == 0) {
          NULL
        } else {
          paste0(H[dim == "G" & FriendlyName %in% Hierarchy]$FriendlyName, collapse = ", ")
        }
      )
      ,
      "S" = AddSegmentHierarchy(
        Data,
        if (nrow(H[dim == "S" &
          FriendlyName %in% Hierarchy]) == 0) {
          NULL
        } else {
          paste0(H[dim == "S" & FriendlyName %in% Hierarchy]$FriendlyName, collapse = ", ")
        }
      ),
      "O" = AddOSHierarchy(Data),
      "P" = AddProductHierarchy(
        Data,
        if (nrow(H[dim == "P" &
          FriendlyName %in% Hierarchy]) == 0) {
          NULL
        } else {
          paste0(H[dim == "P" & FriendlyName %in% Hierarchy]$FriendlyName, collapse = ", ")
        }
      ),
      "V" = Data
    )
  }
  Data <- Data[, TAPChunks:::OrderColumn(Data), with = F]

  return(Data)
}
