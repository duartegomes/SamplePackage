#' @title UseAsSlicer
#' @description TODO
#' @details TODO
#' @family Reporting and saving tools
#' @param Data A data table
#' @param ReportName TODO
#' @export
#' @author JTA - The Data Scientists
#' @return TODO
#' @seealso \code{\link{TAPChunks}}
UseAsSlicer <- function(Data, ReportName = "test") {
  if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
  if (is.null(Data)) TAPChunks:::catError("Data file not specified.")

  temp.folder <- ShowCachePath()
  if (length(list.files(temp.folder)) != 0) {
    if (sum(list.files(temp.folder) == ReportName) == 1) {
      answer <-
        menu(c("Yes", "No"), title = "Is there a report with the same name, do you want to continue?\n")
      if (answer == 1) {
        unlink(file.path(temp.folder, ReportName),
          recursive = T,
          force = T
        )

        dir.create(file.path(temp.folder, ReportName))

        TAPChunks:::catInfo("The folder has been replaced.")
      } else {
        TAPChunks:::catError("The operation was interrupted.")
      }
    } else {
      dir.create(file.path(temp.folder, ReportName))
    }
  } else {
    dir.create(file.path(temp.folder, ReportName))
  }

  test.prod <-
    intersect(names(Data), gsub(
      "_Name|Parent",
      "",
      c(Product_Hierarchy[Dimension == "Product"]$ViewColumn, "variable")
    ))
  test.geo <-
    intersect(names(Data), gsub(
      "_Name|Parent",
      "",
      c(Product_Hierarchy[Dimension == "Geography"]$ViewColumn, "WCountry")
    ))
  test.seg <-
    intersect(names(Data), gsub(
      "_Name|Parent",
      "",
      c(Product_Hierarchy[Dimension == "Segment"]$ViewColumn, "Segment_Code")
    ))
  test.os <-
    intersect(names(Data), c("Platform", "Architecture", "Ecosystem", "OS"))

  if (length(test.prod) > 1) {
    map_prod <- unique(Data[, test.prod, with = F])[, ID_Prod := .I]
    Data <- merge(Data, map_prod, by = test.prod)[, !test.prod, with = F]
    TAPChunks:::catInfo("Product map was successfully created.")
    TAPChunks:::SaveLocal(map_prod, Path = file.path(temp.folder, ReportName))
  } else {
    TAPChunks:::catInfo("No Product dimensions found.")
  }

  if (length(test.geo) > 1) {
    map_geo <- unique(Data[, c(test.geo), with = F])[, ID_Geo := .I]
    Data <- merge(Data, map_geo, by = test.geo)[, !test.geo, with = F]
    TAPChunks:::catInfo("Geography map was successfully created.")
    TAPChunks:::SaveLocal(map_geo, Path = file.path(temp.folder, ReportName))
  } else {
    TAPChunks:::catInfo("No Geography dimensions found.")
  }

  if (length(test.seg) > 1) {
    map_seg <- unique(Data[, test.seg, with = F])[, ID_Seg := .I]
    Data <- merge(Data, map_seg, by = test.seg)[, !test.seg, with = F]
    TAPChunks:::catInfo("Segment map was successfully created.")
    TAPChunks:::SaveLocal(map_seg, Path = file.path(temp.folder, ReportName))
  } else {
    TAPChunks:::catInfo("No Segment dimensions found.")
  }

  if (length(test.os) > 1) {
    map_OS <- unique(Data[, test.os, with = F])[, ID_OS := .I]
    Data <- merge(Data, map_OS, by = test.os)[, !test.os, with = F]
    TAPChunks:::catInfo("OS map was successfully created.")
    TAPChunks:::SaveLocal(map_OS, Path = file.path(temp.folder, ReportName))
  } else {
    TAPChunks:::catInfo("No OS dimensions found.")
  }
  fact_table <- Data
  TAPChunks:::SaveLocal(fact_table, Path = file.path(temp.folder, ReportName))
}
