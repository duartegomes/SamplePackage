#' @title Apply/Calculate penetration in Data Set
#' @param Data Dataset where the penetration will be applied
#' @param Dimension Dimensions to be included in the final output
#' @param PenetrationDimension Dimension that is the Penetration target
#' @export
#' @family Reporting and saving tools
#' @import data.table
#' @author JTA - The Data Scientists
#'
ApplyPenetration <-
  function(Data, Dimension, PenetrationDimension) {
    if (all(class(Data) == "data.frame")) Data <- data.table::as.data.table(Data)
    if (is.null(Data)) TAPChunks:::catError("Data file not specified.")

    if (sum(Dimension %in% PenetrationDimension) != 0) {
      TAPChunks:::catError(
        "The same dimension can not appear simultaneously in the parameters Dimension and PenetrationDimension."
      )
    }
    if (sum(c(Dimension, PenetrationDimension) %in% names(Data)) != length(c(Dimension, PenetrationDimension))) {
      TAPChunks:::catError(
        "There is no match between the source attributes and the required dimensions."
      )
    }

    HierarchyMAP <- TAPChunks:::GetMapping("H")[IsVariant != "Y"]
    ## TODO-temp solution
    HierarchyMAP$DisplayName <-
      gsub("Parent", "", HierarchyMAP$DisplayName)

    HierarchyMAP <-
      HierarchyMAP[(!(Level_ID == 2 & DisplayName == "Product0")) & HierarchyName != "DimH_DSSTGeo"]

    HierarchyDIM <-
      unique(HierarchyMAP[DisplayName %in% Dimension, .(DimensionCategory, DisplayName, Level_ID)])
    metadataDIM <-
      HierarchyDIM[, .SD[Level_ID == min(Level_ID)], by = "DimensionCategory"]$DisplayName

    if (sum(Dimension %in% HierarchyMAP$DisplayName) != length(Dimension)) {
      extraCols <- setdiff(Dimension, HierarchyMAP$DisplayName)

      metadataDIM <- c(metadataDIM, extraCols)

      HierarchyDIM <- JoinChunks(
        HierarchyDIM,
        data.table::data.table(
          DimensionCategory = paste0("cat", 1:length(extraCols)),
          DisplayName = extraCols,
          Level_ID = 1
        )
      )
    }

    message <-
      unique(HierarchyDIM[order(Level_ID), .(DimensionCategory, DisplayName)])
    message[DisplayName %in% metadataDIM, DisplayName := paste0(DisplayName)]
    message <-
      message[, paste0(
        DimensionCategory,
        "(",
        paste0(DisplayName, collapse = ", "),
        ")"
      ), by = DimensionCategory]$V1

    TAPChunks:::catInfo(sprintf("Dimension: \n\t-%s", paste(message, collapse = "\n\t-")))

    HierarchyPEN <-
      unique(HierarchyMAP[DisplayName %in% PenetrationDimension, .(DimensionCategory, DisplayName, Level_ID)])

    if (sum(PenetrationDimension %in% HierarchyMAP$DisplayName) != length(PenetrationDimension)) {
      extraCols <- setdiff(PenetrationDimension, HierarchyPEN$DisplayName)

      HierarchyPEN <- JoinChunks(
        HierarchyPEN,
        data.table::data.table(
          DimensionCategory = paste0("cat", 1:length(extraCols)),
          DisplayName = extraCols,
          Level_ID = 1
        )
      )
    }

    metadataPEN <-
      HierarchyPEN[, .SD[Level_ID == min(Level_ID)], by = "DimensionCategory"]$DisplayName

    message <-
      unique(HierarchyPEN[order(Level_ID), .(DimensionCategory, DisplayName)])
    message[DisplayName %in% metadataPEN, DisplayName := paste0(DisplayName)]
    message <-
      message[, paste0(
        DimensionCategory,
        "(",
        paste0(DisplayName, collapse = ", "),
        ")"
      ), by = DimensionCategory]$V1

    TAPChunks:::catInfo(sprintf(
      "Penetration filters: \n\t-%s",
      paste(message, collapse = "\n\t-")
    ))

    dataPenFill <- ShowSampleSize(Data, c(metadataDIM, metadataPEN))
    if ("dirty_sample" %in% names(dataPenFill)) TAPChunks:::catError("The Data isn't clean")

    dataDimension <- ShowSampleSize(Data, c(metadataDIM))

    longMap <- NULL
    for (i in c(metadataDIM, metadataPEN)) {
      if (is.null(longMap)) {
        longMap <- dataPenFill[, .(unique(get(i)))]
        data.table::setnames(longMap, "V1", i)
      } else {
        longMap <- longMap[, unique(dataPenFill[[i]]), by = names(longMap)]
        data.table::setnames(longMap, "V1", i)
      }
    }
    timestamp <- unique(Data$timestamp)
    longMap <- longMap[, timestamp, by = names(longMap)]


    HierarchyDIM <- JoinChunks(HierarchyDIM, HierarchyPEN)

    for (i in HierarchyDIM$DimensionCategory) {
      if (nrow(HierarchyDIM[DimensionCategory == i]) > 1) {
        map.temp <-
          unique(Data[, HierarchyDIM[DimensionCategory == i, DisplayName],
            with =
              F
          ])
        longMap <- merge(longMap,
          map.temp,
          all.x = T,
          by = intersect(names(longMap), names(map.temp))
        )
      }
    }
    longMap <- merge(
      longMap,
      dataPenFill,
      by = c(metadataDIM, metadataPEN, "timestamp"),
      all.x = T
    )
    longMap[, unweighted_total := unique(.SD[!is.na(unweighted_total), unique(unweighted_total)]),
      by =
        "timestamp"
    ]
    uniqueCol <- c("timestamp")
    longMap[is.na(longMap)] <- 0
    longMap <-
      merge(
        longMap,
        dataDimension,
        by = c(metadataDIM, uniqueCol),
        suffixes = c("_N", "_D"),
        all.x = T
      )
    return(longMap)
  }
