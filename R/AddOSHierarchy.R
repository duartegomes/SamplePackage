#' @title Add OS Mappings to a data chunk
#' @description
#' This function reads Operating System mappings from the RDR. The RDR typically
#' stores text patterns which are used to match to the os field in the raw data.
#'
#' This makes it easy for the user to deal with inconsistencies and to get a simple
#' and consistent hierarchy for OS. This function is mainly used for Workload and PCclient data.
#' @section OS Hierarchy:
#' The function will read the OS name from the data chunk and will match to the pattern
#' that is stored in the RDR.  After replacing all the names matched it will show the
#' percentage of OS values that were not able to be recognised. The original column
#' denoting operating system is not returned and the following columns are added:
#' \tabular{ll}{
#' \strong{Platform} \tab Server or Client \cr
#' \strong{Architecture} \tab \cr
#' \strong{Ecosystem} \tab \cr
#' \strong{OS} \tab }
#' @family RDR Integration Tools
#' @inheritParams AddGeoHierarchy
#' @param OSColumn The column name to be mapped. If none, it has as default 'device_os' or 'os_name'
#' @param Format 'Full' to show all OS mappings | 'Unknown' to show only OS that are unknown | 'NULL' to not create a report.
#' @inheritSection AddGeoHierarchy Reference Data Repository
#' @export
#' @examples AddOSHierarchy(TestClientChunk)
#' @author JTA - The Data Scientists
#' @return Returns a data table without the original column of the os and a new one named 'os.map'
#' @seealso \code{\link{TAPChunks}}
AddOSHierarchy <- function(Data, OSColumn, Format) {
  FileName <- deparse(substitute(Data))

  if (missing(Data)) {
    TAPChunks:::catError("Please input a valid Chunk")
  }
  if (missing(OSColumn)) {
    OSColumn <- names(Data)[match(c("os_name", "device_os"), names(Data), nomatch = F)]
    if (length(OSColumn) == 0) TAPChunks:::catError("Please select the OS column to map.")

    TAPChunks:::catInfo(sprintf("%s from %s will be mapped", OSColumn, deparse(substitute(Data))))
  } else {
    if (length(names(Data)[match(OSColumn, names(Data), nomatch = F)]) == 0) {
      TAPChunks:::catError("Please select a valid os column to map.")
    }
  }

  Data <- data.table::data.table(Data)
  Data.os <- Data[,
    .N,
    by = OSColumn
  ]
  data.table::setkeyv(Data.os, OSColumn)
  data.table::setkeyv(Data, OSColumn)

  Data.os[, OSColumn := eval(parse(text = OSColumn))]

  # remove special characters
  Data.os[, OSColumn := gsub("[^0-9A-Za-z-]", " ", OSColumn)]
  Data.os[, OSColumn := gsub("\\s+", " ", OSColumn)]

  # creating a higher hierarchy level to the os
  Data.os[, OS := "NA"]

  os.mapping <- TAPChunks:::GetMapping("O")

  # Pattern Matching of os
  for (i in 1:nrow(os.mapping)) {
    Data.os[
      OSColumn %in% grep(os.mapping[i]$MatchPattern,
        OSColumn,
        ignore.case = T,
        value = T
      )
      & OS == "NA",
      OS := os.mapping[i]$OS
    ]
  }


  Data.os[OS == "NA", OS := "Unknown"]

  if (!missing(Format)) {
    Format <- tolower(Format)

    if (Format == "unknown" | Format == "all") {
      TAPChunks:::RMarkdown(Data, Data.os, Format, FileName)
    }

    Path <- TAPChunks:::ShowCachePath()

    cache <- data.table(list.files(paste0(Path, "/Logs/OSMappings")))[, grep(".Rdata", V1, value = TRUE)]

    unlink(paste0(Path, "/Logs/OSMappings", "/", cache))
  }



  mapped <- Data.os[OS != "Unknown", sum(N)]
  total <- Data.os[, sum(N)]
  mapped <- round(mapped / total * 100, 2)

  unmapped <- Data.os[OS == "Unknown", data.table::uniqueN(OSColumn)]

  TAPChunks:::catInfo(sprintf(
    "%s%% OSs were mapped.\nThere are %s of Unknown OSs",
    mapped, unmapped
  ))

  Data <- Data[Data.os]
  Data <- Data[,
    !c(OSColumn, "N"),
    with = F
  ]

  os.mapping <- os.mapping[, !"MatchPattern"]
  os.mapping <- unique(os.mapping[OS %in% Data[, unique(OS)]])
  Data <- merge(Data, os.mapping, by = "OS", all.x = T)

  Data <- Data[,
    TAPChunks:::OrderColumn(Data),
    with = F
  ]

  return(Data)
}
