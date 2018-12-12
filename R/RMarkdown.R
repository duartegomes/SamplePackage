#' @title RMarkdown
#' @description
#' Create a html file that is saved in ShowCachePath()
#' @details TODO
#' @importFrom rmarkdown render
#' @family Internal Utilities
#' @keywords internal
#' @param Source A data chunk
#' @param MappingTable The column name to map. If none, it has as default 'device_os' or 'os_name'
#' @param Format 'Full' to show all OS mappings | 'Unknown' to show only OS that are unknown | 'NULL' to not create a report.
#' @author JTA - The Data Scientists
#' @return Returns a html file that is saved in ShowCachePath()
#' @seealso \code{\link{TAPChunks}}
RMarkdown <- function(Source, MappingTable = source.os, Format, FileName) {
  if (!missing(Format)) {
    timestamp <- Source[, unique(timestamp)]
    source.null <- MappingTable[, !c("N"), with = F]
    source.os.UNK <- source.null[OS == "Unknown"]
    objsave <- c("source.null", "source.os.UNK", "timestamp")
    Path <- ShowCachePath()

    if (!file.exists(file.path(Path, "Logs"))) {
      dir.create(file.path(Path, "Logs"))
    }

    if (!file.exists(file.path(Path, "Logs/OSMappings"))) {
      dir.create(file.path(Path, "Logs/OSMappings"))
    }

    save(list = objsave, file = paste0(Path, "/Logs/OSMappings/OSMapping.Rdata"))

    Format <- tolower(Format)

    if (Format == "all") {
      suppressMessages(capture.output(rmarkdown::render("LogsDefinition/OSMapping.Rmd", output_dir = paste0(Path, "/Logs/OSMappings"), output_file = paste0("OSMapping", FileName, ".html"))))

      TAPChunks:::catInfo(paste("Log page is available here:", paste0(Path, "/Logs/OSMappings/", "OSMapping", FileName, ".html")))
    }

    if (Format == "unknown") {
      suppressMessages(capture.output(rmarkdown::render("LogsDefinition/OSMappingUnknown.Rmd", output_dir = paste0(Path, "/Logs/OSMappings"), output_file = paste0("OSMappingUnknown", FileName, ".html"))))

      TAPChunks:::catInfo(paste("Log page is available here: ", paste0(Path, "/Logs/OSMappings/", "OSMappingUnknown", FileName, ".html")))
    }
  }
}
