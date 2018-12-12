#' @title  Appending data to a disk file
#' @description This function allows users to append a set of csv files into one single file.
#' @param ... files to append
#' @param Folder Folder where files are, if missing: CachePath will be called
#' @param Name Name to save CSV
#' @param Remove boolean: if true all input files files will be deleted after being appended
#' @family Reporting and saving tools
#' @author JTA - The Data Scientists
#' @export
#' @seealso \code{\link{TAPChunks}}
AppendCSV <- function(..., Folder, Name, Remove = F) {
  if (missing(Folder)) {
    TAPChunks:::catInfo("\nFolder not selected. It will look for files in TAPChunks.")
    Folder <- ShowCachePath()
  } else {
    Folder <- file.path(path.expand("~"), Folder)
    if (!file.exists(Folder)) {
      dir.create(Folder)
    }
  }

  if (missing(Name)) {
    TAPChunks:::catInfo("\nName of file not defined. Name will be 'All.csv'.")
    Name <- "All.csv"
  } else {
    if (length(grep(".csv", Name)) == 0) Name <- paste0(Name, ".csv")
  }

  files <- paste(match.call(expand.dots = FALSE)$...)

  if (length(files) == 0) {
    TAPChunks:::catError("\nPlease select files to append.")
  }

  new.files <- list()
  names.rows <- data.table::data.table(1)
  for (file in files) {
    if (exists(file)) {
      data.table::fwrite(get(file), file = file.path(Folder, paste0(file, ".csv")))
      names.rows <- suppressWarnings(cbind(names.rows, names(get(file))))
      new.files <- c(paste0(file, ".csv"), new.files)
    } else {
      if (length(grep(".csv", file)) == 1) {
        new.files <- c(file, new.files)
        names.rows <- suppressWarnings(cbind(names.rows, names(data.table::fread(file, nrow = 1L))))
      }
    }
  }

  names(names.rows) <- paste(1:ncol(names.rows))
  names.rows[, 1 := NULL]
  for (i in 2:ncol(names.rows)) {
    if (length(setdiff(names.rows[["1"]], names.rows[[paste(i)]])) != 0) {
      TAPChunks:::catError("\nSources to append don't have the same columns.")
    }
  }

  files.append <- list.files(Folder,
    pattern = ".csv"
  )

  if (length(setdiff(new.files, files.append)) != 0) {
    TAPChunks:::catError(sprintf("\n %s not found", setdiff(new.files, files.append)))
  }

  if (length(new.files) > 1) {
    new.files <- paste(new.files, collapse = "+")
  }

  shell(sprintf("cd %s & copy %s %s", # go to dir of saved partition and copy every csv files
    Folder, new.files, Name, # path to save melted file
    translate = T # path can be read with "\"
  ))

  if (Remove) {
    new.files <- setdiff(new.files, Name) # get all files except Appended one
    do.call(unlink, list(new.files)) # remove list of csv files
  }
}
