#' @title Run a script iteratively over a series of months
#' @description Any system is limited by resources and telemetry data can be extremely large.
#' The TAP framework has a variety of solutions to allow for processing large quantities of
#' data and this is one of the most simple tools.  This function allows a user to take a TAP chunks
#' script that processes one month of data and then to run it repeatedly for many different months.
#' @section Setting up an iteration:
#' Your script may contain areas which will run over a series of months and also areas that are intended
#' to be run just once.  Typically a script has code to set up a function before iteration happens and code to
#' finish off
#'
#' @param Script The name of the script stored in your working directory or a full path name to where your
#' scrit is stored.  If a script name is not given the function will open an interactive dialog box for
#' the user to select their script.
#' @param From The start month from which we will run the script. This is a required field.
#' @param To The end month up until which we will run the script. This is a required field.
#' @author JTA: The Data Scientists
#' @export
#' @family Compute Tools
#' @seealso \code{\link{TAPChunks}}
#'
IterateScript <- function(Script = "", From, To) {
  catIterate <- function(x) {
    TAPChunks:::catStyle(
      Header = "Iterate:",
      Text = x,
      Style = "i",
      Color = "blue"
    )
  }


  if (missing(From) || missing(To)) TAPChunks:::catError("Need to provide both the 'From' date and the 'To' date.")

  periods <- TAPChunks::TimeStamps(From = From, To = To)

  if (!file.exists(Script)) {
    Script <- choose.files(
      multi = F,
      caption = "Choose your R script to be iterated",
      filters = c("R Scripts", "All Files", "*.r;*.R", "*.*"),
      index = 1
    )
  }

  readScript <- readLines(Script)

  ScriptSrc <- srcfile("Script.R")
  sf <- try(parse(text = readScript, keep.source = T, srcfile = ScriptSrc), silent = T)
  pd <- data.table::as.data.table(getParseData(ScriptSrc))

  publish <- grep("PublishADL", sf)

  if (length(publish) > 0) {
    begs <- grep("BeginIterate()", sf)
    ends <- grep("EndIterate()", sf)
    obj <- sf[begs:ends]
    obj <-
      data.table::as.data.table(getParseData(parse(text = as.character(obj))))
    obj <-
      obj[token == "SYMBOL" &
        shift(token, type = "lead", n = 2) == "LEFT_ASSIGN", unique(text)]

    assign("oldTablePeriods", NULL, envir = .GlobalEnv)
    for (i in publish) {
      TAPChunks:::GetTimestampByObj(sf[i], obj)
    }

    periodsAppend <-
      oldTablePeriods[, .N, by = "oldPeriods"][N == length(unique(oldTablePeriods$Data)), oldPeriods]
    timestampIntersect <- intersect(periods, periodsAppend)
    periods <- setdiff(periods, periodsAppend)

    if (length(periods) == 0) {
      TAPChunks:::catError("All required TimeStamps are already available in the project in the ADL.")
    }
    if (length(periodsAppend) == 0 | is.na(periodsAppend)) {
      removePeriods <- oldTablePeriods[!is.na(oldPeriods)]
    } else {
      if (length(timestampIntersect == 1)) {
        if (!is.na(timestampIntersect)) {
          catIterate(Text = sprintf("The following timestamp already exists in the project: \n\t\t - %s", paste(timestampIntersect, collapse = "\n\t\t - ")))
        }
      } else {
        catIterate(Text = sprintf("The following timestamps already exist in the project: \n\t\t - %s", paste(timestampIntersect, collapse = "\n\t\t - ")))
      }
      removePeriods <- oldTablePeriods[!(oldPeriods %in% periodsAppend)]
    }

    if (nrow(removePeriods) != 0) {
      for (i in removePeriods$Data) {
        assign(i, fread(file.path(ShowCachePath(), paste0(i, ".csv"))))

        assign(
          i,
          get(i)[!(timestamp %in% removePeriods[Data == i, oldPeriods])]
        )

        fwrite(get(i), file = file.path(ShowCachePath(), paste0(i, ".csv")))
      }
    }

    pathIterate <- file.path(ShowCachePath())

    scriptAppend <-
      sprintf(
        "data.table::fwrite(%s, file = \"%s/%s.csv\", append = T)",
        unique(oldTablePeriods$Data),
        pathIterate,
        unique(oldTablePeriods$Data)
      )
    ScriptRead <-
      sprintf(
        "%s<-data.table::fread(file = \"%s/%s.csv\")",
        unique(oldTablePeriods$Data),
        pathIterate,
        unique(oldTablePeriods$Data)
      )
    ends <- grep("EndIterate()", sf)
    scriptAppend <-
      try(parse(text = scriptAppend, keep.source = T), silent = T)
    ScriptRead <-
      try(parse(text = ScriptRead, keep.source = T), silent = T)

    sf <-
      c(sf[1:(ends - 1)], scriptAppend, sf[ends], ScriptRead, sf[(ends + 1):length(sf)])
    sf <- try(parse(text = as.character(sf), keep.source = T, srcfile = ScriptSrc), silent = T)
    pd <- data.table::as.data.table(getParseData(ScriptSrc))
  }


  if (!class(sf) == "try-error") {
    begs <- grep("BeginIterate()", sf)
    ends <- grep("EndIterate()", sf)
    reads <- intersect(grep("ReadADLChunk", sf), grep("Iterate[[:space:]]+=[[:space:]]+T", sf, perl = T))

    if (length(begs) == 0) TAPChunks:::catError("Missing Iterate Block.")
    if (length(reads) == 0) TAPChunks:::catError("Missing Iterate Read Statement.")
    if (length(begs) != length(ends)) TAPChunks:::catError("Unmatching number of BeginIterate and EndIterate markers.")
    if (length(reads) != length(begs)) TAPChunks:::catError("Iterate Enable ReadADLChunk not included in an Iterate Block. ")

    for (i in 1:length(begs)) {
      if (!((begs[i] < reads[i]) & (reads[i] < ends[i]))) {
        TAPChunks:::catError("Iterate block found without an iterate enabled ReadADLChunk.")
      }
    }

    pd[, row := .I]

    readCandidates <- pd[ grepl("Iterate", text) & token == "SYMBOL_SUB"]

    goodCandidates <- numeric()
    for (k in 1:nrow(readCandidates)) {
      Check <- TRUE
      if (pd[row == readCandidates[k][["row"]] + 1][["token"]] != "EQ_SUB") Check <- FALSE
      if (pd[row == readCandidates[k][["row"]] + 2][["token"]] != "SYMBOL") Check <- FALSE
      if (pd[row == readCandidates[k][["row"]] + 2][["text" ]] != "T") Check <- FALSE

      if (Check) goodCandidates <- c(goodCandidates, k)
    }

    readCandidates <- readCandidates[goodCandidates]

    env <- parent.frame()

    line <- 1

    for (i in 1:length(begs)) {
      if (line == 1) {
        catIterate("Running the start of your script up to the start of the first iterate block.")
        TAPChunks:::DetectError(sf, line, begs[i], env)
      } else {
        catIterate(sprintf("Running the code between block %i and block %i.", i - 1, i))
        TAPChunks:::DetectError(sf, line, begs[i], env)
      }

      # keepObjects <- ls(envir = env)

      catIterate(sprintf("Running iterate block #: %i", i))

      .GlobalEnv$Iter_env$assets <- c(.GlobalEnv$Iter_env$assets, "slice")

      for (slice in periods) {
        catIterate(sprintf("Running time period: %s", slice))

        sf[reads[i]] <- TAPChunks:::RecreateReadADLChunkExpression(sf[reads[i]], slice)

        TAPChunks:::DetectError(sf, begs[i] + 1, ends[i], env)

        if (length(publish) > 0) {
          if (slice == min(periods) & NA %in% oldTablePeriods$oldPeriods) {
            for (objNames in unique(oldTablePeriods$Data)) {
              if (!(is.data.table(get(objNames)) &
                "timestamp" %in% names(get(objNames)))) {
                posfwrite <- grep(paste0("data.table::fwrite\\(", objNames), sf)
                posfread <-
                  grep(paste(objNames, "data.table::fread\\(", sep = " <- "), sf)
                posfwrite <-
                  max(posfwrite[posfwrite < ends & posfwrite > begs])
                posfread <- min(posfread[posfread > ends])

                sf[c(posfwrite, posfread)] <-
                  parse(text = paste0(objNames, "<-", objNames))
              }
            }
          }
        }

        gc()
      }

      catIterate("Removing the Iterate Control Environment.")
      rm(Iter_env, envir = .GlobalEnv)

      line <- ends[i] + 1
    }
    if (line <= length(sf)) {
      catIterate("Running the code after your last iterate block.")
      TAPChunks:::DetectError(sf, line, length(sf), env)
    }
  } else {
    e <- attr(sf, "condition")
    catIterate(
      sprintf(
        "A syntax error was found in your script during initial load: \n %s",
        e$message
      )
    )
  }
}
