#' @title GetTimestampByObjInternal
#' @description Internal function that helps the GetTimeStampbyObj funtio to get the timestamp that already exists in the dataset
#' @inheritParams PublishADL
#' @author JTA - The Data Scientists
#' @family Internal Utilities
#' @seealso \code{\link{TAPChunks}}
GetTimestampByObjInternal <- function(Data, Project, ChunkName, FileExtension, Iterate) {
  DeleteCache(PatternFile = ChunkName, ForceDelete = T)
  file <- paste(Data, FileExtension, sep = ".")



  if (Iterate) {
    oldPeriods <- NA

    if (Project %in% TAPChunks:::ListADLDirectory(Directory = "Publication")$Directory) {
      if (file %in% TAPChunks:::ListADLDirectory(Directory = file.path("Publication", Project))$Directory) {
        # Get the existent objects and save in the Globlal Env
        test <- try(
          assign(
            Data,
            ReadADLFile(
              SpecificPath = file.path("Publication", Project),
              FileName = paste(Data, FileExtension, sep = ".")
            )
          ),
          silent = T
        )

        if (data.table::is.data.table(test)) {
          oldPeriods <- unique(get(Data)$timestamp)
        }
      }
    }

    # if OldTablePeriods doesn't exist creates as empty
    if (length(intersect(ls(envir = .GlobalEnv), "oldTablePeriods")) == 0) assign("oldTablePeriods", NULL, envir = .GlobalEnv)

    # join the timestamp by object
    assign(
      "oldTablePeriods", rbind(oldTablePeriods, data.table::data.table(Data, oldPeriods)),
      envir = .GlobalEnv
    )
  }
}
