#' @title  Deleting files from the local cache
#' @description
#' Users will need to maintain their local cache. This function allows all the cache
#' to be cleared or just selected files.
#' @details The filename can either be an explicit name or can use
#' wildcards.  DeleteCache("*") will remove all files. The function
#' will confirm the choice before actually removing files.
#' @param PatternFile Pattern name of the files to be deleted. Default is "*". 
#' @param ForceDelete If True will also delete folders.
#' @examples DeleteCache("*")
#' @family Cache Controls
#' @author Jonathan Tooley Associados Lda
#' @return Returns an error status to show if the command completed succesfully or not
#' @export
#' @seealso \code{\link{TAPChunks}}
# Backlog #139
DeleteCache  <-
  function(PatternFile = "**", ForceDelete = F) {
    path      <-  ShowCachePath()
    directory <-  ListCacheDirectory()
    if(data.table::is.data.table(directory)) {

      directory[, all_tolower := tolower(`File Name`)]
      PatternFile <- tolower(PatternFile)

      if (substr(PatternFile, 1, 1) != "*") {

        PatternFile <- paste0("^", PatternFile)
      }

      if (substr(PatternFile, nchar(PatternFile), nchar(PatternFile)) != "*") {

        PatternFile <- paste0(PatternFile, "$")
      }

      PatternFile <- gsub("\\*", ".*", PatternFile)
      files <- directory[grep(PatternFile, all_tolower), `File Name`]
      if (length(files) == 0) {
        catInfo("No files available with the pattern.")
      }else {

        catInfo(sprintf("Files list: \n\t\t%s\n",
                        paste( 1:length(files), files, sep = " - " , collapse = "\n\t\t")))

        answer<-"NO"
        if(ForceDelete == F){
          answer <-
            readline(cat("Do you want to delete ALL chunks listed?\n\tY-Yes\n\tN-No\n\tRange of chunks - Insert the number separated by commas:\n"))
          answer <- tolower(answer)}


        if (answer == "y" | ForceDelete == T) {
          for (i in files) {
            unlink(file.path(path, i), recursive = T)
          }
          catSuccess(sprintf("%s files have been deleted",length(files)),length(files))
        }else {
          if (answer == "n" & ForceDelete == F) {
            catWarning("Operation Cancelled!","Cancelled")
          }else {
            files <- files[as.numeric(strsplit(gsub("\\s", "", answer), ",")[[1]])]
            for (i in files) {
              unlink(file.path(path, i), recursive = T)

            }
            catSuccess(sprintf("%s files have been deleted", length(files)), length(files))
          }
        }
      }
    }
  }



#' @title Establishing, pointing and set to the cache folder
#' @description
#' This function allocates a path where the cache will be stored, if Directory parameter is NULL then it will assume a location in user home directory. 
#' It then checks that the cache exists, creating it if not.
#' @details
#' If the cache is placed in the users home directory in a folder called
#' TAPCache.
#' @param Directory Path to set the Cache location
#' @author Jonathan Tooley Associados Lda
#' @export
#' @examples ShowCachePath()
#' @family Cache Controls
#' @return A string with the path to the cache folder.
#' @seealso \code{\link{TAPChunks}}
ShowCachePath  <- function(Directory) {

  if(!exists("Configuration_env")) {
    GetConfiguration()
  }

  if(missing(Directory)){

    if(!is.null(Configuration_env$Message$Configuration$cache.path)){

      cache.path <- Configuration_env$Message$Configuration$cache.path

    }else{

      cache.path <- path.expand("~")
      cache.path <- paste0(cache.path, "/TAPCache")
    }

  }else{
    cache.path <- path.expand(Directory)
  }

  if (!file.exists(cache.path)) {
    dir.create(cache.path)
  }

  Configuration_env$Message$Configuration$cache.path <- cache.path

  cache      <- list.files(cache.path, recursive  = T)
  file.names <- list.files(cache.path)
  if(sum(grepl("\\/", cache)) >= 1){
    list.folders <- paste(sapply(setdiff(file.names, cache), paste0, collapse = ", "), collapse = ", ")

    catWarning(sprintf("The subdirectory %s has been found and any files in them will not be recognized by TAPChunks",  list.folders),  list.folders)
  }
  return(cache.path)
}

#' @title Establishing and pointing to the cache
#' @keywords internal
#' @details This function has been renamed.  See \code{\link{ShowCachePath}}
CachePath  <- function(Directory){
  catWarning("This function has been renamed to ShowCachePath.","ShowCachePath")
}


#' @title Protect a table in memory from deletion
#' @description
#' This function add an attribute that protects the object to be deleted
#' when the user uses the CleanMemory function.
#' @param ... No parameters needed
#' @author Jonathan Tooley Associados Lda
#' @export
#' @family Cache Controls
#' @return A object with attribute set as protect.
#' @seealso \code{\link{TAPChunks}}
# Backlog #156
Protect <- function (...){
  obj <- match.call(expand.dots = FALSE)$...
  obj <- paste(obj[[1]])

  DT <- get(obj)
  attr(DT, "Protect") <- T

  eval(parse(text = sprintf("%s<<-%s", obj, "DT")))
}

#' @title Unprotect the object that has been assigned as Protected
#' @description
#' This function removes the Protect attribute from the object.
#' When the user uses the CleanMemory function the object will be deleted.
#' @param ... No parameters needed
#' @author Jonathan Tooley Associados Lda
#' @export
#' @family Cache Controls
#' @return A object without attribute set as protect.
#' @seealso \code{\link{TAPChunks}}
# Backlog 156
Unprotect <- function (...){

  obj <- match.call(expand.dots = FALSE)$...
  obj <- paste(obj[[1]])

  DT <- get(obj)
  attr(DT, "Protect") <- NULL

  eval(parse(text = sprintf("%s<<-%s", obj, "DT")))

}

#' @title Clean large tables from the memory
#' @description This function removes all the objects that
#' are using more than the specified number of Mbytes in ram
#' @details Per default the is size cut-off is set as 10MB so all
#' tables above this size will be cleared from memory. The function
#' will not remove any tables that have had their protect attribut set.
#' See \code{\link{Protect}} and \code{\link{Unprotect}} for more information
#' on protecting tables in memory.
#' @author Jonathan Tooley Associados Lda
#' @export
#' @param LimitMB Limit value in MB.
#' @examples CleanMemory(20)
#' @family Cache Controls
#' @return Cleans the memory, no return.
#' @seealso \code{\link{TAPChunks}}
# Backlog 156
CleanMemory <- function(LimitMB = 10){
  remove_obj <- NULL
  for (i in ls(envir = .GlobalEnv)){

    obj.size <- format(object.size(get(i)), units = "Mb")
    obj.size <- as.numeric(gsub(" Mb", "", obj.size))

    if (is.null(attributes(get(i))$Protect) & obj.size >= LimitMB){

      remove_obj <- c(remove_obj, i)
    }
  }

  if (!is.null(remove_obj)){
    rm(list = remove_obj, envir = .GlobalEnv)
    catInfo(paste0("Removed Objects:\n\t\t-", paste0(remove_obj, collapse = "\n\t\t-")))
  }

  startRAM <- memory.size()
  lowRAM   <- startRAM * .1
  while (startRAM != lowRAM){

    startRAM <- lowRAM
    gc()
    lowRAM <- memory.size()
  }
}


#' @title Delete outdated Rdata files in cache folder
#' @description Delete Rdata files with more 30 days in cache folder
#' @author Jonathan Tooley Associados Lda
#' @export
#' @examples DeleteOutdatedCache()
#' @family Cache Controls
#' @return Cleans old files in cache, no return.
#' @seealso \code{\link{TAPChunks}}

DeleteOutdatedCache <- function(){

  path <- ShowCachePath()
  files.name <- ListCacheDirectory(ShowAttributes = T)

  oldfiles  <- files.name[ `File Created` <= Sys.Date() - as.difftime(tim = 30, units = "days") & grepl(".Rdata", `File Name`)]

  for (i in oldfiles[,`File Name`]) {

    unlink(file.path(path, i), recursive = T)
  }
}
