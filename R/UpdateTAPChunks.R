#' @title UpdateTAPChunks
#' @description Update TAPChunks version in case there is a more recent one.
#' @import desc
#' @examples UpdatePackage()
#' @author JTA - The Data Scientists
#' @return None
#' @family Internal Utilities
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
UpdateTAPChunks <- function() {
  if (TAPChunks:::TestVPN()) {
    Built <- installed.packages(lib.loc = .libPaths())[grep("TAPChunks", installed.packages(lib.loc = .libPaths())), "Built"]
    Installed <- installed.packages(lib.loc = .libPaths())[grep("TAPChunks", installed.packages(lib.loc = .libPaths())), "Version"]

    temp.folder <- "//capengsharedev/JTAPackages/"
    files <- list.files(temp.folder)
    if (length(files) == 0) {
      TAPChunks:::catWarning("In folder'//capengsharedev/JTAPackages', no files were found. Please check that you have access.")
      TAPChunks:::catWarning("Can't update TAPChunks.")

      return(invisible)
    }

    package <- grep("tar.gz", files, value = T)
    package <- max(package)
    version <- substr(package, gregexpr(".tar", package)[[1]][1] - 5, gregexpr(".tar", package)[[1]][1] - 1)

    directory <- getwd()
    setwd(paste0(.libPaths()[1], "/TAPChunks"))
    desc <- desc::description$new()
    Deps <- data.table::data.table(desc$get_deps())
    Imports <- Deps[type == "Imports"]$package
    Imports <- paste(Imports, collapse = ",")
    Imports <- paste0(Imports, "\n(>= 1.3.4)")
    Suggests <- Deps[type == "Suggests"]$package
    Suggests <- paste(Suggests, collapse = ",")
    DependsP <- Deps[type == "Depends"]$package
    DependsV <- Deps[type == "Depends"]$version
    Depends <- paste(DependsP, DependsV)


    availableP <- matrix(c(
      "TAPChunks", version, Depends, Imports, Suggests, "file LICENSE",
      package, temp.folder
    ), nrow = 1, ncol = 8, dimnames = list("TAPChunks"))

    colnames(availableP) <- c(
      "Package", "Version", "Depends", "Imports", "Suggests", "License", "File",
      "Repository"
    )
    setwd(directory)

    if (Installed < version) {
      TAPChunks:::catInfo("Library TAPChunks:")
      TAPChunks:::catInfo(sprintf("Installed version %s in %s", Installed, .libPaths()[1]))

      TAPChunks:::catInfo(sprintf("New version %s in %s", version, temp.folder))


      packages <- c("httr", "curl", "jsonlite", "data.table", "RODBC", "yaml", "desc")

      for (n in packages) {
        TAPChunks:::CheckPackage(n)
      }

      install.packages(
        pkgs = paste0(temp.folder, package), repos = NULL,
        type = "source", available = availableP, dependencies = c("Depends", "Imports")
      )
      unloadNamespace("TAPChunks")
      library(TAPChunks)
    }
  } else {
    TAPChunks:::catWarning("Can't update TAPChunks.")
    return(invisible)
  }
}
