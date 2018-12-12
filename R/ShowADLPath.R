#' @title Finding the path to files in the data lake
#' @description
#' The data lake has a series of defined locations where various sources are stored.
#' This function will return the path for a given source. If the user doesn't give any parameter at all, then the system will switch to an
#' interactive mode whereby the user may select the source from a series of options.
#' @param Source
#' If the Source parameter is not provided then the interactive mode
#' requests a numeric selection (n). A user may also supply a string
#' with one of the following (Source) values:\cr
#' \tabular{rlll}{
#' \tab  n \strong{Source}       \tab \strong{Effect} \tab \strong{Data Documentation:}\cr
#' \tab  1 \emph{Email}          \tab For the Spiceworks Email data \tab \code{\link{TestEmailChunk}}  \cr
#' \tab  2 \emph{Client}         \tab For the Spiceworks PC Client data \tab \code{\link{TestClientChunk}} \cr
#' \tab  3 \emph{Networkhv}      \tab For the Spiceworks Network data by hypervisor \tab \code{\link{TestNetworkhvChunk}} \cr
#' \tab  4 \emph{Networkos}      \tab For the Spiceworks Network data by operating system \tab \code{\link{TestNetworkosChunk}} \cr
#' \tab  5 \emph{ServerRoles}    \tab For the Spiceworks Server Roles data \tab \code{\link{TestServerRolesChunk}} \cr
#' \tab  6 \emph{WorkloadDB}     \tab For the Spiceworks Workload data for database products \tab \code{\link{TestWorkloadDBChunk}} \cr
#' \tab  7 \emph{WorkloadEco}    \tab This chunk lists the virt flag and data type along with Hypervisor and OS for the intersection of Network and VID \tab \code{\link{TestWorkloadEcoChunk}} \cr
#' \tab  8 \emph{WorkloadHW}     \tab This chunk lists the metadata that we received relating to server hardware. \tab \code{\link{TestWorkloadHWChunk}} \cr
#' \tab  9 \emph{WorkloadN}      \tab This chunk lists the virt flag and data type along with Hypervisor and OS for the Network style data \tab \code{\link{TestWorkloadNChunk}} \cr
#' \tab 10 \emph{WorkloadVID}    \tab This chunk lists the virt flag and data type along with Hypervisor and OS for the VID style data \tab \code{\link{TestWorkloadVIDChunk}} \cr
#' \tab 11 \emph{WorkloadWL}     \tab For the Spiceworks workload data for non-database products \tab \code{\link{TestWorkloadWLChunk}} \cr
#' \tab 12 \emph{CCMCost}        \tab For the Spiceworks CCM Cost data\cr
#' \tab 13 \emph{CCMUsage}       \tab For the Spiceworks CCM Usage data\cr
#' \tab 14 \emph{OnlineServices} \tab For the Spiceworks Online Services Data\cr
#' \tab 15 \emph{VID}            \tab For the Spiceworks VID Data\cr
#' \tab 16 \emph{ServerAge}      \tab For the Spiceworks Server Age Data\cr
#' \tab 17 \emph{Cosmos}         \tab For the Cosmos Data\cr
#' }
#' @author JTA - The Data Scientists
#' @return Returns a string with the path releative to the root of the ADL directory. The string
#' is tagged with an attribute \emph{Source} which has the value of the source selected in the parameter or by
#' menu option. If the user cancels from the menu the function returns NULL.
#' @family Internal Utilities
#' @keywords internal
#' @examples TAPChunks:::ShowADLPath("Email")
#' @seealso \code{\link{TAPChunks}}
ShowADLPath <- function(Source) {
  if (!exists("Configuration_env", envir = .GlobalEnv)) {
    TAPChunks:::GetConfiguration()
  }

  source_available <- names(Configuration_env$Source)

  if (missing(Source)) {
    Index_source <- menu(
      choices = c(source_available), title =
        "No source has been selected, choose one of the following or press 0 to cancel the process:"
    )

    if (Index_source == 0) {
      TAPChunks:::catError("No source selected.")
    }
  }

  if (length(intersect(tolower(Source), tolower(source_available))) == 0) {
    TAPChunks:::catError(
      Text = c("The requested source does not exist.", "The available sources are:", source_available),
      Highlight = "The available sources are:"
    )
  } else {
    Index_source <- which(tolower(source_available) == tolower(Source))
  }

  Source <- source_available[Index_source]

  data.lake.path <- Configuration_env$Source[[Source]]$path

  if (!is.null(data.lake.path)) data.table::setattr(data.lake.path, "source", Source)
  if (is.null(data.lake.path)) TAPChunks:::catWarning("The source requested is not available.")

  return(data.lake.path)
}
