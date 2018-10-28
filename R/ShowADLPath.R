#' @title Finding the path to files in the data lake
#' @description
#' The data lake has a series of defined locations where various sources are stored.
#' This function will return the path for a given source. If the user passes an unknown
#' source (or doesn't give any parameter at all) then a list of sources will be shown.
#' @param Source
#' Name of the source, if the Source parameter is not provided then a list of sources will be shown:\cr
#' \tabular{lll}{
#' \tab \strong{Source}       \tab \strong{Effect} \tab \strong{Data Documentation:}\cr
#' \tab \emph{Email}          \tab For the Spiceworks Email data \tab \code{\link{TestEmailChunk}}  \cr
#' \tab \emph{Client}         \tab For the Spiceworks PC Client data \tab \code{\link{TestClientChunk}} \cr
#' \tab \emph{Networkhv}      \tab For the Spiceworks Network data by hypervisor \tab \code{\link{TestNetworkhvChunk}} \cr
#' \tab \emph{Networkos}      \tab For the Spiceworks Network data by operating system \tab \code{\link{TestNetworkosChunk}} \cr
#' \tab \emph{ServerRoles}    \tab For the Spiceworks Server Roles data \tab \code{\link{TestServerRolesChunk}} \cr
#' \tab \emph{WorkloadDB}     \tab For the Spiceworks Workload data for database products \tab \code{\link{TestWorkloadDBChunk}} \cr
#' \tab \emph{WorkloadEco}    \tab This chunk lists the virt flag and data type along with Hypervisor and OS for the intersection of Network and VID \tab \code{\link{TestWorkloadEcoChunk}} \cr
#' \tab \emph{WorkloadHW}     \tab This chunk lists the metadata that we received relating to server hardware. \tab \code{\link{TestWorkloadHWChunk}} \cr
#' \tab \emph{WorkloadN}      \tab This chunk lists the virt flag and data type along with Hypervisor and OS for the Network style data \tab \code{\link{TestWorkloadNChunk}} \cr
#' \tab \emph{WorkloadVID}    \tab This chunk lists the virt flag and data type along with Hypervisor and OS for the VID style data \tab \code{\link{TestWorkloadVIDChunk}} \cr
#' \tab \emph{WorkloadWL}     \tab For the Spiceworks workload data for non-database products \tab \code{\link{TestWorkloadWLChunk}} \cr
#' \tab \emph{CCMCost}        \tab For the Spiceworks CCM Cost data\cr
#' \tab \emph{CCMUsage}       \tab For the Spiceworks CCM Usage data\cr
#' \tab \emph{OnlineServices} \tab For the Spiceworks Online Services Data\cr
#' \tab \emph{VID}            \tab For the Spiceworks VID Data\cr
#' \tab \emph{ServerAge}      \tab For the Spiceworks Server Age Data\cr
#' \tab \emph{Cosmos}         \tab For the Cosmos Data\cr
#' }
#' @author JTA - The Data Scientists
#' @return Returns a string with the path releative to the root of the ADL directory. The sting
#' is tagged with an attribute \emph{Source} which has the value of the source selected in the parameter or by
#' menu option. If the user cancels from the menu the function returns NULL.
#' @export
#' @family Data Lake Controls
#' @examples ShowADLPath("Email")
#' @seealso \code{\link{TAPChunks}}
ShowADLPath <- function(Source) {

  if (!exists("Configuration_env", envir = .GlobalEnv)){
    GetConfiguration()
  }

  source_available <- names(Configuration_env$Source)

  if(missing(Source)){

    TAPChunks::catError(Text      = c("No source has been selected.","The available sources are:", source_available),
                               Highlight = "The available sources are:")
  }

  if (length(intersect(tolower(Source), tolower(source_available))) == 0) {

    TAPChunks::catError(Text      = c("The requested source does not exist.","The available sources are:", source_available),
                        Highlight = "The available sources are:")



    #comment Menu because we may use in the future.
    #Index_source <- menu(choices = c(source_available), title =
    #                       "The requested source does not exist, choose one of the following or press 0 to cancel the process:")

    #if( Index_source == 0){
    #  catError("No source selected.")
    #}
  } else{
    Index_source <- which(tolower(source_available) == tolower(Source))
  }

  Source <- source_available[Index_source]

  data.lake.path  <- Configuration_env$Source[[Source]]$path

  if (!is.null(data.lake.path)) data.table::setattr(data.lake.path, "source", Source)
  if (is.null(data.lake.path)) catWarning("The source requested is not available.")

  return(data.lake.path)

}

#' @title Finding the path to files in the data lake
#' @keywords internal
#' @details This has been renamed and is deprecated. See \code{\link{ShowADLPath}}
#' @seealso \code{\link{TAPChunks}}
#' @export
ADLPath <- function(Source = ""){
  catWarning("This function has been renamed ShowADLPath and has been deprecated.")
}
