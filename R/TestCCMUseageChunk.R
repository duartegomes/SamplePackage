#' @title Test CCM Usage Data
#' @name TestCCMUsageChunk
#' @family Test Data
#' @description
#' This is an example of a raw CCM Usage chunk as we receive them from SpiceWorks. The
#' data file is limited to 500 records and relates to just one period - 2017M01
#' @section File Columns:
#' \tabular{ll}{
#' \strong{Column Name} \tab \strong{Description} \cr
#' \emph{uuid} \tab \cr
#' \emph{platform} \tab \cr
#' \emph{service} \tab \cr
#' \emph{resource_type} \tab \cr
#' \emph{os} \tab \cr
#' \emph{region} \tab \cr
#' \emph{metric} \tab \cr
#' \emph{value} \tab \cr
#' \emph{unit} \tab \cr
#' \emph{timestamp} \tab \cr
#' \emph{row_id} \tab \cr
#' \emph{NoProd}\tab \cr}
#' @section Example Content:
#' \tabular{lllllllllll}{
#' \strong{uuid} \tab \strong{platform} \tab \strong{service} \tab \strong{resource_type} \tab \strong{os} \tab \strong{region} \tab \strong{metric} \tab \strong{value} \tab \strong{unit} \tab \strong{timestamp} \tab \strong{NoProd}\cr
#' 93a3b... \tab Azure \tab Data Services \tab Microsoft.Sql/servers/databases          \tab  \tab australiaeast \tab Data Services - Basic Database Days - SQL Database              \tab 792      \tab Hours     \tab 2017M01 \tab NA\cr
#' 93a3b... \tab Azure \tab Storage       \tab Microsoft.Storage/storageAccounts        \tab  \tab australiaeast \tab Storage - Standard IO - Table (GB) - Locally Redundant          \tab 7.20E-05 \tab GB        \tab 2017M01 \tab NA\cr
#' 93a3b... \tab Azure \tab Storage       \tab Microsoft.Storage/storageAccounts        \tab  \tab australiaeast \tab Storage - Standard IO - Hot Block Blob (GB) - Locally Redundant \tab 0.00012  \tab GB        \tab 2017M01 \tab NA\cr
#' 93a3b... \tab Azure \tab Storage       \tab Microsoft.ClassicStorage/storageAccounts \tab  \tab australiaeast \tab Storage - Standard IO - Page Blob/Disk (GB) - Geo Redundant     \tab 1.742206 \tab GB        \tab 2017M01 \tab NA\cr
#' 65163... \tab Azure \tab Networking    \tab Microsoft.ClassicNetwork/virtualNetworks \tab  \tab westeurope    \tab Networking - Virtual Network                                    \tab TRUE     \tab Existence \tab 2017M01 \tab NA\cr
#' 65163... \tab Azure \tab Networking    \tab Microsoft.Network/virtualNetworks        \tab  \tab eastus        \tab Networking - Virtual Network                                    \tab TRUE     \tab Existence \tab 2017M01 \tab NA\cr
#' 65163... \tab Azure \tab Storage       \tab Microsoft.Storage/storageAccounts        \tab  \tab eastus        \tab Storage - Standard IO - Page Blob/Disk (GB) - Locally Redundant \tab 0.33392  \tab GB        \tab 2017M01 \tab NA\cr
#' 65163... \tab Azure \tab Storage       \tab Microsoft.Storage/storageAccounts        \tab  \tab eastus        \tab Storage - Standard IO - Table (GB) - Locally Redundant          \tab 4.80E-05 \tab GB        \tab 2017M01 \tab NA\cr
#' 65163... \tab Azure \tab Data Services \tab Microsoft.Sql/servers/databases          \tab  \tab westeurope    \tab Data Services - Standard S1 Database Days - SQL Database        \tab 350      \tab Hours     \tab 2017M01 \tab NA
#' }
#' @aliases CCMUsage
#'
#' @docType data
#' @author JTA - The Data Scientists
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL
