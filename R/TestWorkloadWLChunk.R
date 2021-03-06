#' @title Test Workload (WL Edition) data
#' @name TestWorkloadWLChunk
#' @family Test Data
#' @description
#' This is an example of a raw Workload (WL Edition) chunk as we receive them from SpiceWorks. The
#' data file is limited to 500 records and relates to just one period - 2016M01.
#' @section File Columns:
#' \tabular{ll}{
#' \strong{Column Name} \tab \strong{Description} \cr
#' \emph{row_id} \tab \cr
#' \emph{uuid} \tab \cr
#' \emph{device_os} \tab \cr
#' \emph{server_roles_uuid} \tab \cr
#' \emph{virt_provider} \tab \cr
#' \emph{virt_flag} \tab \cr
#' \emph{data_type} \tab \cr
#' \emph{host_id} \tab \cr
#' \emph{timestamp} \tab \cr
#' \emph{variable} \tab \cr
#' \emph{value}\tab \cr}
#' @section Example Content:
#' \tabular{llllllrllr}{
#' \strong{uuid} \tab \strong{device_os} \tab \strong{server_roles_uuid} \tab \strong{virt_provider} \tab \strong{virt_flag} \tab \strong{data_type} \tab \strong{host_id} \tab \strong{timestamp} \tab \strong{variable} \tab \strong{value}\cr
#' 0235a... \tab Windows SBS 2003       \tab         \tab        \tab P \tab N \tab 0 \tab 2016M01 \tab app_dev_deployment   \tab 1\cr
#' 0235a... \tab Windows SBS 2003       \tab         \tab        \tab P \tab N \tab 0 \tab 2016M01 \tab collaborative_apps   \tab 2\cr
#' 0235a... \tab Windows SBS 2003       \tab         \tab        \tab P \tab N \tab 0 \tab 2016M01 \tab security             \tab 1\cr
#' 0235a... \tab Windows SBS 2003       \tab         \tab        \tab P \tab N \tab 0 \tab 2016M01 \tab msft_exchange        \tab 2\cr
#' 0235a... \tab Windows Server 2012 R2 \tab 2fa78c8d\tab vmware \tab V \tab N \tab 0 \tab 2016M01 \tab server_count         \tab 1\cr
#' 0235a... \tab Windows SBS 2003       \tab         \tab        \tab P \tab N \tab 0 \tab 2016M01 \tab server_count         \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab db06dcf2\tab msft   \tab V \tab N \tab 0 \tab 2016M01 \tab app_dev_deployment   \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab 33ae81ff\tab msft   \tab V \tab N \tab 0 \tab 2016M01 \tab app_dev_deployment   \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab db06dcf2\tab msft   \tab V \tab N \tab 0 \tab 2016M01 \tab authoring_publishing \tab 1
#' }
#' @section Variables Included:
#' \tabular{ll}{
#' \strong{variable} \tab \strong{Notes}\cr
#' \emph{ app_dev_deployment } \tab \cr
#' \emph{ authoring_publishing } \tab \cr
#' \emph{ collaborative_apps } \tab \cr
#' \emph{ engineering } \tab \cr
#' \emph{ msft_exchange } \tab \cr
#' \emph{ msft_lync } \tab \cr
#' \emph{ msft_sharepoint } \tab \cr
#' \emph{ os_and_storage_sw } \tab \cr
#' \emph{ other_db_and_bi } \tab \cr
#' \emph{ security } \tab \cr
#' \emph{ server_count } \tab \cr
#' \emph{ system_infrastructure_network_mgmt }\tab \cr}
#' @aliases app_dev_deployment authoring_publishing collaborative_apps engineering
#' msft_exchange msft_lync msft_sharepoint os_and_storage_sw
#' other_db_and_bi security system_infrastructure_network_mgmt WorkloadWL workloadwl
#' @docType data
#' @author JTA - The Data Scientists
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL
