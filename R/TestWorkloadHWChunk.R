#' @title Test Workload (Hardware Edition) data
#' @name TestWorkloadHWChunk
#' @family Test Data
#' @description
#' This is an example of a raw Workload (Hardware Edition) chunk as we receive them from SpiceWorks. The
#' data file is limited to 500 records and relates to just one period - 2016M01.
#' @section File Columns:
#' \tabular{ll}{
#' \strong{Column Name} \tab \strong{Description} \cr
#' \emph{row_id} \tab \cr
#' \emph{uuid} \tab \cr
#' \emph{device_os} \tab \cr
#' \emph{storage_gb} \tab \cr
#' \emph{cpu_class} \tab \cr
#' \emph{form_factor} \tab \cr
#' \emph{memory} \tab \cr
#' \emph{cpu_manu} \tab \cr
#' \emph{oem} \tab \cr
#' \emph{server_roles_uuid} \tab \cr
#' \emph{virt_provider} \tab \cr
#' \emph{virt_flag} \tab \cr
#' \emph{data_type} \tab \cr
#' \emph{host_id} \tab \cr
#' \emph{physical_cpu} \tab \cr
#' \emph{cores_per_cpu} \tab \cr
#' \emph{timestamp} \tab \cr
#' \emph{variable} \tab \cr
#' \emph{value}\tab \cr}
#' @section Example Content:
#' \tabular{llllllllllllrrrllr}{
#' \strong{uuid} \tab \strong{device_os} \tab \strong{storage_gb} \tab \strong{cpu_class} \tab \strong{form_factor} \tab \strong{memory} \tab \strong{cpu_manu} \tab \strong{oem} \tab \strong{server_roles_uuid} \tab \strong{virt_provider} \tab \strong{virt_flag} \tab \strong{data_type} \tab \strong{host_id} \tab \strong{physical_cpu} \tab \strong{cores_per_cpu} \tab \strong{timestamp} \tab \strong{variable} \tab \strong{value}\cr
#' 0235a... \tab Windows Server 2012 R2 \tab 1 - 500      \tab NonServer \tab S \tab 1to8    \tab Intel \tab N/A      \tab 2fa78c8d \tab vmware  \tab V  \tab N   \tab  0 \tab 2 \tab 4 \tab 2016M01 \tab server_count \tab 1\cr
#' 0235a... \tab Windows SBS 2003       \tab 1 - 500      \tab Server    \tab S \tab 1to8    \tab Intel \tab dell     \tab          \tab         \tab P  \tab N   \tab  0 \tab 2 \tab 4 \tab 2016M01 \tab server_count \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab 4001 - 10000 \tab Server    \tab S \tab 17 - 32 \tab Intel \tab hp       \tab 3f00cde0 \tab Hyper-V \tab HP \tab VID \tab 45 \tab 2 \tab 4 \tab 2016M01 \tab server_count \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab 1001 - 4000  \tab Server    \tab S \tab 1to8    \tab Intel \tab N/A      \tab db06dcf2 \tab msft    \tab V  \tab N   \tab  0 \tab 2 \tab 4 \tab 2016M01 \tab server_count \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab 1001 - 4000  \tab NonServer \tab S \tab 1to8    \tab Intel \tab gigabyte \tab 304af45a \tab Hyper-V \tab HP \tab VID \tab  7 \tab 1 \tab 2 \tab 2016M01 \tab server_count \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab 1 - 500      \tab Server    \tab S \tab 1to8    \tab Intel \tab N/A      \tab f1028cb3 \tab msft    \tab V  \tab N   \tab  0 \tab 2 \tab 4 \tab 2016M01 \tab server_count \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab 1 - 500      \tab Server    \tab S \tab 1to8    \tab Intel \tab N/A      \tab 33ae81ff9\tab msft    \tab V  \tab N   \tab  0 \tab 2 \tab 4 \tab 2016M01 \tab server_count \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab 1 - 500      \tab Server    \tab S \tab 1to8    \tab Intel \tab N/A      \tab 9d9c5222e\tab msft    \tab V  \tab N   \tab  0 \tab 1 \tab 4 \tab 2016M01 \tab server_count \tab 1\cr
#' 5bfd2... \tab Windows Server 2008 FE \tab 1001 - 4000  \tab Server    \tab S \tab 1to8    \tab Intel \tab hp       \tab 95220072b\tab         \tab P  \tab N   \tab  0 \tab 1 \tab 4 \tab 2016M01 \tab server_count \tab 1
#' }
#' @section Variables Included:
#' This file only contains server counts and so does not have a list of product columns.
#' @aliases WorkloadHW workloadhw
#' @docType data
#' @author JTA - The Data Scientists
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL
