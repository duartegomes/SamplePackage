#' @title Test Workload (Ecosystem Edition) data
#' @name TestWorkloadEcoChunk
#' @family Test Data
#' @description
#' This is an example of a raw Workload (Ecosystem Edition) chunk as we receive them from SpiceWorks. The
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
#' f50de... \tab Windows Server 2012 R2 \tab 3f00cde0c36 \tab Hyper-V     \tab HP \tab VID \tab  45 \tab 2016M01 \tab server_count \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab db06dcf2d73 \tab msft        \tab V  \tab N   \tab   0 \tab 2016M01 \tab server_count \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab 304af45a698 \tab Hyper-V     \tab HP \tab VID \tab   7 \tab 2016M01 \tab server_count \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab f1028cb3688 \tab msft        \tab V  \tab N   \tab   0 \tab 2016M01 \tab server_count \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab 33ae81ff9bd \tab msft        \tab V  \tab N   \tab   0 \tab 2016M01 \tab server_count \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab 9d9c5222e86 \tab msft        \tab V  \tab N   \tab   0 \tab 2016M01 \tab server_count \tab 1\cr
#' ca5d3... \tab VMware ESXi            \tab            \tab VMware ESXi \tab HP \tab VID \tab 228 \tab 2016M01 \tab server_count \tab 1\cr
#' ca5d3... \tab OracleServer           \tab            \tab vmware      \tab V  \tab N   \tab   0 \tab 2016M01 \tab server_count \tab 3\cr
#' ca5d3... \tab Windows Server 2012 R2 \tab 508ae077a8a\tab VMware ESXi \tab V  \tab VID \tab  80 \tab 2016M01 \tab server_count \tab 1
#' }
#' @section Variables Included:
#' This file only contains server counts and so does not have a list of product columns.
#' @aliases WorkloadEco workloadeco
#' @docType data
#' @author JTA - The Data Scientists
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL
