#' @title Test Workload (VID Edition) data
#' @name TestWorkloadVIDChunk
#' @family Test Data
#' @description
#' This is an example of a raw Workload (VID Edition) chunk as we receive them from SpiceWorks. The
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
#' f50de... \tab Windows Server 2012 R2 \tab 3f00cde0c363e5\tab Hyper-V     \tab HP \tab VID \tab  45 \tab 2016M01 \tab server_count \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab 304af45a698c98\tab Hyper-V     \tab HP \tab VID \tab   7 \tab 2016M01 \tab server_count \tab 1\cr
#' ca5d3... \tab VMware ESXi            \tab               \tab VMware ESXi \tab HP \tab VID \tab 228 \tab 2016M01 \tab server_count \tab 1\cr
#' ca5d3... \tab Windows Server 2012 R2 \tab 508ae077a8a5f4\tab VMware ESXi \tab V  \tab VID \tab  80 \tab 2016M01 \tab server_count \tab 1\cr
#' ca5d3... \tab Windows Server 2012 R2 \tab 1428bc13cdff43\tab VMware ESXi \tab V  \tab VID \tab  80 \tab 2016M01 \tab server_count \tab 1\cr
#' ca5d3... \tab Windows Server 2012 R2 \tab 688aaef017d10d\tab VMware ESXi \tab V  \tab VID \tab  80 \tab 2016M01 \tab server_count \tab 1\cr
#' ca5d3... \tab VMware ESXi            \tab               \tab VMware ESXi \tab HP \tab VID \tab  80 \tab 2016M01 \tab server_count \tab 1\cr
#' ca5d3... \tab Windows Server 2008 R2 \tab 512ed32db63a8e\tab VMware ESXi \tab V  \tab VID \tab  80 \tab 2016M01 \tab server_count \tab 1\cr
#' ba8f7... \tab VMware ESXi            \tab               \tab VMware ESXi \tab HP \tab VID \tab 581 \tab 2016M01 \tab server_count \tab 1
#' }
#' @section Variables Included:
#' This file only contains server counts and so does not have a list of product columns.
#' @aliases WorkloadVID workloadvid
#' @docType data
#' @author JTA - The Data Scientists
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL
