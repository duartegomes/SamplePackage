#' @title Test Workload (Network Edition) data
#' @name TestWorkloadNChunk
#' @family Test Data
#' @description
#' This is an example of a raw Workload (Network Edition) chunk as we receive them from SpiceWorks. The
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
#' 0235a... \tab Windows Server 2012 R2 \tab 2fa78c8d879bfee\tab vmware \tab V \tab N \tab 0 \tab 2016M01 \tab server_count \tab 1\cr
#' 0235a... \tab Windows SBS 2003       \tab                \tab        \tab P \tab N \tab 0 \tab 2016M01 \tab server_count \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab db06dcf2d730cea\tab msft   \tab V \tab N \tab 0 \tab 2016M01 \tab server_count \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab f1028cb368849dc\tab msft   \tab V \tab N \tab 0 \tab 2016M01 \tab server_count \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab 33ae81ff9bd8495\tab msft   \tab V \tab N \tab 0 \tab 2016M01 \tab server_count \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab 9d9c5222e860c3f\tab msft   \tab V \tab N \tab 0 \tab 2016M01 \tab server_count \tab 1\cr
#' 5bfd2... \tab Windows Server 2008 FE \tab 95220072b458323\tab        \tab P \tab N \tab 0 \tab 2016M01 \tab server_count \tab 1\cr
#' ca5d3... \tab OracleServer           \tab                \tab vmware \tab V \tab N \tab 0 \tab 2016M01 \tab server_count \tab 3\cr
#' ca5d3... \tab Windows Server 2008 R2 \tab 22aa13dbbb22bd4\tab vmware \tab V \tab N \tab 0 \tab 2016M01 \tab server_count \tab 1
#' }
#' @section Variables Included:
#' This file only contains server counts and so does not have a list of product columns.
#' @aliases WorkloadN workloadn
#' @docType data
#' @author JTA - The Data Scientists
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL
