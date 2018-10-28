#' @title Test VID Data
#' @name TestVIDChunk
#' @family Test Data
#' @description
#' This is an example of a raw VID chunk as we receive them from SpiceWorks. The
#' data file is limited to 500 records and relates to just one period - 2017M01
#' @section File Columns:
#' \tabular{ll}{
#' \strong{Column Name} \tab \strong{Description} \cr
#' \emph{id} \tab \cr
#' \emph{uuid} \tab \cr
#' \emph{host_id} \tab \cr
#' \emph{vm_id} \tab \cr
#' \emph{oem} \tab \cr
#' \emph{virtualization_software} \tab \cr
#' \emph{operating_system} \tab \cr
#' \emph{timestamp} \tab \cr
#' \emph{row_id} \tab \cr
#' \emph{NoProd}\tab \cr}
#' @section Example Content:
#' \tabular{rlrrlllll}{
#' \strong{id} \tab \strong{uuid} \tab \strong{host_id} \tab \strong{vm_id} \tab \strong{oem} \tab \strong{virtualization_software} \tab \strong{operating_system} \tab \strong{timestamp} \tab \strong{NoProd}\cr
#'  1 \tab ba8f7... \tab 581 \tab  2 \tab intel   \tab VMware ESXi            \tab Windows Server 2008 R2                      \tab 2017M01 \tab NA\cr
#'  2 \tab ba8f7... \tab 581 \tab  1 \tab intel   \tab VMware ESXi            \tab Windows Server 2008 R2                      \tab 2017M01 \tab NA\cr
#'  3 \tab be44c... \tab  41 \tab 14 \tab fujitsu \tab Windows Server 2012 R2 \tab Windows Small Business Server 2011 Standard \tab 2017M01 \tab NA\cr
#'  4 \tab be44c... \tab  41 \tab  7 \tab fujitsu \tab Windows Server 2012 R2 \tab Windows Server 2008 R2 Standard             \tab 2017M01 \tab NA\cr
#'  5 \tab be44c... \tab  41 \tab 27 \tab fujitsu \tab Windows Server 2012 R2 \tab Windows Server 2012 R2 Standard             \tab 2017M01 \tab NA\cr
#'  6 \tab be44c... \tab  41 \tab 28 \tab fujitsu \tab Windows Server 2012 R2 \tab Windows Server 2012 R2 Standard             \tab 2017M01 \tab NA\cr
#'  7 \tab be44c... \tab  41 \tab 29 \tab fujitsu \tab Windows Server 2012 R2 \tab Windows Server 2012 R2 Standard             \tab 2017M01 \tab NA\cr
#' 14 \tab 690e0... \tab   6 \tab  1 \tab hp      \tab Windows Server 2012    \tab Windows Server 2012                         \tab 2017M01 \tab NA\cr
#' 15 \tab 690e0... \tab   6 \tab  2 \tab hp      \tab Windows Server 2012    \tab Windows Server 2012 Standard                \tab 2017M01 \tab NA
#' }
#' @section Variables Included:
#' @aliases VID Vid vid
#' @docType data
#' @author JTA - The Data Scientists
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL
