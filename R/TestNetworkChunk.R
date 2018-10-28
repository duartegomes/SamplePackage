#' @title Test Network (by Hypervisor) data
#' @family Test Data
#' @name TestNetworkhvChunk
#' @description
#' This is an example of a raw Networkhv chunk as we receive them from SpiceWorks. The
#' data file is limited to 500 records and relates to just one period - 2016M01.
#' @section File Columns:
#' \tabular{ll}{
#' \strong{Column Name} \tab \strong{Description} \cr
#' \emph{row_id} \tab This is the identifier of the original row from where the data was obtained.
#' This allows us to recreate the exact same representation that we received from SpiceWorks.\cr
#' \emph{uuid} \tab This is the standard org identifier for Spiceworks files.\cr
#' \emph{timestamp} \tab The month that the data was delivered from Spiceworks in the format 2015M01.\cr
#' \emph{variable}  \tab The name of the original Spiceworks product column or 'NoProd' if the record did not have any column values. \cr
#' \emph{value}     \tab The value that was in the original column. \cr
#' }
#' @section Example Content:
#' \tabular{lllr}{
#' \strong{uuid} \tab \strong{timestamp} \tab \strong{variable} \tab \strong{value}\cr
#' b2265... \tab 2016M01 \tab total_physical_servers  \tab 23\cr
#' b2265... \tab 2016M01 \tab total_server_vms        \tab  1\cr
#' b2265... \tab 2016M01 \tab vm_vmware               \tab  1\cr
#' b2265... \tab 2016M01 \tab total_server_os         \tab 24\cr
#' b2265... \tab 2016M01 \tab total_windows_server_os \tab 23\cr
#' 0235a... \tab 2016M01 \tab total_physical_servers  \tab  1\cr
#' 0235a... \tab 2016M01 \tab total_server_vms        \tab  1\cr
#' 0235a... \tab 2016M01 \tab vm_vmware               \tab  1\cr
#' 0235a... \tab 2016M01 \tab total_server_os         \tab  2
#' }
#' @section Variables Included:
#' \tabular{ll}{
#' \strong{variable} \tab \strong{Notes}\cr
#' \emph{ total_physical_servers } \tab \cr
#' \emph{ total_server_os } \tab \cr
#' \emph{ total_server_vms } \tab \cr
#' \emph{ total_windows_server_os } \tab \cr
#' \emph{ vm_virtual_box } \tab \cr
#' \emph{ vm_vmware } \tab \cr
#' \emph{ vm_wyse } \tab \cr
#' \emph{ vm_xen }\tab \cr}
#' @aliases vm_virtual_box vm_vmware vm_wyse vm_xen Networkhv networkhv
#' @docType data
#' @author JTA - The Data Scientists
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL

#' @title Test Network (by Operating System) data
#' @name TestNetworkosChunk
#' @family Test Data
#' @description
#' This is an example of a raw Networkos chunk as we receive them from SpiceWorks. The
#' data file is limited to 500 records and relates to just one period - 2016M01.
#' @section File Columns:
#' \tabular{ll}{
#' \strong{Column Name} \tab \strong{Description} \cr
#' \emph{row_id} \tab This is the identifier of the original row from where the data was obtained.
#' This allows us to recreate the exact same representation that we received from SpiceWorks.\cr
#' \emph{uuid} \tab This is the standard org identifier for Spiceworks files.\cr
#' \emph{timestamp} \tab The month that the data was delivered from Spiceworks in the format 2015M01.\cr
#' \emph{variable}  \tab The name of the original Spiceworks product column or 'NoProd' if the record did not have any column values. \cr
#' \emph{value}     \tab The value that was in the original column. \cr
#' }
#' @section Example Content:
#' \tabular{lllr}{
#' \strong{uuid} \tab \strong{timestamp} \tab \strong{variable} \tab \strong{value}\cr
#' b2265... \tab 2016M01 \tab total_physical_servers  \tab 23\cr
#' b2265... \tab 2016M01 \tab total_server_os         \tab 24\cr
#' b2265... \tab 2016M01 \tab total_windows_server_os \tab 23\cr
#' b2265... \tab 2016M01 \tab os_win_server_2000      \tab  4\cr
#' b2265... \tab 2016M01 \tab os_win_server_2003      \tab 17\cr
#' b2265... \tab 2016M01 \tab os_win_server_2008      \tab  2\cr
#' b2265... \tab 2016M01 \tab os_windows_2000         \tab  1\cr
#' 0235a... \tab 2016M01 \tab total_physical_servers  \tab  1\cr
#' 0235a... \tab 2016M01 \tab total_server_os         \tab  2
#' }
#' @section Variables Included:
#' \tabular{ll}{
#' \strong{variable} \tab \strong{Notes}\cr
#' \emph{ os_linux } \tab \cr
#' \emph{ os_mac_osx } \tab \cr
#' \emph{ os_redhat } \tab \cr
#' \emph{ os_win_server_2000 } \tab \cr
#' \emph{ os_win_server_2003 } \tab \cr
#' \emph{ os_win_server_2008 } \tab \cr
#' \emph{ os_win_server_2012 } \tab \cr
#' \emph{ os_windows_2000 } \tab \cr
#' \emph{ os_windows_xp } \tab \cr
#' \emph{ total_physical_servers } \tab \cr
#' \emph{ total_server_os } \tab \cr
#' \emph{ total_windows_server_os }\tab \cr}
#' @aliases os_linux os_mac_osx os_redhat
#' os_win_server_2000 os_win_server_2003 os_win_server_2008 os_win_server_2012
#' os_windows_2000 os_windows_xp Networkos networkos
#' total_physical_servers total_server_os total_windows_server_os
#' @docType data
#' @author JTA - The Data Scientists
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL
