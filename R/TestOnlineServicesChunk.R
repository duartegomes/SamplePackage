#' @title Test
#' @name TestOnlineServicesChunk
#' @family Test Data
#' @description
#' This is an example of a raw Online Services chunk as we receive them from SpiceWorks. The
#' data file is limited to 500 records and relates to just one period - 2017M01
#' @section File Columns:
#' \tabular{ll}{
#' \strong{Column Name} \tab \strong{Description} \cr
#' \emph{row_id} \tab \cr
#' \emph{id} \tab \cr
#' \emph{uuid} \tab \cr
#' \emph{sw_agent_online} \tab \cr
#' \emph{os_name} \tab \cr
#' \emph{device_count} \tab \cr
#' \emph{timestamp} \tab \cr
#' \emph{variable} \tab \cr
#' \emph{value}\tab \cr}
#' @section Example Content:
#' \tabular{rlrlrllr}{
#' \strong{id} \tab \strong{uuid} \tab \strong{sw_agent_online} \tab \strong{os_name} \tab \strong{device_count} \tab \strong{timestamp} \tab \strong{variable} \tab \strong{value}\cr
#'  154845 \tab f50de... \tab 0 \tab Windows 10 Pro  \tab  1 \tab 2017M01 \tab google_drive_install \tab 1\cr
#'  395007 \tab f50de... \tab 0 \tab Windows 10 Pro  \tab  4 \tab 2017M01 \tab NoProd               \tab 1\cr
#' 1653143 \tab 5bfd2... \tab 0 \tab Windows 7 Pro   \tab  2 \tab 2017M01 \tab icloud_install       \tab 1\cr
#'  772342 \tab 5bfd2... \tab 0 \tab Windows 7 Pro   \tab  1 \tab 2017M01 \tab dropbox_install      \tab 1\cr
#'   86857 \tab 5bfd2... \tab 0 \tab Windows 7 Pro   \tab 15 \tab 2017M01 \tab NoProd               \tab 1\cr
#' 1485771 \tab ad5fe... \tab 0 \tab Windows 10 Pro  \tab  2 \tab 2017M01 \tab dropbox_install      \tab 1\cr
#'  606052 \tab ad5fe... \tab 0 \tab Windows 8.1 Pro \tab  1 \tab 2017M01 \tab NoProd               \tab 1\cr
#'  741731 \tab ad5fe... \tab 0 \tab Windows XP Pro  \tab  9 \tab 2017M01 \tab NoProd               \tab 1\cr
#'  969531 \tab ad5fe... \tab 0 \tab Vista Business  \tab  7 \tab 2017M01 \tab NoProd               \tab 1
#' }
#' @section Variables Included:
#' \tabular{ll}{
#' \strong{variable} \tab \strong{Notes}\cr
#' \emph{ dropbox_install } \tab \cr
#' \emph{ dropbox_online } \tab \cr
#' \emph{ google_apps_install } \tab \cr
#' \emph{ google_apps_online } \tab \cr
#' \emph{ google_docs_online } \tab \cr
#' \emph{ google_drive_install } \tab \cr
#' \emph{ google_drive_online } \tab \cr
#' \emph{ icloud_install } \tab \cr
#' \emph{ iwork_install } \tab \cr
#' \emph{ NoProd } \tab \cr
#' \emph{ o365_install } \tab \cr
#' \emph{ o365_online } \tab \cr
#' \emph{ onedrive_online } \tab \cr
#' \emph{ paid_google_docs_cloudservice }\tab \cr}
#' @aliases google_drive_install NoProd icloud_install
#' dropbox_install google_apps_online o365_install
#' google_apps_install iwork_install o365_online
#' dropbox_online paid_google_docs_cloudservice
#' google_drive_online google_docs_online onedrive_online
#' @docType data
#' @author JTA - The Data Scientists
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL
