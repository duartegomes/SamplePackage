#' @title Test Client data
#' @name TestClientChunk
#' @family Test Data
#' @description
#' This is an example of a raw Client chunk as we receive them from SpiceWorks. The
#' data file is limited to 500 records and relates to just one period - 2016M01.
#' @section File Columns:
#' \tabular{ll}{
#' \strong{Column Name} \tab \strong{Description} \cr
#' \emph{row_id} \tab This is the identifier of the original row from where the data was obtained.
#' This allows us to recreate the exact same representation that we received from SpiceWorks. \cr
#' \emph{id} \tab This is the original id applied by Spiceworks. Its use is deprecated.\cr
#' \emph{uuid} \tab This is the standard org identifier for Spiceworks files.\cr
#' \emph{msft_office_2010_standard_revised_flag} \tab \cr
#' \emph{iwork_ofm_ie_flag} \tab \cr
#' \emph{slack_flag} \tab \cr
#' \emph{oem} \tab \cr
#' \emph{form_factor} \tab \cr
#' \emph{os_name} \tab \cr
#' \emph{timestamp} \tab The month that the data was delivered from Spiceworks in the format 2015M01.\cr
#' \emph{variable} \tab The name of the original Spiceworks product column.\cr
#' \emph{value}\tab The value that was in the original column.\cr}
#' @section Example Content:
#' \tabular{rlrrrlllllr}{
#' \strong{id} \tab \strong{uuid} \tab \strong{msft_office_2010_standard_revised_flag} \tab \strong{iwork_ofm_ie_flag} \tab \strong{slack_flag} \tab \strong{oem} \tab \strong{form_factor} \tab \strong{os_name} \tab \strong{timestamp} \tab \strong{variable} \tab \strong{value}\cr
#' 903 \tab f50de... \tab 1 \tab 1 \tab 1 \tab asustek  \tab L \tab Windows 10 Pro \tab 2016M01 \tab device_count              \tab 1\cr
#' 904 \tab f50de... \tab 1 \tab 1 \tab 1 \tab gigabyte \tab D \tab Windows 10 Pro \tab 2016M01 \tab device_count              \tab 1\cr
#' 905 \tab f50de... \tab 1 \tab 1 \tab 1 \tab lenovo   \tab L \tab Windows 10 Pro \tab 2016M01 \tab device_count              \tab 1\cr
#' 906 \tab f50de... \tab 1 \tab 1 \tab 1 \tab gigabyte \tab D \tab Windows 10 Pro \tab 2016M01 \tab device_count              \tab 1\cr
#' 907 \tab f50de... \tab 1 \tab 1 \tab 1 \tab Other    \tab D \tab Windows 10 Pro \tab 2016M01 \tab device_count              \tab 1\cr
#' 904 \tab f50de... \tab 1 \tab 1 \tab 1 \tab gigabyte \tab D \tab Windows 10 Pro \tab 2016M01 \tab msft_office_2013_pro_plus \tab 1\cr
#' 906 \tab f50de... \tab 1 \tab 1 \tab 1 \tab gigabyte \tab D \tab Windows 10 Pro \tab 2016M01 \tab msft_office_2013_pro_plus \tab 1\cr
#' 907 \tab f50de... \tab 1 \tab 1 \tab 1 \tab Other    \tab D \tab Windows 10 Pro \tab 2016M01 \tab msft_office_2013_pro_plus \tab 1\cr
#' 907 \tab f50de... \tab 1 \tab 1 \tab 1 \tab Other    \tab D \tab Windows 10 Pro \tab 2016M01 \tab google_drive              \tab 1
#' }
#' @section Variables Included:
#' \tabular{ll}{
#' \strong{variable} \tab \strong{Notes}\cr
#' \emph{ apple_icloud } \tab \cr
#' \emph{ box } \tab \cr
#' \emph{ cisco_webex } \tab \cr
#' \emph{ device_count } \tab \cr
#' \emph{ dropbox } \tab \cr
#' \emph{ google_apps } \tab \cr
#' \emph{ google_chrome } \tab \cr
#' \emph{ google_drive } \tab \cr
#' \emph{ internet_explorer } \tab \cr
#' \emph{ itunes } \tab \cr
#' \emph{ mozilla_firefox } \tab \cr
#' \emph{ msft_office_2000 } \tab \cr
#' \emph{ msft_office_2003_basic } \tab \cr
#' \emph{ msft_office_2003_pro } \tab \cr
#' \emph{ msft_office_2007_basic } \tab \cr
#' \emph{ msft_office_2007_pro_plus } \tab \cr
#' \emph{ msft_office_2007_smb } \tab \cr
#' \emph{ msft_office_2010_home } \tab \cr
#' \emph{ msft_office_2010_pro } \tab \cr
#' \emph{ msft_office_2010_pro_plus } \tab \cr
#' \emph{ msft_office_2010_standard_revised } \tab \cr
#' \emph{ msft_office_2013_pro } \tab \cr
#' \emph{ msft_office_2013_pro_plus } \tab \cr
#' \emph{ msft_office_2013_standard } \tab \cr
#' \emph{ msft_office_365 } \tab \cr
#' \emph{ msft_skydrive } \tab \cr
#' \emph{ msft_works } \tab \cr
#' \emph{ pandora } \tab \cr
#' \emph{ skype }\tab \cr}
#' @aliases Client client apple_icloud box cisco_webex
#' dropbox google_apps google_chrome google_drive
#' internet_explorer itunes mozilla_firefox msft_office_2000
#' msft_office_2003_basic msft_office_2003_pro msft_office_2007_basic
#' msft_office_2007_pro_plus msft_office_2007_smb msft_office_2010_home
#' msft_office_2010_pro msft_office_2010_pro_plus msft_office_2010_standard_revised
#' msft_office_2013_pro msft_office_2013_pro_plus msft_office_2013_standard
#' msft_office_365 msft_skydrive msft_works pandora skype
#' @docType data
#' @author JTA - The Data Scientists
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL
