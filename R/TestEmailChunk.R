#' @title Test Email data
#' @name TestEmailChunk
#' @family Test Data
#' @description
#' This is an example of a raw Email chunk as we receive them from SpiceWorks. The
#' data file is limited to 1000 records.  There are 500 records relating to 2015M01 and 500 relating to
#' 2016M01. This file can therefore be used to illustrate the functioning of the longitudinal set function.
#' @section File Columns:
#' \tabular{ll}{
#' \strong{Column Name} \tab \strong{Description} \cr
#' \emph{row_id} \tab This is the identifier of the original row from where the data was obtained.
#' This allows us to recreate the exact same representation that we received from SpiceWorks.\cr
#' \emph{uuid} \tab This is the standard org identifier for Spiceworks files.\cr
#' \emph{sw_flag} \tab Data provided by Spiceworks.  The definition is not clear.  \cr
#' \emph{email_delivery} \tab Antispam, Hosted or Onsite. \cr
#' \emph{email_server} \tab Data on the email server used by the org. \cr
#' \emph{host} \tab Data on the email host used by the org. \cr
#' \emph{timestamp} \tab The month that the data was delivered from Spiceworks in the format 2015M01.\cr
#' \emph{variable} \tab The name of the original Spiceworks product column or 'NoProd' if the record did not have any column values. \cr
#' \emph{value} \tab The value that was in the original column. \cr
#' }
#' @section Example Content:
#' \tabular{lrlllllr}{
#'  \strong{uuid} \tab \strong{sw_flag} \tab \strong{email_delivery} \tab \strong{email_server} \tab \strong{host} \tab \strong{timestamp} \tab \strong{variable} \tab \strong{value}\cr
#'  b2265... \tab 1 \tab onsite   \tab microsoft                         \tab on site                  \tab 2015M01 \tab exchange_other \tab 1\cr
#'  0235a... \tab 1 \tab hosted   \tab                                   \tab office 365 - protection  \tab 2015M01 \tab exchange_2003  \tab 1\cr
#'  0235a... \tab 1 \tab hosted   \tab                                   \tab office 365 - protection  \tab 2015M01 \tab lotus_notes_v8 \tab 1\cr
#'  a517a... \tab 1 \tab onsite   \tab exim                              \tab on site                  \tab 2015M01 \tab NoProd         \tab 1\cr
#'  c5f2d... \tab 1 \tab onsite   \tab Lookup/Timeout Error              \tab on site                  \tab 2015M01 \tab exchange_2003  \tab 1\cr
#'  c5f2d... \tab 1 \tab onsite   \tab Lookup/Timeout Error              \tab on site                  \tab 2015M01 \tab exchange_other \tab 5\cr
#'  21502... \tab 1 \tab onsite   \tab Symantec Messaging Gateway - SPAM \tab on site                  \tab 2015M01 \tab exchange_2007  \tab 3\cr
#'  54788... \tab 1 \tab antispam \tab                                   \tab Roaring Penguin Software \tab 2015M01 \tab NoProd         \tab 1\cr
#'  34c25... \tab 0 \tab hosted   \tab                                   \tab other                    \tab 2015M01 \tab NoProd         \tab 1
#' }
#' @section Variables Included:
#' \tabular{ll}{
#' \strong{variable} \tab \strong{Notes}\cr
#' \emph{exchange_2000} \tab \cr
#' \emph{exchange_2003} \tab \cr
#' \emph{exchange_2007} \tab \cr
#' \emph{exchange_2007_685.25} \tab \cr
#' \emph{exchange_2010} \tab \cr
#' \emph{exchange_2013} \tab \cr
#' \emph{exchange_other} \tab \cr
#' \emph{groupwise} \tab \cr
#' \emph{lotus_notes_other} \tab \cr
#' \emph{lotus_notes_v6} \tab \cr
#' \emph{lotus_notes_v7} \tab \cr
#' \emph{lotus_notes_v8} \tab \cr
#' \emph{microsoft_sbs} \tab \cr
#' \emph{NoProd} \tab The Spiceworks data file includes orgs that have no on prem products
#' listed. In order for this data to fit the chunk format these records have a virtual
#' product called \emph{NoProd} added for completion. \cr
#' }
#' @aliases Email email exchange_2000 exchange_2003 exchange_2007 exchange_2007_685.25
#' exchange_2010 exchange_2013 exchange_other
#' groupwise lotus_notes_other lotus_notes_v6 lotus_notes_v7 lotus_notes_v8
#' microsoft_sbs NoProd
#' @docType data
#' @author JTA - The Data Scientists
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL
