#' @title Test ServerRoles data
#' @name TestServerRolesChunk
#' @family Test Data
#' @description
#' This is an example of a raw Server Role chunk as we receive them from SpiceWorks. The
#' data file is limited to 500 records and relates to just one period - 2016M01.
#' @section File Columns:
#' \tabular{ll}{
#' \strong{Column Name} \tab \strong{Description} \cr
#' \emph{row_id} \tab This is the identifier of the original row from where the data was obtained.
#' This allows us to recreate the exact same representation that we received from SpiceWorks.\cr
#' \emph{id} \tab \cr
#' \emph{uuid} \tab This is the standard org identifier for Spiceworks files.\cr
#' \emph{server_roles_uuid} \tab \cr
#' \emph{timestamp} \tab \cr
#' \emph{variable} The name of the original Spiceworks Product Column.\tab \cr
#' \emph{value}\tab \cr}
#' @section Example Content:
#' \tabular{rllllr}{
#' \strong{id} \tab \strong{uuid} \tab \strong{server_roles_uuid} \tab \strong{timestamp} \tab \strong{variable} \tab \strong{value}\cr
#' 1 \tab 0235a... \tab 2fa78c8.... \tab 2016M01 \tab cat_base               \tab 1\cr
#' 1 \tab 0235a... \tab 2fa78c8.... \tab 2016M01 \tab file_and_storage_481   \tab 1\cr
#' 1 \tab 0235a... \tab 2fa78c8.... \tab 2016M01 \tab cat_born_cloud         \tab 1\cr
#' 1 \tab 0235a... \tab 2fa78c8.... \tab 2016M01 \tab web_2                  \tab 1\cr
#' 1 \tab 0235a... \tab 2fa78c8.... \tab 2016M01 \tab dotnet_45_features_466 \tab 1\cr
#' 1 \tab 0235a... \tab 2fa78c8.... \tab 2016M01 \tab cat_cloud_infra        \tab 1\cr
#' 1 \tab 0235a... \tab 2fa78c8.... \tab 2016M01 \tab powershell_417         \tab 1\cr
#' 1 \tab 0235a... \tab 2fa78c8.... \tab 2016M01 \tab cat_legacy             \tab 1\cr
#' 1 \tab 0235a... \tab 2fa78c8.... \tab 2016M01 \tab wow64_support_340      \tab 1
#' }
#' @section Variables Included:
#' \tabular{ll}{
#' \strong{variable} \tab \strong{Notes}\cr
#' \emph{ ad_cert_svcs_16 } \tab \cr
#' \emph{ ad_domain_svcs_10 } \tab \cr
#' \emph{ ad_fed_svcs_8 } \tab \cr
#' \emph{ ad_lite_dir_svcs_9 } \tab \cr
#' \emph{ apps_1 } \tab \cr
#' \emph{ branchcache_324 } \tab \cr
#' \emph{ cat_base } \tab \cr
#' \emph{ cat_born_cloud } \tab \cr
#' \emph{ cat_cloud_infra } \tab \cr
#' \emph{ cat_legacy } \tab \cr
#' \emph{ dhcp_12 } \tab \cr
#' \emph{ dns_13 } \tab \cr
#' \emph{ dotnet_351_36 } \tab \cr
#' \emph{ dotnet_45_features_466 } \tab \cr
#' \emph{ essntls_exprnce_485 } \tab \cr
#' \emph{ failover_cluster_33 } \tab \cr
#' \emph{ file_6 } \tab \cr
#' \emph{ file_and_storage_481 } \tab \cr
#' \emph{ group_policy_69 } \tab \cr
#' \emph{ hyperv_20 } \tab \cr
#' \emph{ msg_queuing_49 } \tab \cr
#' \emph{ multipath_io_57 } \tab \cr
#' \emph{ powershell_417 } \tab \cr
#' \emph{ powershell_int_scripting_351 } \tab \cr
#' \emph{ print_7 } \tab \cr
#' \emph{ remote_admin_67 } \tab \cr
#' \emph{ remote_desktop_18 } \tab \cr
#' \emph{ telnet_client_44 } \tab \cr
#' \emph{ web_2 } \tab \cr
#' \emph{ win_activation_svcs_41 } \tab \cr
#' \emph{ win_deploy_svcs_19 } \tab \cr
#' \emph{ win_powershell_66 } \tab \cr
#' \emph{ win_server_update_21 } \tab \cr
#' \emph{ wins_svr_40 } \tab \cr
#' \emph{ winsvr_backup_39 } \tab \cr
#' \emph{ wow64_support_340 }\tab \cr}
#' @aliases ad_cert_svcs_16 ad_domain_svcs_10 ad_fed_svcs_8 ad_lite_dir_svcs_9
#' apps_1 branchcache_324 cat_base cat_born_cloud cat_cloud_infra cat_legacy
#' dhcp_12 dns_13 dotnet_351_36 dotnet_45_features_466 essntls_exprnce_485
#' failover_cluster_33 file_6 file_and_storage_481 group_policy_69 hyperv_20
#' msg_queuing_49 multipath_io_57 powershell_417 powershell_int_scripting_351
#' print_7 remote_admin_67 remote_desktop_18 telnet_client_44 web_2
#' win_activation_svcs_41 win_deploy_svcs_19 win_powershell_66 win_server_update_21
#' wins_svr_40 winsvr_backup_39 wow64_support_340 ServerRoles serverroles
#' @docType data
#' @author JTA - The Data Scientists
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL
