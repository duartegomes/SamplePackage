#' TAPChunks: A package for curating and manipulating chunks of Telemetry data
#'
#' The TAPChunks package provides these categories of important functions:
#'  \enumerate{
#'   \item Understanding what is available on the data lake
#'   \item Controlling your local cache
#'   \item Weighting and cleaning data
#'   \item Manipulating data
#'   \item Reporting data
#'   \item Internal utilities
#'   \item Example data files
#' }
#'
#' @section 1. Understanding what is available on the data lake:
#' For the current release the TAPChunks package works with a series of data
#' files that are loaded by an internal CMR process to our Azure Data Lake.  This uses
#' big data technologies to give us a highly scalable storage area with telemetry data. \cr\cr
#' \strong{Functions:} \cr
#' See what is in the Data Lake:      \code{\link{ADLDirectory}} \cr
#' To copy chunks from the lake to the cache: \code{\link{ADLReadChunk}} \cr
#' To copy files  from the lake to the cache: \code{\link{ADLRead}} \cr
#' To read files from Azure Blob Storage: \code{\link{ABSRead}} \cr
#' To save files in the Data Lake: \code{\link{ADLSave}} \cr
#'
#'
#'
#' @section 2. Controlling your local cache:
#' All data must be copied from the data lake to a local environment called the cache.  This is
#' a directory that the TAPChunks package will establish on the user's PC where copies of the
#' chunks can be stored.\cr\cr
#' \strong{Functions:} \cr
#' To get the path of the local cache: \code{\link{CachePath}} \cr
#' To delete files from your cache: \code{\link{DeleteCache}} \cr
#' To list files that are in your cache: \code{\link{CacheDirectory}} \cr
#' To refresh data that has been superseded on the lake: \code{\link{RefreshCache}} \cr
#' To see if data has been superseded on the lake: \code{\link{ShowCacheStatus}} \cr
#' To copy chunks from the lake to the cache: \code{\link{ADLRead}}
#'
#' @section 3. Weighting and cleaning data:
#' The data chunks that we receive from Spiceworks are stored in a raw format that is completely
#' faithful to the original data.  This means that the chunks may have unclean data and the data
#' is not weighted.  Because of this the TAPChunks package offers some simple functions to help
#' the user perform these tasks.\cr\cr
#' \strong{Functions:} \cr
#' To keep only the clean data in a chunk: \code{\link{CleanChunk}} \cr
#' To review the weight calculation: \code{\link{WeightCalculation}} \cr
#' To apply weights to a chunk:  \cr
#'
#' @section 4. Manipulating data:
#' The functions in this section are all designed to allow for the simple manipulation
#' and summarization of data chunks. \cr\cr
#' \strong{Functions:} \cr
#' To generate weighted and unweighted org counts:  \cr
#' To generate weighted and unweighted device counts:  \cr
#' To generate weighted and unweighted install counts:  \cr
#' To keep only the data from orgs which exist in all time periods:  \cr
#' To filter items from data: \code{\link{FilterChunk}} \cr
#' To join chunks keeping all data: \code{\link{JoinChunks}} \cr
#' To join chunks keeping only the data for orgs in the intersection:  \cr
#'
#' @section 5. Reporting data:
#' The functions in this section allow the user to deliver cubes and reports more easily. \cr\cr
#' \strong{Functions:} \cr
#'
#' @section 6. Internal utilities:
#' These are functions that are typically only used internally within the TAPChunks
#' package.  \cr\cr
#' \strong{Functions:} \cr
#' To check that the system has internet access: \code{\link{TestNet}} \cr
#' To check the latest demographics data: \code{\link{CheckDemogDB}} \cr
#' To load the latest demographics data: \code{\link{LoadDemogDB}} \cr
#' To open a connection to the data lake: \code{\link{ConnectToADL}} \cr
#' To generate a list of Spiceworks timestamps: \code{\link{TimeStamps}} \cr
#' To check that data is a valid chunk: \code{\link{ValidateChunk}} \cr
#' To get the Path on the data lake to a source: \code{\link{ADLPath}} \cr
#' To get the file prefix used to distinguish a source: \code{\link{ADLPrefix}} \cr
#'
#' @section 7. Example data files:
#' In order to help users to run the examples given in the help pages there
#' are example data files provided. These can be used for experimentation, training and
#' as test data to ensure scripts are working. They are exact copies of Spiceworks data
#' but are limited to only 500 records each.  \cr\cr
#' \strong{Data:} \cr
#' \tabular{ll}{
#' Email       \tab \code{\link{TestEmailChunk}}  \cr
#' Client      \tab \code{\link{TestClientChunk}} \cr
#' Networkhv   \tab \code{\link{TestNetworkhvChunk}}   \cr
#' Networkos   \tab \code{\link{TestNetworkosChunk}}   \cr
#' ServerRoles \tab \code{\link{TestServerRolesChunk}} \cr
#' WorkloadDB  \tab \code{\link{TestWorkloadDBChunk}}  \cr
#' WorkloadEco \tab \code{\link{TestWorkloadEcoChunk}} \cr
#' WorkloadHW  \tab \code{\link{TestWorkloadHWChunk}} \cr
#' WorkloadN   \tab \code{\link{TestWorkloadNChunk}} \cr
#' WorkloadVID \tab \code{\link{TestWorkloadVIDChunk}} \cr
#' WorkloadWL  \tab \code{\link{TestWorkloadWLChunk}} \cr
#'}
#' @docType package
#' @name TAPChunks
NULL









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
#' @author Jonathan Tooley Associados Lda
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL

#' @title Test Workload (Database Edition) data
#' @name TestWorkloadDBChunk
#' @family Test Data
#' @description
#' This is an example of a raw Workload (Database Edition) chunk as we receive them from SpiceWorks. The
#' data file is limited to 500 records and relates to just one period - 2016M01.
#' @section File Columns:
#' \tabular{ll}{
#' \strong{Column Name} \tab \strong{Description} \cr
#' \emph{row_id} \tab \cr
#' \emph{uuid} \tab \cr
#' \emph{server_roles_uuid} \tab \cr
#' \emph{timestamp} \tab \cr
#' \emph{variable} \tab \cr
#' \emph{value}\tab \cr}
#' @section Example Content:
#' \tabular{llllr}{
#' \strong{uuid} \tab \strong{server_roles_uuid} \tab \strong{timestamp} \tab \strong{variable} \tab \strong{value}\cr
#' 0235a... \tab                 \tab 2016M01 \tab db_sql_server_2005_express \tab 1\cr
#' 0235a... \tab 2fa78c8d879b... \tab 2016M01 \tab server_count               \tab 1\cr
#' 0235a... \tab                 \tab 2016M01 \tab server_count               \tab 1\cr
#' f50de... \tab 3f00cde0c363e...\tab 2016M01 \tab server_count               \tab 1\cr
#' f50de... \tab db06dcf2d730c...\tab 2016M01 \tab server_count               \tab 1\cr
#' f50de... \tab 304af45a698c9...\tab 2016M01 \tab server_count               \tab 1\cr
#' f50de... \tab f1028cb368849...\tab 2016M01 \tab server_count               \tab 1\cr
#' f50de... \tab 33ae81ff9bd84...\tab 2016M01 \tab server_count               \tab 1\cr
#' f50de... \tab 9d9c5222e860c...\tab 2016M01 \tab server_count               \tab 1
#' }
#' @section Variables Included:
#' \tabular{ll}{
#' \strong{variable} \tab \strong{Notes}\cr
#' \emph{ db_mysql_51_older_revised } \tab \cr
#' \emph{ db_mysql_55_revised } \tab \cr
#' \emph{ db_mysql_56_revised } \tab \cr
#' \emph{ db_mysql_old } \tab \cr
#' \emph{ db_mysql_revised } \tab \cr
#' \emph{ db_postgres_old } \tab \cr
#' \emph{ db_postgres_revised } \tab \cr
#' \emph{ db_postgresql_9_revised } \tab \cr
#' \emph{ db_sql_server_2000_other } \tab \cr
#' \emph{ db_sql_server_2005_express } \tab \cr
#' \emph{ db_sql_server_2005_old } \tab \cr
#' \emph{ db_sql_server_2005_standard } \tab \cr
#' \emph{ db_sql_server_2008_enterprise_revised } \tab \cr
#' \emph{ db_sql_server_2008_express_revised } \tab \cr
#' \emph{ db_sql_server_2008_old_revised } \tab \cr
#' \emph{ db_sql_server_2008_r2_enterprise_revised } \tab \cr
#' \emph{ db_sql_server_2008_r2_express_revised } \tab \cr
#' \emph{ db_sql_server_2008_r2_old_revised } \tab \cr
#' \emph{ db_sql_server_2008_r2_other_revised } \tab \cr
#' \emph{ db_sql_server_2008_r2_standard_revised } \tab \cr
#' \emph{ db_sql_server_2008_standard_revised } \tab \cr
#' \emph{ db_sql_server_2012_enterprise } \tab \cr
#' \emph{ db_sql_server_2012_express } \tab \cr
#' \emph{ db_sql_server_2012_other } \tab \cr
#' \emph{ db_sql_server_2012_standard } \tab \cr
#' \emph{ db_sql_server_2014_express } \tab \cr
#' \emph{ server_count }\tab \cr}
#' @aliases db_mysql_51_older_revised db_mysql_55_revised db_mysql_56_revised
#' db_mysql_old db_mysql_revised db_postgres_old db_postgres_revised db_postgresql_9_revised
#' db_sql_server_2000_other db_sql_server_2005_express db_sql_server_2005_old
#' db_sql_server_2005_standard db_sql_server_2008_enterprise_revised db_sql_server_2008_express_revised
#' db_sql_server_2008_old_revised db_sql_server_2008_r2_enterprise_revised
#' db_sql_server_2008_r2_express_revised db_sql_server_2008_r2_old_revised
#' db_sql_server_2008_r2_other_revised db_sql_server_2008_r2_standard_revised
#' db_sql_server_2008_standard_revised db_sql_server_2012_enterprise db_sql_server_2012_express
#' db_sql_server_2012_other db_sql_server_2012_standard db_sql_server_2014_express WorkloadDB workloaddb
#' @docType data
#' @author Jonathan Tooley Associados Lda
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL

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
#' @author Jonathan Tooley Associados Lda
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL

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
#' @author Jonathan Tooley Associados Lda
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL

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
#' @author Jonathan Tooley Associados Lda
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL

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
#' @author Jonathan Tooley Associados Lda
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL

#' @title Test Workload (WL Edition) data
#' @name TestWorkloadWLChunk
#' @family Test Data
#' @description
#' This is an example of a raw Workload (WL Edition) chunk as we receive them from SpiceWorks. The
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
#' 0235a... \tab Windows SBS 2003       \tab         \tab        \tab P \tab N \tab 0 \tab 2016M01 \tab app_dev_deployment   \tab 1\cr
#' 0235a... \tab Windows SBS 2003       \tab         \tab        \tab P \tab N \tab 0 \tab 2016M01 \tab collaborative_apps   \tab 2\cr
#' 0235a... \tab Windows SBS 2003       \tab         \tab        \tab P \tab N \tab 0 \tab 2016M01 \tab security             \tab 1\cr
#' 0235a... \tab Windows SBS 2003       \tab         \tab        \tab P \tab N \tab 0 \tab 2016M01 \tab msft_exchange        \tab 2\cr
#' 0235a... \tab Windows Server 2012 R2 \tab 2fa78c8d\tab vmware \tab V \tab N \tab 0 \tab 2016M01 \tab server_count         \tab 1\cr
#' 0235a... \tab Windows SBS 2003       \tab         \tab        \tab P \tab N \tab 0 \tab 2016M01 \tab server_count         \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab db06dcf2\tab msft   \tab V \tab N \tab 0 \tab 2016M01 \tab app_dev_deployment   \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab 33ae81ff\tab msft   \tab V \tab N \tab 0 \tab 2016M01 \tab app_dev_deployment   \tab 1\cr
#' f50de... \tab Windows Server 2012 R2 \tab db06dcf2\tab msft   \tab V \tab N \tab 0 \tab 2016M01 \tab authoring_publishing \tab 1
#' }
#' @section Variables Included:
#' \tabular{ll}{
#' \strong{variable} \tab \strong{Notes}\cr
#' \emph{ app_dev_deployment } \tab \cr
#' \emph{ authoring_publishing } \tab \cr
#' \emph{ collaborative_apps } \tab \cr
#' \emph{ engineering } \tab \cr
#' \emph{ msft_exchange } \tab \cr
#' \emph{ msft_lync } \tab \cr
#' \emph{ msft_sharepoint } \tab \cr
#' \emph{ os_and_storage_sw } \tab \cr
#' \emph{ other_db_and_bi } \tab \cr
#' \emph{ security } \tab \cr
#' \emph{ server_count } \tab \cr
#' \emph{ system_infrastructure_network_mgmt }\tab \cr}
#' @aliases app_dev_deployment authoring_publishing collaborative_apps engineering
#' msft_exchange msft_lync msft_sharepoint os_and_storage_sw
#' other_db_and_bi security system_infrastructure_network_mgmt WorkloadWL workloadwl
#' @docType data
#' @author Jonathan Tooley Associados Lda
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL

#' @title Test CCM Cost Data
#' @name TestCCMCostChunk
#' @family Test Data
#' @description
#' This is an example of a raw CCM Cost chunk as we receive them from SpiceWorks. The
#' data file is limited to 500 records and relates to just one period - 2017M01
#' @section File Columns:
#' \tabular{ll}{
#' \strong{Column Name} \tab \strong{Description} \cr
#' \emph{uuid} \tab \cr
#' \emph{platform} \tab \cr
#' \emph{service} \tab \cr
#' \emph{resource_type} \tab \cr
#' \emph{cost} \tab \cr
#' \emph{timestamp} \tab \cr
#' \emph{row_id} \tab \cr
#' \emph{NoProd}\tab \cr}
#' @section Example Content:
#' \tabular{llllrll}{
#' \strong{uuid} \tab \strong{platform} \tab \strong{service} \tab \strong{resource_type} \tab \strong{cost} \tab \strong{timestamp} \tab \strong{NoProd}\cr
#' d3a89... \tab Azure \tab Networking        \tab Microsoft.Network/publicIPAddresses \tab   0.00 \tab 2017M01 \tab NA\cr
#' d3a89... \tab Azure \tab Security Center   \tab microsoft.security/pricingtiers     \tab   0.00 \tab 2017M01 \tab NA\cr
#' d3a89... \tab Azure \tab Recovery Services \tab Microsoft.RecoveryServices/vaults   \tab  25.05 \tab 2017M01 \tab NA\cr
#' d3a89... \tab Azure \tab Storage           \tab Microsoft.RecoveryServices/vaults   \tab 145.30 \tab 2017M01 \tab NA\cr
#' d3a89... \tab Azure \tab Storage           \tab Microsoft.Storage/storageAccounts   \tab 285.34 \tab 2017M01 \tab NA\cr
#' d3a89... \tab Azure \tab Data Management   \tab Microsoft.Storage/storageAccounts   \tab   2.37 \tab 2017M01 \tab NA\cr
#' d3a89... \tab Azure \tab Networking        \tab Microsoft.Storage/storageAccounts   \tab   0.07 \tab 2017M01 \tab NA\cr
#' d3a89... \tab Azure \tab Networking        \tab Microsoft.Compute/virtualMachines   \tab   2.51 \tab 2017M01 \tab NA\cr
#' d3a89... \tab Azure \tab Virtual Machines  \tab Microsoft.Compute/virtualMachines   \tab 282.10 \tab 2017M01 \tab NA
#' }
#' @aliases CCMCost
#' @docType data
#' @author Jonathan Tooley Associados Lda
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL

#' @title Test CCM Usage Data
#' @name TestCCMUsageChunk
#' @family Test Data
#' @description
#' This is an example of a raw CCM Usage chunk as we receive them from SpiceWorks. The
#' data file is limited to 500 records and relates to just one period - 2017M01
#' @section File Columns:
#' \tabular{ll}{
#' \strong{Column Name} \tab \strong{Description} \cr
#' \emph{uuid} \tab \cr
#' \emph{platform} \tab \cr
#' \emph{service} \tab \cr
#' \emph{resource_type} \tab \cr
#' \emph{os} \tab \cr
#' \emph{region} \tab \cr
#' \emph{metric} \tab \cr
#' \emph{value} \tab \cr
#' \emph{unit} \tab \cr
#' \emph{timestamp} \tab \cr
#' \emph{row_id} \tab \cr
#' \emph{NoProd}\tab \cr}
#' @section Example Content:
#' \tabular{lllllllllll}{
#' \strong{uuid} \tab \strong{platform} \tab \strong{service} \tab \strong{resource_type} \tab \strong{os} \tab \strong{region} \tab \strong{metric} \tab \strong{value} \tab \strong{unit} \tab \strong{timestamp} \tab \strong{NoProd}\cr
#' 93a3b... \tab Azure \tab Data Services \tab Microsoft.Sql/servers/databases          \tab  \tab australiaeast \tab Data Services - Basic Database Days - SQL Database              \tab 792      \tab Hours     \tab 2017M01 \tab NA\cr
#' 93a3b... \tab Azure \tab Storage       \tab Microsoft.Storage/storageAccounts        \tab  \tab australiaeast \tab Storage - Standard IO - Table (GB) - Locally Redundant          \tab 7.20E-05 \tab GB        \tab 2017M01 \tab NA\cr
#' 93a3b... \tab Azure \tab Storage       \tab Microsoft.Storage/storageAccounts        \tab  \tab australiaeast \tab Storage - Standard IO - Hot Block Blob (GB) - Locally Redundant \tab 0.00012  \tab GB        \tab 2017M01 \tab NA\cr
#' 93a3b... \tab Azure \tab Storage       \tab Microsoft.ClassicStorage/storageAccounts \tab  \tab australiaeast \tab Storage - Standard IO - Page Blob/Disk (GB) - Geo Redundant     \tab 1.742206 \tab GB        \tab 2017M01 \tab NA\cr
#' 65163... \tab Azure \tab Networking    \tab Microsoft.ClassicNetwork/virtualNetworks \tab  \tab westeurope    \tab Networking - Virtual Network                                    \tab TRUE     \tab Existence \tab 2017M01 \tab NA\cr
#' 65163... \tab Azure \tab Networking    \tab Microsoft.Network/virtualNetworks        \tab  \tab eastus        \tab Networking - Virtual Network                                    \tab TRUE     \tab Existence \tab 2017M01 \tab NA\cr
#' 65163... \tab Azure \tab Storage       \tab Microsoft.Storage/storageAccounts        \tab  \tab eastus        \tab Storage - Standard IO - Page Blob/Disk (GB) - Locally Redundant \tab 0.33392  \tab GB        \tab 2017M01 \tab NA\cr
#' 65163... \tab Azure \tab Storage       \tab Microsoft.Storage/storageAccounts        \tab  \tab eastus        \tab Storage - Standard IO - Table (GB) - Locally Redundant          \tab 4.80E-05 \tab GB        \tab 2017M01 \tab NA\cr
#' 65163... \tab Azure \tab Data Services \tab Microsoft.Sql/servers/databases          \tab  \tab westeurope    \tab Data Services - Standard S1 Database Days - SQL Database        \tab 350      \tab Hours     \tab 2017M01 \tab NA
#' }
#' @aliases CCMUsage
#'
#' @docType data
#' @author Jonathan Tooley Associados Lda
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL




