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
#' @author JTA - The Data Scientists
#' @keywords internal
#' @seealso \code{\link{TAPChunks}}
NULL
