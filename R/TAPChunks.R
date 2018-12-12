#' @title A package for curating and manipulating chunks of Telemetry data
#' @description
#' This is a package of functions developed to simplify working with telemetry data. The package
#' is part of the wider TAP architecture which is described here \code{\link{Architecture}}
#'
#' The TAPChunks package provides these categories of important functions:
#'  \enumerate{
#'   \item Data Lake Tools
#'   \item Cache Controls
#'   \item Chunk Manipulators
#'   \item RDR Integration Tools
#'   \item Weighting and Cleaning Functions
#'   \item Modeling Tools
#'   \item Compute Tools
#'   \item Reporting and Saving Tools
#'   \item Utilities
#'   \item Example data files
#' }
#'
#' @section 1. Data Lake Tools:
#' For the current release the TAPChunks package works with a series of data
#' files that are loaded by an internal CMR process to our Azure Data Lake.  This uses
#' big data technologies to give us a highly scalable storage area with telemetry data. \cr\cr
#' \strong{Functions:} \cr
#' List files in the Data Lake  \code{\link{ListADLDirectory}} \cr
#' Download Files from Blob Storage  \code{\link{ReadABSFile}} \cr
#' Reading Source Chunks from the Data Lake  \code{\link{ReadADLChunk}} \cr
#' Reading user data from the Data Lake  \code{\link{ReadADLFile}} \cr
#' Save file in the Data Lake  \code{\link{UploadLocallyToADL}} \cr
#'
#' @section 2. Cache Controls:
#' All data must be copied from the data lake to a local environment called the cache.  This is
#' a directory that the TAPChunks package will establish on the user's PC where copies of the
#' chunks can be stored.\cr\cr
#' \strong{Functions:} \cr
#' Deleting files from the local cache  \code{\link{DeleteCache}} \cr
#' Delete outdated Rdata files in cache folder  \code{\link{DeleteOutdatedCache}} \cr
#' List files in the local cache  \code{\link{ListCacheDirectory}} \cr
#' Detecting when data on the Data Lake has been updated  \code{\link{RefreshCache}} \cr
#' Establishing, pointing and set to the cache folder  \code{\link{ShowCachePath}} \cr
#' Detecting when data on the Data Lake has been updated  \code{\link{ShowCacheStatus}} \cr
#'
#' @section 3. Chunk Manipulators:
#' The functions in this section are all designed to allow for the simple manipulation
#' and summarization of data chunks. \cr\cr
#' \strong{Functions:} \cr
#' Aggregate chunks to create smaller chunks  \code{\link{AggregateChunk}} \cr
#' Add Calculated Columns to data chunks  \code{\link{CalculateColumn}} \cr
#' Filtering data chunks  \code{\link{FilterChunk}} \cr
#' Joining two data chunks to form one  \code{\link{JoinChunks}} \cr
#' Merge Chunks Objects  \code{\link{MergeChunks}} \cr
#' Select Columns in Chunk  \code{\link{SelectColumns}} \cr
#' Shape chunk - Pivot and Unpivot  \code{\link{ShapeChunk}} \cr
#' Summarizing a file to see the sample size  \code{\link{ShowSampleSize}} \cr
#'
#' @section 4. RDR Integration Tools:
#' \cr\cr
#' \strong{Functions:} \cr
#' Add Demographics to a data chunk\code{\link{AddFirmographics}}
#' Add Geography hierarchy to a data chunk  \code{\link{AddGeoHierarchy}} \cr
#' Adding Firmographics (Geo, Segment and Vertical)  \code{\link{AddGSVHierarchy}} \cr
#' Add OS Mappings to a Data Chunk  \code{\link{AddOSHierarchy}} \cr
#' Add Product hierarchy to a data chunk  \code{\link{AddProductHierarchy}} \cr
#' Add Segment hierarchy to a data chunk  \code{\link{AddSegmentHierarchy}} \cr
#' Add Vertical hierarchy to a data chunk  \code{\link{AddVerticalHierarchy}} \cr
#'
#' @section 5. Weighting and cleaning data:
#' The data chunks that we receive from Spiceworks are stored in a raw format that is completely
#' faithful to the original data.  This means that the chunks may have unclean data and the data
#' is not weighted.  Because of this the TAPChunks package offers some simple functions to help
#' the user perform these tasks.\cr\cr
#' \strong{Functions:} \cr
#' Forming the Longitudinal Data Set  \code{\link{ApplyLongitudinalSet}} \cr
#' Weighting and cleaning functions: Applying weights to data  \code{\link{ApplyWeightCalc}} \cr
#' Stripping Spiceworks chunks of unclean records  \code{\link{CleanChunk}} \cr
#' Show the Size of a Longitudinal Set  \code{\link{ShowLongitudinalSize}} \cr
#' Reviewing a weight calculation  \code{\link{ShowWeightCalc}} \cr
#'
#' @section 6. Modeling Tools:
#' \cr\cr
#' \strong{Functions:} \cr
#' Preparing data for IPF  \code{\link{PrepareIPF}} \cr
#' Run an Iterative Proportional Fit  \code{\link{RunIPF}} \cr
#'
#' @section 7. Compute Tools:
#' \cr\cr
#' \strong{Functions:} \cr
#' Delimiter for use when setting up Iterations  \code{\link{BeginIterate}} \cr
#' Clean large tables from the memory  \code{\link{CleanMemory}} \cr
#' Delimiter for use when setting up Iterations  \code{\link{EndIterate}} \cr
#' Run a script iteratively over a series of months  \code{\link{IterateScript}} \cr
#' Protect a table in memory from deletion  \code{\link{Protect}} \cr
#' Unprotect the object that has been assigned as Protected  \code{\link{Unprotect}} \cr
#'
#' @section 8. Reporting and saving tools:
#' The functions in this section allow the user to deliver cubes and reports more easily. \cr\cr
#' \strong{Functions:} \cr
#' Reporting and saving tools: Appending data to a disk file  \code{\link{AppendCSV}} \cr
#' Reporting and saving tools: Calculate penetration in Data Set  \code{\link{ApplyPenetration}} \cr
#' Export to SQL  \code{\link{ExportToSQL}} \cr
#' Publish File into the Data Lake  \code{\link{PublishADL}} \cr
#' Save file in the Data Lake  \code{\link{SaveADL}} \cr
#' UseAsSlicer  \code{\link{UseAsSlicer}} \cr
#'
#' @section 9. Utilities:
#' These are functions that are used to assist users.
#' package.  \cr\cr
#' \strong{Functions:}
#' Pass Data to an Excel Spreadsheet  \code{\link{Xcel}} \cr
#'
#' @section 10. Example data files:
#' In order to help users to run the examples given in the help pages there
#' are example data files provided. These can be used for experimentation, training and
#' as test data to ensure scripts are working. They are exact copies of Spiceworks data
#' but are limited to only 500 records each (except for the TestEmailChunk which has two
#' periods and 1000 records.  \cr\cr
#' \strong{Data Files:} \cr
#' \code{\link{TestCCMCostChunk}} \cr
#' \code{\link{TestCCMUsageChunk}} \cr
#' \code{\link{TestClientChunk}} \cr
#' \code{\link{TestEmailChunk}} \cr
#' \code{\link{TestNetworkhvChunk}} \cr
#' \code{\link{TestNetworkosChunk}} \cr
#' \code{\link{TestOnlineServicesChunk}} \cr
#' \code{\link{TestServerRolesChunk}} \cr
#' \code{\link{TestVIDChunk}} \cr
#' \code{\link{TestWorkloadDBChunk}} \cr
#' \code{\link{TestWorkloadEcoChunk}} \cr
#' \code{\link{TestWorkloadHWChunk}} \cr
#' \code{\link{TestWorkloadNChunk}} \cr
#' \code{\link{TestWorkloadVIDChunk}} \cr
#' \code{\link{TestWorkloadWLChunk}} \cr
#'
#' @docType package
#' @name TAPChunks
NULL

#' @title TAP Architecture
#' @name Architecture
#' @description
#' \if{html}{\figure{architecture.jpg}{options: width="60\%" alt="Figure: architecture.jpg"}}
#' \if{latex}{\figure{architecture.pdf}{options: width="6in" alt="Figure: architecture.pdf"}}
NULL

#' @export
.onLoad <- function(libname, pkgname){
  packageStartupMessage(paste('Welcome to TAPChunks version:', packageVersion("TAPChunks")))
  packageStartupMessage("For more information please type '?TAPChunks'. or visit the on-line manual at")
  packageStartupMessage("http://portalsbi/sites/marketresearch/Shared%20Documents/TAP/TAPChunks.html")
  #UpdateTAPChunks()
}
