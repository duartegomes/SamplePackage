% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AppendCSV.R
\name{AppendCSV}
\alias{AppendCSV}
\title{Appending data to a disk file}
\usage{
AppendCSV(..., Folder, Name, Remove = F)
}
\arguments{
\item{...}{files to append}

\item{Folder}{Folder where files are, if missing: CachePath will be called}

\item{Name}{Name to save CSV}

\item{Remove}{boolean: if true all input files files will be deleted after being appended}
}
\description{
This function allows users to append a set of csv files into one single file.
}
\seealso{
\code{\link{TAPChunks}}

Other Reporting and saving tools: \code{\link{ApplyPenetration}},
  \code{\link{ExportToSQL}}, \code{\link{PublishADL}},
  \code{\link{SaveADL}}, \code{\link{UseAsSlicer}}
}
\author{
JTA - The Data Scientists
}
\concept{Reporting and saving tools}
