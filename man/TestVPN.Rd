% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TestVPN.R
\name{TestVPN}
\alias{TestVPN}
\title{Test For Internet Connectivity}
\usage{
TestVPN()
}
\value{
Returns a logical TRUE when the ping was successful else FALSE.
}
\description{
This is an internal function that can be used by other tools
to detect web connectivity.  This is so the load processes may
continue to work (via the cache files) even when the user is
offline.
}
\details{
This function only works on windows operating systems and
functions by sending a single ping request to "cmr-jta"
}
\examples{
ifelse (TAPChunks:::TestVPN(), "We have net connectivity.", "Offline")
}
\seealso{
\code{\link{TAPChunks}}

Other Internal Utilities: \code{\link{AddDimension}},
  \code{\link{AddFirmographics}},
  \code{\link{AddHierarchy}}, \code{\link{CheckChunkInfo}},
  \code{\link{CheckDemogDB}}, \code{\link{CheckMemory}},
  \code{\link{CheckPackage}},
  \code{\link{CheckRequirements}},
  \code{\link{CheckSourceYaml}},
  \code{\link{CheckSpaceDisk}}, \code{\link{ConnectToADL}},
  \code{\link{CreateTable}}, \code{\link{DetectError}},
  \code{\link{FormatList}}, \code{\link{GetConfiguration}},
  \code{\link{GetMapping}},
  \code{\link{GetTimestampByObjInternal}},
  \code{\link{GetTimestampByObj}}, \code{\link{HttpCode}},
  \code{\link{LoadDemogDB}},
  \code{\link{OpenSqlConnection}},
  \code{\link{OptionsMessage}}, \code{\link{OrderColumn}},
  \code{\link{RMarkdown}},
  \code{\link{RecreateReadADLChunkExpression}},
  \code{\link{SaveLocal}}, \code{\link{ShowADLPath}},
  \code{\link{ShowADLPrefix}},
  \code{\link{ShowConfigurationPath}},
  \code{\link{TestNet}}, \code{\link{TimeStamps}},
  \code{\link{Unzip}}, \code{\link{UpdateTAPChunks}},
  \code{\link{ValidateChunk}}, \code{\link{WriteFmt}}
}
\author{
JTA - The Data Scientists
}
\concept{Internal Utilities}
\keyword{internal}
