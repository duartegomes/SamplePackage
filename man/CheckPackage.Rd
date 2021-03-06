% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CheckPackage.R
\name{CheckPackage}
\alias{CheckPackage}
\title{Check for package dependencies}
\usage{
CheckPackage(lbr)
}
\arguments{
\item{lbr}{Library}
}
\description{
Check if the package exists and install in case doesn't exist.
}
\examples{
TAPChunks:::CheckPackage(x)
}
\seealso{
\code{\link{TAPChunks}}

Other Internal Utilities: \code{\link{AddDimension}},
  \code{\link{AddFirmographics}},
  \code{\link{AddHierarchy}}, \code{\link{CheckChunkInfo}},
  \code{\link{CheckDemogDB}}, \code{\link{CheckMemory}},
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
  \code{\link{TestNet}}, \code{\link{TestVPN}},
  \code{\link{TimeStamps}}, \code{\link{Unzip}},
  \code{\link{UpdateTAPChunks}},
  \code{\link{ValidateChunk}}, \code{\link{WriteFmt}}
}
\author{
JTA - The Data Scientists
}
\concept{Internal Utilities}
\keyword{internal}
