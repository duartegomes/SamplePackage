% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ValidateChunk.R
\name{ValidateChunk}
\alias{ValidateChunk}
\title{Validating data chunks}
\usage{
ValidateChunk(Data)
}
\arguments{
\item{Data}{A data chunk to be validated.}
}
\value{
Returns a logical TRUE if the chunk is valid else it returns FALSE.
}
\description{
This routine will validate data to ensure that it is a valid data chunk.  It is used
by many of the other functions before they start work on altering the chunk that
was presented.
}
\details{
In order to be valid the data presented must be a data table with a uuid column.
}
\examples{
TAPChunks:::ValidateChunk(email)
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
  \code{\link{TestNet}}, \code{\link{TestVPN}},
  \code{\link{TimeStamps}}, \code{\link{Unzip}},
  \code{\link{UpdateTAPChunks}}, \code{\link{WriteFmt}}
}
\author{
JTA - The Data Scientists
}
\concept{Internal Utilities}
\keyword{internal}
