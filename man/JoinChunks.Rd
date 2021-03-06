% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/JoinChunks.R
\name{JoinChunks}
\alias{JoinChunks}
\title{Joining data chunks to form one}
\usage{
JoinChunks(...)
}
\arguments{
\item{...}{Chunks to be combined}
}
\value{
Returns a chunk with the chunks provided combined together
}
\description{
This function joins chunks of data together to form one appended table.
}
\details{
This function performs a row join of data chunks by appending the chunks
together. The resulting data chunk has all the data from all submitted chunks and
so is a full outer join. If the submitted chunks have the same column format then
the resulting chunk will also retain this format. If either of the submitted chunks
has a column not present in the other then this will not join.\cr\cr
The data is not reweighted after join.
}
\seealso{
\code{\link{TAPChunks}}

Other Chunk Manipulators: \code{\link{AggregateChunk}},
  \code{\link{CalculateColumn}}, \code{\link{FilterChunk}},
  \code{\link{MergeChunks}}, \code{\link{SelectColumns}},
  \code{\link{ShapeChunk}}, \code{\link{ShowSampleSize}}
}
\author{
JTA - The Data Scientists
}
\concept{Chunk Manipulators}
