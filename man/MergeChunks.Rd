% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MergeChunks.R
\name{MergeChunks}
\alias{MergeChunks}
\title{Merge Chunks Objects}
\usage{
MergeChunks(x, y, ..., join = "inner")
}
\arguments{
\item{x, y}{Tables to be merged}

\item{...}{by Column names in x and y to merge on or
by.x, by.y Vectors of column names in x and y to merge on}

\item{join}{"inner", "outer", "left" or "right"}
}
\description{
This function merges 2 datasets/ chunks.
}
\examples{
MergeChunks(TestEmailChunk, TestClientChunk, by = "uuid, timestamp")
}
\seealso{
\code{\link{TAPChunks}}

Other Chunk Manipulators: \code{\link{AggregateChunk}},
  \code{\link{CalculateColumn}}, \code{\link{FilterChunk}},
  \code{\link{JoinChunks}}, \code{\link{SelectColumns}},
  \code{\link{ShapeChunk}}, \code{\link{ShowSampleSize}}
}
\author{
JTA - The Data Scientists
}
\concept{Chunk Manipulators}
