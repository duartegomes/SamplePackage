% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FilterChunk.R
\name{FilterChunk}
\alias{FilterChunk}
\title{Filtering data chunks}
\usage{
FilterChunk(Data, Filter = 1 == 1, ...)
}
\arguments{
\item{Data}{A data chunk}

\item{Filter}{A string defining the filter condition.  This is a calculation that must
return a boolean value of TRUE or FALSE. For a lot more information on writing calculations
please refer to \code{\link{CalculateColumn}}.}
}
\description{
This function allows users to apply a filter to a data chunk to receive
a subset of the input data.
}
\examples{
FilterChunk(TestEmailChunk, Filter = variable == 'exchange_2010')
}
\seealso{
\code{\link{TAPChunks}}

Other Chunk Manipulators: \code{\link{AggregateChunk}},
  \code{\link{CalculateColumn}}, \code{\link{JoinChunks}},
  \code{\link{MergeChunks}}, \code{\link{SelectColumns}},
  \code{\link{ShapeChunk}}, \code{\link{ShowSampleSize}}
}
\author{
JTA - The Data Scientists
}
\concept{Chunk Manipulators}
