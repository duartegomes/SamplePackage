% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Manipulation.R
\name{FilterChunk}
\alias{FilterChunk}
\title{Filtering data chunks}
\usage{
FilterChunk(Data, Filter = 1 == 1, ...)
}
\arguments{
\item{Data}{A data chunk}

\item{Filter}{A string defining the filter condition}

\item{...}{(TODO)}
}
\description{
This function allows users to apply a filter to a data chunk to receive
a subset of the input data.
}
\examples{
FilterChunk(TestEmailChunk, filter = "variable = 'exchange_2010'")
}
\seealso{
\code{\link{TAPChunks}}

Other Chunk Manipulators: \code{\link{AddFirmographics}},
  \code{\link{AddOSHierarchy}},
  \code{\link{AggregateChunk}},
  \code{\link{CalculateColumn}}, \code{\link{JoinChunks}},
  \code{\link{RMarkdown}}, \code{\link{ShapeChunk}},
  \code{\link{UseAsSlicer}}
}
\author{
Jonathan Tooley Associados Lda
}