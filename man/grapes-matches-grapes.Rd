% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matches.R
\name{\%matches\%}
\alias{\%matches\%}
\title{Matching text patterns to data in chunks}
\usage{
Pattern \%matches\% Y
}
\arguments{
\item{Pattern}{A string that contains a regular expression.}

\item{Y}{A string or a vector of strings.}
}
\description{
This is a function that can match a string pattern to data in a chunk
}
\details{
A feature that is typical to SpiceWorks chunks is a series of variables that
we wish to group together into one. To see an example of this consider the various
variables that are used to describe versions of Microsoft Exchange: \cr\cr
exchange_other \cr
exchange_2003 \cr
exchange_2007 \cr
exchange_2010 \cr
exchange_2000 \cr
exchange_2013 and \cr
exchange_2007_685.25 \cr\cr
This function allows the user to easily select all of these by matching these
variables to a string pattern. For example \emph{"Exchange" \%matches\% variable} is a filter condition
that will extract all rows where the string \emph{Exchange} occurs in the variable.  The test is not case
sensitive.
}
\examples{
"Exchange" \%matches\% TestEmailChunk$variable
}
\seealso{
\code{\link{TAPChunks}}
}
\author{
JTA - The Data Scientists
}
