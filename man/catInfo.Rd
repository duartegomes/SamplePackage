% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/catInfo.R
\name{catInfo}
\alias{catInfo}
\title{Print Information Messages}
\usage{
catInfo(Text, Highlight = NULL)
}
\arguments{
\item{Text}{A string with the message that we wish to report to the user}

\item{Highlight}{A string that can be found in the string that was passed to the Text parameter
and which will be highlighted in bold text.
Defaults to NULL which results in no highlighting.}
}
\description{
Print Information message with Highlighed text
}
\details{
This is best shown by some examples:
\preformatted{
TAPChunks:::catInfo("Hello user.", "user")}
 - Hello \strong{user}.
\preformatted{
TAPChunks:::catInfo("Hello user.", "Name")}
 - Hello user.
}
\seealso{
\code{\link{TAPChunks}}

Other Console Messaging Functions: \code{\link{catError}},
  \code{\link{catStyle}}, \code{\link{catSuccess}},
  \code{\link{catSuperUser}}, \code{\link{catWarning}}
}
\author{
JTA - The Data Scientists
}
\concept{Console Messaging Functions}
\keyword{internal}
