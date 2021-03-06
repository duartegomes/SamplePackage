% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/YellowText.R
\name{YellowText}
\alias{YellowText}
\title{Printing Yellow Text}
\usage{
YellowText(Text)
}
\arguments{
\item{Text}{String to apply font color.}
}
\value{
Returns a string.
}
\description{
This function applies yellow font color to the text submitted by wrapping the text
with ANSI escape sequences.
}
\examples{
  cat(TAPChunks:::YellowText("Hello World!"))
message(TAPChunks:::YellowText("Status100"))
}
\seealso{
\code{\link{TAPChunks}}

Other Text Formatters: \code{\link{BlueText}},
  \code{\link{BoldText}}, \code{\link{GreenText}},
  \code{\link{ItalicText}}, \code{\link{RedText}},
  \code{\link{StrikeText}}, \code{\link{UnderLineText}}
}
\author{
JTA - The Data Scientists
}
\concept{Text Formatters}
\keyword{internal}
