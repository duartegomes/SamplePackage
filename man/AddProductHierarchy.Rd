% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AddProductHierarchy.R
\name{AddProductHierarchy}
\alias{AddProductHierarchy}
\title{Add Product hierarchy to a data chunk}
\usage{
AddProductHierarchy(Data, SelectHierarchy = NULL)
}
\arguments{
\item{Data}{This is a required field and is usually a TAP chunk but it can be any item of class "data.table" or "data.frame".
If the user submits a data frame then this will first be converted to a data table before the calculated column is added.
This is because data tables are far more efficient in R than a data frame.
The resulting table that is returned by this function will also be a data table even if a data frame was submitted.}

\item{SelectHierarchy}{Name of the Hierarchy to be added in the Chunk}
}
\description{
This function allows you to add a Product hierarchy to the chunk.
}
\section{Product Hierarchy}{

The raw data that we receive from SpiceWorks contains a great deal of product
information.  This is typically shown in the many product columns that we see in a typical
raw file and that gets transposed when written as a chunk.
The product descriptions are detailed and so to summarize our finindings it is necessary
to add a product hierarchy.
}

\section{Hierarchies available}{

\tabular{llll}{
\strong{Hierarchy Name}   \tab \strong{Level1} \tab \strong{Level2} \tab \strong{Level3}    \cr
\strong{Product (default)}          \tab Product0        \tab Product1        \tab Product2\cr
\strong{Product Type}     \tab Product0        \tab ProductType     \cr
\strong{Product Category} \tab Product0        \tab ProductCategory}
}

\section{Reference Data Repository}{

The Reference Data Repository (RDR) allows stores master data relating to
our taxonomy.  It can be used to add taxonomy hierarchies to chunks that have had
firmographic data added. Firmographic data is
established by our data load processes (which refer to the RDR to allocate firmographics)
and is stored in a separate database.

Functions that access the RDR require an internet connection which has access to
MS Corp net.
}

\examples{
AddProductHierarchy(TestEmailChunk, "ProductType")
}
\seealso{
\code{\link{TAPChunks}}

Other RDR Integration Tools: \code{\link{AddGSVHierarchy}},
  \code{\link{AddGeoHierarchy}},
  \code{\link{AddOSHierarchy}},
  \code{\link{AddSegmentHierarchy}},
  \code{\link{AddVerticalHierarchy}}
}
\author{
JTA - The Data Scientists
}
\concept{RDR Integration Tools}
