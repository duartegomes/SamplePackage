% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TestCCMCostChunk.R
\docType{data}
\name{TestCCMCostChunk}
\alias{TestCCMCostChunk}
\alias{CCMCost}
\title{Test CCM Cost Data}
\description{
This is an example of a raw CCM Cost chunk as we receive them from SpiceWorks. The
data file is limited to 500 records and relates to just one period - 2017M01
}
\section{File Columns}{

\tabular{ll}{
\strong{Column Name} \tab \strong{Description} \cr
\emph{uuid} \tab \cr
\emph{platform} \tab \cr
\emph{service} \tab \cr
\emph{resource_type} \tab \cr
\emph{cost} \tab \cr
\emph{timestamp} \tab \cr
\emph{row_id} \tab \cr
\emph{NoProd}\tab \cr}
}

\section{Example Content}{

\tabular{llllrll}{
\strong{uuid} \tab \strong{platform} \tab \strong{service} \tab \strong{resource_type} \tab \strong{cost} \tab \strong{timestamp} \tab \strong{NoProd}\cr
d3a89... \tab Azure \tab Networking        \tab Microsoft.Network/publicIPAddresses \tab   0.00 \tab 2017M01 \tab NA\cr
d3a89... \tab Azure \tab Security Center   \tab microsoft.security/pricingtiers     \tab   0.00 \tab 2017M01 \tab NA\cr
d3a89... \tab Azure \tab Recovery Services \tab Microsoft.RecoveryServices/vaults   \tab  25.05 \tab 2017M01 \tab NA\cr
d3a89... \tab Azure \tab Storage           \tab Microsoft.RecoveryServices/vaults   \tab 145.30 \tab 2017M01 \tab NA\cr
d3a89... \tab Azure \tab Storage           \tab Microsoft.Storage/storageAccounts   \tab 285.34 \tab 2017M01 \tab NA\cr
d3a89... \tab Azure \tab Data Management   \tab Microsoft.Storage/storageAccounts   \tab   2.37 \tab 2017M01 \tab NA\cr
d3a89... \tab Azure \tab Networking        \tab Microsoft.Storage/storageAccounts   \tab   0.07 \tab 2017M01 \tab NA\cr
d3a89... \tab Azure \tab Networking        \tab Microsoft.Compute/virtualMachines   \tab   2.51 \tab 2017M01 \tab NA\cr
d3a89... \tab Azure \tab Virtual Machines  \tab Microsoft.Compute/virtualMachines   \tab 282.10 \tab 2017M01 \tab NA
}
}

\seealso{
\code{\link{TAPChunks}}

Other Test Data: \code{\link{TestCCMUsageChunk}},
  \code{\link{TestClientChunk}},
  \code{\link{TestEmailChunk}},
  \code{\link{TestNetworkhvChunk}},
  \code{\link{TestNetworkosChunk}},
  \code{\link{TestOnlineServicesChunk}},
  \code{\link{TestServerRolesChunk}},
  \code{\link{TestVIDChunk}},
  \code{\link{TestWorkloadDBChunk}},
  \code{\link{TestWorkloadEcoChunk}},
  \code{\link{TestWorkloadHWChunk}},
  \code{\link{TestWorkloadNChunk}},
  \code{\link{TestWorkloadVIDChunk}},
  \code{\link{TestWorkloadWLChunk}}
}
\author{
JTA - The Data Scientists
}
\concept{Test Data}
\keyword{internal}
