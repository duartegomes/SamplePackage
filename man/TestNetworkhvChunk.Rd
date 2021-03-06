% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TestNetworkChunk.R
\docType{data}
\name{TestNetworkhvChunk}
\alias{TestNetworkhvChunk}
\alias{vm_virtual_box}
\alias{vm_vmware}
\alias{vm_wyse}
\alias{vm_xen}
\alias{Networkhv}
\alias{networkhv}
\title{Test Network (by Hypervisor) data}
\description{
This is an example of a raw Networkhv chunk as we receive them from SpiceWorks. The
data file is limited to 500 records and relates to just one period - 2016M01.
}
\section{File Columns}{

\tabular{ll}{
\strong{Column Name} \tab \strong{Description} \cr
\emph{row_id} \tab This is the identifier of the original row from where the data was obtained.
This allows us to recreate the exact same representation that we received from SpiceWorks.\cr
\emph{uuid} \tab This is the standard org identifier for Spiceworks files.\cr
\emph{timestamp} \tab The month that the data was delivered from Spiceworks in the format 2015M01.\cr
\emph{variable}  \tab The name of the original Spiceworks product column or 'NoProd' if the record did not have any column values. \cr
\emph{value}     \tab The value that was in the original column. \cr
}
}

\section{Example Content}{

\tabular{lllr}{
\strong{uuid} \tab \strong{timestamp} \tab \strong{variable} \tab \strong{value}\cr
b2265... \tab 2016M01 \tab total_physical_servers  \tab 23\cr
b2265... \tab 2016M01 \tab total_server_vms        \tab  1\cr
b2265... \tab 2016M01 \tab vm_vmware               \tab  1\cr
b2265... \tab 2016M01 \tab total_server_os         \tab 24\cr
b2265... \tab 2016M01 \tab total_windows_server_os \tab 23\cr
0235a... \tab 2016M01 \tab total_physical_servers  \tab  1\cr
0235a... \tab 2016M01 \tab total_server_vms        \tab  1\cr
0235a... \tab 2016M01 \tab vm_vmware               \tab  1\cr
0235a... \tab 2016M01 \tab total_server_os         \tab  2
}
}

\section{Variables Included}{

\tabular{ll}{
\strong{variable} \tab \strong{Notes}\cr
\emph{ total_physical_servers } \tab \cr
\emph{ total_server_os } \tab \cr
\emph{ total_server_vms } \tab \cr
\emph{ total_windows_server_os } \tab \cr
\emph{ vm_virtual_box } \tab \cr
\emph{ vm_vmware } \tab \cr
\emph{ vm_wyse } \tab \cr
\emph{ vm_xen }\tab \cr}
}

\seealso{
\code{\link{TAPChunks}}

Other Test Data: \code{\link{TestCCMCostChunk}},
  \code{\link{TestCCMUsageChunk}},
  \code{\link{TestClientChunk}},
  \code{\link{TestEmailChunk}},
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
