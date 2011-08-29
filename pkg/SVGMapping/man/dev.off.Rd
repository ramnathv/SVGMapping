\name{dev.off}
\alias{dev.off}
\title{Close a pseudo graphic device}
\description{
  This function closes a pseudo graphic device opened with \code{\link{devSVGMapping}}. 
}
\usage{
dev.off(which=dev.cur())
}
\arguments{
\item{which}{an integer specifying a device number.}
}
\details{
Given a device number this function first calls \code{\link[grDevices]{dev.off}} of the \code{grDevices} package to actually closed the \sQuote{real} underlying device. If this device has been created using \code{\link{devSVGMapping}} then graphics output are used for substitution in the related template. If it is not the case, the process stop. 
}
\seealso{
\sQuote{real} underlying devices are closed using the \code{\link[grDevices]{dev.off}} function in the \code{grDevices} package.
}
