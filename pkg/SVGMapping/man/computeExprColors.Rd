\name{computeExprColors}
\alias{computeExprColors}
\title{Compute expression colors from microarray-like data}
\description{
This function is used by \code{\link{mapDataSVG}} to compute colors from log-ratios.
It computes a vector of colors from a vector of microarray expression values.
The defaults parameters are designed with two-color microarray log-ratios in mind.
}
\usage{
myexprcolors <- computeExprColors(X, col=microarrayColors, NA.color="#999999", a=-2, b=2)
}
\arguments{
\item{X}{The (numeric) vector of expression values.}
\item{col}{The color vector, in #RRGGBB format, to map uniformly to [a,b]}
\item{a,b}{The range of values that \code{col} represents. Values lower that a or higher than be
are respectively treated as a and b.}
\item{NA.color}{The color to use for NAs in X.}
}
\keyword{misc}
