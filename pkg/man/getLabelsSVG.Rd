\name{getLabelsSVG}
\alias{getLabelsSVG}
\title{Get all gene labels of SVG file}
\description{
Get the set of all genes present in SVG file.
}
\usage{
getLabelsSVG(svg, what="*", geneAttribute="inkscape:label")
}
\arguments{
\item{svg}{SVG data (an \code{XMLInternalDocument}).}
\item{what}{The type of SVG nodes to consider, for example "svg:path". By default any type of SVG element.}
\item{geneAttribute}{The SVG attribute that tells which gene the element is.}
}
\seealso{
  \itemize{
    \item{\code{\link{loadSVG}} to load your SVG file (to do before)}
  }
}
\keyword{misc}
