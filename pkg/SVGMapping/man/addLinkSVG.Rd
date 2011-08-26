\name{addLinkSVG}
\alias{addLinkSVG}
\title{Add a link to an SVG element}
\description{
  This function adds a clickable link to an SVG element.
  The original XML node is removed, because a new one is created as a child of the new <a> node.
}
\usage{
addLinkSVG(node, url)
}
\arguments{
\item{node}{The SVG node we add the clickable link to.}
\item{url}{The destination URL of the link.}
}
\value{
The new XML node, as the original node is no longer in the tree.
}
\seealso{
  \itemize{
    \item{\code{\link{addLinkByLabelSVG}} does the same but the node is specified by label}
  }
}
\keyword{misc}
