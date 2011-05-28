\name{setAttributeSVG}
\alias{setAttributeSVG}
\alias{getAttributeSVG}
\title{Set an attribtue value of an SVG node}
\description{
  This function is provided to make it easy to modify an attribute in a SVG file,
  while preserving namespace specification (like inkscape:label), which is not
  the case when using \code{xmlAttrs(node)[[attname]] = attval}.
}
\usage{
setAttributeSVG(node, attname, attval)
}
\arguments{
\item{node}{The SVG node of which you want to modify an attribute. Modified in place.}
\item{attname}{The attribute name to modify/create}
\item{attval}{The value to set the attribute to}
}
\examples{
root <- getNodeSet(mysvg, "//svg:svg")[[1]]
setAttributeSVG(root, "width", 1000)
}
\seealso{
  \itemize{
    \item{\code{\link{setStyleSVG}} to modify a specific parameter in the "style" attribute}
  }
}
\keyword{misc}
