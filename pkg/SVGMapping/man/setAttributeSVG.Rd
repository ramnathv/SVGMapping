\name{setAttributeSVG}
\alias{setAttributeSVG}
\alias{getAttributeSVG}
\title{Set an attribtue value of an SVG node}
\description{
  This function is provided to make it easy to modify an attribute in a SVG file,
  while preserving namespace specification (like inkscape:label), which is not
  the case when using \code{xmlAttrs(node)[[attname]] <- attval}.
}
\usage{
setAttributeSVG(node, attname, attval)
getAttributeSVG(node, attname)
}
\arguments{
\item{node}{The SVG node of which you want to read/modify an attribute.}
\item{attname}{The attribute name to read/modify/create.}
\item{attval}{The value to set the attribute to.}
}
\value{
For \code{getAttributeSVG}, the value of the attribute, or NULL if it does not exist.
}
\examples{
mysvg <- loadSVG(system.file("extdata/example.svg", package="SVGMapping"))
root <- getNodeSet(mysvg, "//svg:svg")[[1]]
getAttributeSVG(root, "width")
setAttributeSVG(root, "width", 400)
\donttest{showSVG(mysvg)}
}
\seealso{
  \itemize{
    \item{\code{\link{setStyleSVG}} to modify a specific parameter in the "style" attribute}
  }
}
\keyword{misc}
