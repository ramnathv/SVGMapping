\name{setStyleSVG}
\alias{setStyleSVG}
\alias{getStyleSVG}
\title{Set a style parameter of an SVG node}
\description{
  This function is provided to make it easy to modify a style parameter in a SVG file.
}
\usage{
setStyleSVG(node, style.parameter, style.value)
}
\arguments{
\item{node}{The SVG node of which you want to modify a style parameter. Modified in place.}
\item{style.parameter}{The style parameter to modify/create ("fill" for example)}
\item{style.value}{The value to set the style parameter to ("#FF0000" for example)}
}
\examples{
# We suppose there is an SVG object with id "path4017" in our file
mynode <- getNodeSet(mysvg, "//svg:path[@id=\\"path4017\\"]")[[1]]
# Suppose the original SVG data contains
# <path style="stroke:black;fill:green">
setStyleSVG(mynode, "fill", "red")
# It now contains
# <path style="stroke:black;fill:red">
}
\seealso{
  \itemize{
    \item{\code{\link{setAttributeSVG}} to modify an attribute}
  }
}
\keyword{misc}
