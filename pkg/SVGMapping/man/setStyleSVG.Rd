\name{setStyleSVG}
\alias{setStyleSVG}
\alias{getStyleSVG}
\title{Set a style parameter of an SVG node}
\description{
  This functions are provided to make it easy to read/modify a style parameter in an SVG file.
}
\usage{
setStyleSVG(node, style.parameter, style.value)
getStyleSVG(node, style.parameter)
}
\arguments{
\item{node}{The SVG node of which you want to read/modify a style parameter. Modified in place.}
\item{style.parameter}{The style parameter to read/modify/create ("fill" for example)}
\item{style.value}{The value to set the style parameter to ("#FF0000" for example)}
}
\value{
For \code{getStyleSVG}, the value of the style parameter, or NULL if it does not exist.
}
\examples{
# This example changes the title "TCA Cycle" color to red.
mysvg <- loadSVG(system.file("extdata/example.svg", package="SVGMapping"))
mynode <- getNodeSet(mysvg, "//svg:text[@id=\"text3091\"]")[[1]]
getStyleSVG(mynode, "fill")
setStyleSVG(mynode, "fill", "red")
\donttest{showSVG(mysvg)}
}
\seealso{
  \itemize{
    \item{\code{\link{setAttributeSVG}} to modify an attribute}
  }
}
\keyword{misc}
