\name{addDefinesSVG}
\alias{addDefinesSVG}
\title{Add nodes to the "defs" section.}
\description{
  This function makes it easier to add SVG nodes to the "defs" section,
  which is where the SVG standard recommends to put all elements which are referred
  to later in the document (like gradients for example).
}
\usage{
addDefinesSVG(svg, nodes)
}
\arguments{
\item{svg}{SVG data (an \code{XMLInternalDocument}). The variable will be modified in place.}
\item{nodes}{The XML Nodes (as created by newXMLNode) to add in the defs section.
You can supply a list of nodes or a single node.}
}
\examples{
mysvg <- loadSVG(system.file("extdata/example.svg", package="SVGMapping"))
# Remove the gradient of the original file
removeNodes(getNodeSet(mysvg, "//svg:linearGradient[@id=\"linearGradient3887\"]")[[1]])
# Create a new gradient
stop1 <- newXMLNode("stop", attrs=list(offset=0, style="stop-color:red"))
stop2 <- newXMLNode("stop", attrs=list(offset=1, style="stop-color:blue"))
gradient <- newXMLNode("linearGradient", attrs=list(id="linearGradient3887"), .children=list(stop1,stop2))
# And add it to the "defs" section
addDefinesSVG(mysvg, gradient)
\donttest{showSVG(mysvg)}
}
\keyword{misc}
