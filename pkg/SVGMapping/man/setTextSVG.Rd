\name{setTextSVG}
\alias{setTextSVG}
\title{Change a text label in an SVG file.}
\description{
  This function changes the content of a text element in the SVG file.
  The text element with attribute searchAttribute equal to searchAttributeValue will have
  its content replaced by the given text.
}
\usage{
setTextSVG(svg, searchAttributeValue, text, searchAttribute="inkscape:label")
}
\arguments{
\item{svg}{SVG data (an \code{XMLInternalDocument}). The variable will be modified in place.}
\item{searchAttributeValue}{The value of the search attribute of the text element to modify.}
\item{text}{The new text content to put in the text element.}
\item{searchAttribute}{The name of the search attribute to use.}
}
\examples{
mysvg <- loadSVG(system.file("extdata/example.svg", package="SVGMapping"))
# Here we use the "id" attribute to find the text element.
# We change the title of the figure to "Krebs cycle" instead of the original "TCA Cycle".
setTextSVG(mysvg, "text3091", "Krebs cycle", searchAttribute="id")
\donttest{showSVG(mysvg)}
}
\keyword{misc}
