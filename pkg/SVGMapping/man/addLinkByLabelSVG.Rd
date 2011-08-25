\name{addLinkByLabelSVG}
\alias{addLinkByLabelSVG}
\title{Add a link to an SVG element found by label}
\description{
  This function adds a clickable link to an SVG element.
  The element is specified by its label.
}
\usage{
addLinkByLabelSVG(svg, searchAttributeValue, url, what="*", searchAttribute = "inkscape:label")
}
\arguments{
\item{svg}{SVG data (an \code{XMLInternalDocument}). The variable will be modified in place.}
\item{searchAttributeValue}{The value of the search attribute of the text element to modify.}
\item{url}{The destination URL of the link.}
\item{what}{The type of SVG nodes to apply the data to, for example "svg:path". By default any type of SVG element.}
\item{searchAttribute}{The name of the search attribute to use.}
}
\examples{
mysvg <- loadSVG(system.file("extdata/example.svg", package="SVGMapping"))
# We add a link on the title (TCA cycle).
addLinkByLabelSVG(mysvg, "text3091", "http://en.wikipedia.org/wiki/TCA_cycle", searchAttribute="id")
\donttest{showSVG(mysvg)}
}
\seealso{
  \itemize{
    \item{\code{\link{addLinkSVG}} is a lower-level equivalent that directly takes the XML node as its argument}
  }
}
\keyword{misc}
