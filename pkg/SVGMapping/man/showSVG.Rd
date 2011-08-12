\name{showSVG}
\alias{showSVG}
\title{Add SVGMapping script and shows SVG file in browser}
\description{
  This function calls \code{\link{saveSVG}} to create a temporary SVG file,
  then creates a temporary HTML file containing the SVG file,
  and opens it with the default browser.
}
\usage{
showSVG(svg, browser=getOption("browser"), add.script=TRUE)
}
\arguments{
\item{svg}{SVG data (an \code{XMLInternalDocument})}
\item{browser}{This argument is passed to \code{\link{browseURL}}.}
\item{add.script}{Add the JavaScript script to the SVG file. Set to FALSE to prevent adding the script.}
}
\examples{
mysvg <- loadSVG(system.file("extdata/example2.svg", package="SVGMapping"))
\donttest{showSVG(mysvg)}
}
\keyword{misc}
