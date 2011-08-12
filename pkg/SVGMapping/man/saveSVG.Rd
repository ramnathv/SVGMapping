\name{saveSVG}
\alias{saveSVG}
\title{Add SVGMapping script and saves SVG file}
\description{
  This function adds a JavaScript script to the SVG file which is necessary to display tooltips,
  and then saves the SVG data to a file, or returns it as a string.
  This functions calls \code{\link{cat}} and therefore accepts the same possible values for
  the \code{file} argument, and returns the same value in the same circumstances.
}
\usage{
saveSVG(svg, file="", add.script=TRUE)
}
\arguments{
\item{svg}{SVG data (an \code{XMLInternalDocument})}
\item{file}{Same as the \code{file} argument in \code{\link{cat}}}
\item{add.script}{Add the JavaScript script to the SVG file. Set to FALSE to prevent adding the script.}
}
\value{
Same as \code{\link{cat}}.
}
\keyword{misc}
