\name{loadSVG}
\alias{loadSVG}
\title{Load an SVG file for use with SVGMapping}
\description{
  This function loads an SVG file for use with SVGMapping.
  For more information, see \code{\link{mapDataSVG}}.
}
\usage{
loadSVG(file)
}
\arguments{
\item{file}{Filename from which to load the SVG, or any other value accepted by \code{\link{xmlTreeParse}}}
}
\value{
An \code{XMLInternalDocument}.
}
\examples{
mysvg <- loadSVG("myfile.svg")
}
\keyword{misc}
\seealso{
You can now use \code{\link{mapDataSVG}}.
}