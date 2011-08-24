\name{addScriptSVG}
\alias{addScriptSVG}
\title{Add a JavaScript script to an SVG file}
\description{
  This function adds a JavaScript script to an SVG data object.
}
\usage{
addScriptSVG(svg, script, id=NULL)
}
\arguments{
\item{svg}{SVG data (an \code{XMLInternalDocument}).}
\item{script}{A string containing the JavaScript code.}
\item{id}{The id to give to the script (optional).
If a script with this id already exists it will be replaced.}
}
\examples{
mysvg <- loadSVG(system.file("extdata/example.svg", package="SVGMapping"))
addScriptSVG(mysvg, "window.alert(\"Hello World\")")
\donttest{showSVG(mysvg)}
}
\keyword{misc}
\seealso{
  \itemize{
    \item{\code{\link{loadSVG}} to load your SVG file (to do before)}
  }
}