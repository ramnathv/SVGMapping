\name{devSVGMapping}
\alias{devSVGMapping}
\title{Create a pseudo graphic device}
\description{
  This function creates a pseudo graphic device. Such device is mostly a wraparound a \sQuote{real} Cairo device. It allows to substitute rectangular object within the templates by standard R graphics outputs (eg \code{\link{plot}}). 
}
\usage{
devSVGMapping(template, file, attribute.name="@inkscape:label", attribute.value="Rplot", width, height, pointsize=10)
}
\arguments{
\item{template}{the template used as a pseud-device for outputs (see the details section). This template must be loaded using the \code{\link{loadSVG}} function.}
\item{file}{the name of the output file. The page number is substituted if a C integer format is included in the character string, as in the default. (The result must be less than PATH_MAX characters long, and may be truncated if not. See postscript for further details.) Tilde expansion is performed where supported by the platform.}
\item{attribute.name}{the attribute name used to select the targeted destination object. Note that only rectangular object can be used as potential target.}
\item{attribute.value}{the attibute value to be match by \code{attribute.name} to select the targeted destination object.}
}
\details{
The destination object is identified by an \code{attribute.name}, \sQuote{@inkscape:label} by default, which match a given \code{attribute.value}, \sQuote{Rplot} by default. Furthermore, this object should a \sQuote{rect} object as defined by the SVG recommendation. After procesing, the target object is substituted with the temporary output content. \code{width} and \code{height} arguments control the size of the temporary output SVG file. Since the template target object may have a different size a scale operation is performed on the temporary output. 

The \code{pointsize} argument controls the global font size of all text strings in the temporary outputs. Note that since a scaling operation is performed during inclusion (see above), the included text size may differ from the one given here.
}
\section{Requirements}{ 
Pseudo-device requires that a cairo engine is correctly installed on your system. Either using the builtin implementation or the Cairo package. During the package initialization we dectect if such device is available and load the required package if necessary.

Pseudo-devices work as a wraparound the underlying real device. It means that all graphic commands will be output to a temporary SVG file. After using the \code{\link{dev.off}} function, the temporary output is included in the processed template using \code{\link{includeSVG}}.
}
\value{
  Returns a length-one named integer vector giving the
  number and name of the active device.
}
\examples{
mysvg <- loadSVG(system.file("extdata/device-example.svg", package="SVGMapping"))

## substitute the rectangle labelled 'Rplot1' 
## with a density plot of a gaussian distribution
devSVGMapping(mysvg, "@inkscape:label", "Rplot1")
plot(density(x=rnorm(100)))
dev.off()

showSVG(mysvg)
}
\keyword{graphics device}
\seealso{
Alternatively you can include your own SVG files using \code{\link{includeSVG}}.
}