\name{includeSVG}
\alias{includeSVG}
\title{Include an SVG in a template}
\description{
This function allows to substitute a target rectangular shape with the content of a SVG file.
}
\usage{
includeSVG(template, file, attribute.name="@inkscape:label", attribute.value="Rplot")
}
\arguments{
\item{template}{the template used as a pseud-device for outputs (see the details section). This template must be loaded using the \code{\link{loadSVG}} function.}
\item{file}{the name of the output file. The page number is substituted if a C integer format is included in the character string, as in the default. (The result must be less than PATH_MAX characters long, and may be truncated if not. See postscript for further details.) Tilde expansion is performed where supported by the platform.}
\item{attribute.name}{the attribute name used to select the targeted destination object. Note that only rectangular object can be used as potential target.}
\item{attribute.value}{the attibute value to be match by \code{attribute.name} to select the targeted destination object.}
}
\details{
The destination object is identified by an \code{attribute.name}, \sQuote{@inkscape:label} by default, which match a given \code{attribute.value}, \sQuote{Rplot} by default. Furthermore, this object should a \sQuote{rect} object as defined by the SVG recommendation. After procesing, the target object is substituted with the temporary output content. Since the template target object may have a different size a scale operation is performed on the temporary output. 
}
\examples{
mysvg <- loadSVG(system.file("extdata/device-example.svg", package="SVGMapping"))

## substitute the rectangle labelled 'Rplot1' 
## with a density plot of a gaussian distribution
includeSVG(mysvg, system.file("extdata/gnokii-Apple2.svg", package="SVGMapping"), "@inkscape:label", "Rplot1")

showSVG(mysvg)
}
\seealso{
Consider \code{\link{devSVGMapping}} for direct drawing using standard R graphics commands.
}
