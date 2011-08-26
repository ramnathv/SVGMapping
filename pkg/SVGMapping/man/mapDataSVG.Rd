\name{mapDataSVG}
\alias{mapDataSVG}
\title{Maps microarray-like data to an SVG file}
\description{
This is the main function of SVGMapping. It allows you to map microarray data (or any other type of data) to an SVG file.
Typically, you can create an SVG file showing a cell mechanism with genes as full circles, and you want the circles to be
coloured according to the gene differential expression (red for induced and green for repressed, for example).
This function allows you to do that, as well as other variants designed with the same idea in mind (see options below).
For example, the circles can be replaced by pie charts showing different experiments or different time values.
It "finds" the SVG objects that represent the genes by using the value of an attribute, by default it is designed
to use the Inkscape "label" field, but this can be changed to any other SVG attribute you want.
This function also allows you to add tooltips (JavaScript-based) showing both the fold change and gene function (for example).
This function can be run several times on the same SVG data to add different information.
}
\usage{
mapDataSVG(svg, numData, tooltipData=numData,
           mode="fill", what="*",
           geneAttribute="inkscape:label",
           col=microarrayColors, NA.color="#999999", colrange=c(-2,2),
           annotation=NULL,
           fillAngle=NULL, pieStartAngle=NULL, pieClockwise=TRUE)
}
\arguments{
\item{svg}{SVG data (an \code{XMLInternalDocument}). The variable will be modified in place.}
\item{numData}{A numeric vector or matrix with genes (character) as rows.
Depending on the mode, the matrix may or may not have several columns (to display several experiments).}
\item{tooltipData}{A vector/matrix with the same shape as numData, but containing the values to be displayed in
the JavaScript-based tooltip window. If you don't want a tooltip window, set this to NULL.
For example, this allows you to map log-ratios to the color range, but display fold changes.
Note that this argument is only used if \code{mode} is a about coloring (ie. is one of "fill", "stroke", "pie", "tooltip-only"),
because we need to compute a color for background in the tooltip window.}
\item{mode}{What you want this function to do.
  \itemize{
  \item{Objects can be colored with "fill" and "stroke" (depending on what to color).
  The colors will be computed by mapping the \code{colrange} (vector with two values) to the colors provided in \code{col}.
  For NA values, \code{NA.color} will be used. The color format is #RRGGBB with hex values.
  The default behaviour is to use a gradient where dark green is -2,
  green is -1, white is 0, red is 1 and dark red is 2 (it is meant to be used for log-ratios from two-color microarrays).}
  \item{If you only want a tooltip window use "tooltip-only".}
  \item{It is also possible to display pie charts (set to "pie") that show different conditions (columns in \code{numData}),
  in which case the SVG elements with the labels are expected to be circles or Inkscape-pseudo-circles
  (paths with sodipodi:cx/cy/rx attributes).}
  \item{An alternative to pie charts for multi-experiment mode is to fill objects with several color stripes.
  If you want to do that, use "fill" with a multi-column matrix for \code{numData}.}
  \item{The "partial-fill" mode makes it possible to display partially filled flasks where the liquid level is
  the numeric data in \code{numData}. This mode works by creating an SVG mask.}
  \item{The last modes are "fill-opacity", "stroke-opacity" and "stroke-width" that set the attribute their name says.
  For these modes, the values in \code{numData} are simply assumed to be the value to set the SVG attribute to.}
  }
}
\item{what}{The type of SVG nodes to apply the data to, for example "svg:path". By default any type of SVG element.}
\item{geneAttribute}{The SVG attribute that tells which gene the element is, or in a general way which row name in \code{numData}.}
\item{col}{A vector of colors (as characters) to use in coloring modes ("fill", "stroke", "pie" and "tooltip-only"). Color format is #RRGGBB. Advanced: In single-experiment fill mode, you may replace color names by references to gradients (in fact by any valid value for the SVG style "fill" parameter).}
\item{NA.color}{The color to use for NA values}
\item{colrange}{The range of values to map uniformly to the list of colors}
\item{annotation}{A function that, when given an gene (ie. a row name in numData) returns a list with fields \code{name} and \code{description} that are used for the tooltip window. Alternatively, you can give a matrix with genes as row names and a \code{name} and \code{description} column that work as just explained. There is also an optional \code{url} field that, if present, is the URL to go to when the SVG object is cliked.}
\item{fillAngle}{Only used in "partial-fill" and multi-experiment "fill" modes to choose the filling angle.
In "partial-fill" the default is -pi/2 (vertical going up) and in "fill" it is 0 (horizontal going right).}
\item{pieStartAngle}{Only used in pie mode, to choose at what angle to start drawing the pie.
The unit is radians, and 0 is the angle pointing right (like in trigonometry convention).
If not specified, the following rule is used:
If there are two conditions, the left part of the pie is 1 and the right is 2, therefore the angle is pi/2.
For more than two, the circle works like a clock (starts at 12 o'clock and goes clockwise),
therefore the angle is -pi/2.}
\item{pieClockwise}{The circular direction of the pie (pie mode only). Set to TRUE for clockwise (the default) or FALSE
for anticlockwise (like in trigonometry).}
}
\note{
  Although this function was created to display microarray data with gene labels in mind, it can be applied
  to any sort of numeric data, in which case "genes" are to be understood as the data you want to display.
}
\examples{
# Import yeast gene annotations (included in SVGMapping)
data(yeastAnnotMatrix)
# Import expression data from GSE7140 (included in SVGMapping)
data(yeastExprData)

# A simple two-color microarray example
# Load an SVG file made with Inkscape, where circles that represent genes have inkscape:label=the-gene-ID
mysvg <- loadSVG(system.file("extdata/example.svg", package="SVGMapping"))
logratios <- yeastExprData[,1]
# Here log-ratios are used for colors but fold-changes are displayed
mapDataSVG(mysvg, logratios, tooltipData=ifelse(logratios>=0, 2^(logratios), -2^-logratios), annotation=yeastAnnotMatrix)
saveSVG(mysvg, file="output1.svg")

# Pie charts showing several conditions (time course)
mysvg <- loadSVG(system.file("extdata/example.svg", package="SVGMapping"))
logratios <- yeastExprData
mapDataSVG(mysvg, logratios, tooltipData=ifelse(logratios>=0, 2^(logratios), -2^-logratios), mode="pie", annotation=yeastAnnotMatrix)
saveSVG(mysvg, file="output2.svg")

# Instead of drawing pies like clocks (start at 12 o'clock and go clockwise),
# draw them using the trigonometry convention (start on the right and anticlockwise).
mysvg <- loadSVG(system.file("extdata/example.svg", package="SVGMapping"))
mapDataSVG(mysvg, yeastExprData, mode="pie", annotation=yeastAnnotMatrix, pieStartAngle=0, pieClockwise=FALSE)
saveSVG(mysvg, file="output2b.svg")

# Filling erlemeyers
mysvg <- loadSVG(system.file("extdata/example2.svg", package="SVGMapping"))
mydata <- c(0.1, 0.3, 0.5)
names(mydata) <- c("citrate", "cis-aconitate", "isocitrate")
mapDataSVG(mysvg, mydata, mode="partial-fill")
saveSVG(mysvg, file="output4.svg")
}
\seealso{
  \itemize{
    \item{\code{\link{loadSVG}} to load your SVG file (to do before)}
    \item{\code{\link{saveSVG}} to save the produced SVG file (when you have finished with calls to \code{mapDataSVG}}
  }
}
\keyword{misc}
