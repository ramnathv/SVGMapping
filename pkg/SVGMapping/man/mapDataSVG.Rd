\name{mapDataSVG}
\alias{mapDataSVG}
\title{Map experimental data to an SVG template file}
\description{
This is the main function of SVGMapping. It allows you to map data to an SVG file.
Typically, you create an SVG template file (see details) and use this function 
to modify attributes of selected shapes (\emph{ie} colors, filling, stroke, 
opacity) given a set of experimental values. 
}
\usage{
mapDataSVG(svg, numData, tooltipData=numData,
           mode="fill", what="*",
           geneAttribute="inkscape:label",
           col=microarrayColors, NA.color="#999999", colrange=c(-2,2),
           annotation=NULL,
           fillAngle=NULL, pieStartAngle=NULL, pieClockwise=TRUE,
           angleRange=NULL, animations=TRUE)
}
\arguments{
\item{svg}{An SVG template stored as an \code{XMLInternalDocument} (see \code{\link{xmlTreeParse}}). The template will be modified in place and must be used for outputs.}
\item{numData}{A numeric vector or matrix with mapped names (character) as rows.
Depending on the mode, the matrix may or may not have several columns (to display several experiments).}
\item{tooltipData}{A vector/matrix similar to numData, but containing the values to be displayed in
the JavaScript-based tooltip window. If you don't want a tooltip window, set this to NULL.}
\item{mode}{Describe the type of mapping operation to perform:
  \itemize{
  
  \item{\code{fill} and \code{stroke}: set the \emph{filling} and \emph{outline} shape colors. 
  First we performed a linear mapping between experimental values and the color range given in \code{colrange} 
  (see below). Then the mapping value is used to select the colors (in hex. #RRGGBB format) provided by the \code{col} parameter. 
  For \code{NA} values the special \code{NA.color} is used. 
  }
  
  \item{\code{pie}: Display \code{numData} values as pie charts (set to "pie"). 
  Slice sizes are computed using values in each columns of \code{numData}).}
  
  \item{\code{partial-fill}: Allows to display the fill color (or gradient) on a portion of a shape (eg. simulate the filling of flasks). 
  The filling ratio is a value in the range [0-1]. This mode depends on the \code{fillAngle} parameter.}
  
  \item{\code{fill-opacity}: Set the opacity level of the filling color. 
  In this case values of \code{numData} are expected to be in the range [0-1].}
  
  \item{\code{stroke-opacity}: Set the opacity level of the stroke color. 
  In this case values of \code{numData} are expected to be in the range [0-1].}
  
  \item{\code{stroke-width}: Set the stroke width of a shape. 
  In this case values of \code{numData} are expected to be valid SVG width.}
  
  \item{\code{rotate}: Rotate a shape or a group to a given angle. The rotation angle value in \code{numData} are given as proportions of the full rotation range (\code{angleRange} parameter).}
  
  \item{\code{tooltip-only}: add a tooltip javascript window (on mouseover event), 
  using values in the \code{tooltipData} and \code{annotation} parameters (see the detail section), to shapes.}
  
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
\item{angleRange}{Should be a vector with two values, \bold{min} and \bold{max}. When using the \emph{rorate} mode, specify the minimum and maximum angle of the rotation.}
\item{animations}{Whether or not to add JavaScript animations.}
}
\details{

This package has been firstly designed to process metabolomic pathways. For example, consider a SVG template document showing a given cell mechanism with genes depicted as full circles. Then, you want these circles to be
coloured according to the differential gene expression level (\emph{eg.} red/grenn for the induced/repressed subsets).
This function allows you to do that, as well as other variants designed with the same picture in mind (see options below).
For example, circles can be replaced by pie charts showing different experiments or different kinetic values.
It "finds" the SVG objects that depict genes by using the value of a given attribute. Since we are mostly using Inkscape to 
sketch our template by default we are searching in the Inkscape "label" attribute field. Nevertheless this can be changed to any other SVG attribute you want (thus you are not limited to Inkscape based templates). Futhermore, this function also allows you to add tooltips (JavaScript-based) showing additional data such as the experimental values, gene annoations (\emph{eg} gene function.

Although this function was originnaly created to process microarray data with gene labels 
in mind, it can be applied to any sort of numeric data, in which case "genes" 
are to be understood as the data you want to display.

%% FILL/STROKE : 
%%The default behaviour is to use a gradient where dark green is -2,
%%  green is -1, white is 0, red is 1 and dark red is 2 (it is meant to be used for log-ratios from two-color microarrays).
%%
%%TOOLTIP
%%* For example, this allows you to map log-ratios to the color range, but display fold changes.
%%
%%* Note that this argument is only used if \code{mode} is a about coloring (ie. is one of "fill", "stroke", "pie", "tooltip-only"),
%%because we need to compute a color for background in the tooltip window.
%%
%%* Explain the role of tooltipData / annotation...
%%
%%PIE
%%
%%* in which case the SVG elements with the labels are expected to be circles or Inkscape-pseudo-circles
%%  (paths with sodipodi:cx/cy/rx attributes).
%%  
%%* An alternative to pie charts for multi-experiment mode is to fill objects with several color stripes.
%%  If you want to do that, use "fill" with a multi-column matrix for \code{numData}.
%%
%%ROTATE:

}
\note{
This function can be run several times on the same SVG data to add different information. 

After all your processing are done (including saving the document), don't foget to free the memory using the \code{\link{free}()} function on your modified template.

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
    \item{\code{\link{loadSVG}} to load your SVG template file (to do before)}
    \item{\code{\link{showSVG}} to display the SVG template (possibly modified) in your navigator}
    \item{\code{\link{saveSVG}} to save the produced SVG file (when you have finished with calls to \code{mapDataSVG}}
  }
}
\keyword{misc}
