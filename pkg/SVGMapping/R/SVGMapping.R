## Copyright (c) 2011, CEA DSV/iBiTecS
## All rights reserved.
## 
## Redistribution and use in source and binary forms, with or without modification,
## are permitted provided that the following conditions are met:
## 
## * Redistributions of source code must retain the above copyright notice, this list
##   of conditions and the following disclaimer.
## 
## * Redistributions in binary form must reproduce the above copyright notice, this
##   list of conditions and the following disclaimer in the documentation and/or
##   other materials provided with the distribution.
## 
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
## ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
## WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
## ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
## (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
## LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
## ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
## SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

load("inst/extdata/microarrayColors.rda")

.set <- function(var,value) {
  assign(var,value,envir=.svgmapping.env)
}

.get <- function(var) {
  return(get(var,envir=.svgmapping.env))
}

.toUserUnit <- function(value) {
  unit <- gsub("(-?[[:digit:].]+)(.*)","U_\\2",value)
  value <- as.numeric(gsub("(-?[[:digit:].]+)(.*)","\\1",value))
  user.unit <- switch(unit,
                      U_mm = 3.543307 * value,
                      U_px = value,
                      U_pt = 1.25 * value,
                      U_pc = 15 * value,
                      U_cm = 35.43307 * value,
                      U_in = 90 * value,
                      U_ = value,
                      default= NA)
    return(user.unit)
}

computeExprColors <- function(X, col=microarrayColors, NA.color="#999999", a=-2, b=2) {
  satval <- 2
  n <- length(col)
  X <- (X - a)/(b - a) * (n-1) + 1
  X <- as.integer(round(X))
  X[X < 1] <- 1
  X[X > n] <- n
  X <- col[X]
  X[is.na(X)] <- NA.color
  return(X)
}

loadSVG <- function(file) {
  svg <- xmlTreeParse(file, useInternalNodes=TRUE, addAttributeNamespaces=TRUE, fullNamespaceInfo=FALSE)
  return(svg)
}

setAttributeSVG <- function(node, attname, attval) {
  ## Namespace prefix is important to not replace the value of an attribute X by the value of something:X when both are present
  attrs <- xmlAttrs(node, addNamespacePrefix=TRUE)
  attrs[[attname]] <- attval
  xmlAttrs(node, suppressNamespaceWarning=TRUE) <- attrs
  return(invisible())
}

getAttributeSVG <- function(node, attname) {
  attrs <- xmlAttrs(node, addNamespacePrefix=TRUE)
  if (attname %in% names(attrs)) {
    return(attrs[[attname]])
  } else {
    return(NULL)
  }
}

setStyleSVG <- function(node, style.parameter, style.value) {
  style <- xmlGetAttr(node, "style", default="")
  style.params <- strsplit(style, ";", fixed=TRUE)[[1]]
  k <- grep(paste("^", style.parameter, ":", sep=""), style.params)
  if (length(k) == 0) {
    style.params <- c(style.params, paste(style.parameter, ":", style.value, sep=""))
  } else {
    style.params[k] <- paste(style.parameter, ":", style.value, sep="")
  }
  style <- paste(style.params, collapse=";")
  setAttributeSVG(node, "style", style)
  return(invisible())
}

getStyleSVG <- function(node, style.parameter) {
  style <- xmlGetAttr(node, "style", default="")
  style.params <- strsplit(style, ";", fixed=TRUE)[[1]]
  k <- grep(paste("^", style.parameter, ":", sep=""), style.params)
  if (length(k) == 0) {
    return(NULL)
  } else {
    return(strsplit(style.params[k], ":", fixed=TRUE)[[1]][[2]])
  }
}

getLabelsSVG <- function(svg, what="*", geneAttribute="inkscape:label") {
  labels <- xpathSApply(svg, paste("//", what, "[@", geneAttribute, "]", sep=""), xmlGetAttr, geneAttribute)
  labels <- unique(labels)
  return(labels)
}


mapDataSVG <- function(svg, numData, tooltipData=numData,
                       mode="fill", what="*",
                       geneAttribute="inkscape:label",
                       col=microarrayColors, NA.color="#999999", colrange=c(-2,2),
                       annotation=NULL, fillAngle=NULL) {
  numData <- as.matrix(numData)
  if (ncol(numData) < 1) stop("numData must contain at least one column")
  if (! (mode %in% c("fill", "stroke", "pie", "tooltip-only"))) {
    tooltipData <- NULL
  }
  if (!is.null(tooltipData)) {
    tooltipData <- as.matrix(tooltipData)
    if (is.numeric(tooltipData)) {
      tooltipData <- sapply(tooltipData, function(x) format(x, nsmall=2, digits=2))
      tooltipData <- matrix(tooltipData, ncol=ncol(numData))
    }
    if (!all(dim(tooltipData) == dim(numData)))
      stop("numData and tooltipData must have the same dimension")
    rownames(tooltipData) <- rownames(numData)
  }
  nodes <- getNodeSet(svg, paste("//", what, "[@", geneAttribute, "]", sep=""))
  getGeneColors <- function(gene) {
    computeExprColors(numData[gene,], col=col, NA.color=NA.color, a=colrange[[1]], b=colrange[[2]])
  }
  getGeneValues <- function(gene) {
    X <- as.character(numData[gene,])
    X[is.na(X)] <- "0"
    return(X)
  }
  if (is.null(annotation)) {
    annotFunction <- function(x) { list(name=x) }
  } else if (is.function(annotation)) {
    annotFunction <- annotation
  } else if (is.matrix(annotation) || is.data.frame(annotation)) {
                                        # annotation is a matrix/data.frame
    annotFunction <- function(x) {
      if (x %in% rownames(annotation)) {
        return(as.list(annotation[x,]))
      } else {
        return(NULL)
      }
    }
  } else {
    stop("Invalid type for annotation")
  }
  addquotes <- function(v) {
    f <- function(x) {
      if (x == "null") {
        "null"
      } else {
        x <- gsub("\\\\", "\\\\\\\\", x)
        x <- gsub('"', '\\\\"', x)
        x <- gsub("'", "\\\\'", x)
        paste("'", x, "'", sep="")
      }
    }
    return(sapply(v, f))
  }
  addToolTip <- if (is.null(tooltipData)) function(...) { NULL } else function(node, gene, geneColors) {
    displayedFoldChanges <- tooltipData[gene,]
    displayedFoldChanges <- as.character(displayedFoldChanges)
    displayedFoldChanges[is.na(displayedFoldChanges)] <- "NA"
    annot <- annotFunction(gene)
    jsargs <- c()
    jsargs[[1]] <- "evt"
    jsargs[[2]] <- addquotes(if (!is.null(annot) && !is.null(annot$name)) annot$name else "null")
    jsargs[[3]] <- addquotes(if (!is.null(annot) && !is.null(annot$description)) annot$description else "null")
    jsargs[[4]] <- paste("new Array(", paste(addquotes(displayedFoldChanges), collapse=","), ")", sep="")
    jsargs[[5]] <- paste("new Array(", paste(addquotes(geneColors), collapse=","), ")", sep="")
    setAttributeSVG(node, "onmouseover", paste("displayAnnotation(", paste(jsargs, collapse=", "), ")", sep=""))
    setAttributeSVG(node, "onmouseout", "hideAnnotation(evt)")
    if (!is.null(annot) && !is.null(annot$url)) {
      setAttributeSVG(node, "onclick", paste("window.open('", annot$url,  "')", sep=""))
    }
    return(invisible())
  }
  if (mode == "fill") {
    if (ncol(numData) < 2) {
                                        # Simple color fill
      for (node in nodes) {
        gene <- xmlGetAttr(node, geneAttribute)
        if (gene %in% rownames(numData)) {
          geneColor <- getGeneColors(gene)
          setStyleSVG(node, "fill", geneColor)
          addToolTip(node, gene, geneColor)
        }
      }
    } else {
                                        # Multi-color fill
      if (is.null(fillAngle))
        fillAngle <- 0
      defs <- getNodeSet(svg, "//svg:defs")
      if (length(defs) == 0) stop("Missing defs node in SVG")
      defs <- defs[[1]]
      for (node in nodes) {
        gene <- xmlGetAttr(node, geneAttribute)
        if (gene %in% rownames(numData)) {
          geneColors <- getGeneColors(gene)
          gradient.id <- paste(getAttributeSVG(node, "id"), "-gradient", sep="")
          gradient.children <- list()
          nconds <- length(geneColors)
          for (j in seq(along=geneColors)) {
            stop1 <- newXMLNode("stop", attrs=list(offset=(j-1)/nconds, style=paste("stop-color:", geneColors[[j]], ";stop-opacity:1", sep="")))
            stop2 <- newXMLNode("stop", attrs=list(offset=j/nconds, style=paste("stop-color:", geneColors[[j]], ";stop-opacity:1", sep="")))
            gradient.children <- c(gradient.children, stop1, stop2)
          }
          x <- cos(fillAngle)
          y <- sin(fillAngle)
          if (x < 0) {
            x1 <- -x
            x2 <- 0
          } else {
            x1 <- 0
            x2 <- x
          }
          if (y < 0) {
            y1 <- -y
            y2 <- 0
          } else {
            y1 <- 0
            y2 <- y
          }
          x1 <- paste(round(x1*100), "%", sep="")
          x2 <- paste(round(x2*100), "%", sep="")
          y1 <- paste(round(y1*100), "%", sep="")
          y2 <- paste(round(y2*100), "%", sep="")
          gradient <- newXMLNode("linearGradient", attrs=list(id=gradient.id, x1=x1, x2=x2, y1=y1, y2=y2), .children=gradient.children)
          addChildren(defs, kids=list(gradient))
          setStyleSVG(node, "fill", paste("url(#", gradient.id, ")", sep=""))
          addToolTip(node, gene, geneColors)
        }
      }
    }
  } else if (mode == "stroke") {
    if (ncol(numData)>1) stop("This mode is not compatible with multiple conditions")
    for (node in nodes) {
      gene <- xmlGetAttr(node, geneAttribute)
      if (gene %in% rownames(numData)) {
        geneColor <- getGeneColors(gene)
        setStyleSVG(node, "stroke", geneColor)
        addToolTip(node, gene, geneColor)
      }
    }
  } else if (mode == "pie") {
    nconds <- ncol(numData)
    alphadeg <- 360/nconds
    alpharad <- 2*pi/nconds
    for (node in nodes) {
      gene <- xmlGetAttr(node, geneAttribute)
      if (gene %in% rownames(numData)) {
        ##print(node)
        geneColors <- getGeneColors(gene)
        attrs <- xmlAttrs(node, addNamespacePrefix=TRUE) # We need to make the difference between cx and sodipodi:cx
        ok <- FALSE
        if (all(c("cx", "cy", "r") %in% names(attrs))) {
          # We have a genuine circle
          ##cat("genuine circle\n")
          # Where is the circle center?
          cx <- as.numeric(attrs[["cx"]])
          cy <- as.numeric(attrs[["cy"]])
          # What is its radius?
          r <- as.numeric(attrs[["r"]])
          ok <- TRUE
        } else if (all(c("sodipodi:cx", "sodipodi:cy", "sodipodi:rx") %in% names(attrs))) {
          # We have an Inkscape circle
          ##cat("inkscape circle\n")
          # Inkscape uses paths for circle but adds non-standard attributes to remember center/radius.
          # Where is the circle center?
          cx <- as.numeric(attrs[["sodipodi:cx"]])
          cy <- as.numeric(attrs[["sodipodi:cy"]])
          # What is its radius?
          r <- as.numeric(attrs[["sodipodi:rx"]])
          ok <- TRUE
        }
        # ok means we can use this element as a circle to put a pie chart in it
        if (ok) {
          if ("transform" %in% names(attrs) && grepl("^matrix\\([^\\(]+\\)$", attrs[["transform"]])) {
            appliedTransform = TRUE
            # We are going to apply the transform matrix manually
            # because we want it applied to the center and radius
            # but not the (xs,ys) and (x,y), because otherwise our pie chart
            # won't start at 12 o'clock.
            parts <- unlist(strsplit(attrs[["transform"]], "[\\(,\\)]"))
            coefs <- as.numeric(parts[2:length(parts)])
            M <- rbind(matrix(coefs, 2), matrix(c(0,0,1), 1))
            newC <- M %*% c(cx, cy, 1)
            cx <- newC[[1]]
            cy <- newC[[2]]
            newR <- M %*% c(r, 0, 1)
            newO <- M %*% c(0, 0, 1)
            r <- sqrt(sum((newR[1:2] - newO[1:2])^2))
          } else {
            appliedTransform = FALSE
            # We will simply copy the transform instruction. (will work if it is a translation)
          }
          # Now build the pie chart
          for (j in seq(along=geneColors)) {
            alpharad0 <- if (nconds > 2) -pi/2 else pi/2
            attrs2 <- list(style=paste("stroke-width:0;stroke:none;fill:", geneColors[[j]], sep=""))
            if (!appliedTransform && "transform" %in% names(attrs)) {
              attrs2[["transform"]] <- attrs[["transform"]]
            }
            xs <- cx + r*cos(alpharad*(j-1) + alpharad0)
            ys <- cy + r*sin(alpharad*(j-1) + alpharad0)
            x  <- cx + r*cos(alpharad*j + alpharad0)
            y  <- cy + r*sin(alpharad*j + alpharad0)
            if (nconds > 1) {
              elementType <- "path"
              attrs2[["d"]] <- paste("M ", xs, ",", ys, " A ", r, ",", r, " ", alphadeg,
                                    " 0 1 ", x, ",", y, " L ", cx, ",", cy, " Z", sep="")
            } else {
              elementType <- "circle"
              attrs2[["cx"]] <- cx
              attrs2[["cy"]] <- cy
              attrs2[["r"]] <- r
            }
            pathNode <- newXMLNode(elementType, attrs=attrs2)
            addSibling(node, kids=list(pathNode), after=FALSE)
          }
        
          # Make the original circle transparent so the pie chart can be seen
          setStyleSVG(node, "fill-opacity", 0)
          # Add tooltip to the circle
          addToolTip(node, gene, geneColors)
        }
      }
    }
  } else if (mode == "tooltip-only") {
    for (node in nodes) {
      gene <- xmlGetAttr(node, geneAttribute)
      if (gene %in% rownames(numData)) {
        geneColor <- getGeneColors(gene)
        addToolTip(node, gene, geneColor)
      }
    }
  } else if (mode == "fill-opacity") {
    if (ncol(numData)>1) stop("This mode is not compatible with multiple conditions")
    for (node in nodes) {
      gene <- xmlGetAttr(node, geneAttribute)
      if (gene %in% rownames(numData)) {
        geneValue <- getGeneValues(gene)
        setStyleSVG(node, "fill-opacity", geneValue)
      }
    }
  } else if (mode == "stroke-opacity") {
    if (ncol(numData)>1) stop("This mode is not compatible with multiple conditions")
    for (node in nodes) {
      gene <- xmlGetAttr(node, geneAttribute)
      if (gene %in% rownames(numData)) {
        geneValue <- getGeneValues(gene)
        setStyleSVG(node, "stroke-opacity", geneValue)
      }
    }
  } else if (mode == "stroke-width") {
    if (ncol(numData)>1) stop("This mode is not compatible with multiple conditions")
    for (node in nodes) {
      gene <- xmlGetAttr(node, geneAttribute)
      if (gene %in% rownames(numData)) {
        geneValue <- getGeneValues(gene)
        setStyleSVG(node, "stroke-width", geneValue)
      }
    }
  } else if (mode == "partial-fill") {
    if (is.null(fillAngle))
      fillAngle <- -pi / 2
    if (ncol(numData)>1) stop("This mode is not compatible with multiple conditions")
    defs <- getNodeSet(svg, "//svg:defs")
    if (length(defs) == 0) stop("Missing defs node in SVG")
    defs <- defs[[1]]
    for (node in nodes) {
      gene <- xmlGetAttr(node, geneAttribute)
      if (gene %in% rownames(numData)) {
        geneValue <- getGeneValues(gene)
        if (geneValue > 1) geneValue <- 1
        if (geneValue < 0) geneValue <- 0
        originalColor <- getStyleSVG(node, "fill")
        if (is.null(originalColor) || grepl("^url", originalColor))
          originalColor < "#0000FF"
        gradient.id <- paste(getAttributeSVG(node, "id"), "-gradient", sep="")
        gradient.children <- list()
        gradient.children[[1]] <- newXMLNode("stop", attrs=list(offset=0, style=paste("stop-color:", originalColor, ";stop-opacity:1", sep="")))
        gradient.children[[2]] <- newXMLNode("stop", attrs=list(offset=geneValue, style=paste("stop-color:", originalColor, ";stop-opacity:1", sep="")))
        gradient.children[[3]] <- newXMLNode("stop", attrs=list(offset=geneValue, style=paste("stop-color:", originalColor, ";stop-opacity:0", sep="")))
        gradient.children[[4]] <- newXMLNode("stop", attrs=list(offset=1, style=paste("stop-color:", originalColor, ";stop-opacity:0", sep="")))
        x <- cos(fillAngle)
        y <- sin(fillAngle)
        if (x < 0) {
          x1 <- -x
          x2 <- 0
        } else {
          x1 <- 0
          x2 <- x
        }
        if (y < 0) {
          y1 <- -y
          y2 <- 0
        } else {
          y1 <- 0
          y2 <- y
        }
        x1 <- paste(round(x1*100), "%", sep="")
        x2 <- paste(round(x2*100), "%", sep="")
        y1 <- paste(round(y1*100), "%", sep="")
        y2 <- paste(round(y2*100), "%", sep="")
        gradient <- newXMLNode("linearGradient", attrs=list(id=gradient.id, x1=x1, x2=x2, y1=y1, y2=y2), .children=gradient.children)
        addChildren(defs, kids=list(gradient))
        setStyleSVG(node, "fill", paste("url(#", gradient.id, ")", sep=""))
      }
    }
  } else {
    stop(paste("Invalid mode:", mode))
  }
  return(invisible())
}

saveSVG <- function(svg, file="") {
  ## Add instruction to initialize script when SVG file is loaded
  root <- xmlRoot(svg)
  setAttributeSVG(root, "onload", "init(evt)")
  ## Produce source XML
  xml <- saveXML(svg, indent=FALSE)
  xml <- gsub("\n</text>","</text>", xml)
  xml <- gsub("\n<tspan","<tspan", xml)
  ## Add the JavaScript script
  con <- file(system.file("extdata/script.js", package="SVGMapping"), "rb")
  rawScript <- readLines(con)
  close(con)
  scriptText <- paste('\n<script type="text/ecmascript">\n<![CDATA[\n\n',
                      paste(rawScript, collapse="\n"),
                      '\n\n]]>\n</script>\n', sep="")
  xml <- gsub("</svg>", paste(scriptText, "</svg>", sep=""), xml)
  ## Write/output the SVG
  cat(xml, file=file)
}

showSVG <- function(svg, browser=getOption("browser")) {
  path <- tempfile()
  svgpath <- paste(path, ".svg", sep="")
  saveSVG(svg, svgpath)
  htmlpath <- paste(path, ".html", sep="")
  con <- file(htmlpath, "w")
  html <- paste('<!DOCTYPE html>
<html>
<head>
</head>
<body>
<object data="', basename(svgpath), '" width="100%" height="100%">
Failed to load SVG image. Does your browser support SVG?
</object>
</body>
</html>
', sep="")
  cat(html, file=con)
  close(con)
  browseURL(paste("file:///", htmlpath, sep=""), browser=browser)
}

devSVGMapping <- function(template, attribute.name="@inkscape:label",
                          attribute.value="Rplot",width=10, height=8,
                          pointsize=10) {

  ## check Cairo
  if(is.null(.get(".cairo")))
    stop("No Cairo SVG driver installed..")
  
  ## check
  if(!is(template,"XMLInternalDocument"))
    stop("Invalid template..")
  .set(".dev.template", template)
  
  ## scan template..
  target.node <- getNodeSet(template,
                            paste("//svg:rect[",attribute.name,"='",
                                  attribute.value,"']",sep=""))
  if(length(target.node) == 0)
    stop("Target Attribute not found..")
  target.node <- target.node[[1]]
  .set(".dev.target.name",attribute.name)
  .set(".dev.target.value", attribute.value)

  ## init. SVG device
  .dev.rplot <- paste(tempfile(pattern="rplot"), ".svg", sep="")
  .set(".dev.rplot", .dev.rplot)
  if (.get(".cairo") == "cairoDevice") {
    Cairo_svg(filename=path.expand(.dev.rplot),
              width=width, heigh=height, pointsize=pointsize)
  } else {
    svg(filename=path.expand(.dev.rplot),
        width=width, heigh=height, pointsize=pointsize)
  }
}

includeSVG <- function(template, file,
                       attribute.name="@inkscape:label",
                       attribute.value="Rplot") {

  ## check
  if(!is(template,"XMLInternalDocument"))
    stop("Invalid template..")

  ## scan template..
  target.node <- getNodeSet(template,
                            paste("//svg:rect[",attribute.name,"='",
                                  attribute.value,"']",sep=""))
  if(length(target.node) == 0)
    stop("Target Attribute not found..")
  target.node <- target.node[[1]]
  
  ## init.
  rplot.svg <- loadSVG(file)
  
  ## 1 - Fix all IDs
  rplot.ids <- as.vector(xpathSApply(rplot.svg, "//@id"))
  
  ## 1.1 - Fix (if any) href to ids
  rplot.nodes <- getNodeSet(rplot.svg, "//*[@xlink:href]")
  tmp <- sapply(rplot.nodes,
                function(el, prefix, ids) {
                  href = xmlGetAttr(el,"xlink:href")
                  if(grepl("^#", href)) {
                    href = substr(href,2, nchar(href))
                    if(href %in% ids) 
                      xmlAttrs(el) <- c('xlink:href'=paste(prefix,href,sep="_"))
                  }
                },
                prefix=paste("#",attribute.value,sep=""),
                ids=rplot.ids)
  
  ## 1.2 - Fix ids
  rplot.nodes <- getNodeSet(rplot.svg, "//*[@id]")
  tmp <- sapply(rplot.nodes, 
                function(el,prefix) {
                  xmlAttrs(el) <- c(id=paste(prefix,xmlGetAttr(el,"id"),sep="_"))
                }, 
                prefix=attribute.value)
  
  ## 2 - Get Dimensions
  s <- xmlRoot(rplot.svg)
  rplot.x <- .toUserUnit(xmlGetAttr(s, "x", "0"))
  rplot.y <- .toUserUnit(xmlGetAttr(s, "y", "0"))
  rplot.w <- .toUserUnit(xmlGetAttr(s, "width", "0"))
  rplot.h <- .toUserUnit(xmlGetAttr(s, "height", "0"))
  rplot.nodes <- xmlChildren(s)

  ## check
  if(length(rplot.nodes) == 0) {
    warning("Empty plot..")
    return()
  }

  ## get target coordinates
  target.x <- .toUserUnit(xmlGetAttr(target.node, "x", "0"))
  target.y <- .toUserUnit(xmlGetAttr(target.node, "y", "0"))
  target.w <- .toUserUnit(xmlGetAttr(target.node, "width", "0"))
  target.h <- .toUserUnit(xmlGetAttr(target.node, "height", "0"))
  target.transform <- xmlGetAttr(target.node, "transform", "")

  ## compute 'transform' instruction
  scale.w = target.w/rplot.w
  scale.h = target.h/rplot.h
  scale = paste("scale(",scale.w,",",scale.h,")",sep="")
  translate.x = target.x - rplot.x
  translate.y = target.y - rplot.y
  translate = paste("translate(",translate.x,",", translate.y,")",sep="")
  transform = paste(target.transform,translate,scale)

  ## create new encaspulating layer
  target.g <- newXMLNode("g",
                         attrs=c(
                           id=paste("Rplot.g",attribute.value,sep=""),
                           transform=transform
                           )
                         )

  ## add Rplot nodes to the layer & replace template rectangle
  tmp <- addChildren(target.g, rplot.nodes)
  tmp <- replaceNodes(target.node, target.g)

  ## free
  free(rplot.svg)
}

dev.off <- function(which=dev.cur()) {

  ## close device
  grDevices::dev.off(which)

  ## init.
  .dev.template <- .get(".dev.template")
  .dev.rplot <- .get(".dev.rplot")
  
  ## check if we are plotting whithin a template?
  if(!is.null(.dev.rplot)) {

    includeSVG(.dev.template, .dev.rplot,
               .get(".dev.target.name"),
               .get(".dev.target.value"))
    
    ## free & unlink stuff
    unlink(.dev.rplot)
  }

  ## eop
  .set(".dev.template", NULL)
  .set(".dev.rplot", NULL)
  .set(".dev.target.name", NULL)
  .set(".dev.target.value", NULL)
}

