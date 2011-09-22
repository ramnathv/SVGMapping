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

svgNS <- "http://www.w3.org/2000/svg"

completeNamespaces <- function(svgdata) {
  NS <- xmlNamespaceDefinitions(svgdata, simplify=TRUE)
  NS[["svg"]] <- svgNS
  return(NS)
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

setTextSVG <- function(svg, searchAttributeValue, text, searchAttribute="inkscape:label") {
  nodes <- getNodeSet(svg, paste("//svg:text[@", searchAttribute, "]", sep=""), namespaces=completeNamespaces(svg))
  for (node in nodes) {
    attval <- getAttributeSVG(node, searchAttribute)
    if (attval == searchAttributeValue) {
      # If the text node has a tspan child, add the text in the tspan child
      for (child in xmlChildren(node)) {
        if (xmlName(child) == "tspan") {
          node <- child
          break
        }
      }
      # Remove previous text
      removeChildren(node, kids=1)
      # Add user-supplied text
      addChildren(node, text)
    }
  }
}

addLinkSVG <- function(node, url) {
  a <- newXMLNode("a", attrs=list("xlink:href"=url, target="_blank"), .children=list(node), namespaceDefinitions=c(xlink="http://www.w3.org/1999/xlink"))
  a <- addSibling(node, a)[[1]]
  removeNodes(node)
  new.node <- xmlChildren(a)[[1]]
  return(new.node)
}

addLinkByLabelSVG <- function(svg, searchAttributeValue, url, what="*", searchAttribute = "inkscape:label") {
  nodes <- getNodeSet(svg, paste("//", what, "[@", searchAttribute, "]", sep=""))
  for (node in nodes) {
      attval <- getAttributeSVG(node, searchAttribute)
      if (attval == searchAttributeValue) {
          addLinkSVG(node, url)
      }
  }
  invisible(NULL)
}

addJavaScriptCallBack <- function(node, attribute, call) {
  originalCall <- getAttributeSVG(node, attribute)
  if (is.null(originalCall)) {
    newCall <- call
  } else {
    newCall <- paste(originalCall, call, sep="; ")
  }
  setAttributeSVG(node, attribute, newCall)
}

mapDataSVG <- function(svg, numData, tooltipData=numData,
                       mode="fill", what="*",
                       geneAttribute="inkscape:label",
                       col=microarrayColors, NA.color="#999999", colrange=c(-2,2),
                       annotation=NULL,
                       fillAngle=NULL, pieStartAngle=NULL, pieClockwise=TRUE,
                       angleRange=NULL,
                       animations=TRUE) {

  ## -- local functions
  getGeneColors <- function(gene) {
    computeExprColors(numData[gene,], col=col,
                      NA.color=NA.color, a=colrange[[1]], b=colrange[[2]])
  }
  getGeneValues <- function(gene) {
    X <- as.character(numData[gene,])
    X[is.na(X)] <- "0"
    return(X)
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

  ## -- check & init.
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
  if (is.null(annotation)) {
    annotFunction <- function(x) { list(name=x) }
  } else if (is.function(annotation)) {
    annotFunction <- annotation
  } else if (is.matrix(annotation) || is.data.frame(annotation)) {
    ## annotation is a matrix/data.frame
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

  ## -- init.
  nodes <- getNodeSet(svg, paste("//", what, "[@", geneAttribute, "]", sep=""))
  addToolTip <- if (is.null(tooltipData))
    function(node, ...) { return(node) }
  else
    function(node, gene, geneColors) {
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
      addJavaScriptCallBack(node, "onmouseover", paste("displayAnnotation(", paste(jsargs, collapse=", "), ")", sep=""))
      addJavaScriptCallBack(node, "onmouseout", "hideAnnotation(evt)")
      if (!is.null(annot) && !is.null(annot$url)) {
        node <- addLinkSVG(node, annot$url)
      }
      return(node)
    }

  ## F I L L I N G
  ## ------------------------------------------------------------
  if (mode == "fill") {
    if (ncol(numData) < 2) {
      ## Simple color fill
      for (node in nodes) {
        gene <- xmlGetAttr(node, geneAttribute)
        if (gene %in% rownames(numData)) {
          geneColor <- getGeneColors(gene)
          setStyleSVG(node, "fill", geneColor)
          node <- addToolTip(node, gene, geneColor)
        }
      }
    } else {
      ## Multi-color fill
      if (is.null(fillAngle))
        fillAngle <- 0
      defs <- getNodeSet(svg, "//svg:defs", namespaces=completeNamespaces(svg))
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
          node <- addToolTip(node, gene, geneColors)
          # Add animations
          if (animations) {
            addJavaScriptCallBack(node, "onmouseover", "animateStripes(evt)")
            addJavaScriptCallBack(node, "onmouseout", "stopAnimation()")
          }
        }
      }
    }
  }

  ## S T R O K E
  ## ------------------------------------------------------------
  else if (mode == "stroke") {
    if (ncol(numData)>1) stop("This mode is not compatible with multiple conditions")
    for (node in nodes) {
      gene <- xmlGetAttr(node, geneAttribute)
      if (gene %in% rownames(numData)) {
        geneColor <- getGeneColors(gene)
        setStyleSVG(node, "stroke", geneColor)
        node <- addToolTip(node, gene, geneColor)
      }
    }
  }
  
  ## P I E
  ## ------------------------------------------------------------
  else if (mode == "pie") {
    nconds <- ncol(numData)
    alphadeg <- 360/nconds
    alpharad <- 2*pi/nconds
    for (node in nodes) {
      gene <- xmlGetAttr(node, geneAttribute)
      if (gene %in% rownames(numData)) {
        geneColors <- getGeneColors(gene)
        # We need to make the difference between cx and sodipodi:cx
        attrs <- xmlAttrs(node, addNamespacePrefix=TRUE) 
        ok <- FALSE
        if (all(c("cx", "cy", "r") %in% names(attrs))) {
          # We have a genuine circle
          # Where is the circle center?
          cx <- as.numeric(attrs[["cx"]])
          cy <- as.numeric(attrs[["cy"]])
          # What is its radius?
          r <- as.numeric(attrs[["r"]])
          ok <- TRUE
        } else if (all(c("sodipodi:cx", "sodipodi:cy", "sodipodi:rx") %in% names(attrs))) {
          # We have an Inkscape circle
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
          # Add a <g> element to group the original shape with the pie parts
          new.group.node <- newXMLNode("g", .children=list(node))
          new.group.node <- addSibling(node, new.group.node)[[1]]
          removeNodes(node)
          node <- xmlChildren(new.group.node)[[1]]
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
          pie.node <- newXMLNode("g")
          addSibling(node, pie.node, after=FALSE)
          for (j in seq(along=geneColors)) {
            if (is.null(pieStartAngle)) {
              alpharad0 <- if (nconds > 2) -pi/2 else pi/2
            } else {
              alpharad0 <- pieStartAngle
            }
            attrs2 <- list(style=paste("stroke-width:0;stroke:none;fill:", geneColors[[j]], sep=""))
            if (!appliedTransform && "transform" %in% names(attrs)) {
              attrs2[["transform"]] <- attrs[["transform"]]
            }
            if (pieClockwise) {
              j.in.pie <- j
            } else {
              j.in.pie <- nconds - j + 1
            }
            xs <- cx + r*cos(alpharad*(j.in.pie-1) + alpharad0)
            ys <- cy + r*sin(alpharad*(j.in.pie-1) + alpharad0)
            x  <- cx + r*cos(alpharad*j.in.pie + alpharad0)
            y  <- cy + r*sin(alpharad*j.in.pie + alpharad0)
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
            addChildren(pie.node, pathNode)
          }
        
          # Make the original circle transparent so the pie chart can be seen
          setStyleSVG(node, "fill-opacity", 0)
          # Add tooltip to the circle
          node <- addToolTip(node, gene, geneColors)
          # Add animations
          if (animations) {
            addJavaScriptCallBack(node, "onmouseover", "animatePie(evt)")
            addJavaScriptCallBack(node, "onmouseout", "stopAnimation()")
          }
        }
      }
    }
  }

  ## T O O L T I P  -  O N L Y
  ## ------------------------------------------------------------
  else if (mode == "tooltip-only") {
    for (node in nodes) {
      gene <- xmlGetAttr(node, geneAttribute)
      if (gene %in% rownames(numData)) {
        geneColor <- getGeneColors(gene)
        node <- addToolTip(node, gene, geneColor)
      }
    }
  }

  ## F I L L   O P A C I T Y
  ## ------------------------------------------------------------
  else if (mode == "fill-opacity") {
    if (ncol(numData)>1) stop("This mode is not compatible with multiple conditions")
    for (node in nodes) {
      gene <- xmlGetAttr(node, geneAttribute)
      if (gene %in% rownames(numData)) {
        geneValue <- getGeneValues(gene)
        setStyleSVG(node, "fill-opacity", geneValue)
      }
    }
  }
  
  ## S T R O K E   O P A C I T Y
  ## ------------------------------------------------------------
  else if (mode == "stroke-opacity") {
    if (ncol(numData)>1) stop("This mode is not compatible with multiple conditions")
    for (node in nodes) {
      gene <- xmlGetAttr(node, geneAttribute)
      if (gene %in% rownames(numData)) {
        geneValue <- getGeneValues(gene)
        setStyleSVG(node, "stroke-opacity", geneValue)
      }
    }
  }

  ## S T R O K E   W I D T H
  ## ------------------------------------------------------------
  else if (mode == "stroke-width") {
    if (ncol(numData)>1) stop("This mode is not compatible with multiple conditions")
    for (node in nodes) {
      gene <- xmlGetAttr(node, geneAttribute)
      if (gene %in% rownames(numData)) {
        geneValue <- getGeneValues(gene)
        setStyleSVG(node, "stroke-width", geneValue)
      }
    }
  }
  
  ## P A R T I A L   F I L L
  ## ------------------------------------------------------------
  else if (mode == "partial-fill") {
    if (is.null(fillAngle))
      fillAngle <- -pi / 2
    if (ncol(numData)>1) stop("This mode is not compatible with multiple conditions")
    defs <- getNodeSet(svg, "//svg:defs", namespaces=completeNamespaces(svg))
    if (length(defs) == 0) stop("Missing defs node in SVG")
    defs <- defs[[1]]
    for (node in nodes) {
      gene <- xmlGetAttr(node, geneAttribute)
      if (gene %in% rownames(numData)) {
        # Add a <g> element to group the original shape with the pie parts
        new.group.node <- newXMLNode("g", .children=list(node))
        new.group.node <- addSibling(node, new.group.node)[[1]]
        removeNodes(node)
        node <- xmlChildren(new.group.node)[[1]]
        
        geneValue <- getGeneValues(gene)
        if (geneValue > 1) geneValue <- 1
        if (geneValue < 0) geneValue <- 0
        gradient.id <- paste(getAttributeSVG(node, "id"), "-gradient", sep="")
        mask.id <- paste(getAttributeSVG(node, "id"), "-mask", sep="")
        gradient.children <- list()
        gradient.children[[1]] <- newXMLNode("stop",
                                             attrs=list(offset=0,
                                               style=paste("stop-color:white;stop-opacity:1", sep="")))
        gradient.children[[2]] <- newXMLNode("stop",
                                             attrs=list(offset=geneValue,
                                               style=paste("stop-color:white;stop-opacity:1", sep="")))
        gradient.children[[3]] <- newXMLNode("stop",
                                             attrs=list(offset=geneValue,
                                               style=paste("stop-color:white;stop-opacity:0", sep="")))
        gradient.children[[4]] <- newXMLNode("stop",
                                             attrs=list(offset=1,
                                               style=paste("stop-color:white;stop-opacity:0", sep="")))
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
        gradient <- newXMLNode("linearGradient",
                               attrs=list(id=gradient.id, x1=x1, x2=x2, y1=y1, y2=y2),
                               .children=gradient.children)
        addChildren(defs, kids=list(gradient))
        
        # make mask
        masknode <- newXMLNode("mask", .children=list(node))
        addSibling(node, masknode, after=FALSE)
        setAttributeSVG(masknode, "id", mask.id)
        node.copy.in.mask <- xmlChildren(masknode)[[1]]
        setStyleSVG(node.copy.in.mask, "fill", paste("url(#", gradient.id, ")", sep=""))
        removeAttributes(node.copy.in.mask, "id")
        
        # make a copy of the node and put it above, so that the stroke is not masked
        node.copy <- addSibling(node, xmlClone(node), after=FALSE)[[1]]
        setStyleSVG(node.copy, "fill-opacity", 0)
        removeAttributes(node.copy, "id")
        
        # mask only the original node
        setAttributeSVG(node, "mask", paste("url(#", mask.id, ")", sep=""))
        # remove its stroke
        setStyleSVG(node, "stroke-opacity", 0)
        
        if (animations) {
          addJavaScriptCallBack(node, "onmouseover", "animatePartialFill(evt)")
          addJavaScriptCallBack(node, "onmouseout", "stopAnimation()")
        }
      }
    }
  }

  ## R O T A T E
  ## ------------------------------------------------------------
  else if (mode == "rotate") {
    if(is.null(angleRange))
      angleRange <- c(0, 360)  # angleRange(2) > angleRange(1) => clockwise

    ## init. Bbox java object to compute bounding-box
    if(length(nodes) > 0) {
      f = tempfile("svg", fileext="svg")
      saveSVG(svg,f,add.script=FALSE)
      BBox = new(J("cea/dsv/ibitecs/org/BBox"))
      BBox$setFilename(f)
      BBox$init()
    }
    for(node in nodes) {
      id <- xmlGetAttr(node, geneAttribute)
      if(id %in% rownames(numData)) {
        xid <- xmlGetAttr(node,"id")
        bbox <- BBox$getBBox(paste("//*[@id='",xid,"']",sep=""))
        if(is.null(bbox)) {
          warning(paste("Failed to compute Bounding-Box for id:",id))
          warning(paste("-- Temporary file:",f))
          warning(paste("-- XPath: ",paste("//*[@id='",xid,"']",sep="")))
        }
        else {
          assign("bbox", bbox, envir=.GlobalEnv)
          ## get inkscape rotation center modifications if any..
          rcx = xmlGetAttr(node, "inkscape:transform-center-x",
            default=0.0, converter=as.double,
            namespaceDefinition=c("inkscape","http://www.inkscape.org/namespaces/inkscape"))
          rcy = xmlGetAttr(node, "inkscape:transform-center-y",
            default=0.0, converter=as.double,
            namespaceDefinition=c("inkscape","http://www.inkscape.org/namespaces/inkscape"))
          cx = bbox[1] + bbox[3]/2 + rcx
          cy = bbox[2] + bbox[4]/2 + rcy
          transform <- xmlGetAttr(node, "transform")
          if(is.null(transform)) transform=""
          angle = numData[id,] *(angleRange[2] - angleRange[1]) + angleRange[1]
          rotate = paste("rotate(",angle,",",cx,",",cy,")",sep="")
          if(transform=="")  
            transform <- rotate
          else
            transform <- paste(transform,rotate)
          setAttributeSVG(node,"transform",transform)
        }
      }
    }
    if(length(nodes) > 0) {
      unlink(f)
    }
  }

  ## I N V A L I D   M O D E
  ## ------------------------------------------------------------
  else {
    stop(paste("Invalid mode:", mode))
  }
  return(invisible())
}

saveSVG <- function(svg, file="", add.script=TRUE) {
  if (add.script) {
    ## Add instruction to initialize script when SVG file is loaded
    root <- xmlRoot(svg)
    setAttributeSVG(root, "onload", "init(evt)")
    ## Add the JavaScript script
    con <- file(system.file("extdata/script.js", package="SVGMapping"), "rb")
    script.lines <- readLines(con)
    close(con)
    script <- paste(script.lines, collapse="\n")
    addScriptSVG(svg, script, id="SVGMapping-script")
  }
  
  ## Produce source XML
  xml <- saveXML(svg, indent=FALSE)
  xml <- gsub("\n</text>","</text>", xml)
  xml <- gsub("\n<tspan","<tspan", xml)
  
  ## Write/output the SVG
  cat(xml, file=file)
}

addScriptSVG <- function(svg, script, id=NULL) {
  cdata <- newXMLCDataNode(paste("\n", script, "\n", sep=""))
  script.attrs <- list(type="text/ecmascript")
  if (!is.null(id)) {
    script.attrs[["id"]] <- id
  }
  scriptnode <- newXMLNode("script", attrs=script.attrs, .children=list(cdata))
  
  # Replace a script node if it has the same id
  replaced <- FALSE
  if (!is.null(id)) {
    with.same.id <- getNodeSet(svg, paste("//svg:script[@id=\"", id, "\"]", sep=""), namespaces=completeNamespaces(svg))
    if (length(with.same.id) > 0) {
      replaceNodes(with.same.id[[1]], scriptnode)
      replaced <- TRUE
    } 
  }
  if (!replaced) {
    addChildren(xmlRoot(svg), kids=list(scriptnode))
  }
}

addDefinesSVG <- function(svg, nodes) {
  defs <- getNodeSet(svg, "//svg:defs", namespaces=completeNamespaces(svg))
  if (length(defs) == 0) {
    defs <- newXMLNode("defs")
    addChildren(xmlRoot(svg), defs)
  } else {
    defs <- defs[[1]]
  }
  addChildren(defs, nodes)
}

showSVG <- function(svg, browser=getOption("browser"), add.script=TRUE) {
  path <- tempfile()
  svgpath <- paste(path, ".svg", sep="")
  saveSVG(svg, svgpath, add.script=add.script)
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
