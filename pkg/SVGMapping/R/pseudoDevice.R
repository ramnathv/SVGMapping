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

## ---------------------------------------------
## P S E U D O   G R A P H I C S    D E V I C E
## ---------------------------------------------

devSVGMapping <- function(template, attribute.name="@inkscape:label",
                          attribute.value="Rplot", width, height,
                          pointsize=10) {

  ## check Cairo
  if(is.null(.get(".cairo")))
    stop("No Cairo SVG driver installed..")
  
  ## check
  if(!is(template,"XMLInternalDocument"))
    stop("Invalid template..")
  
  ## scan template..
  target.node <- getNodeSet(template,
                            paste("//svg:rect[",attribute.name,"='",
                                  attribute.value,"']",sep=""), namespaces=c(svg=svgNS))
  if(length(target.node) == 0)
    stop("Target Attribute not found..")
  target.node <- target.node[[1]]
  
  if (missing(width) || missing(height)) {
    ## get target coordinates
    target.w <- .toUserUnit(xmlGetAttr(target.node, "width", 10*90))
    target.h <- .toUserUnit(xmlGetAttr(target.node, "height", 8*90))
    # convert from default user units to inches
    width <- target.w/90
    height <- target.h/90
  }

  ## init. SVG device
  .dev.rplot <- paste(tempfile(pattern="rplot"), ".svg", sep="")
  if (.get(".cairo") == "cairoDevice") {
    Cairo_svg(filename=path.expand(.dev.rplot),
              width=width, height=height, pointsize=pointsize)
  } else {
    svg(filename=path.expand(.dev.rplot),
        width=width, height=height, pointsize=pointsize)
  }

  ## update device info
  devinfo <- list("template" = template,
                  "target.name" = attribute.name,
                  "target.value" = attribute.value,
                  "rplot" = .dev.rplot
                  )
  devid <- dev.cur()
  .addDeviceInfo(devid, devinfo)

  ## return the device ID
  return(invisible(devid))
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
                                  attribute.value,"']",sep=""), namespaces=c(svg=svgNS))
  if(length(target.node) == 0)
    stop("Target Attribute not found..")
  target.node <- target.node[[1]]
  
  ## init.
  rplot.svg <- loadSVG(file)
  
  ## 1 - Fix all IDs
  rplot.ids <- as.vector(xpathSApply(rplot.svg, "//@id"))
  
  ## 1.1.1 - Fix (if any) href to ids
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
  
  ## 1.1.2 Fix url(#...) references
  for (node in getNodeSet(rplot.svg, "//*")) {
    attrs <- xmlAttrs(node, addNamespacePrefix = TRUE)
    for (attname in names(attrs)) {
      attval <- attrs[[attname]]
      if (grepl("url\\(\\#.+\\)", attval)) {
        attval <- gsub("url\\(\\#", paste("url(#", attribute.value, "_", sep=""), attval)
        attrs[[attname]] <- attval
        xmlAttrs(node, suppressNamespaceWarning = TRUE) <- attrs
      }
    }
  }
  
  ## 1.2 - Fix ids
  rplot.nodes <- getNodeSet(rplot.svg, "//*[@id]")
  tmp <- sapply(rplot.nodes, 
                function(el,prefix) {
                  xmlAttrs(el) <- c(id=paste(prefix,xmlGetAttr(el,"id"),sep="_"))
                }, 
                prefix=attribute.value)
  
  ## 2 - Get Dimensions
  s <- xmlRoot(rplot.svg)
  viewBox <- getAttributeSVG(s, "viewBox")
  if (!is.null(viewBox)) {
    viewBox <- as.numeric(strsplit(viewBox, " ")[[1]])
    rplot.x <- viewBox[1]
    rplot.y <- viewBox[2]
    rplot.w <- viewBox[3]
    rplot.h <- viewBox[4]
  } else {
    rplot.x <- .toUserUnit(xmlGetAttr(s, "x", "0"))
    rplot.y <- .toUserUnit(xmlGetAttr(s, "y", "0"))
    rplot.w <- .toUserUnit(xmlGetAttr(s, "width", "0"))
    rplot.h <- .toUserUnit(xmlGetAttr(s, "height", "0"))
  }
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
  devinfo <- .getDeviceInfo(which)
  
  ## check if we are plotting whithin a template?
  if(!is.null(devinfo)) {
    includeSVG(devinfo$template, devinfo$rplot,
               devinfo$target.name,devinfo$target.value)
    
    ## free & unlink stuff
    unlink(devinfo$rplot)
    .addDeviceInfo(which, NULL)
  }
}
