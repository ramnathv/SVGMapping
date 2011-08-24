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


## ----------------------------------------
## Pseudo Graphic Device stuff
## ----------------------------------------

devSVGMapping <- function(template, attribute.name="@inkscape:label",
                          attribute.value="Rplot",width=10, height=8,
                          pointsize=10) {

  ## check Cairo
  if(is.null(.get(".cairo")))
    stop("No Cairo SVG driver installed..")
  
  ## check
  if(!is(template$xml,"XMLInternalDocument"))
    stop("Invalid template..")
  .set(".dev.template", template)
  
  ## scan template..
  target.node <- getNodeSet(template$xml,
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
  if(!is(template$xml,"XMLInternalDocument"))
    stop("Invalid template..")

  ## scan template..
  target.node <- getNodeSet(template$xml,
                            paste("//svg:rect[",attribute.name,"='",
                                  attribute.value,"']",sep=""))
  if(length(target.node) == 0)
    stop("Target Attribute not found..")
  target.node <- target.node[[1]]
  
  ## init.
  rplot.svg <- loadSVG(file)
  
  ## 1 - Fix all IDs
  rplot.ids <- as.vector(xpathSApply(rplot.svg$xml, "//@id"))
  
  ## 1.1.1 - Fix (if any) href to ids
  rplot.nodes <- getNodeSet(rplot.svg$xml, "//*[@xlink:href]")
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
  for (node in getNodeSet(rplot.svg$xml, "//*")) {
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
  rplot.nodes <- getNodeSet(rplot.svg$xml, "//*[@id]")
  tmp <- sapply(rplot.nodes, 
                function(el,prefix) {
                  xmlAttrs(el) <- c(id=paste(prefix,xmlGetAttr(el,"id"),sep="_"))
                }, 
                prefix=attribute.value)
  
  ## 2 - Get Dimensions
  s <- xmlRoot(rplot.svg$xml)
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
  free(rplot.svg$xml)
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
