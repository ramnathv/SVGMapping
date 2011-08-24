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
## I N T E R N A L S   F U N C T I O N S
## ----------------------------------------

.get <- function(var) {
  return(get(var,envir=.svgmapping.env))
}

.set <- function(var,value) {
  assign(var,value,envir=.svgmapping.env)
}

.getPtrAddress <- function(ptr) {
  if(typeof(ptr) == "externalptr") 
    return(gsub("(<pointer: )(.*)(>)","\\2",deparse(ptr)))
  else
    return(NULL)
}

.getMetaData <- function(svg) {

  ## init.
  svg.add <- .getPtrAddress(svg)
  if(is.null(svg.add)) return (NULL)
  templates_meta <- .get("Templates.MetaData")
  
  ## retrieve metadata
  if(svg.add %in% names(templates_meta))
    return(templates_meta[svg.add])
  else
    return(NULL)
}

.setMetaData <- function(svg, metadata) {

  ## init.
  svg.add <- .getPtrAddress(svg)
  if(is.null(svg.add)) return (NULL)
  templates_meta <- .get("Templates.MetaData")

  ## assign metadata
  templates_meta[svg.add] <- metadata
  .set("Templates.MetaData", templates_meta)
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
