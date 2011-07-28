## Package Initialization
.svgmapping.env <- new.env()

.onLoad <- function(libname, pkgname) {

  ## internals
  .checkCairo <- function() {

    ## 0 - builtin SVG Cairo
    gr.cap <- capabilities()
    if(gr.cap["cairo"]) return("builtin")
    
    ## 1 - Find CairoDevice in the search-path
    loaded = search()
    if(length(grep("package:cairoDevice", loaded)) == 1)
      return("cairoDevice")
  
    ## 2 - Cairo + CairoDevice package available
    cairo.dev = find.package("cairoDevice", quiet=TRUE)
    if(length(cairo.dev) > 0) {
      ret=require("cairoDevice", quietly=TRUE)
      if(ret) return("cairoDevice")
    }
  
    ## eop
    return(NULL)
  }
  
  ## init. package env.
  .set(".dev.template", NULL)
  .set(".dev.rplot", NULL)
  .set(".dev.target.name", NULL)
  .set(".dev.target.value", NULL)

  ## init. check CAIRO support
  cairo <- .checkCairo()
  if(is.null(cairo))
    warning("No SVG Cairo engine.. ('devSVGMapping' won't work)")
  .set(".cairo", cairo)

}
