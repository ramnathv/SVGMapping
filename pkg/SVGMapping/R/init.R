## Package Initialization
.svgmapping.env <- new.env()

.onLoad <- function(libname, pkgname) {

  ## internals
  .checkCairo <- function() {

    ## 0 - builtin SVG Cairo
    gr.cap <- capabilities()
    if(gr.cap["cairo"]) return(TRUE)
    
    ## 1 - Find Cairo + CairoDevice in the search-path
    loaded = search()
    if(length(grep("package:(Cairo|cairoDevice)", loaded)) == 2)
      return(TRUE)
  
    ## 2 - Cairo + CairoDevice package available
    cairo = find.package("Cairo", quiet=TRUE)
    if(length(cairo) > 0) {
      cairo.dev = find.package("cairoDevice", quiet=TRUE)
      if(length(cairo.dev) > 0) {
        ret = require("Cairo", quietly=TRUE)
        if(ret) ret=require("cairoDevice", quietly=TRUE)
      if(ret) return(TRUE)
      }
    }
  
    ## eop
    return(FALSE)
  }
  
  ## init. package env.
  .set(".dev.template", NULL)
  .set(".dev.rplot", NULL)
  .set(".dev.target.name", NULL)
  .set(".dev.target.value", NULL)

  ## init. check CAIRO support
  cairo <- .checkCairo()
  if(!cairo)
    warning("No SVG Cairo engine.. ('devSVGMapping' won't work)")
  .set(".cairo", cairo)

}
