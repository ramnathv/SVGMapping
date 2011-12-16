library(XML)


KGML_URL <- "http://www.genome.jp/kegg-bin/download?entry=%s&format=kgml"
CACHE_FOLDER <- paste(tempdir(),"kegg_files",sep="/")
if(!file.exists(CACHE_FOLDER)) dir.create(CACHE_FOLDER)

forgeKGMLurl <- function(pathwayid, organism="hsa") {
  
  ## -- build kgml url
  if(grepl("^[a-z]",pathwayid))
    url <- pathwayid
  else
    url <- paste(organism,pathwayid,sep="")
  
  ## -- check url
  if(!grepl("^[a-zA-Z]{2,3}[0-9]{5}$",url))
    return(NULL)
  else
    return(sprintf(KGML_URL,url))
}

forgeCacheFile <- function(pathwayid,organism="hsa") {
  return(paste(CACHE_FOLDER,paste(organism,pathwayid,".xml",sep=""),sep="/"))
}

KEGGTemplate <- function(pathwayid, organism="hsa") {
  
  ## -- split (if req.) pathway id
  if(nchar(pathwayid) > 5) {
    organism <- pathwayid
    pathwayid <- substring(pathwayid,nchar(pathwayid)-4)
    organism <- substring(organism,1,nchar(organism)-5)
  }
  
  ## -- init: download reference maps..
  ec_file <- forgeCacheFile(pathwayid,"ec")
  ko_file <- forgeCacheFile(pathwayid,"ko")
  rn_file <- forgeCacheFile(pathwayid,"rn")
  if(!file.exists(ec_file)) 
    download.file(forgeKGMLurl(pathwayid,"ec"), ec_file, method="auto")
  if(!file.exists(ko_file)) 
    download.file(forgeKGMLurl(pathwayid,"ec"), ko_file, method="auto")
  if(!file.exists(rn_file)) 
    download.file(forgeKGMLurl(pathwayid,"ec"), rn_file, method="auto")
  
  ## -- parse the kgml file
  kgml_file <- forgeKGMLurl(pathwayid=pathwayid,organism=organism)
  if(is.null(kgml_file)) {
    warning("Invalid pathway/organism identifiers")
    return(NULL)
  }
  kgml <- xmlTreeParse(kgml_file, isURL=TRUE, useInternalNodes=TRUE, 
                       addAttributeNamespaces=TRUE, fullNamespaceInfo=FALSE)
  
  ## -- eop  
  return(kgml)
}