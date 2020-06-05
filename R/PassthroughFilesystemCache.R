#' Get a provider of UK stats
#'
#' This function sets up a connection to the omop databse.
#' @keywords omop
#' @import dplyr
#' @export
PassthroughFilesystemCache = R6::R6Class("PassthroughFilesystemCache", public=list(
  
  wd = NULL,
  cache = list(),
  nocache = FALSE,
  debug = FALSE,
  
  initialize = function(workingDirectory, nocache = FALSE, debug=FALSE) {
    self$wd = path.expand(workingDirectory)
    self$nocache = nocache
    self$debug = debug
  },
  
  #' @description a pass through 2 level cache (memory / saved file / orElse function)
  getSaved = function(id, orElse, ...) {
    filename = paste0(self$wd,"/",id,".rda")
    if(self$nocache) {
      self$cache[[id]] <- NULL
      unlink(filename)
    }
    if (exists(id, where=self$cache)) {
      return(self$cache[[id]]) 
    }
    if(file.exists(filename)) {
      message("using cached:", id)
      map = readRDS(filename)
      self$cache[[id]]=map
      return(map)
    } else {
      message("caching: ", id)
      map = orElse(...)
      saveRDS(map,filename)
      self$cache[[id]]=map
      return(map)
    }
  },
  
  downloadAndUnzip = function(id, url, pattern) {
    onsZip = paste0(self$wd,"/",id,".zip")
    unzipDir = paste0(self$wd,"/",id)
    if(!file.exists(onsZip)) {
      download.file(url, destfile = onsZip)
    } 
    if (!dir.exists(unzipDir)) {
      dir.create(unzipDir)
      unzip(onsZip, exdir=unzipDir, junkpaths = TRUE)
    }
    csvfile = paste0(unzipDir,"/",list.files(unzipDir,recursive = TRUE,pattern = pattern))
    return(csvfile)
  },
  
  download = function(id, url, type="csv") {
    onsZip = paste0(self$wd,"/",id,".",type)
    if(!file.exists(onsZip)) {
      download.file(url, destfile = onsZip)
    } 
    return(onsZip)
  }
  
  
  
  
))