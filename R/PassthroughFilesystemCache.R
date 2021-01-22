#' Caching and dataset management
#' @export
PassthroughFilesystemCache = R6::R6Class("PassthroughFilesystemCache", 
  active=list(
    
    todayWd = function() {
      path = paste0(self$wd,"/",Sys.Date())
      if (!dir.exists(path)) dir.create(path)
      return(path)
    },
    tmpWd = function() {
      path = paste0(self$wd,"/tmp")
      if (!dir.exists(path)) dir.create(path)
      return(path)
    }
  
  ), public=list(
  
    cache = list(),
    nocache = FALSE,
    debug = FALSE,
    wd = NULL,
    
    initialize = function(wd = tempdir(), nocache = FALSE, debug=FALSE, ...) {
      self$nocache = nocache
      self$debug = debug
      self$wd = path.expand(wd) %>% stringr::str_remove("/$") #path.expand(providerController$directory) %>% stringr::str_remove("/$")
    },
    
    getDaily = function(id, orElse, ...) {
      # dots = rlang::list2(...)
      self$getSaved(id = paste0(id,"-",Sys.Date()), dir = self$todayWd, orElse=orElse, ...) #!!!dots)
    },
    
    getHashCached = function(object, operation, params = NULL, orElse, ...) {
      #dots = rlang::list2(...)
      id = paste0(operation,"-",as.character(openssl::md5(serialize(object, connection = NULL))))
      self$getSaved(id=id, dir=self$tmpWd, params=params, orElse=orElse, object, ...)
    },
    
    #' @description a pass through 2 level cache (memory / saved file / orElse function)
    getSaved = function(id, orElse, ..., params = NULL, nocache=NULL, dir=NULL) {
      # dots = rlang::list2(...)
      # if("nocache" %in% names(dots)) {
      #   nocache = dots$nocache
      #   dots$nocache = NULL
      # } else {
      #   nocache = self$nocache
      # }
      # if("dir" %in% names(dots)) {
      #   dir = dots$dir
      #   dots$dir = NULL
      # } else {
      #   dir = self$wd
      # }
      if (identical(nocache,NULL)) {
        nocache = self$nocache
      }
      if (identical(dir,NULL)) {
        dir = self$wd
      }
      
      # 
      # if(length(dots)>0) {
      #   dots = dots[order(names(dots))]
      #   id = paste0(id,"-",as.character(openssl::md5(serialize(dots, connection = NULL))))
      # }
      if(!identical(params,NULL)) {
        id = paste0(id,"-",as.character(openssl::md5(serialize(params, connection = NULL))))
      }
      filename = paste0(dir,"/",id,".rda")
      if(nocache) {
        self$cache[[id]] <- NULL
        unlink(filename)
      }
      if (exists(id, where=self$cache)) {
        return(self$cache[[id]]) 
      }
      if(file.exists(filename)) {
        message("using cached: ", id)
        map = readRDS(filename)
        self$cache[[id]]=map
        return(map)
      } else {
        message("caching: ", id)
        #map = rlang::exec("orElse", !!!params, !!!dots)
        if(!identical(params,NULL)) {
          map = orElse(!!!params, ...)
        } else {
          map = orElse(...)
        }
        saveRDS(map,filename)
        self$cache[[id]]=map
        return(map)
      }
    }
    
    
))

