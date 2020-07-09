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
    },
    capac = function() return(self$controller$capac),
    geog = function() return(self$controller$geog),
    demog = function() return(self$controller$demog),
    codes = function() return(self$controller$codes),
    datasets = function() return(self$controller$datasets),
    spim = function() return(self$controller$spim),
    postcodes = function() return(self$controller$postcodes)
  
  ), public=list(
  
    cache = list(),
    nocache = FALSE,
    debug = FALSE,
    controller = NULL,
    wd = NULL,
    
    initialize = function(providerController, nocache = FALSE, debug=FALSE) {
      
      self$controller = providerController
      self$nocache = nocache
      self$debug = debug
      self$wd = path.expand(providerController$directory) %>% stringr::str_remove("/$")
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
    getSaved = function(id, orElse, params = NULL, ...) {
      dots = rlang::list2(...)
      if("nocache" %in% names(dots)) {
        nocache = dots$nocache
        dots$nocache = NULL
      } else {
        nocache = self$nocache
      }
      if("dir" %in% names(dots)) {
        dir = dots$dir
        dots$dir = NULL
      } else {
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
        map = rlang::exec("orElse", !!!params, !!!dots)
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
    },
    
    downloadDaily = function(id, url, type="csv") {
      onsZip = paste0(self$todayWd,"/",id,"-",Sys.Date(),".",type)
      if(!file.exists(onsZip)) {
        download.file(url, destfile = onsZip)
      } 
      return(onsZip)
    },
    
    
    
    normaliseGender = function(gender) {
      case_when(
        is.na(gender) ~ NA_character_,
        gender %>% stringr::str_detect("f|F") ~ "female",
        gender %>% stringr::str_detect("m|M") ~ "male",
        gender %>% stringr::str_detect("u|U") ~ "unknown",
        TRUE ~ "unknown")
    },
    
    
    #TODO: is this the right place for this?
    cutByAge = function(age, ageBreaks = NULL) {
      if(identical(ageBreaks,NULL)) return(rep(NA_character_, length(age)))
      ageLabels = c(
        paste0("<",ageBreaks[1]),
        paste0(ageBreaks[1:(length(ageBreaks)-1)],"-",ageBreaks[2:(length(ageBreaks))]-1),
        paste0(ageBreaks[length(ageBreaks)],"+"))
      ageBreaks2 = c(-Inf,ageBreaks,Inf)
      ageCat = forcats::fct_explicit_na(
        cut(age,breaks = ageBreaks2,labels=ageLabels,ordered_result = TRUE,right=FALSE,include.lowest = TRUE),
        na_level = "unknown"
      )
      return(ageCat)
    },
  
    breakFromCats = function(ageCat) {
      tmp = ageCat %>% unique() %>% stringr::str_extract("[0-9]+") %>% unique() %>% as.numeric()
      return(tmp[!is.na(tmp)])
    },
    
    #' @description ordered factor from age range labels
    #' @param ageCat - a vector of age categories as strings
    #' @param ageLabels - a vector of age range labels
    
    #' @return an ordered factor of age categories
    ageCatToFactor = function(ageCat, ageLabels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")) {
      factor(
        ageCat,
        levels = ageLabels,
        ordered = TRUE
      )
    },
    
    #' @description create an ordered factor of ages from a continuous age 
    #' @param age - a vector of ages
    #' @param ageLabels - a vector of age range labels
    
    #' @return an ordered factor of age categories
    ageToAgeCat = function(age, ageLabels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")) {
      ageBreaks = c(ageLabels %>% stringr::str_extract("^[0-9]+") %>% as.integer(),Inf)
      return(cut(age,
                 breaks = ageBreaks,
                 labels = ageLabels,
                 include.lowest = TRUE, ordered_result = TRUE
      ))
    },
    
    #' @description Calculate an estimate of rate of change of Rt using a loess
    #' @param R0timeseries a grouped df containing R0 timeseries including a date and a Median(R) column from EpiEstim
    
    importTimeseries = function(df, dateVar,valueExpr,statisticExpr,typeExpr,codeExpr="no code",nameExpr="no name",codeTypeExpr="undefined", sourceExpr = "undefined",ageCatExpr=NA,genderExpr=NA,subgroupExpr=NA) covidTimeseriesFormat({
      dateVar = ensym(dateVar)
      codeExpr = enexpr(codeExpr)
      nameExpr = enexpr(nameExpr)
      codeTypeExpr = enexpr(codeTypeExpr)
      ageCatExpr = enexpr(ageCatExpr)
      genderExpr = enexpr(genderExpr)
      subgroupExpr = enexpr(subgroupExpr)
      sourceExpr = enexpr(sourceExpr)
      valueExpr = enexpr(valueExpr)
      statisticExpr = enexpr(statisticExpr)
      typeExpr = enexpr(typeExpr)
      out = df %>% dplyr::mutate(
        date = !!dateVar,
        code = !!codeExpr,
        name = !!nameExpr,
        codeType = !!codeTypeExpr,
        ageCat = !!ageCatExpr,
        gender = !!genderExpr,
        subgroup = !!subgroupExpr,
        value = !!valueExpr,
        statistic = !!statisticExpr,
        source = !!sourceExpr,
        type = !!typeExpr
      ) # %>% dplyr::select(
      #  date,code,name,codeType,ageCat,gender,subgroup,value,statistic,type,source
      #)
      return(covidTimeseriesFormat(out))
      # TODO:
      # code and name combination should be unique
      # statistic from list of approved types
      # type from list of approved types
      # date is a date
      # ageCat is a well formatted age category
    })
    
    
))

