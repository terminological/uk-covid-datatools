#' Get a provider of UK stats
#'
#' This function sets up a connection to the omop databse.
#' @keywords omop
#' @import dplyr
#' @export
UKDemographicsProvider = R6::R6Class("UKDemographicsProvider", inherit=PassthroughFilesystemCache, public = list(
  
  #TODO:
  # Gridded facebook UK population data
  geog = NULL,
  codes = NULL,
  
  initialize = function(geographyProvider, codeMappingProvider, ...) {
    self$geog = geographyProvider
    self$codes = codeMappingProvider
    super$initialize(workingDirectory = geographyProvider$wd, ...)
  },
  
  # TODO: Northern Ireland detailed demographics:
  # shapes:
  # https://www.opendatani.gov.uk/dataset/osni-open-data-50k-boundaries-local-government-districts-1993
  # pop:
  # https://www.opendatani.gov.uk/dataset/population-projections-for-areas-within-northern-ireland-2018-based/resource/5625f768-680b-4684-9d30-d9456dbe2681
  # https://www.opendatani.gov.uk/dataset/3333626e-b96e-4b90-82fb-474c6c03b868/resource/5625f768-680b-4684-9d30-d9456dbe2681/download/mid-2018-based-population-projections-for-areas-within-northern-ireland-lgd92.csv
  
  #' @description get the full range of demographics data at most detailed resolution
  getDetailedDemographics = function() {
    self$getSaved("DEMOG_DETAIL", orElse=function() {
      # England and wales:
      # https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2018sape21dt1a/sape21dt2mid2018lsoasyoaestimatesunformatted.zip
      destfile = paste0(self$wd,"/demographicsUK.zip")
      if(!file.exists(destfile)) download.file(url="https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2018sape21dt1a/sape21dt2mid2018lsoasyoaestimatesunformatted.zip",destfile = destfile)
      unzip(destfile,junkpaths = TRUE,exdir=self$wd,overwrite = TRUE)
      
      # A zipped excel file
      # sheets are: Mid-2018 Males A5:CP34758
      # sheets are: Mid-2018 Females A5:CP34758
      UKdemog = paste0(self$wd,"/SAPE21DT2-mid-2018-lsoa-syoa-estimates-unformatted.xlsx")
      
      convert = function(demogByLSOA) {
        ageCols = colnames(demogByLSOA)[!is.na(as.integer(stringr::str_remove(colnames(demogByLSOA),"\\+")))]
        
        tmp = demogByLSOA %>%
          dplyr::select(-`All Ages`) %>%
          tidyr::pivot_longer(cols=all_of(ageCols),names_to = "age",values_to = "count") #, names_ptypes = list(age=integer()))
        # browser()
        tmp = tmp %>% dplyr::rename(code = `Area Codes`, name=`Area Names`) %>% #, total=`All Ages`) %>%
          dplyr::mutate(age = as.integer(stringr::str_remove(age,"\\+")), codeType="LSOA11")
        return(tmp)
      }

      demogByLSOA_M <- readxl::read_excel(UKdemog, sheet="Mid-2018 Males", skip = 4) %>% convert() %>% dplyr::mutate(gender = "M") 
      demogByLSOA_F <- readxl::read_excel(UKdemog, sheet="Mid-2018 Females", skip = 4) %>% convert() %>% dplyr::mutate(gender = "F") 
      
      scotDemogM = paste0(self$wd,"/demographicsScot_M.xlsx")
      scotDemogF = paste0(self$wd,"/demographicsScot_F.xlsx")
      
      # Scotland:
      # https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series#2018
      # males - https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape-time-series/males/sape-2018-males.xlsx
      # sheet - Table 1b Males (2018) A6:CR6982
      # females - https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape-time-series/females/sape-2018-females.xlsx
      # sheet - Table 1c Females (2018) A6:CR6982
      
      if(!file.exists(scotDemogM)) download.file(url="https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape-time-series/males/sape-2018-males.xlsx",destfile = scotDemogM)
      if(!file.exists(scotDemogF)) download.file(url="https://www.nrscotland.gov.uk/files//statistics/population-estimates/sape-time-series/females/sape-2018-females.xlsx",destfile = scotDemogF)
      
      convert2 = function(demogBySGDZ) {
        demogBySGDZ = demogBySGDZ %>% dplyr::select(-...4,-...5)
        ageCols = colnames(demogBySGDZ)[!is.na(as.integer(stringr::str_remove(colnames(demogBySGDZ),"\\.+")))]
        demogBySGDZ = demogBySGDZ %>% dplyr::pivot_longer(cols=all_of(ageCols),names_to="age",values_to="count")
        demogBySGDZ = demogBySGDZ %>% dplyr::mutate(age = as.integer(stringr::str_remove(age,"\\.+"))-6, codeType="SGDZ11") %>% dplyr::rename(code = DataZone2011Code, name=DataZone2011Name) %>% dplyr::select(-CouncilArea2018Name)
        return(demogBySGDZ)
      }
      
      demogBySGDZ_M =  read_excel(scotDemogM, sheet = "Table 1b Males (2018)", range = "A6:CR6982") %>% convert2() %>% dplyr::mutate(gender = "M") 
      demogBySGDZ_F =  read_excel(scotDemogF, sheet = "Table 1c Females (2018)", range = "A6:CR6982") %>% convert2() %>% dplyr::mutate(gender = "F") 
      
      demographics = bind_rows(
        demogByLSOA_M,
        demogByLSOA_F,
        demogBySGDZ_M,
        demogBySGDZ_F
      )
      
      return(demographics)
    })
  },
  
  #' @description get demographics interpolated to a specific shape file and aggregated to a given set of age bands, with optionally combining genders
  #' @param mapId = the mapId
  #' @param outputShape = the sf object containing the shapefile of the desired output which may be grouped by desired output
  #' @param outputVars = the desired output columns from the output shapefile enclosed by vars(...) (defaults to grouping of outputShape)
  #' @param ageBreaks = where to cut age groups? e.g. c(15,65,80) (max 90)
  #' @param combineGenders = merge the genders
  getDemographicsForShape = function(mapId, outputShape = self$getMap(mapId) %>% dplyr::group_by(code,name), outputVars = outputShape %>% dplyr::groups(), ageBreaks = seq(5,90,5), combineGenders=FALSE) {
    
    # Cut the detailed demographics into desired age bands
    ageLabels = c(
      paste0("<",ageBreaks[1]),
      paste0(ageBreaks[1:(length(ageBreaks)-1)],"-",ageBreaks[2:(length(ageBreaks))]-1),
      paste0(ageBreaks[length(ageBreaks)],"+"))
    ageBreaks2 = c(-Inf,ageBreaks,Inf)
    
    demog = self$getDetailedDemographics() %>% 
      dplyr::mutate(ageCat = cut(age,breaks = ageBreaks2,labels=ageLabels,ordered_result = TRUE,right=FALSE,include.lowest = TRUE)) %>%
      dplyr::select(-age) 
    if (isTRUE(combineGenders)) {
      demog = demog %>% dplyr::group_by(ageCat,code) %>% dplyr::summarise(count = sum(count))
    } else {
      demog = demog %>% dplyr::group_by(ageCat,gender,code) %>% dplyr::summarise(count = sum(count))
    }
    
    mapping = self$geog$interpolateByArea(
      inputDf = demog, 
      inputMapId = "DEMOG", 
      inputShape = self$geog$getDemographicsMap(),
      interpolateVar = count,
      outputMapId = mapId,
      outputShape = outputShape,
      outputVars = outputVars,
      aggregateFn = sum
    )
    
    return(mapping)
  },
  
  getDemographicsForCodeTypes = function(codeTypes, ageBreaks = seq(5,90,5), combineGenders=FALSE) {
    # get demographics encoded to level of LSOA and DZ
    ageLabels = c(
      paste0("<",ageBreaks[1]),
      paste0(ageBreaks[1:(length(ageBreaks)-1)],"-",ageBreaks[2:(length(ageBreaks))]-1),
      paste0(ageBreaks[length(ageBreaks)],"+"))
    ageBreaks2 = c(-Inf,ageBreaks,Inf)
    
    demog = self$getDetailedDemographics() %>% 
      dplyr::mutate(ageCat = cut(age,breaks = ageBreaks2,labels=ageLabels,ordered_result = TRUE,right=FALSE,include.lowest = TRUE)) %>%
      dplyr::select(-age) 
    if (isTRUE(combineGenders)) {
      demog = demog %>% dplyr::group_by(ageCat,code) %>% dplyr::summarise(count = sum(count))
    } else {
      demog = demog %>% dplyr::group_by(ageCat,gender,code) %>% dplyr::summarise(count = sum(count))
    }
    
    browser(expr = self$debug)
    
    codeMap = self$codes$getTransitiveClosure() %>% dplyr::filter(toCodeType %in% codeTypes)
    tmp = demog %>% dplyr::ungroup() %>% dplyr::select(code) %>% dplyr::distinct() %>% dplyr::left_join(codeMap, by=c("code"="fromCode"))
    tmp = tmp %>% dplyr::group_by(code) %>%  dplyr::summarise(hits = sum(ifelse(is.na(toCode),0,1)))
    
    if(any(tmp$hits > 1)) warning("There are some LSOA or DZ areas which map to more than one output area. This will result in double counting of the total population.")
    else if(any(tmp$hits == 0)) warning("There are some LSOA areas which are missing in the output areas, the output does not cover the whole population area.")
    else message("Deomgraphic mapping: A one to one mapping between input and output exists!")
    
    if (isTRUE(combineGenders)) {
      out = demog %>% dplyr::left_join(codeMap, by=c("code"="fromCode")) %>% dplyr::select(-code) %>% dplyr::rename(code = toCode, codeType = toCodeType) %>% dplyr::group_by(ageCat,code)  %>% dplyr::summarise(count = sum(count))
    } else {
      out = demog %>% dplyr::left_join(codeMap, by=c("code"="fromCode")) %>% dplyr::select(-code) %>% dplyr::rename(code = toCode, codeType = toCodeType) %>% dplyr::group_by(ageCat,code,gender)  %>% dplyr::summarise(count = sum(count))
    }
    
    return(out)
  },
  
  getDemographicsFromLSOAanDZMapping = function(mappingDf, fromCodeVar = "fromCode", toCodeVar = "toCode", fromCodeFracExpr = NULL, ageBreaks = seq(5,90,5), combineGenders=FALSE) {
    # fromCodeWeightVar, toCodeWeightVar, intersectionWeightVar / weightVar (=intersectionArea / fromCodeArea)
    fromCodeVar = ensym(fromCodeVar)
    toCodeVar = ensym(toCodeVar)
    fromCodeFracExpr = tryCatch(enexpr(fromCodeFracExpr),error = function() expr(1))
    # get demographics encoded to level of LSOA and DZ
    ageLabels = c(
      paste0("<",ageBreaks[1]),
      paste0(ageBreaks[1:(length(ageBreaks)-1)],"-",ageBreaks[2:(length(ageBreaks))]-1),
      paste0(ageBreaks[length(ageBreaks)],"+"))
    ageBreaks2 = c(-Inf,ageBreaks,Inf)
    
    demog = self$getDetailedDemographics() %>% 
      dplyr::mutate(ageCat = cut(age,breaks = ageBreaks2,labels=ageLabels,ordered_result = TRUE,right=FALSE,include.lowest = TRUE)) %>%
      dplyr::select(-age) 
    if (isTRUE(combineGenders)) {
      demog = demog %>% dplyr::group_by(ageCat,code) %>% dplyr::summarise(count = sum(count))
    } else {
      demog = demog %>% dplyr::group_by(ageCat,gender,code) %>% dplyr::summarise(count = sum(count))
    }
    
    browser(expr = self$debug)
    
    #codeMap = self$codes$getTransitiveClosure() %>% dplyr::filter(toCodeType %in% codeTypes)
    codeMap = mappingDf %>% dplyr::rename(tmp_fromCode = !!fromCodeVar, tmp_toCode = !!toCodeVar) %>% dplyr::mutate(tmp_frac = !!fromCodeFracExpr) %>% dplyr::select(tmp_fromCode, tmp_toCode, tmp_frac)
    tmp = demog %>% dplyr::ungroup() %>% dplyr::select(code) %>% dplyr::distinct() %>% dplyr::left_join(codeMap, by=c("code"="tmp_fromCode"))
    tmp = tmp %>% dplyr::group_by(code) %>%  dplyr::summarise(hits = sum(ifelse(is.na(toCode),0,1)))
    
    if(any(tmp$hits > 1)) warning("There are some LSOA or DZ areas which map to more than one output area. This will result in double counting of the total population.")
    else if(any(tmp$hits == 0)) warning("There are some LSOA areas which are missing in the output areas, the output does not cover the whole population area.")
    else message("Deomgraphic mapping: A one to one mapping between input and output exists!")
    
    if (isTRUE(combineGenders)) {
      out = demog %>% 
        dplyr::left_join(codeMap, by=c("code"="tmp_fromCode")) %>% 
        dplyr::select(-code) %>% 
        dplyr::mutate(count = count*tmp_frac) %>% 
        dplyr::group_by(ageCat,!!toCodeVar)  %>% 
        dplyr::summarise(count = sum(count)) %>%
        dplyr::ungroup()
    } else {
      out = demog %>% 
        dplyr::left_join(codeMap, by=c("code"="tmp_fromCode")) %>% 
        dplyr::select(-code) %>% 
        dplyr::mutate(count = count*tmp_frac) %>% 
        dplyr::group_by(ageCat,gender,!!toCodeVar) %>% 
        dplyr::summarise(count = sum(count)) %>%
        dplyr::ungroup()
    }
    
    return(out)
  }
  
))

# library(tidyverse)
# library(sf)
ugp = UKGeographyProvider$new("~/Data/maps")
#upp = UKPostcodeProvider$new("~/Data/maps")
#ump = UKCodeMappingProvider$new(upp)
udp = UKDemographicsProvider$new(ugp,ump)
# udp$debug = TRUE
tmp = udp$getDemographicsForCodeTypes(codeTypes=c("HB","LHB","CCG"))

tmp = udp$getDemographicsForCodeTypes(codeTypes=c("PHEC"))
# usp$loadAllMaps()
# # dm = usp$getPHEDashboardMap()
# # #usp$saveShapefile("DASH_LTLA", dm)
# # tmp = usp$getDemographics("DASH_LTLA", dm %>% dplyr::group_by(code,name), combineGenders = FALSE)
# 
# # sf = usp$getDemographicsMap()
# # d = usp$getDetailedDemographics()
# # x = usp$getDemographics("CTRY19")
# # sf = usp$getDemographics("WD11")
# # # sf = usp$getDemographics("LSOA11")
# # # sf = usp$getDemographics("SGDZ11")
# # sf = usp$getDemographics("SHB19")
# # usp$preview("LHB19")
# # sf = usp$getDemographics("CTYUA19")
# # sf = usp$getDemographics("LAD19")
# # sf = usp$getDemographics("CCG20")
# # sf = usp$getDemographics("NHSER20")
# # sf = usp$getDemographics("PHEC16")
# # sf = usp$getDemographics("LGD12")
# # plot(sf)
# 
# tmp = usp$getHospitals(icuBeds>0 & sector=="NHS Sector")
# catch = usp$createCatchment(
#   supplyId = "ICUBEDS", supplyShape = tmp %>% dplyr::rename(hospId = code, hospName = name) %>% dplyr::group_by(hospId,hospName), supplyIdVar = hospId, supplyVar = icuBeds,
#   demandId = "DEMOG", demandShape = usp$getDemographicsMap(), demandVar = count, demandIdVar = code
# )
# 
# usp$preview(shape=catch$map %>% dplyr::mutate(per100K = 100000*icuBeds/count),nameVar = hospName, codeVar = per100K, poi=catch$suppliers, poiNameVar = hospName, poiCodeVar = icuBeds)
