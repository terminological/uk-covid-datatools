#' UK ONS demographics
#' @export
UKDemographicsProvider = R6::R6Class("UKDemographicsProvider", inherit=DataProvider, public = list(
  
  initialize = function(providerController, ...) {
    super$initialize(providerController, ...)
  },
  
  #' @description get the full range of demographics data at most detailed resolution
  getDetailedDemographics = function(...) {
    self$getSaved("DEMOG_DETAIL",..., orElse=function(...) {
      # England and wales:
      # https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates
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
        
        tmp = tmp %>% dplyr::rename(code = `Area Codes`, name=`Area Names`) %>% #, total=`All Ages`) %>%
          dplyr::mutate(age = as.integer(stringr::str_remove(age,"\\+")), codeType="LSOA11")
        return(tmp)
      }

      demogByLSOA_M <- readxl::read_excel(UKdemog, sheet="Mid-2018 Males", skip = 4) %>% convert() %>% dplyr::mutate(gender = "male") 
      demogByLSOA_F <- readxl::read_excel(UKdemog, sheet="Mid-2018 Females", skip = 4) %>% convert() %>% dplyr::mutate(gender = "female") 
      
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
        demogBySGDZ = demogBySGDZ %>% 
          tidyr::pivot_longer(cols=all_of(ageCols),names_to="age",values_to="count")
        demogBySGDZ = demogBySGDZ %>% 
          dplyr::mutate(age = as.integer(stringr::str_remove(age,"\\.+"))-6, codeType="SGDZ11") %>% dplyr::rename(code = DataZone2011Code, name=DataZone2011Name) %>% dplyr::select(-CouncilArea2018Name)
        return(demogBySGDZ)
      }
      
      demogBySGDZ_M =  readxl::read_excel(scotDemogM, sheet = "Table 1b Males (2018)", range = "A6:CR6982") %>% convert2() %>% dplyr::mutate(gender = "male") 
      demogBySGDZ_F =  readxl::read_excel(scotDemogF, sheet = "Table 1c Females (2018)", range = "A6:CR6982") %>% convert2() %>% dplyr::mutate(gender = "female") 
      
      demogNI = readr::read_csv("https://www.opendatani.gov.uk/dataset/3333626e-b96e-4b90-82fb-474c6c03b868/resource/64bd8dc4-935f-4bdd-9232-90ff33f24732/download/mid-2018-based-population-projections-for-areas-within-northern-ireland-lgd14.csv")
      demogNI = demogNI %>% 
        dplyr::mutate(gender = stringr::str_to_lower(Gender), codeType="LGD") %>%
        dplyr::filter(Mid_Year_Ending==2020) %>%
        dplyr::select(code =Geo_Code, name=Geo_Name, age=Age, count=Population_Projection, gender, codeType) %>%
        dplyr::filter(gender %in% c("male","female"))
      
      demographics = dplyr::bind_rows(
        demogByLSOA_M,
        demogByLSOA_F,
        demogBySGDZ_M,
        demogBySGDZ_F,
        demogNI
      )
      
      return(demographics)
    })
  },
  
  getDemographicsWithEstimatedEthnicity = function(...) {
    self$getSaved("DEMOG_ETHNIC",..., orElse=function(...) {
      #https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationcharacteristicsresearchtables
      ethnPopFile = self$download(id = "ONS_DEMOG_ETHNICITY",
                    url = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationcharacteristicsresearchtables%2fdecember2019/supportingtablesforpub.xlsx",
                    type = "xlsx")
      ethnPop = readxl::read_excel(ethnPopFile,sheet = "Table A",skip = 6,na = "NA")
      ethnPop = ethnPop %>% 
        rename(
          code=`Area Code`,
          name=`Area Name`,
        ) %>% select(-`2011 Census Supergroup`,-Total) %>%
        filter(!is.na(code)) %>%
        mutate(
          `Afro-carribbean` = `Black / African / Caribbean / Black British`,
          `Asian` = `Asian / Asian British`,
          `White` = `White British`+`All Other White`,
          `Other` = `Other ethnic group`+`Mixed / Multiple ethnic groups`
        ) %>% select(-`White British`,-`All Other White`,-`Mixed / Multiple ethnic groups`,-`Asian / Asian British`,-`Black / African / Caribbean / Black British`,-`Other ethnic group`)
      ethnPop = ethnPop %>% pivot_longer(cols=c(everything(),-code,-name),names_to="ethnicity_final",values_to="thousands")
      ethnPop = ethnPop %>% mutate(thousands = ifelse(is.na(thousands),0,thousands)) %>% group_by(code,name) %>% mutate(proportion = thousands/sum(thousands))
      ethnPop = ethnPop %>% self$codes$findNamesByCode()
      
      tmp = self$getDetailedDemographics() %>% group_by(code,name) %>% summarise(count=sum(count))
      tc = self$codes$getTransitiveClosure() %>% semi_join(tmp, by=c("fromCode"="code")) %>% semi_join(ethnPop, by=c("toCode"="code"))
      tc2 = tc %>% group_by(fromCode) %>% arrange(distance,path) %>% filter(row_number()==1)
      
      # TODO: Codes from demographics map that are not covered by England only ethnicity estimates. To make this work we would need to have ethnicity estimates for NI & Scotland
      # tmp %>% anti_join(tc2, by=c("code"="fromCode")) %>% View()
      
      tmp2 = tmp %>% inner_join(tc2, by=c("code"="fromCode")) %>% inner_join(ethnPop %>% ungroup() %>% select(-name), by=c("toCode"="code"))
      tmp2 = tmp2 %>% mutate(count=count*proportion) %>% select(code,name,ethnicity_final,count)
      return(tmp2)
    })
  },
  
  getIMDData = function(...) {
    self$getSaved("IMD",..., orElse=function(...) {
      readr::read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv")
    })
  },
  
  
  #' @description LSOA & Scottish Data Zones
  getDemographicsMap = function(...) {
    tmp = self$getDetailedDemographics()
    tmp = tmp %>% dplyr::group_by(code) %>% dplyr::summarise(count = sum(count))
    self$getSaved("DEMOG",...,orElse = function(...) {return(
      rbind(
        self$geog$getMap("LSOA11"),
        self$geog$getMap("DZ11"),
        self$geog$getMap("LGD12")
      ) %>% dplyr::group_by(code,name))}) %>% dplyr::left_join(tmp, by="code")
  },
  
  #' @description get demographics interpolated to a specific shape file and aggregated to a given set of age bands, with optionally combining genders
  #' @param mapId = the mapId
  #' @param outputShape = the sf object containing the shapefile of the desired output which may be grouped by desired output
  #' @param outputVars = the desired output columns from the output shapefile enclosed by vars(...) (defaults to code and name)
  #' @param ageBreaks = where to cut age groups? e.g. c(15,65,80) (max 90)
  #' @param combineGenders = merge the genders
  getDemographicsForShape = function(mapId, outputShape = self$geog$getMap(mapId), outputVars = vars(code,name), ageBreaks = seq(5,90,5), combineGenders=FALSE) {
    
    # Cut the detailed demographics into desired age bands
    
    demog = self$getDetailedDemographics() %>% 
      dplyr::mutate(ageCat = self$cutByAge(age,ageBreaks = ageBreaks)) %>%
      dplyr::select(-age) 
    if (isTRUE(combineGenders)) {
      demog = demog %>% dplyr::group_by(ageCat,code) %>% dplyr::summarise(count = sum(count))
    } else {
      demog = demog %>% dplyr::group_by(ageCat,gender,code) %>% dplyr::summarise(count = sum(count))
    }
    
    mapping = self$geog$interpolateByArea(
      inputDf = demog, 
      inputMapId = "DEMOG", 
      inputShape = self$getDemographicsMap(),
      interpolateVar = count,
      outputMapId = mapId,
      outputShape = outputShape,
      outputVars = outputVars,
      aggregateFn = sum
    )
    
    return(mapping)
  },
  
  getDemographicsForCodeTypes = function(codeTypes, ageBreaks = seq(5,90,5), combineGenders=FALSE) {
    codeMap = self$codes$getTransitiveClosure() %>% dplyr::filter(fromCodeType %in% c("LSOA","DZ","LGD") & toCodeType %in% codeTypes)
    out = self$getDemographicsFromWeightedMapping(codeMap, fromCode, toCode, weightExpr = 1, ageBreaks = ageBreaks, combineGenders = combineGenders)
    
    return(out)
    
  },
  
  getDemographicsFromWeightedMapping = function(mappingDf, fromCodeVar = "fromCode", toCodeVar = "toCode", outputCodeVar = "code", weightExpr = 1, ageBreaks = seq(5,90,5), combineGenders=FALSE) {
    # fromCodeWeightVar, toCodeWeightVar, intersectionWeightVar / weightVar (=intersectionArea / fromCodeArea)
    fromCodeVar = ensym(fromCodeVar)
    toCodeVar = ensym(toCodeVar)
    outputCodeVar = ensym(outputCodeVar)
    weightExpr = enexpr(weightExpr)
    # get demographics encoded to level of LSOA and DZ
    
    demog = self$getDetailedDemographics() %>% 
      dplyr::mutate(ageCat = self$cutByAge(age, ageBreaks)) %>%
      dplyr::select(-age) 
    
    if (isTRUE(combineGenders)) demog = demog %>% dplyr::mutate(gender=NA)
    demog = demog %>% dplyr::group_by(ageCat,gender,code) %>% dplyr::summarise(count = sum(count))
    
    browser(expr = self$debug)
    
    #codeMap = self$codes$getTransitiveClosure() %>% dplyr::filter(toCodeType %in% codeTypes)
    codeMap = mappingDf %>% dplyr::rename(tmp_fromCode = !!fromCodeVar, tmp_toCode = !!toCodeVar) %>% dplyr::mutate(tmp_frac = !!weightExpr) %>% dplyr::select(tmp_fromCode, tmp_toCode, tmp_frac)
    tmp = demog %>% dplyr::ungroup() %>% dplyr::select(code) %>% dplyr::distinct() %>% dplyr::left_join(codeMap, by=c("code"="tmp_fromCode"))
    tmp = tmp %>% dplyr::group_by(code) %>%  dplyr::summarise(hits = sum(ifelse(is.na(tmp_toCode),0,1)))
    
    if(any(tmp$hits > 1)) warning("There are some LSOA or DZ areas which map to more than one output area. This will result in double counting of the total population.")
    else if(any(tmp$hits == 0)) warning("There are some LSOA areas which are missing in the output areas, the output does not cover the whole population area.")
    else message("Demographic mapping: A one to one mapping between input and output exists!")
    
    out = demog %>% 
      dplyr::left_join(codeMap, by=c("code"="tmp_fromCode")) %>% 
      dplyr::select(-code) %>% 
      dplyr::rename(!!outputCodeVar := tmp_toCode) %>%
      dplyr::mutate(count = count*ifelse(is.na(tmp_frac),1,tmp_frac)) %>% 
      dplyr::group_by(ageCat,gender,!!outputCodeVar) %>% 
      dplyr::summarise(count = sum(count)) %>%
      dplyr::ungroup()
    
    return(out)
  },
  
  
  getSingleDigitDemographics = function(mappingDf, fromCodeVar = "fromCode", toCodeVar = "toCode", outputCodeVar = "code", weightExpr = 1, combineGenders=FALSE) {
    # fromCodeWeightVar, toCodeWeightVar, intersectionWeightVar / weightVar (=intersectionArea / fromCodeArea)
    fromCodeVar = ensym(fromCodeVar)
    toCodeVar = ensym(toCodeVar)
    outputCodeVar = ensym(outputCodeVar)
    weightExpr = enexpr(weightExpr)
    
    demog = self$getDetailedDemographics()
    if (isTRUE(combineGenders)) {
      demog = demog %>% 
        dplyr::mutate(gender=NA) %>%
        dplyr::group_by(age,gender,code) %>% 
        dplyr::summarise(count = sum(count))
    }
    
    browser(expr = self$debug)
    
    #codeMap = self$codes$getTransitiveClosure() %>% dplyr::filter(toCodeType %in% codeTypes)
    codeMap = mappingDf %>% 
      dplyr::rename(tmp_fromCode = !!fromCodeVar, tmp_toCode = !!toCodeVar) %>% 
      dplyr::mutate(tmp_frac = !!weightExpr) %>% 
      dplyr::select(tmp_fromCode, tmp_toCode, tmp_frac)
    tmp = demog %>% dplyr::ungroup() %>% dplyr::select(code) %>% dplyr::distinct() %>% dplyr::left_join(codeMap, by=c("code"="tmp_fromCode"))
    tmp = tmp %>% dplyr::group_by(code) %>%  dplyr::summarise(hits = sum(ifelse(is.na(tmp_toCode),0,1)))
    
    if(any(tmp$hits > 1)) warning("There are some LSOA or DZ areas which map to more than one output area. This will result in double counting of the total population.")
    else if(any(tmp$hits == 0)) warning("There are some LSOA areas which are missing in the output areas, the output does not cover the whole population area.")
    else message("Demographic mapping: A one to one mapping between input and output exists!")
    
    out = demog %>% 
      dplyr::left_join(codeMap, by=c("code"="tmp_fromCode")) %>% 
      dplyr::select(-code) %>% 
      dplyr::rename(!!outputCodeVar := tmp_toCode) %>%
      dplyr::mutate(count = count*ifelse(is.na(tmp_frac),1,tmp_frac)) %>% 
      dplyr::group_by(age,gender,!!outputCodeVar) %>% 
      dplyr::summarise(count = sum(count)) %>%
      dplyr::ungroup()
    
    return(out)
  },
  
  # demographicWeightedMap = function(mapId, shapefile = self$geog$getMap(mapId)) {
  #   intersect = self$geog$getIntersection(
  #     inputMapId = "DEMOG", inputShape = self$getDemographicsMap(),
  #     outputMapId = mapId, outputShape = shapefile
  #   )
  #   
  #   geoFile = self$downloadAndUnzip(
  #     id = "FACEBOOK-MAP",
  #     url = "https://data.humdata.org/dataset/b9a7b4a3-75a7-4de1-b741-27d78e8d0564/resource/9007503c-5bf3-450f-8f3f-ca06682f0192/download/population_gbr_2019-07-01_geotiff.zip",
  #     pattern = "tif$"
  #   )
  #   
  #   fbRaster = raster::raster(geoFile, crs=4326)
  #   fbLowResRaster=raster::aggregate(fbRaster,5,fun=sum)
  #   
  #   # The following chunk computes the mean elevation value for each unique polygon in cont,
  #   # cont.elev <- extract(elevation, cont, fun=mean, sp=TRUE) 
  #   
  #   types <- vapply(sf::st_geometry(intersect), function(x) {class(x)[2]}, "")
  #   polys <- intersect[ grepl("*POLYGON", types), ]
  #   intersectSP = as(polys, "Spatial")
  #   
  #   intersectSP2 = raster::extract(fbLowResRaster,intersectSP, fun=sum, sp=TRUE)
  #   
  #   
  # },
  
  transitiveClosureWeightedMap = function(df, codeVar="code", codeTypeVar = "codeType", ...) {
    codeVar = ensym(codeVar)
    codeTypeVar = ensym(codeTypeVar)
    df2 = df %>% select(toCode = !!codeVar, toCodeType = !!codeTypeVar) %>% distinct()
    self$getHashCached(object = df2, operation = "WEIGHTED-MAP", orElse=function(df2) {
      df3 = self$codes$getTransitiveClosure() %>% 
        dplyr::filter(fromCodeType %in% c("LSOA","DZ","LGD")) %>% 
        dplyr::select(-toCodeType) %>% 
        dplyr::inner_join(df2, by="toCode") %>% 
        dplyr::select(fromCode,toCode,toCodeType) %>% 
        dplyr::distinct() %>%
        dplyr::group_by(fromCode,toCodeType) %>%
        dplyr::mutate(weight = 1/n()) %>%
        dplyr::ungroup()
      
      return(df3)
    }, ...)
  },
  
  findDemographics = function(df, codeVar="code", codeTypeVar="codeType", ageCatVar="ageCat", genderVar="gender", ...) {
    if ("population" %in% colnames(df)) return(df)
    codeVar = ensym(codeVar)
    codeTypeVar = ensym(codeTypeVar)
    ageCatVar = ensym(ageCatVar)
    genderVar = ensym(genderVar)
    
    # select just the demographic quantities needed from input
    df2 = df %>% dplyr::ungroup() %>%
      dplyr::select(code=!!codeVar, codeType=!!codeTypeVar, ageCat=!!ageCatVar, gender=!!genderVar)
    
    df6 = self$getHashCached(object = df2, operation = "FIND_DEMOGRAPHICS",..., orElse = function(df2,...) {
    # normalise the age range representation and convert to upper and lower bound
      df2 = df2 %>% 
        dplyr::mutate(tmp_ageCat = ageCat %>% as.character() %>% stringr::str_replace(">([0-9]+)","\\1-120") %>% stringr::str_replace("<([0-9]+)","0-\\1") %>% stringr::str_replace("([0-9]+)\\+","\\1-120")) %>%
        dplyr::mutate(tmp_ageCat = ifelse(is.na(tmp_ageCat) | stringr::str_to_lower(tmp_ageCat)=="unknown","0-120",tmp_ageCat)) %>%
        tidyr::separate(col = tmp_ageCat, into=c("tmp_ageMin","tmp_ageMax"), sep="[^0-9]+") %>% 
        dplyr::mutate(tmp_ageMin = as.integer(tmp_ageMin),tmp_ageMax = as.integer(tmp_ageMax)) %>%
        dplyr::mutate(
          tmp_ageMax = ifelse(!is.na(ageCat) & ageCat %>% stringr::str_detect("<([0-9]+)"), tmp_ageMax-1, tmp_ageMax),
          tmp_ageMin = ifelse(!is.na(ageCat) & ageCat %>% stringr::str_detect(">([0-9]+)"), tmp_ageMin+1, tmp_ageMin)
        )
      
      
      #TODO: check and fix: "Adding missing grouping variables: `source`, `type`, `statistic`"
      
      # convert age range into upper and lower bound and select only those categories represented in output
      df2 = df2 %>% 
        dplyr::select(code,codeType,gender,ageCat,tmp_ageMin,tmp_ageMax) %>% 
        dplyr::distinct()
      
      # map codes from LSOA / DZ space to output space
      df3 = self$transitiveClosureWeightedMap(df, !!codeVar, !!codeTypeVar, ...)
      
      # aggregate demographics from LSOA space to output space
      df4 = self$getDetailedDemographics() %>% 
        dplyr::inner_join(df3, by=c("code" = "fromCode")) %>%
        dplyr::group_by(age,gender,toCode, toCodeType) %>% 
        dplyr::summarise(count=sum(count*weight))
  
      # join demographics to output requirements and apply age and gender filters, then aggregate to output form.    
      df5 = df4 %>% 
        dplyr::rename(code = toCode, codeType = toCodeType, gender1 = gender) %>% 
        dplyr::inner_join(df2, by=c("code","codeType")) %>%
        dplyr::filter(age <= tmp_ageMax & age >= tmp_ageMin) %>% 
        dplyr::filter(gender1 == gender | is.na(gender) | gender=="unknown") %>% 
        dplyr::group_by(code,codeType,gender,ageCat)
      
      
      df6 = df5 %>%
        dplyr::summarise(population = sum(count))
      
      browser(expr = self$debug)
      return(df6)
    })
    
    # combine output with input
    out = df %>% left_join(df6, by=c("code","gender","ageCat","codeType"),suffix=c(".old",""))
    return(out)
  }
  
  # assemble map from LSOA/DZ to codes in output set (CodeMappingProvider and NHSCapacityProvider)
  # getSingleDigitDemographics for output set codes
  # joinByCodeAndAgeCat to NHSDatasetProvider output
  
))

