#' Get a provider of UK stats
#'
#' This function sets up a connection to the omop databse.
#' @keywords omop
#' @import dplyr
#' @export
UKStatisticsProvider = R6::R6Class("UKStatisticsProvider", inherit=PassthroughFilesystemCache, public = list(
  
  #### Fields ----
  sources = list(
    #### maps ----
    maps = list(
      WD11 = list(
        # https://geoportal.statistics.gov.uk/datasets/wards-december-2011-boundaries-ew-bgc
        url = "https://opendata.arcgis.com/datasets/bc0c7a7e865643cb90eae44bc4f15df0_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D",
        mapName = "Wards__December_2011__Boundaries_EW_BGC",
        codeCol = "wd11cd",
        nameCol = "wd11nm",
        altCodeCol = "wd11cdo",
        simplify = FALSE
      ),
      LSOA11 = list(
        # https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-boundaries-ew-bgc
        url = "https://opendata.arcgis.com/datasets/e993add3f1944437bc91ec7c76100c63_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D",
        mapName = "Lower_Layer_Super_Output_Areas__December_2011__Boundaries_EW_BGC",
        codeCol = "LSOA11CD",
        nameCol = "LSOA11NM",
        altCodeCol = NA,
        simplify = FALSE
      ),
      SGDZ11 = list(
        # 
        url = "http://sedsh127.sedsh.gov.uk/Atom_data/ScotGov/ZippedShapefiles/SG_DataZoneBdry_2011.zip",
        mapName = "SG_DataZone_Bdry_2011",
        codeCol = "DataZone",
        nameCol = "Name",
        altCodeCol = NA,
        simplify = FALSE
      ),
      SHB19 = list(
        # https://www.spatialdata.gov.scot/geonetwork/srv/api/records/f12c3826-4b4b-40e6-bf4f-77b9ed01dc14
        url = "http://sedsh127.sedsh.gov.uk/Atom_data/ScotGov/ZippedShapefiles/SG_NHS_HealthBoards_2019.zip",
        mapName = "SG_NHS_HealthBoards_2019",
        codeCol = "HBCode",
        nameCol = "HBName",
        altCodeCol = NA,
        simplify = FALSE
      ),
      LHB19 = list(
        # https://geoportal.statistics.gov.uk/datasets/local-health-boards-april-2019-boundaries-wa-buc
        url = "https://opendata.arcgis.com/datasets/def40bdc98a9457aa108eb3a5fb052b1_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D",
        mapName = "Local_Health_Boards__April_2019__Boundaries_WA_BUC",
        codeCol = "lhb19cd",
        nameCol = "lhb19nm",
        altCodeCol = NA,
        simplify = FALSE
      ),
      CTYUA19 = list(
        # https://geoportal.statistics.gov.uk/datasets/counties-and-unitary-authorities-april-2019-boundaries-ew-buc-1
        url = "https://opendata.arcgis.com/datasets/a917c123e49d436f90660ef6a9ceb5cc_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D",
        mapName = "Counties_and_Unitary_Authorities__April_2019__Boundaries_EW_BUC",
        codeCol = "ctyua19cd",
        nameCol = "ctyua19nm",
        altCodeCol = NA,
        simplify = FALSE
      ),
      LAD19 = list(
        # https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-buc
        url = "https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D",
        mapName = "Local_Authority_Districts__December_2019__Boundaries_UK_BUC",
        codeCol = "lad19cd",
        nameCol = "lad19nm",
        altCodeCol = NA,
        simplify = FALSE
      ),
      CCG20 = list(
        # https://geoportal.statistics.gov.uk/datasets/clinical-commissioning-groups-april-2020-generalised-clipped-boundaries-en
        url = "https://opendata.arcgis.com/datasets/e33a6b14379f4d0b9890f9dfa26f8a1f_1.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D",
        mapName = "Clinical_Commissioning_Groups__April_2020__EN_BGC",
        codeCol = "ccg20cd",
        nameCol = "ccg20nm",
        altCodeCol = NA,
        simplify = FALSE
      ),
      NHSER20 = list(
        # https://geoportal.statistics.gov.uk/datasets/nhs-england-regions-april-2020-boundaries-en-bgc
        url = "https://opendata.arcgis.com/datasets/87511f3ae00a40208741c685f827c1d3_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D",
        mapName = "NHS_England_Regions__April_2020__Boundaries_EN_BGC",
        codeCol = "nhser20cd",
        nameCol = "nhser20nm",
        altCodeCol = NA,
        simplify = FALSE
      ),
      PHEC16 = list(
        # https://geoportal.statistics.gov.uk/datasets/public-health-england-centres-december-2016-generalised-clipped-boundaries-in-england
        url = "https://opendata.arcgis.com/datasets/91d15139a82e47fc8167a11c0e4e86de_2.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D",
        mapName = "Public_Health_England_Centres__December_2016__Boundaries",
        codeCol = "phec16cd",
        nameCol = "phec16nm",
        altCodeCol = NA,
        simplify = FALSE
      ),
      CTRY19 = list(
        # https://geoportal.statistics.gov.uk/datasets/countries-december-2019-boundaries-uk-bgc/data
        url = "https://opendata.arcgis.com/datasets/b789ba2f70fe45eb92402cee87092730_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D",
        mapName = "Countries__December_2019__Boundaries_UK_BGC",
        codeCol = "ctry19cd",
        nameCol = "ctry19nm",
        altCodeCol = NA,
        simplify = FALSE
      ),
      LGD12 = list(
        # https://geoportal.statistics.gov.uk/datasets/countries-december-2019-boundaries-uk-bgc/data
        url = "http://osni-spatialni.opendata.arcgis.com/datasets/eaa08860c50045deb8c4fdc7fa3dac87_2.zip",
        mapName = "OSNI_Open_Data_-_Largescale_Boundaries_-_Local_Government_Districts__2012_",
        codeCol = "LGDCode",
        nameCol = "LGDNAME",
        altCodeCol = NA,
        simplify = FALSE
      )
    ),
    #### code mapping ----
    mapping = list(
      PHEC19CDH_to_PHEC19CD = list(
        url="https://opendata.arcgis.com/datasets/e1ab849323534ea7be1920a2598a4c30_0.csv", 
        fromCode="PHEC19CDH", 
        toCode="PHEC19CD", 
        rel="synonym"
      ),
      CCG20CDH_to_CCG20CD = list(
        url="https://opendata.arcgis.com/datasets/bfb87228cf9e4c44bad0cffa353d0fc8_0.csv",
        fromCode="CCG20CDH",
        toCode="CCG20CD",
        rel="synonym"
      ),
      NHSER19CDH_to_NHSER19CD = list(
        url = "https://opendata.arcgis.com/datasets/a84ae875f03c4553b49cdec08eb8e13c_0.csv",
        fromCode="NHSER19CD",
        toCode="NHSER19CDH", 
        rel="synonym"
      )
    ),
    #### tweaks ----
    tweak = list(
      DEMOG = list(
        # anglesea 1 & 3, thames river crossing 3
        add = tibble::tibble(from = c("W01000015","W01000011","E01016012"), to=c("W01000103","W01000072","E01024172"))
        #TODO: islands in scotland
        #remove = tibble()
      )
    )
    #### end of list ----
  ),
  
  #### Methods ----
  #' @description get a map as an sf object
  #' @param codeType the map you want
  getMap = function(mapId) {
    if (!(mapId %in% names(self$sources$maps))) stop("Unknown code type: ",mapId)
    self$getSaved(mapId, orElse = function(loader) {
      wardsZip = paste0(self$wd,"/",mapId,".zip")
      unzipDir = paste0(self$wd,"/",mapId)
      if(!file.exists(wardsZip)) {
        download.file(loader$url,wardsZip)
        wd = getwd()
        if (!dir.exists(unzipDir)) dir.create(unzipDir)
        setwd(unzipDir)
        unzip(wardsZip, exdir=unzipDir, junkpaths = TRUE)
        setwd(wd)
      }
      map = sf::st_read(paste0(unzipDir,"/",loader$mapName,".shp")) %>% sf::st_transform(crs=4326)# %>% nngeo::st_remove_holes()
      if(loader$simplify) map = suppressWarnings(map %>% sf::st_simplify(dTolerance=0.001))
      map = self$standardiseMap(map, loader$codeCol, loader$nameCol, loader$altCodeCol, mapId)
      return(map %>% group_by(code,name))
    }, loader = self$sources$maps[[mapId]])
  },
  
  #' @description LSOA & Scottish Data Zones
  getDemographicsMap = function() {
    tmp = self$getDetailedDemographics()
    tmp = tmp %>% group_by(code) %>% summarise(count = sum(count))
    self$getSaved("DEMOG",orElse = function() {return(
      rbind(
        self$getMap("LSOA11"),
        self$getMap("SGDZ11")
      ) %>% group_by(code,name))}) %>% left_join(tmp, by="code")
  },
  
  #' @description England LADs, Scotland Health Board, Wales Health Board
  getPHEDashboardMap = function() {
    self$getSaved("DASH_LTLA",orElse = function() {return(
      rbind(
        self$getMap("LAD19") %>% filter(code %>% stringr::str_starts("E")),
        self$getMap("SHB19"), 
        self$getMap("LHB19")
      ) %>% group_by(code,name))
  })},
  
  
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
          select(-`All Ages`) %>%
          tidyr::pivot_longer(cols=all_of(ageCols),names_to = "age",values_to = "count") #, names_ptypes = list(age=integer()))
        # browser()
        tmp = tmp %>% rename(code = `Area Codes`, name=`Area Names`) %>% #, total=`All Ages`) %>%
          mutate(age = as.integer(stringr::str_remove(age,"\\+")), codeType="LSOA11")
        return(tmp)
      }

      demogByLSOA_M <- read_excel(UKdemog, sheet="Mid-2018 Males", skip = 4) %>% convert() %>% mutate(gender = "M") 
      demogByLSOA_F <- read_excel(UKdemog, sheet="Mid-2018 Females", skip = 4) %>% convert() %>% mutate(gender = "F") 
      
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
        demogBySGDZ = demogBySGDZ %>% select(-...4,-...5)
        ageCols = colnames(demogBySGDZ)[!is.na(as.integer(stringr::str_remove(colnames(demogBySGDZ),"\\.+")))]
        demogBySGDZ = demogBySGDZ %>% pivot_longer(cols=all_of(ageCols),names_to="age",values_to="count")
        demogBySGDZ = demogBySGDZ %>% mutate(age = as.integer(stringr::str_remove(age,"\\.+"))-6, codeType="SGDZ11") %>% rename(code = DataZone2011Code, name=DataZone2011Name) %>% select(-CouncilArea2018Name)
        return(demogBySGDZ)
      }
      
      demogBySGDZ_M =  read_excel(scotDemogM, sheet = "Table 1b Males (2018)", range = "A6:CR6982") %>% convert2() %>% mutate(gender = "M") 
      demogBySGDZ_F =  read_excel(scotDemogF, sheet = "Table 1c Females (2018)", range = "A6:CR6982") %>% convert2() %>% mutate(gender = "F") 
      
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
  #' @param outputShape = the sf object containing the shapefile of the desired output which must be grouped by desired output
  #' @param outputShape = the sf object containing the shapefile of the desired output which must be grouped by desired output
  #' @param ageBreaks = where to cut age groups? e.g. c(15,65,80) (max 90)
  #' @param combineGenders = merge the genders
  getDemographics = function(mapId, outputShape = self$getMap(mapId) %>% group_by(code,name), outputVars = outputShape %>% groups(), ageBreaks = seq(5,90,5), combineGenders=FALSE) {
    
    ageLabels = c(
      paste0("<",ageBreaks[1]),
      paste0(ageBreaks[1:(length(ageBreaks)-1)],"-",ageBreaks[2:(length(ageBreaks))]-1),
      paste0(ageBreaks[length(ageBreaks)],"+"))
    ageBreaks2 = c(-Inf,ageBreaks,Inf)
    demog = self$getDetailedDemographics() %>% 
      mutate(ageCat = cut(age,breaks = ageBreaks2,labels=ageLabels,ordered_result = TRUE,right=FALSE,include.lowest = TRUE)) %>%
      select(-age) 
    if (isTRUE(combineGenders)) {
      demog = demog %>% group_by(ageCat,code) %>% summarise(count = sum(count))
    } else {
      demog = demog %>% group_by(ageCat,gender,code) %>% summarise(count = sum(count))
    }
    
    
    mapping = self$interpolateByArea(
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
  
  #' @description get the intersection between to maps with ids. Caches the result in the working directory.
  getIntersection = function( inputMapId, inputShape = self$getMap(inputMapId), outputMapId,  outputShape = self$getMap(outputMapId)) {
    return(self$getSaved(
      paste0("INTERSECT_",inputMapId,"_",outputMapId),
      orElse = function() {
        # browser()
        message("calculating intersection ....")
        inputShape = inputShape %>% sf::st_cast(to="POLYGON")
        inputShape = inputShape %>% rename_all(function(x) paste0(x,".src"))
        outputShape = outputShape %>% sf::st_cast(to="POLYGON")
        tmp = inputShape %>% sf::st_intersection(inputShape)
        message("calculating intersection areas....")
        tmp$intersectionArea = tmp %>% sf::st_area() %>% as.numeric()
        return(tmp)
      }))
  },
  
  #' interpolate a variable from one set of shapes to another
  #' 
  #' @param inputDf - a grouped dataframe containing the statistic to be interpolated  
  #' @param interpolateVar - the statistic, 
  #' @param inputShape - an input map, 
  #' @param inputIdVar - an id shared between the grouped data fram and the input map, 
  #' @param outputShape - an output map which must be grouped by the desired output, 
  #' @import dplyr
  #' @return a dataframe containing the grouping columns, the outputIdVar and the interpolated value of interpolateVar
  #' @export
  interpolateByArea = function(inputDf, 
      inputMapId, inputShape = self$getMap(inputMapId), inputIdVar = "code", 
      interpolateVar, 
      outputMapId, outputShape = self$getMap(outputMapId) %>% group_by(codeType,code,name), outputVars = outputShape %>% groups(),
      aggregateFn = sum
    ) {
    
    inputIdVar = ensym(inputIdVar)
    interpolateVar = ensym(interpolateVar)
    grps = inputDf %>% groups()
    grps = grps[sapply(grps,as_label) != as_label(inputIdVar) & sapply(grps,as_label) != as_label(interpolateVar)]
    inputDf = inputDf %>% ungroup()# %>% group_by(!!!grps)
    if(!(as_label(inputIdVar) %in% colnames(inputDf))) 
      stop("Join id column missing from inputDf: ",as_label(inputIdVar))
    if(!(as_label(inputIdVar) %in% colnames(inputShape))) 
      stop("Join id column missing from inputShape: ",as_label(inputIdVar))
    if(identical(outputVars,NULL)) 
      stop("Output shape must be grouped by something that uniquely identifies desired output, or outputVars must be specified") 
    
    intersection = self$getIntersection(
      inputMapId,
      inputShape,
      outputMapId,
      outputShape)
    
    inputIdVar2 = sym(paste0(as_label(inputIdVar),".src"))
    
    intersection = intersection %>% tibble::as_tibble() %>% mutate(
      fracInput = intersectionArea/area.src
    ) 
    
    mapping = intersection %>% tibble::as_tibble() %>% select(!!inputIdVar2, fracInput, !!!outputVars)
    tmpInput = inputDf %>% rename(!!inputIdVar2 := !!inputIdVar)
    mapping = suppressWarnings(mapping %>% inner_join(tmpInput, by=as_label(inputIdVar2)))
    mapping = mapping %>% mutate(intersectionValue = !!interpolateVar * fracInput)
    mapping = mapping %>% group_by(!!!grps, !!!outputVars) %>% group_modify(function(d,g,...) {
      return(
        tibble::tibble(agg = do.call(aggregateFn, list(x=d$intersectionValue))) %>% rename(!!interpolateVar := agg)
      )
    })
    
    return(mapping)
  },
  
  #' @description create a neighbourhood network from a shapefile
  #' @param mapId - a the ID of the map
  #' @param shape - a sf object, if not present will be loaded from cache
  #' @param idVar - the varable containing the coded identifier of the map
  #' @import dplyr
  #' @return an edge list of ids with from and to columns
  #' @export
  createNeighbourNetwork = function(mapId, shape = self$getMap(mapId) %>% group_by(code,name), idVar="code") {
    idVar = ensym(idVar)
    self$getSaved(paste0("NN_",mapId), orElse=function() {
      shape = shape %>% mutate(tmp_id = row_number())
      graph = shape %>% sf::st_intersects()
      edges = tibble::tibble(
        from_tmp_id = rep(1:length(graph),sapply( graph, length)),
        to_tmp_id = unlist(graph %>% purrr::flatten())
      )
      edges = edges %>% 
        left_join(shape %>% tibble::as_tibble() %>% select(from_tmp_id = tmp_id, from = !!idVar), by="from_tmp_id") %>%
        left_join(shape %>% tibble::as_tibble() %>% select(to_tmp_id = tmp_id, to = !!idVar), by="to_tmp_id") %>%
        filter(from != to) %>%
        select(-from_tmp_id, -to_tmp_id)
      return(edges)
    })
  },
  
  # TODO: index of multiple deprivation
  # but not a standard across the 4 nations.
  # https://en.wikipedia.org/wiki/Multiple_deprivation_index#List_of_UK_deprivation_indexes
  # getIMD = function() {
  #   self$getSaved("IMD", function() {
  #     imdFile = paste0(self$wd,"/IMDbyLSOA.csv")
  #     if(!file.exists(imdFile)) download.file(url="https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv",destfile = imdFile)
  #     tmp = readr::read_csv(imdFile)
  #     imdFile2 = paste0(self$wd,"/IMDbySGDZ.xls")
  #     if(!file.exists(imdFile)) download.file(url="https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2020/01/scottish-index-of-multiple-deprivation-2020-data-zone-look-up-file/documents/scottish-index-of-multiple-deprivation-data-zone-look-up/scottish-index-of-multiple-deprivation-data-zone-look-up/govscot%3Adocument/SIMD%2B2020v2%2B-%2Bdatazone%2Blookup.xlsx", imdFile2)
  #     tmp2 = readxl::read_excel(imdFile2, sheet = "SIMD 2020v2 DZ lookup data")
  #     browser()
  #   })
  # },
  
  #' @description standardise all maps to a minimal set of attributes with consistent naming
  standardiseMap = function(sf, codeCol, nameCol, altCodeCol, codeType) {
    sf = sf %>% rename(code = !!codeCol, name = nameCol) %>% mutate(code = as.character(code), name=as.character(name))
    if(!is.na(altCodeCol)) {
      sf = sf %>% rename(altCode = !!altCodeCol) %>% mutate(as.character(altCode))
    } else {
      sf = sf %>% mutate(altCode = as.character(NA))
    }
    sf = sf %>% mutate(codeType = codeType) %>% select(codeType, code, name, altCode)
    sf$area = sf %>% sf::st_area() %>% as.numeric()
    return(sf)
  },
  
  #' @description save a shapefile to disk in the current working directory
  #' @param mapId - a mapId - will become the zip filename
  #' @param  - a zip directory
  #' @return a dataframe containing the grouping columns, the outputIdVar and the interpolated value of interpolateVar
  #' @export
  saveShapefile = function(mapId, shape = self$getMap(mapId), overwrite=FALSE) {
    zipDir = paste0(getwd(),"/",mapId)
    if (!dir.exists(zipDir)) dir.create(zipDir)
    suppressWarnings(sf::st_write(shape, paste0(zipDir,"/",mapId, ".shp"), driver="ESRI Shapefile"))
    zip(zipfile = paste0(zipDir,".zip"),files=mapId)
    unlink(zipDir, recursive=TRUE)
  },
  
  #' @description warm up caches
  loadAllMaps = function() {
    self$getMap("WD11")
    self$getMap("LSOA11")
    self$getMap("SGDZ11")
    self$getMap("SHB19")
    self$getMap("LHB19")
    self$getMap("CTYUA19")
    self$getMap("LAD19")
    self$getMap("CCG20")
    self$getMap("NHSER20")
    self$getMap("PHEC16")
    self$getMap("CTRY19")
    self$getMap("LGD12")
  },
  
  getHospitals = function(...) {
    self$getSaved("HOSPITALS",orElse = function() {
      gbHospitals <- read_csv(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vQj6X8rIlBlsD5bK-PMcBT9wjAWh60dTTJLfuczqsiKnYzYiN_4KjYAh4HWWkf4v1RH6ih7C78FhdiN/pub?gid=128715098&single=true&output=csv","&nocache=",sample(1:1000000,1)))
      hospSf = sf::st_as_sf(gbHospitals %>% rename(code = hospitalId) %>% select(-nation), coords=c("long","lat"), crs=4326)
    }) %>% filter(...)
  },
  
  createCatchment = function(
      supplyId, supplyShape, supplyIdVar = "code", supplyVar, supplyOutputVars = supplyShape %>% groups(),
      demandId = "DEMOG", demandShape = self$getDemographicsMap(), demandVar = "count", demandIdVar = "code", 
      growthRates = function(capacityPerDemand, multiplier = 1.1) {return( rank(capacityPerDemand) / length(capacityPerDemand) * multiplier )},
      distanceModifier = function(distanceToSupply) {return(2/(1+distanceToSupply/min(0.1,mean(distanceToSupply))))},
      tweakNetwork = self$sources$tweak$DEMOG
    ) {
    
    self$getSaved(id = paste0(supplyId,"_CATCHMENT_",demandId), orElse = function() {
    
      supplyIdVar = ensym(supplyIdVar)
      supplyVar = ensym(supplyVar)
      demandIdVar = ensym(demandIdVar)
      demandVar = ensym(demandVar)
      
      demandShape = demandShape %>% ungroup() %>% rename(demandCode = !!demandIdVar, demand = !!demandVar) %>% mutate(tmp_demand_id = row_number())
      # preserve the features we want to output
      supplyFeatures = supplyShape %>% ungroup() %>% tibble::as_tibble() %>% select(!!supplyIdVar, !!!supplyOutputVars, !!supplyVar) %>% distinct()
      supplyPoints = supplyShape %>% ungroup()
      
      supplyShape = supplyShape %>% ungroup() %>% rename(supplyCode = !!supplyIdVar, supply = !!supplyVar) %>% mutate(tmp_supply_id = row_number()) 
      
      
      # create an edgelist of supplyIdVar, demandIdVar
      # containment is a list of lists
      containment = demandShape %>% sf::st_contains(supplyShape)
      supplyMapping = tibble::tibble(
        tmp_demand_id = rep(1:length(containment),sapply(containment, length)),
        tmp_supply_id = unlist(containment %>% purrr::flatten()) 
      ) %>% distinct()
      resourceProvider = supplyMapping %>%
        left_join(demandShape %>% tibble::as_tibble() %>% select(tmp_demand_id, demandCode), by="tmp_demand_id") %>%
        left_join(supplyShape %>% tibble::as_tibble() %>% select(tmp_supply_id, supplyCode, supply), by="tmp_supply_id") %>%
        select(-tmp_demand_id, -tmp_supply_id)
      # if there are multiple providers in one area we merge them (creating a new supplier id).
      mergedSuppliers = NULL
      if(any(resourceProvider %>% group_by(demandCode) %>% count() %>% pull(n)>1)) {
        warning("More than one supplier was found in a single region. These the first value will be picked, and the total capacity combined, but as a result the catchment map will be missing some values from the supplier list.")
        mergedSuppliers = resourceProvider %>% group_by(demandCode) %>% filter(n()>1)
      }
      resourceProvider = resourceProvider %>% group_by(demandCode) %>% summarise(supplyCode := first(supplyCode), supply=sum(supply))%>% rename(areaId = demandCode)
      
      areaNetwork = self$createNeighbourNetwork(demandId, demandShape, demandCode) 
      if(exists("add",where = tweakNetwork)) areaNetwork = areaNetwork %>% union(tweakNetwork$add) %>% union(tweakNetwork$add %>% rename(tmp = from) %>% rename(from=to,to=tmp)) 
      if(exists("remove",where = tweakNetwork)) areaNetwork = areaNetwork %>% setdiff(tweakNetwork$remove) %>% setdiff(tweakNetwork$remove %>% rename(tmp = from) %>% rename(from=to,to=tmp)) 
      areaNetwork = areaNetwork %>% rename(fromAreaId = from, toAreaId = to)
      
      entireArea = demandShape %>% ungroup() %>% select(areaId = demandCode, areaSize = area, areaDemand = demand) %>% 
          semi_join(
            areaNetwork %>% select(areaId=toAreaId) %>% union(resourceProvider %>% select(areaId)), 
            by="areaId") %>% tibble::as_tibble() 
        
      # ensure entire area is connected - otherwise we get problems with islands
      
      areasWithResource = entireArea %>% 
        inner_join(resourceProvider, by="areaId") %>% 
        rename(supplierId = supplyCode, supplyCapacity = supply) %>% tibble::as_tibble()
        
      suppliedArea = areasWithResource %>% mutate(
        distanceToSupply = 0,
        demandDistanceToSupply = 0,
        accumulatedGrowth = 0,
        iteration = 0
      )
        
      remaining = NULL
      # loop?
      repeat {
        suppliedArea = suppliedArea %>% group_by(supplierId) %>% mutate(
          totalDemand = sum(areaDemand),
          capacityPerDemand = supplyCapacity/totalDemand
        ) %>% ungroup() 
        
        unSuppliedArea = entireArea %>% anti_join(suppliedArea, by="areaId")
        message("areas remaining: ",nrow(unSuppliedArea))
        
        if(nrow(unSuppliedArea) == 0) break
        remaining = c(remaining,nrow(unSuppliedArea))
        if (sum(remaining == nrow(unSuppliedArea)) > 4) {
          warning("terminating early with missing areas - it looks like ",nrow(unSuppliedArea), " areas are not connected")
          break;
        }
        # stop if unsupplied area is empty
        
        # growing areas are - A) adjoin unsupplied areas B) have accumulated enough to grow
        unSuppliedNeighbourAreas = areaNetwork %>% semi_join(suppliedArea, by=c("fromAreaId"="areaId")) %>% inner_join(unSuppliedArea, by=c("toAreaId"="areaId")) %>% rename(areaId = toAreaId)
        intoUnsupplied = unSuppliedNeighbourAreas %>% group_by(fromAreaId) %>% summarise(newAreas = n(), newAreaDemand = sum(areaDemand))
        
        # TODO: this definintion only allows growth into areas that are not already supplied
        growingArea = suppliedArea %>% rename(fromAreaId = areaId) %>% 
          inner_join(intoUnsupplied, by=c("fromAreaId"), suffix=c(".old","")) %>% 
          mutate( 
            newTotalDemand = totalDemand+newAreaDemand,
            newCapacityPerDemand = supplyCapacity/newTotalDemand
          ) %>%
          mutate(
            accumulatedGrowth = accumulatedGrowth + growthRates(newCapacityPerDemand) #*distanceModifier(distanceToSupply) #TODO: make this work
          )
        # browser()
        suppliedArea = suppliedArea %>% left_join(
          growingArea %>% select(areaId = fromAreaId, newAccumulated = accumulatedGrowth), by="areaId") %>% 
          mutate(accumulatedGrowth = ifelse(!is.na(newAccumulated), newAccumulated, accumulatedGrowth)) %>%
          select(-newAccumulated)
        
        
        newlySuppliedArea = growingArea %>% filter(accumulatedGrowth > 1) %>% inner_join(unSuppliedNeighbourAreas, by="fromAreaId", suffix=c(".from",""))
        newlySuppliedArea = newlySuppliedArea %>%
          mutate(
            distanceToSupply = distanceToSupply + sqrt(areaSize.from),
            demandDistanceToSupply = demandDistanceToSupply + areaDemand.from,
            accumulatedGrowth = accumulatedGrowth - 1,
            iteration = iteration + 1
          ) %>%
          select(-ends_with(".from"), -fromAreaId)
        
        # ensure only one supplier gets the target area
        newlySuppliedArea = newlySuppliedArea %>% group_by(areaId) %>% arrange(desc(capacityPerDemand),desc(distanceToSupply)) %>% filter(row_number() <=1) %>% ensurer::ensure_that(length(unique(.$areaId)) == length(.$areaId))
        
        #browser()
        
        suppliedArea = suppressWarnings(bind_rows(suppliedArea, newlySuppliedArea)) %>% ensurer::ensure_that(length(unique(.$areaId)) == length(.$areaId))
        suppliedArea = suppliedArea %>% select(areaId,areaSize,areaDemand,supplierId, supplyCapacity, distanceToSupply, demandDistanceToSupply, iteration, accumulatedGrowth, geometry)
      }
      suppliedArea = suppliedArea %>% select(-accumulatedGrowth) %>% sf::st_as_sf()
      
      # reassemble features preserved from original supply dataframe
      message("assembling catchment area map...")
      #browser()
      
      suppliedMap = suppliedArea %>% sf::st_buffer(0) %>% group_by(supplierId) %>% summarise(
        area=sum(areaSize),
        !!demandVar:=sum(areaDemand),
        !!supplyVar:=first(supplyCapacity)
      ) %>% rename(!!supplyIdVar := supplierId) %>% nngeo::st_remove_holes()
      suppliedMap = supplyFeatures %>% inner_join(suppliedMap %>% tibble::as_tibble(), by=as_label(supplyIdVar), suffix=c(".original",""))%>% sf::st_as_sf(crs=4326)
      
      # mapping of demandIdVar to supplyIdVar - this will miss any merged ids.
      crossMapping = suppliedArea %>% tibble::as_tibble() %>% select(!!demandIdVar := areaId, !!supplyIdVar := supplierId)
      # crossMapping = crossMapping %>% separate_rows(originalId, sep = "\\|")
      
      
      
      out = list(
        map = suppliedMap,
        suppliers = supplyPoints,
        crossMapping = crossMapping,
        mergedSuppliers = mergedSuppliers,
        suppliedArea = suppliedArea,
        notSuppliedArea = unSuppliedArea %>% sf::st_as_sf()
      )
      return(out)
    })
  },
  
  preview = function(mapId = NA, shape=self$getMap(mapId), nameVar = "name", codeVar = "code", poi=NULL, poiNameVar = "name", poiCodeVar = "code") {
    nameVar = ensym(nameVar)
    codeVar = ensym(codeVar)
    poiNameVar = ensym(poiNameVar)
    poiCodeVar = ensym(poiCodeVar)
    tmp = shape %>% rename(name = !!nameVar, code = !!codeVar)
    leaf = leaflet::leaflet(tmp) %>% 
      leaflet::addTiles() %>% 
      leaflet::addPolygons(label = as.character(tmp$name), popup = as.character(tmp$code))
    if(!identical(poi,NULL)) {
      poi = poi %>%  rename(name = !!poiNameVar, code = !!poiCodeVar)
      leaf = leaf %>% leaflet::addCircleMarkers(data=poi, color="#FF0000", label= as.character(poi$name), popup = as.character(poi$code))
    }
    return(leaf)
  }
  
))

library(tidyverse)
library(sf)
usp = UKStatisticsProvider$new("~/Data/maps")
usp$loadAllMaps()
# dm = usp$getPHEDashboardMap()
# #usp$saveShapefile("DASH_LTLA", dm)
# tmp = usp$getDemographics("DASH_LTLA", dm %>% group_by(code,name), combineGenders = FALSE)

# sf = usp$getDemographicsMap()
# d = usp$getDetailedDemographics()
# x = usp$getDemographics("CTRY19")
# sf = usp$getDemographics("WD11")
# # sf = usp$getDemographics("LSOA11")
# # sf = usp$getDemographics("SGDZ11")
# sf = usp$getDemographics("SHB19")
# usp$preview("LHB19")
# sf = usp$getDemographics("CTYUA19")
# sf = usp$getDemographics("LAD19")
# sf = usp$getDemographics("CCG20")
# sf = usp$getDemographics("NHSER20")
# sf = usp$getDemographics("PHEC16")
# sf = usp$getDemographics("LGD12")
# plot(sf)

tmp = usp$getHospitals(icuBeds>0 & sector=="NHS Sector")
catch = usp$createCatchment(
  supplyId = "ICUBEDS", supplyShape = tmp %>% rename(hospId = code, hospName = name) %>% group_by(hospId,hospName), supplyIdVar = hospId, supplyVar = icuBeds,
  demandId = "DEMOG", demandShape = usp$getDemographicsMap(), demandVar = count, demandIdVar = code
)

usp$preview(shape=catch$map %>% mutate(per100K = 100000*icuBeds/count),nameVar = hospName, codeVar = per100K, poi=catch$suppliers, poiNameVar = hospName, poiCodeVar = icuBeds)
