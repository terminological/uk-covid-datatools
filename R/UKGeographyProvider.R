#' UK Geography
#' @export
UKGeographyProvider = R6::R6Class("UKGeographyProvider", inherit=DataProvider, public = list(
  
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
      WD19 = list(
        # https://geoportal.statistics.gov.uk/datasets/wards-december-2019-boundaries-ew-bgc
        url = "https://opendata.arcgis.com/datasets/bf1a23cfe83f4da9844e7f34e4824d03_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D",
        mapName = "Wards__December_2019__Boundaries_EW_BGC",
        codeCol = "wd19cd",
        nameCol = "wd19nm",
        altCodeCol = NA,
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
      MSOA11 = list(
        # https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-boundaries-ew-bgc
        url = "https://opendata.arcgis.com/datasets/5d4e4cc075ef4a40acbe6e50735451ef_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D",
        mapName = "Middle_Layer_Super_Output_Areas__December_2011__EW_BGC_V2",
        codeCol = "MSOA11CD",
        nameCol = "MSOA11NM",
        altCodeCol = NA,
        simplify = FALSE
      ),
      DZ11 = list(
        # https://data.gov.uk/dataset/ab9f1f20-3b7f-4efa-9bd2-239acf63b540/data-zone-boundaries-2011
        url = "http://sedsh127.sedsh.gov.uk/Atom_data/ScotGov/ZippedShapefiles/SG_DataZoneBdry_2011.zip",
        mapName = "SG_DataZone_Bdry_2011",
        codeCol = "DataZone",
        nameCol = "Name",
        altCodeCol = NA,
        simplify = FALSE
      ),
      HB19 = list(
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
      LAD20 = list(
        # https://geoportal.statistics.gov.uk/datasets/local-authority-districts-may-2020-boundaries-uk-buc
        url = "https://opendata.arcgis.com/datasets/910f48f3c4b3400aa9eb0af9f8989bbe_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D",
        mapName = "Local_Authority_Districts__May_2020__UK_BUC",
        codeCol = "LAD20CD",
        nameCol = "LAD20NM",
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
        # https://data.gov.uk/dataset/05f72866-b72b-476a-b6f3-57bd4a768674/osni-open-data-largescale-boundaries-local-government-districts-2012
        url = "http://osni-spatialni.opendata.arcgis.com/datasets/eaa08860c50045deb8c4fdc7fa3dac87_2.zip",
        mapName = "OSNI_Open_Data_-_Largescale_Boundaries_-_Local_Government_Districts__2012_",
        codeCol = "LGDCode",
        nameCol = "LGDNAME",
        altCodeCol = NA,
        simplify = FALSE
      ),
      OUTCODE = list(
        # https://www.opendoorlogistics.com/downloads/
        url = "https://www.opendoorlogistics.com/wp-content/uploads/Data/UK-postcode-boundaries-Jan-2015.zip",
        mapName = "Districts", # Areas or Sectors
        codeCol = "name",
        nameCol = NA,
        altCodeCol = NA,
        simplify = FALSE
      ),
      ISO3166_2 = list(
        # https://gadm.org/download_country_v3.html
        url="https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_GBR_shp.zip",
        mapName="gadm36_GBR_2",
        codeCol = "GID_2",
        nameCol = "NAME_2",
        altCodeCol = "HASC_2",
        simplify = FALSE
      ),
      ISO3166_3 = list(
        # https://gadm.org/download_country_v3.html
        url="https://biogeo.ucdavis.edu/data/gadm3.6/shp/gadm36_GBR_shp.zip",
        mapName="gadm36_GBR_3",
        codeCol = "GID_3",
        nameCol = "NAME_3",
        altCodeCol = "HASC_3",
        simplify = FALSE
      )
    ),
    #### tweaks ----
    tweak = list(
      DEMOG = list(
        # anglesea 1 & 2, thames river crossing 3
        add = tibble::tibble(from = c("W01000015","W01000011","E01016012"), to=c("W01000103","W01000072","E01024172"))
        #TODO: islands in scotland
        #remove = tibble()
      )
    )
    #### end of list ----
  ),
  
  initialize = function(providerController, ...) {
    super$initialize(providerController, ...)
  },
  
  getMapList = function() {
    return(names(self$sources$maps))
  },
  
  #### Methods ----
  #' @description get a map as an sf object
  #' @param codeType the map you want
  getMap = function(mapId,...) {
    if (!(mapId %in% names(self$sources$maps))) stop("Unknown code type: ",mapId)
    self$getSaved(mapId, ..., orElse = function(loader,...) {
      # wardsZip = paste0(self$wd,"/",mapId,".zip")
      # unzipDir = paste0(self$wd,"/",mapId)
      # if(!file.exists(wardsZip)) {
      #   download.file(loader$url,wardsZip)
      #   wd = getwd()
      #   if (!dir.exists(unzipDir)) dir.create(unzipDir)
      #   setwd(unzipDir)
      #   unzip(wardsZip, exdir=unzipDir, junkpaths = TRUE)
      #   setwd(wd)
      # }
      loader = self$sources$maps[[mapId]]
      pattern = paste0(ifelse(is.na(loader$mapName),"",loader$mapName),"\\.shp$")
      mapFile = self$downloadAndUnzip(id=mapId,url=loader$url,pattern=pattern)
      map = sf::st_read(mapFile) %>% sf::st_transform(crs=4326)# %>% nngeo::st_remove_holes()
      browser(expr=self$debug)
      if(loader$simplify) map = suppressWarnings(map %>% sf::st_simplify(dTolerance=0.001))
      map = self$standardiseMap(map, !!loader$codeCol, !!loader$nameCol, !!loader$altCodeCol, mapId)
      return(map %>% ungroup() %>% sf::st_as_sf()) #dplyr::group_by(code,name))
    })
  },
  
  #' @description England LADs, Scotland Health Board, Wales Health Board
  getPHEDashboardMap = function(...) {
    self$getSaved("DASH_LTLA",...,orElse = function(...) {return(
      rbind(
        self$getMap("LAD19") %>% dplyr::filter(code %>% stringr::str_starts("E")),
        self$getMap("SHB19"), 
        self$getMap("LHB19")
      ) %>% dplyr::ungroup()) #group_by(code,name))
  })},
  
  
  getIntersection = function( inputMapId, inputShape = self$getMap(inputMapId), outputMapId,  outputShape = self$getMap(outputMapId),...) {
    return(self$getSaved(
      paste0("INTERSECT_",inputMapId,"_",outputMapId),
      ...,
      orElse = function(...) {
        # browser()
        message("calculating intersection ....")
        inputShape = inputShape %>% sf::st_cast(to="POLYGON")
        inputShape = inputShape %>% dplyr::rename_all(function(x) paste0(x,".src"))
        outputShape = outputShape %>% sf::st_cast(to="POLYGON")
        tmp = inputShape %>% sf::st_intersection(outputShape)
        message("calculating intersection areas....")
        tmp$intersectionArea = tmp %>% sf::st_area() %>% as.numeric()
        return(tmp)
      }))
  },
 
  getContainedIn = function( inputSf,  outputShape = self$getMap(outputMapId), outputMapId=NA,  inputIdVar = "code", outputIdVar = "code") {
    inputIdVar = ensym(inputIdVar)
    outputIdVar = ensym(outputIdVar)
    #browser()
    outputShape = outputShape %>% dplyr::mutate(tmp_output_id = row_number())
    inputSf = inputSf %>% dplyr::mutate(tmp_input_id = row_number())
    containment = outputShape %>% sf::st_contains(inputSf)
    mapping = tibble::tibble(
        tmp_output_id = rep(1:length(containment),sapply(containment, length)),
        tmp_input_id = unlist(containment %>% purrr::flatten()) 
      ) %>% 
      dplyr::distinct() %>% 
      dplyr::left_join(inputSf %>% tibble::as_tibble() %>% select(tmp_input_id, from = !!inputIdVar), by="tmp_input_id") %>%
      dplyr::left_join(outputShape %>% tibble::as_tibble() %>% select(tmp_output_id, to = !!outputIdVar), by="tmp_output_id") %>%
      dplyr::select(-tmp_input_id,-tmp_output_id)
    return(mapping)
  },
  
  #' #' description get the intersection between to maps with ids. Caches the result in the working directory.
  #' unionByGroup = function(groupedSf, ...) {
  #'   grps = groupedSf %>% groups()
  #'   if (length(grps)==0) stop("Must be grouped")
  #'   catchmentMap = groupedSf %>% group_modify(function(d,g,...) {
  #'     d %>% summarise(...) %>% mutate(geometry=d %>% sf::st_union())
  #'   }) %>% sf::st_as_sf(crs=4326)
  #'   return(catchmentMap)
  #' },
  
  #' @description interpolate a variable from one set of shapes to another
  #' @param inputDf - a grouped dataframe containing the statistic to be interpolated  
  #' @param interpolateVar - the statistic, 
  #' @param inputShape - an input map, 
  #' @param inputIdVar - an id shared between the grouped data fram and the input map, 
  #' @param outputShape - an output map which must be grouped by the desired output, 
  #' @return a dataframe containing the grouping columns, the outputIdVar and the interpolated value of interpolateVar
  interpolateByArea = function(inputDf, 
      inputMapId, inputShape = self$getMap(inputMapId), inputIdVar = "code", 
      interpolateVar, 
      outputMapId, outputShape = self$getMap(outputMapId) %>% dplyr::group_by(codeType,code,name), outputVars = outputShape %>% dplyr::groups(),
      aggregateFn = sum
    ) {
    
    inputIdVar = ensym(inputIdVar)
    interpolateVar = ensym(interpolateVar)
    grps = inputDf %>% dplyr::groups()
    grps = grps[sapply(grps,as_label) != as_label(inputIdVar) & sapply(grps,as_label) != as_label(interpolateVar)]
    inputDf = inputDf %>% dplyr::ungroup()# %>% dplyr::group_by(!!!grps)
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
    
    intersection = intersection %>% tibble::as_tibble() %>% dplyr::mutate(
      fracInput = intersectionArea/area.src
    ) 
    
    mismatch = sum(intersection$intersectionArea)/sum(intersection$area.src)
    
    if(mismatch < 0.99) warning("Input and output shapes don't match: there is a ",100*mismatch,"% difference in areas")
    
    mapping = intersection %>% tibble::as_tibble() %>% dplyr::select(!!inputIdVar2, fracInput, !!!outputVars)
    tmpInput = inputDf %>% dplyr::rename(!!inputIdVar2 := !!inputIdVar)
    mapping = suppressWarnings(mapping %>% dplyr::inner_join(tmpInput, by=as_label(inputIdVar2)))
    mapping = mapping %>% dplyr::mutate(intersectionValue = !!interpolateVar * fracInput)
    mapping = mapping %>% dplyr::group_by(!!!grps, !!!outputVars) %>% dplyr::group_modify(function(d,g,...) {
      return(
        tibble::tibble(agg = do.call(aggregateFn, list(x=d$intersectionValue))) %>% dplyr::rename(!!interpolateVar := agg)
      )
    })
    
    return(mapping)
  },
  
  #' @description create a neighbourhood network from a shapefile
  #' @param mapId - a the ID of the map
  #' @param shape - a sf object, if not present will be loaded from cache
  #' @param idVar - the varable containing the coded identifier of the map
  
  #' @return an edge list of ids with from and to columns
  createNeighbourNetwork = function(mapId, shape = self$getMap(mapId) %>% dplyr::group_by(code,name), idVar="code",...) {
    idVar = ensym(idVar)
    self$getSaved(paste0("NN_",mapId),..., orElse=function(...) {
      shape = shape %>% dplyr::mutate(tmp_id = row_number())
      graph = shape %>% sf::st_intersects()
      edges = tibble::tibble(
        from_tmp_id = rep(1:length(graph),sapply( graph, length)),
        to_tmp_id = unlist(graph %>% purrr::flatten())
      )
      edges = edges %>% 
        dplyr::left_join(shape %>% tibble::as_tibble() %>% dplyr::select(from_tmp_id = tmp_id, from = !!idVar), by="from_tmp_id") %>%
        dplyr::left_join(shape %>% tibble::as_tibble() %>% dplyr::select(to_tmp_id = tmp_id, to = !!idVar), by="to_tmp_id") %>%
        dplyr::filter(from != to) %>%
        dplyr::select(-from_tmp_id, -to_tmp_id)
      return(edges)
    })
  },
  
  # TODO: index of multiple deprivation
  # but not a standard across the 4 nations.
  # https://en.wikipedia.org/wiki/Multiple_deprivation_index#List_of_UK_deprivation_indexes
  # getIMD = function(...) {
  #   self$getSaved("IMD",..., orElse=function(...) {
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
    codeCol = ensym(codeCol)
    nameCol = tryCatch(ensym(nameCol), error = function(e) NULL)
    altCodeCol = tryCatch(ensym(altCodeCol), error = function(e) NULL)
    sf = sf %>% dplyr::mutate(tmp_code = as.character(!!codeCol))
    if(!identical(nameCol,NULL)) {
      sf = sf %>% dplyr::mutate(tmp_name = as.character(!!nameCol))
      sf = sf %>% dplyr::select(-!!nameCol)
    } else {
      sf = sf %>% dplyr::mutate(tmp_name = as.character(!!codeCol))
    }
    sf = sf %>% dplyr::select(-!!codeCol) %>% dplyr::rename(code = tmp_code,name = tmp_name)
    
    if(!identical(altCodeCol,NULL)) {
      sf = sf %>% dplyr::rename(altCode = !!altCodeCol) %>% dplyr::mutate(altCode = as.character(altCode))
    } else {
      sf = sf %>% dplyr::mutate(altCode = as.character(NA))
    }
    sf = sf %>% dplyr::mutate(codeType = codeType) %>% dplyr::select(codeType, code, name, altCode)
    sf$area = sf %>% sf::st_area() %>% as.numeric() 
    return(sf %>% sf::st_zm())
  },
  
  #' @description save a shapefile to disk in the current working directory
  #' @param mapId - a mapId - will become the zip filename
  #' @param  - a zip directory
  #' @return a dataframe containing the grouping columns, the outputIdVar and the interpolated value of interpolateVar
  saveShapefile = function(mapId, shape = self$getMap(mapId), overwrite=FALSE) {
    zipDir = paste0(getwd(),"/",mapId)
    if (!dir.exists(zipDir)) dir.create(zipDir)
    suppressWarnings(sf::st_write(shape, paste0(zipDir,"/",mapId, ".shp"), driver="ESRI Shapefile"))
    zip(zipfile = paste0(zipDir,".zip"),files=mapId)
    unlink(zipDir, recursive=TRUE)
  },
  
  #' @description warm up caches
  loadAllMaps = function() {
    lapply(names(self$sources$maps), self$getMap)
  },
  
  
  #' @description create a catchment area map from 
  #' @param supplyShape - a sf object containing a list of the locations of supply points, with a column containing supply capacity, for example NHS hospital sites, with a bed 
  #' @param supplyIdVar - the variable name of the identifier of the supplier or group of suppliers. For example this could be an NHS trust (multiple sites)
  #' @param supplyVar - the column name of the supply parameter. This could be number of beds in a hospital.
  #' @param supplyOutputVars - the columns from the input that are to be retained in the output
  #' @param demandShape - the sf object with the geographical map of the demand surface. For example the geographical distribution of the population served,
  #' @param demandIdVar - the column name of the unique identifier of the areas,
  #' @param demandVar - the column name of the demand parameter. This could be the population in each region
  #' @param growthRates - a function to calculate 
  #' @param distanceModifier - distance modifier 
  #' @param tweakNetwork - a named list containing extra linkages beyond those inferred by the demandShape topology. These are used to add in bridges 
  #' @param outputMap - catch
  #' @return a dataframe containing the grouping columns, the outputIdVar and the interpolated value of interpolateVar
  createCatchment = function(
      supplyShape, supplyIdVar = "code", supplyVar, supplyOutputVars = supplyShape %>% dplyr::groups(),
      demandId, demandShape, demandIdVar = "code", demandVar, 
      growthRates = function(capacityPerDemand, multiplier = 1.1) {return( rank(capacityPerDemand) / length(capacityPerDemand) * multiplier )},
      distanceModifier = function(distanceToSupply) {return(2/(1+distanceToSupply/min(0.1,mean(distanceToSupply))))},
      tweakNetwork = self$sources$tweak$DEMOG, outputMap = TRUE
    ) {
    
      supplyIdVar = ensym(supplyIdVar)
      supplyVar = ensym(supplyVar)
      demandIdVar = ensym(demandIdVar)
      demandVar = ensym(demandVar)
      
      # rename key columns for consistent 
      demandShape = demandShape %>% dplyr::ungroup() %>% dplyr::rename(demandCode = !!demandIdVar, demand = !!demandVar) %>% dplyr::mutate(tmp_demand_id = row_number())
      # preserve the features we want to output
      supplyFeatures = supplyShape %>% dplyr::ungroup() %>% tibble::as_tibble() %>% dplyr::select(!!supplyIdVar, !!!supplyOutputVars, !!supplyVar) %>% dplyr::distinct()
      supplyPoints = supplyShape %>% dplyr::ungroup()
      
      supplyShape = supplyShape %>% dplyr::ungroup() %>% dplyr::rename(supplyCode = !!supplyIdVar, supply = !!supplyVar) %>% dplyr::mutate(tmp_supply_id = row_number()) 
      
      # define $V_j$ as the geographic region in $G$ containing point $P_j$
      # create an edgelist of supplyIdVar, demandIdVar
      # containment is a list of lists
      containment = demandShape %>% sf::st_contains(supplyShape)
      supplyMapping = tibble::tibble(
        tmp_demand_id = rep(1:length(containment),sapply(containment, length)),
        tmp_supply_id = unlist(containment %>% purrr::flatten()) 
      ) %>% dplyr::distinct()
      resourceProvider = supplyMapping %>%
        dplyr::left_join(demandShape %>% tibble::as_tibble() %>% dplyr::select(tmp_demand_id, demandCode), by="tmp_demand_id") %>%
        dplyr::left_join(supplyShape %>% tibble::as_tibble() %>% dplyr::select(tmp_supply_id, supplyCode, supply), by="tmp_supply_id") %>%
        dplyr::select(-tmp_demand_id, -tmp_supply_id)
      # if there are multiple providers in one area we merge them (creating a new supplier id).
      # resource provider is $G_j$
      
      # merge multiple providers in same are into single $P_j$ in $V_i$
      mergedSuppliers = NULL
      if(any(resourceProvider %>% dplyr::group_by(demandCode) %>% dplyr::count() %>% dplyr::pull(n)>1)) {
        warning("More than one supplier was found in a single region. These the first value will be picked, and the total capacity combined, but as a result the catchment map will be missing some values from the supplier list.")
        mergedSuppliers = resourceProvider %>% dplyr::group_by(demandCode) %>% dplyr::filter(n()>1)
      }
      resourceProvider = resourceProvider %>% dplyr::group_by(demandCode) %>% dplyr::summarise(supplyCode := first(supplyCode), supply=sum(supply))%>% dplyr::rename(areaId = demandCode)
      
      # define the neigbourhood network $N(V_x)$ - connecting disconnected areas
      areaNetwork = self$createNeighbourNetwork(demandId, demandShape, demandCode) 
      if(exists("add",where = tweakNetwork)) areaNetwork = areaNetwork %>% dplyr::union(tweakNetwork$add) %>% dplyr::union(tweakNetwork$add %>% dplyr::rename(tmp = from) %>% dplyr::rename(from=to,to=tmp)) 
      if(exists("remove",where = tweakNetwork)) areaNetwork = areaNetwork %>% dplyr::setdiff(tweakNetwork$remove) %>% dplyr::setdiff(tweakNetwork$remove %>% dplyr::rename(tmp = from) %>% dplyr::rename(from=to,to=tmp)) 
      areaNetwork = areaNetwork %>% dplyr::rename(fromAreaId = from, toAreaId = to)
      
      entireArea = demandShape %>% dplyr::ungroup() %>% dplyr::select(areaId = demandCode, areaSize = area, areaDemand = demand) %>% 
          semi_join(
            areaNetwork %>% dplyr::select(areaId=toAreaId) %>% dplyr::union(resourceProvider %>% dplyr::select(areaId)), 
            by="areaId") %>% tibble::as_tibble() 
        
      # ensure entire area is connected - otherwise we get problems with islands
      
      # areas with resource is $G_j$
      areasWithResource = entireArea %>% 
        dplyr::inner_join(resourceProvider, by="areaId") %>% 
        dplyr::rename(supplierId = supplyCode, supplyCapacity = supply) %>% tibble::as_tibble()
        
      suppliedArea = areasWithResource %>% dplyr::mutate(
        distanceToSupply = 0,
        demandDistanceToSupply = 0,
        accumulatedGrowth = 0,
        iteration = 0
      )
        
      remaining = NULL
      # loop?
      repeat {
        suppliedArea = suppliedArea %>% dplyr::group_by(supplierId) %>% dplyr::mutate(
          totalDemand = sum(areaDemand),
          capacityPerDemand = supplyCapacity/totalDemand
        ) %>% dplyr::ungroup() 
        
        unSuppliedArea = entireArea %>% dplyr::anti_join(suppliedArea, by="areaId")
        message("areas remaining: ",nrow(unSuppliedArea))
        
        if(nrow(unSuppliedArea) == 0) break
        remaining = c(remaining,nrow(unSuppliedArea))
        if (sum(remaining == nrow(unSuppliedArea)) > 4) {
          warning("terminating early with missing areas - it looks like ",nrow(unSuppliedArea), " areas are not connected")
          break;
        }
        # stop if unsupplied area is empty
        
        # growing areas are - A) adjoin unsupplied areas B) have accumulated enough to grow
        unSuppliedNeighbourAreas = areaNetwork %>% dplyr::semi_join(suppliedArea, by=c("fromAreaId"="areaId")) %>% dplyr::inner_join(unSuppliedArea, by=c("toAreaId"="areaId")) %>% dplyr::rename(areaId = toAreaId)
        intoUnsupplied = unSuppliedNeighbourAreas %>% dplyr::group_by(fromAreaId) %>% dplyr::summarise(newAreas = n(), newAreaDemand = sum(areaDemand))
        
        # TODO: this definintion only allows growth into areas that are not already supplied
        growingArea = suppliedArea %>% dplyr::rename(fromAreaId = areaId) %>% 
          dplyr::inner_join(intoUnsupplied, by=c("fromAreaId"), suffix=c(".old","")) %>% 
          dplyr::mutate( 
            newTotalDemand = totalDemand+newAreaDemand,
            newCapacityPerDemand = supplyCapacity/newTotalDemand
          ) %>%
          dplyr::mutate(
            accumulatedGrowth = accumulatedGrowth + growthRates(newCapacityPerDemand) #*distanceModifier(distanceToSupply) #TODO: make this work
          )
        # browser()
        suppliedArea = suppliedArea %>% dplyr::left_join(
          growingArea %>% dplyr::select(areaId = fromAreaId, newAccumulated = accumulatedGrowth), by="areaId") %>% 
          dplyr::mutate(accumulatedGrowth = ifelse(!is.na(newAccumulated), newAccumulated, accumulatedGrowth)) %>%
          dplyr::select(-newAccumulated)
        
        
        newlySuppliedArea = growingArea %>% dplyr::filter(accumulatedGrowth > 1) %>% dplyr::inner_join(unSuppliedNeighbourAreas, by="fromAreaId", suffix=c(".from",""))
        newlySuppliedArea = newlySuppliedArea %>%
          dplyr::mutate(
            distanceToSupply = distanceToSupply + sqrt(areaSize.from),
            demandDistanceToSupply = demandDistanceToSupply + areaDemand.from,
            accumulatedGrowth = accumulatedGrowth - 1,
            iteration = iteration + 1
          ) %>%
          dplyr::select(-ends_with(".from"), -fromAreaId)
        
        # ensure only one supplier gets the target area
        newlySuppliedArea = newlySuppliedArea %>% dplyr::group_by(areaId) %>% dplyr::arrange(desc(capacityPerDemand),desc(distanceToSupply)) %>% dplyr::filter(row_number() <=1) %>% ensurer::ensure_that(length(unique(.$areaId)) == length(.$areaId))
        
        #browser()
        
        suppliedArea = suppressWarnings(bind_rows(suppliedArea, newlySuppliedArea)) %>% ensurer::ensure_that(length(unique(.$areaId)) == length(.$areaId))
        suppliedArea = suppliedArea %>% dplyr::select(areaId,areaSize,areaDemand,supplierId, supplyCapacity, distanceToSupply, demandDistanceToSupply, iteration, accumulatedGrowth, geometry)
      }
      suppliedArea = suppliedArea %>% dplyr::select(-accumulatedGrowth) %>% sf::st_as_sf()
      
      crossMapping = suppliedArea %>% tibble::as_tibble() %>% dplyr::select(!!demandIdVar := areaId, !!supplyIdVar := supplierId)
      out = list(
        suppliers = supplyPoints,
        mergedSuppliers = mergedSuppliers,
        crossMapping = crossMapping,
        suppliedArea = suppliedArea,
        notSuppliedArea = unSuppliedArea %>% sf::st_as_sf()
      )
      
      if (outputMap) {
        # reassemble features preserved from original supply dataframe
        message("assembling catchment area map...")
        #browser()
        
        suppliedMap = suppliedArea %>% sf::st_buffer(0) %>% dplyr::group_by(supplierId) %>% dplyr::summarise(
          area=sum(areaSize),
          !!demandVar:=sum(areaDemand),
          !!supplyVar:=first(supplyCapacity)
        ) %>% dplyr::rename(!!supplyIdVar := supplierId) %>% nngeo::st_remove_holes()
        suppliedMap = supplyFeatures %>% dplyr::inner_join(suppliedMap %>% tibble::as_tibble(), by=as_label(supplyIdVar), suffix=c(".original",""))%>% sf::st_as_sf(crs=4326)
        out$map = suppliedMap
        
      } 
      
      return(out)
    
  },
  
  preview = function(shape=self$getMap(mapId), mapId = NA, nameVar = "name", codeVar = "code", poi=NULL, poiNameVar = "name", poiCodeVar = "code") {
    nameVar = ensym(nameVar)
    codeVar = ensym(codeVar)
    poiNameVar = ensym(poiNameVar)
    poiCodeVar = ensym(poiCodeVar)
    tmp = shape %>% dplyr::rename(name = !!nameVar, code = !!codeVar)
    leaf = leaflet::leaflet(tmp) %>% 
      leaflet::addTiles() %>% 
      leaflet::addPolygons(label = as.character(tmp$name), popup = as.character(tmp$code))
    if(!identical(poi,NULL)) {
      poi = poi %>%  dplyr::rename(name = !!poiNameVar, code = !!poiCodeVar)
      leaf = leaf %>% leaflet::addCircleMarkers(data=poi, color="#FF0000", label= as.character(poi$name), popup = as.character(poi$code))
    }
    return(leaf)
  },
 
 plot = function(shape=self$getMap(mapId), mapId = NA) {
   ggplot(shape)+geom_sf()
 }
  
))

