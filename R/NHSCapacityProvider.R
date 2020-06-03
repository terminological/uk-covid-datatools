#' Get a provider of UK stats
#'
#' This function sets up a connection to the omop databse.
#' @keywords omop
#' @import dplyr
#' @export
NHSCapacityProvider = R6::R6Class("NHSCapacityProvider", inherit=PassthroughFilesystemCache, public = list(
  
  geog = NULL,
  pcode = NULL,
  
  initialize = function(geographyProvider, postcodeProvider) {
    self$geog = geographyProvider
    self$pcode = postcodeProvider
    super$initialise(geographyProvider$wd)
  },
  
  getHospitals = function(...) {
    self$getSaved("HOSPITALS",orElse = function() {
      gbHospitals <- readr::read_csv(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vQj6X8rIlBlsD5bK-PMcBT9wjAWh60dTTJLfuczqsiKnYzYiN_4KjYAh4HWWkf4v1RH6ih7C78FhdiN/pub?gid=128715098&single=true&output=csv","&nocache=",sample(1:1000000,1)))
      hospSf = sf::st_as_sf(gbHospitals %>% rename(code = hospitalId) %>% select(-nation), coords=c("long","lat"), crs=4326)
    }) %>% filter(...)
  },
  
  getNightingales = function(...) {
    self$getSaved("NIGHTINGALES",orElse = function() {
      nightingales <- readr::read_csv(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vQj6X8rIlBlsD5bK-PMcBT9wjAWh60dTTJLfuczqsiKnYzYiN_4KjYAh4HWWkf4v1RH6ih7C78FhdiN/pub?gid=683986407&single=true&output=csv","&nocache=",sample(1:1000000,1)), 
            col_types = cols(dateOpened = col_date(format = "%Y-%m-%d")))
      nightingales = sf::st_as_sf(nightingales %>% rename(code = hospitalId) %>% select(-nation), coords=c("long","lat"), crs=4326)
    }) %>% filter(...)
  }
  

))

# library(tidyverse)
# library(sf)
# usp = UKStatisticsProvider$new("~/Data/maps")
# usp$loadAllMaps()
# # dm = usp$getPHEDashboardMap()
# # #usp$saveShapefile("DASH_LTLA", dm)
# # tmp = usp$getDemographics("DASH_LTLA", dm %>% group_by(code,name), combineGenders = FALSE)
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
#   supplyId = "ICUBEDS", supplyShape = tmp %>% rename(hospId = code, hospName = name) %>% group_by(hospId,hospName), supplyIdVar = hospId, supplyVar = icuBeds,
#   demandId = "DEMOG", demandShape = usp$getDemographicsMap(), demandVar = count, demandIdVar = code
# )
# 
# usp$preview(shape=catch$map %>% mutate(per100K = 100000*icuBeds/count),nameVar = hospName, codeVar = per100K, poi=catch$suppliers, poiNameVar = hospName, poiCodeVar = icuBeds)
