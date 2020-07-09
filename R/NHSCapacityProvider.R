#' Capacity data from NHS
#' @export
NHSCapacityProvider = R6::R6Class("NHSCapacityProvider", inherit=PassthroughFilesystemCache, public = list(
  
  getHospitals = function(...) {
    self$getSaved("HOSPITALS",...,orElse = function(...) {
      gbHospitals <- readr::read_csv(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vQj6X8rIlBlsD5bK-PMcBT9wjAWh60dTTJLfuczqsiKnYzYiN_4KjYAh4HWWkf4v1RH6ih7C78FhdiN/pub?gid=128715098&single=true&output=csv","&nocache=",sample(1:1000000,1)))
      hospSf = sf::st_as_sf(gbHospitals %>% 
                              dplyr::rename(code = hospitalId) %>% 
                              dplyr::select(-nation), coords=c("long","lat"), crs=4326)
    })
  },
  
  getNightingales = function(...) {
    self$getSaved("NIGHTINGALES",...,orElse = function(...) {
      nightingales <- readr::read_csv(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vQj6X8rIlBlsD5bK-PMcBT9wjAWh60dTTJLfuczqsiKnYzYiN_4KjYAh4HWWkf4v1RH6ih7C78FhdiN/pub?gid=683986407&single=true&output=csv","&nocache=",sample(1:1000000,1)), 
            col_types = readr::cols(dateOpened = readr::col_date(format = "%Y-%m-%d")))
      nightingales = sf::st_as_sf(nightingales %>% 
                                    dplyr::rename(code = hospitalId) %>% 
                                    dplyr::select(-nation), coords=c("long","lat"), crs=4326)
    })
  },
  
  getNHSSiteIcuCatchment = function(...) {
    self$getSaved("SITE_ICU_CATCHMENT",...,orElse = function(...) {
      tmp = self$getHospitals() %>% dplyr::filter(icuBeds>0 & sector=="NHS Sector")
      catch = self$geog$createCatchment(
        supplyShape = tmp %>% dplyr::rename(hospId = code, hospName = name) %>% dplyr::group_by(hospId,hospName), 
        supplyIdVar = hospId, 
        supplyVar = icuBeds,
        demandId = "DEMOG", 
        demandShape = self$demog$getDemographicsMap(),
        demandIdVar = code, 
        demandVar = count,
        outputMap = TRUE
      )
    })
  },
  
  getNHSTrustIcuCatchment = function(...) {
    self$getSaved("TRUST_ICU_CATCHMENT",...,orElse = function(...) {
      tmp = self$getHospitals() %>% dplyr::filter(icuBeds>0 & sector=="NHS Sector")
      catch = self$geog$createCatchment(
        supplyShape = tmp %>% dplyr::group_by(trustId,trustName), 
        supplyIdVar = trustId, 
        supplyVar = icuBeds,
        demandId = "DEMOG", 
        demandShape = self$demog$getDemographicsMap(),
        demandIdVar = code, 
        demandVar = count,
        outputMap = TRUE
      )
    })
  },

  getNHSSiteAcuteCatchment = function(...) {
    self$getSaved("SITE_ACUTE_CATCHMENT",...,orElse = function(...) {
      tmp = self$getHospitals() %>% dplyr::filter(acuteBeds>0 & sector=="NHS Sector")
      catch = self$geog$createCatchment(
        supplyShape = tmp %>% dplyr::rename(hospId = code, hospName = name) %>% dplyr::group_by(hospId,hospName), 
        supplyIdVar = hospId, 
        supplyVar = acuteBeds,
        demandId = "DEMOG", 
        demandShape = self$demog$getDemographicsMap(),
        demandIdVar = code, 
        demandVar = count,
        outputMap = TRUE
      )
    })
  },
  
  getNHSTrustAcuteCatchment = function(...) {
    self$getSaved("TRUST_ACUTE_CATCHMENT",...,orElse = function(...) {
      tmp = self$getHospitals() %>% dplyr::filter(acuteBeds>0 & sector=="NHS Sector")
      catch = self$geog$createCatchment(
        supplyShape = tmp %>% dplyr::group_by(trustId,trustName), 
        supplyIdVar = trustId, 
        supplyVar = acuteBeds,
        demandId = "DEMOG", 
        demandShape = self$demog$getDemographicsMap(),
        demandIdVar = code, 
        demandVar = count,
        outputMap = TRUE
      )
    })
  },
  
  getNHSTrustAcuteDemographics = function(combineGenders = TRUE) {
    test = ncp$getNHSTrustAcuteCatchment()
    return(self$demog$getSingleDigitDemographics(test$crossMapping, code, trustId, weightExpr = 1, combineGenders = combineGenders))
  },
  
  getNHSTrustIcuDemographics = function(combineGenders = TRUE) {
    test = ncp$getNHSTrustIcuCatchment()
    return(self$demog$getSingleDigitDemographics(test$crossMapping, code, trustId, weightExpr = 1, combineGenders = combineGenders))
  },
  
  getNHSSiteAcuteDemographics = function(combineGenders = TRUE) {
    test = ncp$getNHSSiteAcuteCatchment()
    return(self$demog$getSingleDigitDemographics(test$crossMapping, code, hospId, weightExpr = 1, combineGenders = combineGenders))
  },
  
  getNHSSiteIcuDemographics = function(combineGenders = TRUE) {
    test = ncp$getNHSSiteIcuCatchment()
    return(self$demog$getSingleDigitDemographics(test$crossMapping, code, hospId, weightExpr = 1, combineGenders = combineGenders))
  }
  
))

