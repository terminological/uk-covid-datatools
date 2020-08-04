#' Control data sources
#' @export
DataProviderController = R6::R6Class("DataProviderController", inherit = PassthroughFilesystemCache, public = list(
  
  directory = NULL,
  
  geog = NULL,
  demog = NULL,
  postcodes = NULL,
  codes = NULL,
  capac = NULL,
  datasets = NULL,
  spim = NULL,
  serial = NULL,
  ident = list(
    maps = list(
      WD11 = "WD11",
      LSOA11 = "LSOA11",
      SGDZ11 = "SGDZ11",
      SHB19 = "SHB19",
      LHB19 = "LHB19",
      CTYUA19 = "CTYUA19",
      LAD19 = "LAD19",
      CCG20 = "CCG20",
      NHSER20 = "NHSER20",
      PHEC16 = "PHEC16",
      CTRY19 = "CTRY19",
      LGD12 = "LGD12"
    ),
    pseudocodes = list(
      E99999999 = "E99999999", #(pseudo) = England (UA);
      W99999999 = "W99999999", #(pseudo) = Wales;
      S99999999 = "S99999999", #(pseudo) = Scotland;
      N99999999 = "N99999999", #(pseudo) = Northern Ireland;
      L99999999 = "L99999999", #(pseudo) = Channel Islands;
      M99999999 = "M99999999"  #(pseudo) = Isle of Man; 
    )
  ),
  
  initialize = function(wd) {
    super$initialize(wd)
    self$directory = wd
  },
  
  updateCodes = function() {
    self$codes$getManualCodes(nocache=TRUE)
    self$codes$getONSRegister(nocache=TRUE)
    self$codes$getODSCodes(nocache=TRUE)
    self$codes$getManualMappings(nocache=TRUE)
    self$codes$getONSMappings(nocache=TRUE)
    self$codes$getODSMappings(nocache=TRUE)
    self$codes$getDescriptions(nocache=TRUE)
    self$codes$getCodes(nocache=TRUE)
    self$codes$getMappings(nocache=TRUE)
    self$codes$getTransitiveClosure(nocache=TRUE)
  },
  
  updateSPIMDatasets = function() {
    self$spim$getPaths(nocache=TRUE)
    self$spim$getDeathsLineList(nocache=TRUE)
    self$spim$getLineList(nocache=TRUE)
    self$spim$getOneOneOne(nocache=TRUE)
    self$spim$getSeroprevalence(nocache=TRUE)
    self$spim$getSPIMextract(nocache=TRUE)
  },
  
  updateDatasets = function() {
    self$datasets$getNHSDeaths(nocache=TRUE)
    self$datasets$getPHEDashboard(nocache=TRUE)
    self$datasets$getPublicOneOneOne(nocache=TRUE)
    self$datasets$getTomWhiteCases(nocache=TRUE)
    self$datasets$getTomWhiteIndicators(nocache=TRUE)
  },
  
  timeseriesProcessor = function(...) {
    TimeseriesProcessingPipeline$new(self, ...)
  },
  
  chessProcessor = function(...) {
    ChessProcessingPipeline$new(self, ...)
  },
  
  survivalProcessor = function(...) {
    SurvivalProcessingPipeline$new(self, ...)
  },
  
  metawardProcessor = function(...) {
    MetawardProcessingPipeline$new(self, ...)
  },
  
  loadSpimSources = function(path, ...) {
    self$spim = SPIMDatasetProvider$new(self, path %>% stringr::str_remove("/$"), ...)
    invisible(self)
  }
  
))

DataProviderController$setup = function(path, spimPath = NULL, ...) {
    dir = path.expand(path) %>% stringr::str_remove("/$")
    out = DataProviderController$new(dir) 
    
    out$geog = UKGeographyProvider$new(out)
    out$demog = UKDemographicsProvider$new(out)
    out$postcodes = UKPostcodeProvider$new(out)
    out$codes = UKCodeMappingProvider$new(out)
    out$capac = NHSCapacityProvider$new(out)
    out$datasets = NHSDatasetProvider$new(out)
    out$serial = SerialIntervalProvider$default(out)
    if (!identical(spimPath,NULL)) out$loadSpimSources(path = spimPath)
    return(out)
}

# # DataProviderController$setup("~/Data/maps/", "~/S3/encrypted/")$updateCodes()
# 
# dpc = DataProviderController$setup("~/Data/maps/", "~/S3/encrypted/")
# #dpc$updateCodes()
# #tmp = dpc$datasets$getTomWhiteCases()
# dataset = dpc$datasets
# self = dpc$timeseriesProcessor()
# testdata = dataset$getNHSDeaths()

