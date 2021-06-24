#' Control data sources
#' @export
DataProviderController = R6::R6Class("DataProviderController", inherit = PassthroughFilesystemCache, public = list(
  
  directory = NULL,
  fileProviders = list(),
  
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
  
  #' @description New provider controller
  #' @param wd a shared cache directory for all uk-covid-datatools
  #' @param ... for compatibility
  #' @return the provider controller
  initialize = function(wd) {
    super$initialize(wd)
    self$directory = wd
  },
  
  unloadCaches = function() {
    self$geog$unloadCache()
    self$demog$unloadCache()
    self$postcodes$unloadCache()
    self$codes$unloadCache()
    self$capac$unloadCache()
    self$datasets$unloadCache()
    self$spim$unloadCache()
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
    invisible(self)
  },
  
  updateSPIMDatasets = function() {
    self$spim$getPaths(nocache=TRUE)
    self$spim$getDeathsLineList(nocache=TRUE)
    self$spim$getLineList(nocache=TRUE)
    self$spim$getOneOneOne(nocache=TRUE)
    self$spim$getSeroprevalence(nocache=TRUE)
    self$spim$getSPIMextract(nocache=TRUE)
    invisible(self)
  },
  
  updateDatasets = function() {
    self$datasets$getNHSDeaths(nocache=TRUE)
    self$datasets$getPHEDashboard(nocache=TRUE)
    self$datasets$getPublicOneOneOne(nocache=TRUE)
    self$datasets$getTomWhiteCases(nocache=TRUE)
    self$datasets$getTomWhiteIndicators(nocache=TRUE)
    invisible(self)
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
  
  loadSpimSources = function(fileProvider, ...) {
    self$spim = SPIMDatasetProvider$new(self, fileProvider, ...)
    invisible(self)
  },
  
  #' Title
  #'
  #' @param configFile 
  #' @param name 
  #'
  #' @return
  #' @export
  fileProvider = function(name, configFile = getOption("ukcovid.config"), nocache=FALSE,...) {
    if (exists(name, where=self$fileProviders) && !nocache) {
      return(self$fileProviders[[name]]) 
    }
    config = yaml::read_yaml(file = configFile)
    cfg = config[[name]]
    if(cfg$type == "local") {
      prov = LocalFileProvider$new(cfg)
    } else if(cfg$type == "s3") {
      prov = S3FileProvider$new(cfg)
    }  else if(cfg$type == "sftp-over-ssh") {
      prov = SFTPOverSSHFileProvider$new(cfg)
    } else if(cfg$type == "sftp") {
      prov = SFTPFileProvider$new(cfg)
    } else stop(paste0("Unknown file provider type: ",cfg$type))
    self$fileProviders[[name]] = prov
    return(self$fileProviders[[name]])
  }
  
))

#' @description Static method for new provider controller
#' @param path a shared cache directory for all uk-covid-datatools
#' @param spimSource configurable source for private files
#' @param ... for compatibility
#' @return the provider controller
DataProviderController$setup = function(path, spimSource = getOption("ukcovid.spim"), ...) {
    dir = path.expand(path) %>% stringr::str_remove("/$")
    out = DataProviderController$new(dir) 
    
    out$geog = UKGeographyProvider$new(out)
    out$demog = UKDemographicsProvider$new(out)
    out$postcodes = UKPostcodeProvider$new(out)
    out$codes = UKCodeMappingProvider$new(out)
    out$capac = NHSCapacityProvider$new(out)
    out$datasets = NHSDatasetProvider$new(out)
    out$serial = SerialIntervalProvider$default(out)
    spimFiles = out$fileProvider(spimSource,...)
    if (!identical(spimFiles,NULL)) out$loadSpimSources(fileProvider = spimFiles)
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

