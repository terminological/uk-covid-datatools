library(tidyverse)
library(rgdal)
#library(ggmap)
library(ggplot2)
library(rgeos)
library(ggspatial)
library(maptools)


getShapefile = function(mapname,url,simplify=TRUE) {
  wardsZip = paste0("~/Git/uk-covid-datatools/data-raw/Maps/",mapname,".zip")
  unzipDir = paste0("~/Git/uk-covid-datatools/data-raw/Maps/",mapname)
  if(!file.exists(wardsZip)) {
    download.file(url,wardsZip)
    wd = getwd()
    if (!dir.exists(unzipDir)) dir.create(unzipDir)
    setwd(unzipDir)
    unzip(wardsZip)
    setwd(wd)
  }
  wardsShapefile = sf::st_read(unzipDir, layer=mapname)  %>% sf::st_transform(crs=4326)
  if(simplify) wardsShapefile = suppressWarnings(wardsShapefile %>% sf::st_simplify(dTolerance=0.001))
  return(wardsShapefile)
}





unionByGroup = function(groupedSf, ...) {
  grps = groupedSf %>% groups()
  if (length(grps)==0) stop("Must be grouped")
  catchmentMap = groupedSf %>% group_modify(function(d,g,...) {
    d %>% summarise(...) %>% mutate(geometry=d %>% sf::st_union())
  }) %>% sf::st_as_sf(crs=4326)
  return(catchmentMap)
}

