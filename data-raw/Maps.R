## code to prepare `Maps` dataset goes here

library(rgdal)
#library(ggmap)
library(ggplot2)
library(rgeos)

library(maptools)

getShapefile = function(mapname,url) {
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
  wardsShapefile = sf::st_read(unzipDir, layer=mapname)
  return(wardsShapefile)
}

# https://www.istat.it/it/archivio/222527
# http://www.istat.it/storage/cartografia/confini_amministrativi/generalizzati/Limiti01012020_g.zip


# https://geoportal.statistics.gov.uk/datasets/counties-and-unitary-authorities-april-2019-boundaries-ew-buc-1
unitaryAuthorityShapefile = getShapefile("Counties_and_Unitary_Authorities_April_2019_Boundaries_EW_BUC", 
          "https://opendata.arcgis.com/datasets/a917c123e49d436f90660ef6a9ceb5cc_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D")

ggplot(unitaryAuthorityShapefile)+geom_sf()

# https://geoportal.statistics.gov.uk/datasets/wards-december-2011-boundaries-ew-bfe
wards = getShapefile("Wards_December_2011_Boundaries_EW_BFE", 
          "https://opendata.arcgis.com/datasets/f04efac388f049508ecf91cafbe70343_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D")
ggplot(wards)+geom_sf()

# https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-buc
ladShapefile = getShapefile("Local_Authority_Districts_December_2019_Boundaries_UK_BUC",
          "https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D")
ggplot(ladShapefile)+geom_sf()


wards2019 = getShapefile("Wards_December_2019_Boundaries_EW_BFE",
  "https://opendata.arcgis.com/datasets/24fd788001d941c5a244dda6fe81dbb2_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D")
ggplot(wards2019)+geom_sf()


UKCovidMaps = list(
  unitaryAuthority = unitaryAuthorityShapefile,
  ward = wards,
  ward2019 = wards2019,
  localAuthorityDistrict = ladShapefile
)

usethis::use_data(UKCovidMaps, overwrite = TRUE)
