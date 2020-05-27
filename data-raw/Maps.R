## code to prepare `Maps` dataset goes here
setwd("~/Git/uk-covid-datatools/data-raw/")
source("./rawDataFunctions.R")

# https://www.istat.it/it/archivio/222527
# http://www.istat.it/storage/cartografia/confini_amministrativi/generalizzati/Limiti01012020_g.zip

#https://www.opendatani.gov.uk/dataset/osni-open-data-largescale-boundaries-local-government-districts-2012
northernIrelandShapefile = getShapefile("OSNI_Open_Data__Largescale_Boundaries__Local_Government_Districts_2012",
          "http://osni-spatialni.opendata.arcgis.com/datasets/eaa08860c50045deb8c4fdc7fa3dac87_2.zip")

#https://www.spatialdata.gov.scot/geonetwork/srv/api/records/f12c3826-4b4b-40e6-bf4f-77b9ed01dc14
scotlandHealthBoardShapeFile = getShapefile("SG_NHS_HealthBoards_2019",
          "http://sedsh127.sedsh.gov.uk/Atom_data/ScotGov/ZippedShapefiles/SG_NHS_HealthBoards_2019.zip")

# https://geoportal.statistics.gov.uk/datasets/local-health-boards-april-2019-boundaries-wa-buc
walesHealthBoardShapefile = getShapefile("Local_Health_Boards_April_2019_Boundaries_WA_BUC",
          "https://opendata.arcgis.com/datasets/def40bdc98a9457aa108eb3a5fb052b1_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D")

# https://geoportal.statistics.gov.uk/datasets/counties-and-unitary-authorities-april-2019-boundaries-ew-buc-1
unitaryAuthorityShapefile = getShapefile("Counties_and_Unitary_Authorities_April_2019_Boundaries_EW_BUC", 
          "https://opendata.arcgis.com/datasets/a917c123e49d436f90660ef6a9ceb5cc_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D") %>% filter(ctyua19cd %>% stringr::str_starts("E"))

# LTLAgeojson = geojsonsf::geojson_sf("https://c19pub.azureedge.net/assets/geo/ltlas_v1.geojson",expand_geometries = TRUE)
# plot(LTLAgeojson)
# View(LTLAgeojson)

ukShapefile = rbind(
  unitaryAuthorityShapefile %>% select(code = ctyua19cd, name = ctyua19nm, area = st_areasha, geometry),
  walesHealthBoardShapefile %>% select(code = lhb19cd, name=lhb19nm, area = st_areasha, geometry),
  northernIrelandShapefile %>% select(code = LGDCode, name=LGDNAME, area = AREA, geometry),
  scotlandHealthBoardShapeFile  %>% select(code = HBCode, name=HBName, area = Shape_Area, geometry)
) %>% mutate(out_code = code)

# object.size(ukShapefile)  
# ukShapefile = suppressWarnings(ukShapefile %>% sf::st_simplify(dTolerance=0.001))
object.size(ukShapefile)



ggplot(ukShapefile)+geom_sf()

# https://geoportal.statistics.gov.uk/datasets/wards-december-2011-boundaries-ew-bfe
wards2011 = getShapefile("Wards_December_2011_Boundaries_EW_BFE", 
          "https://opendata.arcgis.com/datasets/f04efac388f049508ecf91cafbe70343_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D")
object.size(wards2011)
ggplot(wards2011)+geom_sf()

# https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-buc
ladShapefile = getShapefile("Local_Authority_Districts_December_2019_Boundaries_UK_BUC",
          "https://opendata.arcgis.com/datasets/3a4fa2ce68f642e399b4de07643eeed3_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D")
ggplot(ladShapefile)+geom_sf()


ukShapefileLTLA = rbind(
  ladShapefile  %>% filter(lad19cd %>% stringr::str_starts("E")) %>% select(code = lad19cd, name = lad19nm, area = st_areasha, geometry),
  walesHealthBoardShapefile %>% select(code = lhb19cd, name=lhb19nm, area = st_areasha, geometry),
  northernIrelandShapefile %>% select(code = LGDCode, name=LGDNAME, area = AREA, geometry),
  scotlandHealthBoardShapeFile  %>% select(code = HBCode, name=HBName, area = Shape_Area, geometry)
) %>% mutate(out_code = code)



# wards2019 = getShapefile("Wards_December_2019_Boundaries_EW_BFE",
#   "https://opendata.arcgis.com/datasets/24fd788001d941c5a244dda6fe81dbb2_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D")
# ggplot(wards2019)+geom_sf()


UKCovidMaps = list(
  unitaryAuthority = unitaryAuthorityShapefile,
  reportingRegions = ukShapefile,
  reportingRegionsLTLA = ukShapefileLTLA,
  ward2011 = wards2011,
  # ward2019 = wards2019,
  localAuthorityDistrict = ladShapefile
)

usethis::use_data(UKCovidMaps, overwrite = TRUE)


writeShapefile(UKCovidMaps$reportingRegions,"~/Git/uk-covid-datatools/data-raw/UK_covid_reporting_regions.zip")
writeShapefile(UKCovidMaps$reportingRegionsLTLA,"~/Git/uk-covid-datatools/data-raw/UK_covid_reporting_regions_LTLA.zip")
