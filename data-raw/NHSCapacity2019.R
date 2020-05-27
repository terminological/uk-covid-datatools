## code to prepare `NHSCapacity2019` dataset goes here

library(rvest)
library(stringr)
library(readr)
library(readxl)
library(tidyverse)

#### Combine ----

gbHospitals <- read_csv(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vQj6X8rIlBlsD5bK-PMcBT9wjAWh60dTTJLfuczqsiKnYzYiN_4KjYAh4HWWkf4v1RH6ih7C78FhdiN/pub?gid=128715098&single=true&output=csv","&nocache=",sample(1:1000000,1)))

#### nightingale hospitals ----
# source:
# https://docs.google.com/spreadsheets/d/1SHmGnfozDyV_ZWby9WQead55GdcJKQNAG6jwsvczsmY/edit?usp=sharing

nightingales <- read_csv(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vQj6X8rIlBlsD5bK-PMcBT9wjAWh60dTTJLfuczqsiKnYzYiN_4KjYAh4HWWkf4v1RH6ih7C78FhdiN/pub?gid=683986407&single=true&output=csv","&nocache=",sample(1:1000000,1)), 
                         col_types = cols(dateOpened = col_date(format = "%Y-%m-%d")))

idMapping <- read_csv(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vQj6X8rIlBlsD5bK-PMcBT9wjAWh60dTTJLfuczqsiKnYzYiN_4KjYAh4HWWkf4v1RH6ih7C78FhdiN/pub?gid=1853095988&single=true&output=csv","&nocache=",sample(1:1000000,1)))

#### combine ----

NHSCapacity2019 = list(
  hospitals = gbHospitals,
  #trusts = englandAndWalesTrusts,
  nightingales = nightingales,
  idMapping = idMapping
)

# write.csv(NHSCapacity2019$hospitals, "~/Git/uk-covid-datatools/data-raw/hospitalCapacity.csv")
# write.csv(NHSCapacity2019$trusts, "~/Git/uk-covid-datatools/data-raw/trustCapacity.csv")

usethis::use_data(NHSCapacity2019, overwrite=TRUE)

#### TODO: eyeball tests ----


tmp = NHSCapacity2019$hospitals %>% filter(tier1) %>% sf::st_as_sf(coords = c("long","lat"), crs=4326)
#ggplot(UKCovidMaps$reportingRegions)+geom_sf()+geom_sf(data=tmp)

leaflet::leaflet() %>% 
   leaflet::addTiles() %>% 
   leaflet::addCircleMarkers(data=tmp, color="#FF0000", popup = as.character(tmp$name))




```