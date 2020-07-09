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

# https://data.england.nhs.uk/dataset/ods-nhs-trusts-and-sites
standardFormat = c( "Organisation Code",  "Name",  "National Grouping",  "High Level Health Geography",  "Address Line 1",  "Address Line 2",  "Address Line 3",  "Address Line 4",
                    "Address Line 5",  "Postcode",  "Open Date",  "Close Date",  "Null 1",  "Organisation SubType Code",  "Parent Organisation Code",  "Null 2",  "Null 3",  "Contact Telephone Number",
                    "Null 4",  "Null 5",  "Null 6",  "Amended Record Indicator",  "Null 7",  "GOR Code",  "Null 8",  "Null 9",  "Null 10"
)

etr = readr::read_csv("https://nhsenglandfilestore.s3.amazonaws.com/ods/etr.csv",col_names = standardFormat,col_types = cols(.default=col_character())) %>% select(-starts_with("Null"))
ets =  readr::read_csv("https://nhsenglandfilestore.s3.amazonaws.com/ods/ets.csv",col_names = standardFormat,col_types = cols(.default=col_character())) %>% select(-starts_with("Null"))
eccg = readr::read_csv("https://nhsenglandfilestore.s3.amazonaws.com/ods/eccg.csv",col_names = standardFormat,col_types = cols(.default=col_character())) %>% select(-starts_with("Null"))


```