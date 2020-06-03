## code to prepare `UK2019Geography` dataset goes here

library(readxl)
library(tidyverse)
#devtools::load_all("~/Git/uk-covid-datatools/")
setwd("~/Git/uk-covid-datatools/data-raw/")
#source("./rawDataFunctions.R")

stdCodes = function(onsCsv, codeVar, nameVar, status="active") {
  codeVar = ensym(codeVar)
  nameVar = ensym(nameVar)
  out = onsCsv %>% rename(code = !!codeVar, name = !!nameVar) %>% select(code,name) %>% distinct() %>% mutate(codeType=as_label(codeVar),status=status)
  return(out)
}

codeMapping = function(onsCsv, fromCodeVar, toCodeVar, relationship="alias_for") {
  fromCodeVar = ensym(fromCodeVar)
  toCodeVar = ensym(toCodeVar)
  out = onsCsv %>% rename(fromCode = !!fromCodeVar, toCode = !!toCodeVar) %>% select(fromCode,toCode) %>% distinct() %>% 
    mutate(fromCodeType=as_label(fromCodeVar),toCodeType=as_label(toCodeVar),relationship = relationship)
  return(out)
}

codes = NULL
mapping = NULL

#### ONS data files ----

counties = readr::read_csv("https://opendata.arcgis.com/datasets/b3d60eecd2e5483384fcd5cf03a82d27_0.csv") 
codes = codes %>% bind_rows(counties %>% stdCodes(CTY19CD, CTY19NM))

countries = readr::read_csv("https://opendata.arcgis.com/datasets/7579a399b413418db5a3bdd1c824bffb_0.csv") 
codes = codes %>% bind_rows(countries %>% stdCodes(CTRY18CD, CTRY18NM))

nhsregion = readr::read_csv("https://opendata.arcgis.com/datasets/a84ae875f03c4553b49cdec08eb8e13c_0.csv")
codes = codes %>% bind_rows(
  nhsregion %>% stdCodes(NHSER19CD, NHSER19NM),
  nhsregion %>% stdCodes(NHSER19CDH, NHSER19NM, "synonym")
)
mapping = mapping %>% bind_rows(nhsregion %>% codeMapping(NHSER19CDH, NHSER19CD))

pheregion = readr::read_csv("https://opendata.arcgis.com/datasets/e1ab849323534ea7be1920a2598a4c30_0.csv")
codes = codes %>% bind_rows(
  pheregion %>% stdCodes(PHEC19CD, PHEC19NM),
  pheregion %>% stdCodes(PHEC19CDH, PHEC19NM, "synonym"),
)
mapping = mapping %>% bind_rows(pheregion %>% codeMapping(PHEC19CDH, PHEC19CD))

ccgs = readr::read_csv("https://opendata.arcgis.com/datasets/bfb87228cf9e4c44bad0cffa353d0fc8_0.csv")
code = codes %>% bind_rows(
  ccgs %>% stdCodes(CCG20CD, CCG20NM),
  ccgs %>% stdCodes(CCG20CDH, CCG20NM,"synonym")
)
mapping = mapping %>% bind_rows(ccgs %>% codeMapping(CCG20CDH, CCG20CD))

#### England NHS ODS data files ----



#### assemble ----

UKOrganisationalCodes = list(
  entity = codes,
  relationship = mapping,
  #TODO location = 
)

usethis::use_data(UKGeographicCodes, overwrite = TRUE)
