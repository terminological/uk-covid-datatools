## code to prepare `UK2019Demographics` dataset goes here

library(readxl)
library(tidyverse)
devtools::load_all("~/Git/uk-covid-datatools/")
setwd("~/Git/uk-covid-datatools/data-raw/")
source("./rawDataFunctions.R")


counties = readr::read_csv("https://opendata.arcgis.com/datasets/b3d60eecd2e5483384fcd5cf03a82d27_0.csv")
counties = counties %>% rename(code = CTY19CD, name = CTY19NM) %>% select(code,name) %>% mutate(codeType="CTY19CD")

UKGeographicCodes = bind_rows(
  counties,
  
)

usethis::use_data(UKGeographicCodes, overwrite = TRUE)
