
library(readxl)
library(usethis)
library(tidyverse)

setwd("~/Git/uk-covid-datatools")
ukCovidDates <- read_excel("data-raw/COVID Dates.xlsx")
ukCovidDates = ukCovidDates %>% mutate(`Start date`=as.Date(`Start date`), `End date`=as.Date(`End date`))
usethis::use_data(ukCovidDates, overwrite = TRUE)