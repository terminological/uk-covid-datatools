#!/usr/bin/Rscript

library(tidyverse)

devtools::load_all("~/Git/uk-covid-datatools/")
# devtools::install_github("terminological/uk-covid-datatools")
# library(ukcovidtools)
devtools::load_all("~/Git/standard-print-output/")
library(rgdal)
library(ggplot2)
library(ggspatial)
library(rgeos)
library(maptools)
library(lubridate)
library(patchwork)
library(sp)

ggplot2::theme_set(standardPrintOutput::defaultFigureLayout())
setwd("~/Git/uk-covid-datatools/vignettes/")
source("./covid-serial-interval.R")
source("./lockdown-impact-data.R")