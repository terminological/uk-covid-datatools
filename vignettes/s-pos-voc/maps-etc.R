#!/usr/bin/Rscript
setwd("~/Git/uk-covid-datatools/")
devtools::load_all("~/Git/arear/")
library(tidyverse)


arear::listStandardMaps()
lsoaMap = arear::getMap("LSOA11")
object.size(lsoaMap)

lsoaMapSimple = lsoaMap %>% rmapshaper::ms_simplify(keep=0.05)

lsoaMapSimpleEng = lsoaMapSimple %>% filter(code %>% stringr::str_starts("E"))
#arear::preview(lsoaMapSimpleEng)


object.size(lsoaMapSimpleEng)
arear::saveShapefile(lsoaMapSimpleEng,"simplifiedLSOA11Eng",dir = "~/Dropbox/covid19/sa-variant/")

# Reference data ----

ladMap = dpc$geog$getMap("LAD19")
ltlaLists = ladMap %>% as_tibble() %>% select(code,name) %>% distinct()
lsoaMap = dpc$geog$getMap("LSOA11") %>% ungroup()
lsoaCentroid = lsoaMap %>% sf::st_centroid()
lsoaXY =  lsoaCentroid %>% mutate(
  x=unname(sf::st_coordinates(.)[,"X"]),
  y=unname(sf::st_coordinates(.)[,"Y"])) %>% 
  as_tibble() %>% 
  select(code,x,y)

