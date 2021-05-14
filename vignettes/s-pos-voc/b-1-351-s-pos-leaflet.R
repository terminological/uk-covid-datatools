#!/usr/bin/Rscript
setwd("~/Git/uk-covid-datatools/")

devtools::load_all("~/Git/standard-print-output/")
devtools::load_all("~/Git/uk-covid-datatools/")


library(patchwork)
library(rgdal)
library(ggplot2)
library(ggspatial)
library(rgeos)
library(maptools)
library(patchwork)
library(sp)
library(sf)

library(data.table)
library(dtplyr)
library(tidyverse, warn.conflicts = FALSE)

ggplot2::theme_set(standardPrintOutput::defaultFigureLayout())
standardPrintOutput::setDefaults()
ukcovidtools::reload()

# Data pipeline
source("./vignettes/b-1-351-s-pos-data.R")
combinedSpositives = createClassifierInput(filename = "s-positives", filterExpr = sGene == "positive", lineages = c("B.1.617.1","B.1.617.2","B.1.351"))
combinedCases = createClassifierInput(filename = "all-cases", from="2021-02-01",lineages = c("B.1.617.1","B.1.617.2","B.1.351","B.1.1.7"))

# Reference data ----

ladMap = dpc$geog$getMap("LAD19")
ltlaLists = ladMap %>% as_tibble() %>% select(code,name) %>% distinct()
lsoaMap = dpc$geog$getMap("LSOA11") %>% ungroup()
lsoaXY = lsoaMap %>% sf::st_centroid() %>% mutate(
  x=unname(sf::st_coordinates(lsoaCentroid)[,"X"]),
  y=unname(sf::st_coordinates(lsoaCentroid)[,"Y"])) %>% 
  as_tibble() %>% 
  select(code,x,y)

# Utility functions ----

saveAndZipClassifier = function(combined, filename, directory="~/Dropbox/covid19/sa-variant/") {
  
  filename = filename %>% stringr::str_replace("\\.csv","")
  
  tmp = combined %>% 
    dpc$spim$augmentLineListWithLSOA() %>%
    inner_join(lsoaXY, by=c("LSOA_code"="code")) %>% 
    distinct() %>% 
    # mutate(
    #   is_617_1 = type == "B.1.617.1",
    #   is_617_2 = type == "B.1.617.2",
    #   is_351 = type == "B.1.351",
    #   is_other_voc = type == "other VOC/VUI",
    #   is_not_voc = type == "non VOC/VUI",
    #   is_variant = type != "non VOC/VUI"
    # ) %>% 
    select(
      # is_variant, 
      # is_617_1, is_617_2, is_351, is_other_voc, is_not_voc,
      type, finalid, 
      date = earliest_specimen_date, 
      sequenced, sGene, 
      #exposure_type, asymptomatic_indicator, 
      #ethnicity_final, age_group=ageCat,
      LSOA_code, LSOA_name, x, y
    )
  
  tmp = readr::write_csv(paste0(directory,filename,"-",Sys.Date(),".csv")) 
  
  tmpdir = getwd()
  setwd(directory)
  zip(paste0(directory,filename,"-",Sys.Date(),".zip"), path.expand(paste0(filename,"-",Sys.Date(),".csv")))
  setwd(tmpdir)
  
  return(tmp)
}

# generate classifier input ----

combinedSpositives = createClassifierInput(from="2020-12-31", filterExpr = sGene == "positive", lineages = c("B.1.617.1","B.1.617.2","B.1.351"))
combinedCases = createClassifierInput(from="2021-02-01",lineages = c("B.1.617.1","B.1.617.2","B.1.351","B.1.1.7"))

# dpc$unloadCaches()
combinedSpositivesLSOA = combinedSpositives %>% saveAndZipClassifier(filename = "s-positives")
combinedCasesLSOA = combinedCases %>% saveAndZipClassifier(filename = "all-cases")


colourScheme = tribble(
  ~type, ~hue, ~sat, ~value,
  "B.1.351", 0,1,1,
  "B.1.617.1", 0.2,0.5,1,
  "B.1.617.2", 0.2,1,1,
  "P.1 & P.2", 0.8,1,1,
  # "B.1.1.318", 0.1,1,1,
  "other VOC/VUI", 0.9,1,1,
  "B.1.1.7", 0.5,0,1,
  "B.1.525", 0.5,0,1,
  "non VOC/VUI", 2/3,1,1,
  "unsequenced S+", 0,0,0
)


## Visualise result in leaflet ----

set.seed(101)

tmp4 = combinedSpositives %>% 
  mutate(type = ifelse(is.na(type),"unsequenced S+", type)) %>%
  filter(date > "2021-02-01") %>% 
  left_join(colourScheme, by="type", suffix=c("",".obs")) %>%
  inner_join(lsoaCentroid,by=c("LSOA_code"="code")) %>%
  sf::st_as_sf() %>% 
  sf::st_jitter()

maxDate = max(tmp4$date,na.rm = TRUE)
r = log(0.5)/-21

tmp4 = tmp4 %>% mutate(
  days = as.numeric(maxDate-date),
  weeks = case_when(
    sequenced ~ as.character(cut(days, c(-Inf,14,28,56,Inf), c("seq: 0-14","seq: 15-28","seq: 28-56","seq: 57+"))),
    !sequenced ~ as.character(cut(days, c(-Inf,7,14,28,Inf), c("s+: 0-7","s+: 8-14","s+: 15-28","s+: 29+"))),
    TRUE ~ NA_character_
  )
)

# free up the memory
# dpc$unloadCaches()

leaflet::leaflet() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  leaflet::addCircles(
    data = tmp4 %>% filter(sequenced),
    stroke = FALSE,
    radius = 400, 
    group = ~weeks,
    fillOpacity = ~exp(-r*as.numeric(maxDate-date)),
    popup = ~glue::glue(
"<b>{date}: {sex}, {age_group}</b>
<ul>
  <li>id: {finalid}</li>
  <li>strain: {type}</li>
  <li>demographic: {ethnicity_final}; {cat}</li>
  <li>exposure: {exposure_type}</li>
  <li>in CTAS: {ctas_status}</li>
  <li>lsoa: {LSOA_code}: {LSOA_name}</li>
  <li>ltla: {LTLA_code}: {LTLA_name}</li>
</ul>"
    ),
    color = ~case_when(
      reinfect56 & asymptomatic_indicator != "Y" ~ "#FF4040",
      reinfect56 ~ "#FF8080",
      asymptomatic_indicator != "Y" ~ "#404040",
      TRUE ~ NA_character_
    ),
    fillColor = ~hsv(hue,sat,value)
  ) %>%
  leaflet::addCircles(
    data = tmp4 %>% filter(!sequenced),
    stroke = TRUE, #~(weeks == "0-7"),
    radius = 500, #~500*predictionQuality/max(predictionQuality), #-floor(as.numeric(maxDate-date)/7)*25, 
    group = ~weeks,
    popup = ~glue::glue(
"<b>{date}: {sex}, {age_group}</b>
<ul>
  <li>id: {finalid}</li>
  <li>demographic: {ethnicity_final}; {cat}</li>
  <li>in CTAS: {ctas_status}</li>
  <li>potential reinfection: {reinfect56}</li>
  <li>detected by screening: {asymptomatic_indicator}</li>
  <li>lsoa: {LSOA_code}: {LSOA_name}</li>
  <li>ltla: {LTLA_code}: {LTLA_name}</li>
</ul>"),
    color = ~case_when(
      reinfect56 & asymptomatic_indicator != "Y" ~ "#FF4040",
      reinfect56 ~ "#FF8080",
      asymptomatic_indicator != "Y" ~ "#404040",
      TRUE ~ NA_character_
    ),
    fillColor = "#A0A0A0",
    #color = ~hsv(hue.col,sat.col,typeScoreReliability),
    #fillColor = ~colour,#~rgb(red.predcol,green.predcol,blue.predcol,normWt)
    fillOpacity = ~exp(-r*days*2)
  ) %>%
  # leaflet::addLayersControl(
  #   overlayGroups = c("0-7","7-14","14-21","21+"),
  #   options = leaflet::layersControlOptions(collapsed = FALSE)
  # ) %>% 
  #
  leaflet::addLayersControl(
    overlayGroups = c("seq: 0-14","seq: 15-28","seq: 28-56","seq: 57+","s+: 0-7","s+: 8-14","s+: 15-28","s+: 29+"),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  ) %>% 
  leaflet::hideGroup(group = c("seq: 28-56","seq: 57+","s+: 15-28","s+: 29+")) %>%
  htmlwidgets::saveWidget(paste0("~/Dropbox/covid19/sa-variant/SPosLSOA2-",Sys.Date(),".html"))

## Leaflet timeline - not working ----
# tmp5 = tmp4 %>%
#   mutate(
#     start = date, 
#     end = date+ifelse(sequenced,28,14),
#     best.red = ifelse(sequenced, red, red.predcol),
#     best.green = ifelse(sequenced, green, green.predcol),
#     best.blue = ifelse(sequenced, blue, blue.predcol),
#     bestCol = rgb(best.red, best.green, best.blue),
#     stroke = !sequenced,
#     radius = ifelse(sequenced,500,400)
#   ) %>%
# 
# geo <- geojsonsf::sf_geojson(tmp5)
#     
# leaflet::leaflet() %>%
#   leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
#   leaftime::addTimeline(
#     data = geo,
#     timelineOpts = leaftime::timelineOptions(
#       styleOptions = NULL, # make sure default style does not override
#     ),
#     sliderOpts = leaftime::sliderOptions(
#       position = "bottomleft"#,
#       #step = 10,
#       #duration = 3000,
#       #showTicks = FALSE
#     )
#   )
# 
# 
# leaflet::leaflet() %>%
#   leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
#   leaftime::addTimeline(
#     data = geo,
#     timelineOpts = leaftime::timelineOptions(
#       styleOptions = NULL, # make sure default style does not override
#       pointToLayer = htmlwidgets::JS(
# "
# function(data, latlng) {
#   return L.circle(
#     latlng,
#     {
#       radius: data.properties.radius,
#       color: data.properties.bestCol,
#       stroke: data.properies.stroke,
#       fillColor: data.properties.bestCol
#     }
#   );
# }
# "
#       )
#     ),
#     sliderOpts = leaftime::sliderOptions(
#       position = "bottomleft"#,
#       #step = 10,
#       #duration = 3000,
#       #showTicks = FALSE
#     )
#   )
#   #htmlwidgets::saveWidget(paste0("~/Dropbox/covid19/sa-variant/SPosLSOA2-",Sys.Date(),".html"))






## First attempt used a rf classification
# tmp4 = tmp3 %>% mutate(z=as.numeric(date-earliestVoc)) %>% select(x,y,z)
# 
# xscale = tmp4 %>% sample() %>% mutate(diff = abs(x-lag(x))) %>% pull(diff) %>% mean(na.rm=TRUE)
# yscale = tmp4 %>% sample() %>% mutate(diff = abs(y-lag(y))) %>% pull(diff) %>% mean(na.rm=TRUE)
# zscale = 7 #tmp4 %>% sample() %>% mutate(diff = abs(z-lag(z))) %>% pull(diff) %>% mean(na.rm=TRUE)
# 
# data = tmp4 %>% mutate(x = x/xscale, y=y/yscale, z=z/zscale) %>% as.matrix()
# data14 = tmp4 %>% mutate(x = x/xscale, y=y/yscale, z=(z+14)/zscale) %>% as.matrix()
# data28 = tmp4 %>% mutate(x = x/xscale, y=y/yscale, z=(z+28)/zscale) %>% as.matrix()
# 
# neighbours = RANN::nn2(data,k = 50)
# neighbours14 = RANN::nn2(data14,query = data,k = 50)
# neighbours28 = RANN::nn2(data28,query = data,k = 50)
# 
# 
# tmp3$vocWeights28 = ifelse(tmp3$is_variant[neighbours28$nn.idx],1/(1+neighbours28$nn.dists),0) %>% matrix(nrow=nrow(tmp3), ncol=50) %>% rowSums(na.rm = TRUE)
# tmp3$nonvocWeights28 = ifelse(!tmp3$is_variant[neighbours28$nn.idx],1/(1+neighbours28$nn.dists),0) %>% matrix(nrow=nrow(tmp3), ncol=50)  %>% rowSums(na.rm = TRUE) 
# tmp3$unknownWeights28 = ifelse(is.na(tmp3$is_variant[neighbours28$nn.idx]),1/(1+neighbours28$nn.dists),0) %>% matrix(nrow=nrow(tmp3), ncol=50)  %>% rowSums(na.rm = TRUE)
# tmp3$weights28 = 1/(1+neighbours28$nn.dists) %>% matrix(nrow=nrow(tmp3), ncol=50)  %>% rowSums(na.rm = TRUE)
# tmp3$vocWeights14 = ifelse(tmp3$is_variant[neighbours14$nn.idx],1/(1+neighbours14$nn.dists),0) %>% matrix(nrow=nrow(tmp3), ncol=50) %>% rowSums(na.rm = TRUE)
# tmp3$nonvocWeights14 = ifelse(!tmp3$is_variant[neighbours14$nn.idx],1/(1+neighbours14$nn.dists),0) %>% matrix(nrow=nrow(tmp3), ncol=50)  %>% rowSums(na.rm = TRUE) 
# tmp3$unknownWeights14 = ifelse(is.na(tmp3$is_variant[neighbours$nn.idx]),1/(1+neighbours$nn.dists),0) %>% matrix(nrow=nrow(tmp3), ncol=50)  %>% rowSums(na.rm = TRUE)
# tmp3$weights14 = 1/(1+neighbours14$nn.dists) %>% matrix(nrow=nrow(tmp3), ncol=50)  %>% rowSums(na.rm = TRUE)
# tmp3$unknownWeights = ifelse(is.na(tmp3$is_variant[neighbours$nn.idx]),1/(1+neighbours$nn.dists),0) %>% matrix(nrow=nrow(tmp3), ncol=50)  %>% rowSums(na.rm = TRUE)
# tmp3$weights = 1/(1+neighbours$nn.dists) %>% matrix(nrow=nrow(tmp3), ncol=50)  %>% rowSums(na.rm = TRUE)
# 
# 
# tmp3 = tmp3 %>% mutate(z=as.numeric(date-earliestVoc))
# tmp3 = tmp3 %>% mutate(variant_num = ifelse(is_variant,1,0))
#   
# glimpse(tmp3)
# classifier = ranger::ranger(
#   is_variant ~ vocWeights28+nonvocWeights28+unknownWeights28+weights28+
#     #vocWeights14+nonvocWeights14+unknownWeights14+weights14+
#     unknownWeights+weights, 
#   data=tmp3 %>% filter(!is.na(is_variant)))
# 
# pred <- predict(classifier, data=tmp3) #, type = "quantiles", quantiles = c(0.1, 0.5, 0.9))
# 
# table(tmp3$is_variant, pred$predictions)
# 
# tmp3 = tmp3 %>% mutate(predicted = pred$predictions)
