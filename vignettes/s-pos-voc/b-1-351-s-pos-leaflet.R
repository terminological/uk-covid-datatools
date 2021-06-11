#!/usr/bin/Rscript
setwd("~/Git/uk-covid-datatools/")

devtools::load_all("~/Git/standard-print-output/")
devtools::load_all("~/Git/uk-covid-datatools/")
devtools::load_all("~/Git/arear/")

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
setwd("~/Git/uk-covid-datatools/")
if (!exists("combinedSpositives")) source("./vignettes/s-pos-voc/s-pos-data.R")

# combinedSpositives = createClassifierInput(filename = "s-positives", filterExpr = sGene == "positive", lineages = c("B.1.617.1","B.1.617.2","B.1.351"))
# combinedCases = createClassifierInput(filename = "all-cases", from="2021-02-01",lineages = c("B.1.617.1","B.1.617.2","B.1.351","B.1.1.7"))

# Reference data ----

ladMap = dpc$geog$getMap("LAD19") %>% ungroup() %>% filter(code %>% stringr::str_starts("E"))

if(!file.exists("~/Dropbox/covid19/sa-variant/LAD-neighbours.csv")) {
  ladNetwork = arear::createNeighbourNetwork(ladMap)
  ladNetwork %>% readr::write_csv("~/Dropbox/covid19/sa-variant/LAD-neighbours.csv")
}

msoaMap = dpc$geog$getMap("MSOA11") %>% ungroup() %>% filter(code %>% stringr::str_starts("E"))

if(!file.exists("~/Dropbox/covid19/sa-variant/MSOA-neighbours.csv")) {
  msoaNetwork = arear::createNeighbourNetwork(msoaMap)
  msoaNetwork %>% readr::write_csv("~/Dropbox/covid19/sa-variant/MSOA-neighbours.csv")
  msoaMapSimple = msoaMap %>% rmapshaper::ms_simplify(keep=0.05, keep_shapes=TRUE)
  arear::saveShapefile(msoaMapSimple,"simplifiedMSOA11",dir = "~/Dropbox/covid19/sa-variant/")
}



ltlaLists = ladMap %>% as_tibble() %>% select(code,name) %>% distinct()

lsoaMap = dpc$geog$getMap("LSOA11") %>% ungroup() %>% filter(code %>% stringr::str_starts("E"))
lsoaCentroid = lsoaMap %>% sf::st_centroid()
lsoaXY =  lsoaCentroid %>% mutate(
  x=unname(sf::st_coordinates(.)[,"X"]),
  y=unname(sf::st_coordinates(.)[,"Y"])) %>% 
  as_tibble() %>% 
  select(code,x,y)

if(!file.exists("~/Dropbox/covid19/sa-variant/LSOA-neighbours.csv")) {
  lsoaNetwork = arear::createNeighbourNetwork(lsoaMap)
  lsoaNetwork %>% readr::write_csv("~/Dropbox/covid19/sa-variant/LSOA-neighbours.csv")
  lsoaMapSimple = lsoaMap %>% rmapshaper::ms_simplify(keep=0.05, keep_shapes=TRUE)
  arear::saveShapefile(lsoaMapSimple,"simplifiedLSOA11",dir = "~/Dropbox/covid19/sa-variant/")
}


# Utility functions ----

# TODO: Synthetic data



# VISUALLY CHECK
# with(combinedSpositives, table(sGene,type,useNA="ifany"))
# with(combinedCases, table(sGene,type,useNA="ifany"))


## NON AGGREGATED MAP ----

variantColourScheme = tribble(
  ~type, ~hue, ~sat, ~value,
  "B.1.351", 0,1,1,
  "AV.1", 0.9,1,1,
  "B.1.617.2", 0.2,1,1,
  #"P.1 & P.2", 0.95,1,1,
  #"B.1.1.318", 0.1,1,1,
  "other VOC/VUI", 0.9,0,1,
  "B.1.1.7", 0.7,1,1,
  #"B.1.525", 0.7,0.5,1,
  "non VOC/VUI", 0,0,0.5,
  "not sequenced", 0.5,1,1
) %>% mutate(variantColour = hsv(hue,sat,value))

sGeneColourScheme = tribble(
  ~sGene, ~hue, ~sat, ~value,
  "positive", 0.2,0.5,1,
  "negative", 0.7,1,1,
  "equivocal", 0.3,1,1,
  "unknown", 0.5,1,1
) %>% mutate(sGeneColour = hsv(hue,sat,value))



## Visualise result in leaflet case by case ----

maxDate = max(combinedCasesLSOA$date)

set.seed(101)
r = log(0.5)/-21

jitteredCases = combinedCases %>% 
  dpc$spim$augmentLineListWithLSOA() %>%
  inner_join(lsoaCentroid, by=c("LSOA_code"="code")) %>% 
  mutate(date = earliest_specimen_date) %>%
  distinct() %>% 
  filter(date > maxDate-42) %>% 
  mutate(
    days = as.numeric(maxDate-date),
    opacity = exp(-r*days)
  ) %>% mutate(
    context = case_when(
      asymptomatic_indicator == "N" ~ "symptoms",
      asymptomatic_indicator == "Y" ~ "screening",
      asymptomatic_indicator == "U" ~ "unknown",
      TRUE ~ NA_character_
    ) %>% ordered(c("symptoms","screening","unknown")),
    ageCat = case_when(
      age < 6 ~ "<6",
      age < 19 ~ "6-18",
      age < 22 ~ "19-21",
      age < 45 ~ "22-44",
      age < 65 ~ "45-64",
      age < 80 ~ "65-79",
      TRUE ~ "80+"
    ) %>% ordered(c("<6","6-18","19-21","22-44","45-64","65-79","80+"))
  ) %>%
  sf::st_as_sf() %>% 
  sf::st_jitter(0.005)

variants = jitteredCases %>%
  filter(!is.na(type)) %>% 
  left_join(variantColourScheme %>% select(type,variantColour), by="type")

cases = jitteredCases %>%
  left_join(sGeneColourScheme %>% select(sGene,sGeneColour), by="sGene")




# free up the memory
# dpc$unloadCaches()

leaflet::leaflet() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) %>%
  leaflet::addCircles(
    data = variants %>% filter(days<28),
    stroke = TRUE,
    color = ~variantColour,
    radius = 100, 
    group = ~type,
    fillOpacity = ~exp(-r*days), #~opacity,
    fillColor = ~variantColour,
    popup = ~glue::glue(
"<b>{date}: {sex}, {ageCat}</b>
<ul>
  <li>id: {finalid}</li>
  <li>strain: {type}</li>
  <li>sGene: {sGene}</li>
  <li>asymptomatic: {asymptomatic_indicator}</li>
  <li>exposure: {exposure_type}</li>
  <li>residential: {residential_category}</li>
  <li>ltla: {LTLA_code}: {LTLA_name}</li>
</ul>"
    )
  ) %>%
  leaflet::addCircles(
    data = cases %>% filter(days<7),
    stroke = FALSE,
    radius = 100, #~500*predictionQuality/max(predictionQuality), #-floor(as.numeric(maxDate-date)/7)*25, 
    group = ~sGene,
    fillOpacity = ~exp(-r*2*days), #opacity,
    popup = ~glue::glue(
"<b>{date}: {sex}, {ageCat}</b>
<ul>
  <li>id: {finalid}</li>
  <li>strain: {type}</li>
  <li>sGene: {sGene}</li>
  <li>asymptomatic: {asymptomatic_indicator}</li>
  <li>exposure: {exposure_type}</li>
  <li>residential: {residential_category}</li>
  <li>ltla: {LTLA_code}: {LTLA_name}</li>
</ul>"
    ),
    color = NA_character_,
    fillColor = ~sGeneColour
  ) %>%
  leaflet::addLayersControl(
    overlayGroups = c(variantColourScheme$type, sGeneColourScheme$sGene),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  ) %>% 
  leaflet::hideGroup(group = c(sGeneColourScheme$sGene)) %>%
  htmlwidgets::saveWidget(paste0("~/Dropbox/covid19/sa-variant/SPosLSOA2-",Sys.Date(),".html"))

# LTLA leaflet ----

devtools::load_all("~/Git/uk-covid-datatools/")


prepLtlaCounts = function(combinedCases, subgroup, sourceName,...) {
  subgroup = ensym(subgroup)
  dpc$getSaved("LTLA_COUNTS",params = list(combinedCases, subgroup, sourceName),..., orElse = function(...) {
  
    ltlaSgeneCounts = combinedCases %>% group_by(
      code = LTLA_code,name = LTLA_name,date = earliest_specimen_date, subgroup = !!subgroup
    ) %>% 
      summarise(value = n()) %>% 
      ungroup() %>%
      mutate(source = sourceName, type = "incidence", statistic = "case", ageCat = NA_character_, gender= NA_character_, codeType="LAD") %>%
      tsp$completeAndRemoveAnomalies(allowZeroDays = TRUE) %>%
      dpc$demog$findDemographics() %>%
      ungroup()
    
    dates = as.Date(min(combinedCases$earliest_specimen_date):max(combinedCases$earliest_specimen_date),"1970-01-01")
    
    ltlaSgeneCounts2 = ltlaSgeneCounts %>%
      tidyr::complete(nesting(codeType,code,name,population,statistic,ageCat,gender,type,source),subgroup,date = dates,fill= list(value.original =0, value = 0, Anomaly=FALSE))
    
    ltlaSgeneCounts2 = ltlaSgeneCounts2 %>%
      covidStandardGrouping() %>%
      arrange(date) %>%
      mutate(
        total.28day = stats::filter(x = value, filter = rep(1,28),sides = 1),
        total.7day = stats::filter(x=value, filter=rep(1,7),sides = 1),
        daily.28day = total.28day / 28,
        daily.7day = total.7day / 7)
    
    ltlaSgeneCounts3 = ltlaSgeneCounts2 %>%
      covidStandardGrouping() %>%
      arrange(date) %>%
      mutate(
        gr.28day = log(lag(pmax(daily.28day,exp(-1)),n=5))-log(lag(pmax(daily.28day,exp(-1)),n=6)),
        gr.7day = log(lag(pmax(daily.7day,exp(-1)),n=5))-log(lag(pmax(daily.7day,exp(-1)),n=6)),
        wow.28day = daily.28day - lag(daily.28day, 7),
        wow.7day = daily.7day - lag(daily.7day, 7),
        dod.28day = daily.28day - lag(daily.28day),
        dod.7day = daily.7day - lag(daily.7day)
      ) %>%
      ungroup() 
  
  return(ltlaSgeneCounts3)
  })
}


ltlaSgeneCounts3 = prepLtlaCounts(combinedCases,subgroup = sGene, sourceName = "s-gene") %>%
  filter(date == max(date)) %>% 
  inner_join(
    sGeneColourScheme %>% select(sGene, sGeneColour), by=c("subgroup"="sGene")
  ) %>% 
  covidStandardGrouping(subgroup) %>%
  mutate(
    popup = glue::glue("
<u>positive taqPath last week:</u>
<li>S+ last week: {sum(ifelse(subgroup=='positive',total.7day,0),na.rm=TRUE)}</li>
<li>S- last week: {sum(ifelse(subgroup=='negative',total.7day,0),na.rm=TRUE)}</li>
<li>equivocal last week: {sum(ifelse(subgroup=='equivocal',total.7day,0),na.rm=TRUE)}</li>
<li>unknown last week: {sum(ifelse(subgroup=='unknown',total.7day,0),na.rm=TRUE)}</li>
<u>taqPath per week per 100K:</u>
<li>S+: {(sum(ifelse(subgroup=='positive',daily.7day,0),na.rm=TRUE)/population*700000) %>% sprintf(fmt='%1.1f')}</li>
<li>S-: {(sum(ifelse(subgroup=='negative',daily.7day,0),na.rm=TRUE)/population*700000) %>% sprintf(fmt='%1.1f')}</li>
<li>equivocal: {(sum(ifelse(subgroup=='equivocal',daily.7day,0),na.rm=TRUE)/population*700000) %>% sprintf(fmt='%1.1f')}</li>
<li>unknown: {(sum(ifelse(subgroup=='unknown',daily.7day,0),na.rm=TRUE)/population*700000) %>% sprintf(fmt='%1.1f')}</li>
"
    )
  ) 

coverage = ltlaSgeneCounts3 %>%
  covidStandardGrouping(subgroup) %>%
  mutate(binom::binom.confint(x=total.7day,n=sum(total.7day,na.rm = TRUE),methods = "wilson")) %>% 
  #rename(value.percent = mean,value.percent.lower = lower,value.percent.higher = higher) %>%
  filter(subgroup=="unknown") %>%
  mutate(coverage = 1-mean) %>%
  mutate(popup = glue::glue("<b>{code}: {name}</b><br/>
<b><u>TaqPath</u></b><br/>
<u>last week:</u>  
<li>tests: {n}</li>
<li>taqpath: {(n-x)}</li>
<li>coverage: {sprintf('%1.0f%% (%1.0f - %1.0f)',(1-mean)*100,(1-upper)*100,(1-lower)*100)}</li>
<u>rolling 7 day, per week per 100K:</u>  
<li>positive tests: {(n/7/population*700000) %>% sprintf(fmt='%1.1f')}</li>
<li>taqpath: {((n-x)/7/population*700000) %>% sprintf(fmt='%1.1f')}</li>
"
  )) %>%
  select(-x,-n)


ltlaLineageCounts = prepLtlaCounts(combinedCases %>% mutate(type = ifelse(is.na(type),"not sequenced",type)),subgroup = type, sourceName = "lineage",nocache=TRUE) %>%
  filter(date == max(date)) %>% 
  inner_join(
    variantColourScheme %>% select(type, variantColour), by=c("subgroup"="type")
  ) %>% 
  covidStandardGrouping(subgroup) %>%
  mutate(
    popup = glue::glue("
<u>sequencing last 28 days:</u>
<li>non VOC/VUI: {sum(ifelse(subgroup=='non VOC/VUI',total.28day,0),na.rm=TRUE)}</li>
<li>B.1.617.2: {sum(ifelse(subgroup=='B.1.617.2',total.28day,0),na.rm=TRUE)}</li>
<li>AV.1: {sum(ifelse(subgroup=='AV.1',total.28day,0),na.rm=TRUE)}</li>
<li>B.1.1.7: {sum(ifelse(subgroup=='B.1.1.7',total.28day,0),na.rm=TRUE)}</li>
<li>B.1.351: {sum(ifelse(subgroup=='B.1.351',total.28day,0),na.rm=TRUE)}</li>
<li>other VOC/VUI: {sum(ifelse(subgroup=='other VOC/VUI',total.28day,0),na.rm=TRUE)}</li>
<li>not sequenced (yet): {sum(ifelse(subgroup=='not sequenced',total.28day,0),na.rm=TRUE)}</li>
<u>sequencing per week per 100K:</u>
<li>non VOC/VUI: {(sum(ifelse(subgroup=='non VOC/VUI',daily.28day,0),na.rm=TRUE)/population*700000) %>% sprintf(fmt='%1.1f')}</li>
<li>B.1.617.2: {(sum(ifelse(subgroup=='B.1.617.2',daily.28day,0),na.rm=TRUE)/population*700000) %>% sprintf(fmt='%1.1f')}</li>
<li>AV.1: {(sum(ifelse(subgroup=='AV.1',daily.28day,0),na.rm=TRUE)/population*700000) %>% sprintf(fmt='%1.1f')}</li>
<li>B.1.1.7: {(sum(ifelse(subgroup=='B.1.1.7',daily.28day,0),na.rm=TRUE)/population*700000) %>% sprintf(fmt='%1.1f')}</li>
<li>B.1.351: {(sum(ifelse(subgroup=='B.1.351',daily.28day,0),na.rm=TRUE)/population*700000) %>% sprintf(fmt='%1.1f')}</li>
<li>other VOC/VUI: {(sum(ifelse(subgroup=='other VOC/VUI',daily.28day,0),na.rm=TRUE)/population*700000) %>% sprintf(fmt='%1.1f')}</li>
<li>not sequenced (yet): {(sum(ifelse(subgroup=='not sequenced',daily.28day,0),na.rm=TRUE)/population*700000) %>% sprintf(fmt='%1.1f')}</li>
"
    )
  ) 


coverageSeq = ltlaLineageCounts %>%
  covidStandardGrouping(subgroup) %>%
  mutate(binom::binom.confint(x=total.28day,n=sum(total.28day,na.rm = TRUE),methods = "wilson")) %>% 
  #rename(value.percent = mean,value.percent.lower = lower,value.percent.higher = higher) %>%
  filter(subgroup=="not sequenced") %>%
  mutate(coverage = 1-mean) %>%
  mutate(popup = glue::glue("
<b><u>sequencing</u></b><br/>
<u>in last 28 days:</u>  
<li>tests: {n}</li>
<li>sequenced (so far): {(n-x)}</li>
<li>coverage: {sprintf('%1.0f%% (%1.0f - %1.0f)',(1-mean)*100,(1-upper)*100,(1-lower)*100)}</li>
<u>rolling 28 day, per week per 100K:</u>  
<li>tests: {(n/28/population*700000) %>% sprintf(fmt='%1.1f')}</li>
<li>sequenced (so far): {((n-x)/28/population*700000) %>% sprintf(fmt='%1.1f')}</li>
"
  )) %>%
  select(-x,-n)

popups = coverage %>% ungroup() %>% select(code, popup1 = popup) %>% distinct() %>% 
  left_join(ltlaSgeneCounts3 %>% ungroup() %>% select(code, popup2 = popup) %>% distinct(), by="code") %>%
  left_join(coverageSeq %>% ungroup() %>% select(code, popup3 = popup) %>% distinct(), by="code") %>%
  left_join(ltlaLineageCounts %>% ungroup() %>% select(code, popup4 = popup) %>% distinct(), by="code")
  
  

popups = popups %>% mutate(popup = paste0(popup1,popup2,popup3,popup4)) %>% select(code,popup)

divPal = leaflet::colorNumeric(
  palette = grDevices::colorRamp(c("#00FF00","#202000","#FF0000"),interpolate = "spline"),
  domain = c(-0.25,0.25)
)

ladMap2 = dpc$geog$getMap("LAD19") %>% filter(code %>% stringr::str_starts("E")) %>% ungroup() %>% rmapshaper::ms_simplify(keep = 0.2, keep_shapes=TRUE) %>% sf::st_buffer(-0.005) 

# add the layers
lf = leaflet::leaflet() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.DarkMatter) 

for (sg in unique(ltlaSgeneCounts3$subgroup)) {
  
  filtered = ladMap2 %>% 
    left_join(
      ltlaSgeneCounts3 %>% ungroup() %>% 
        mutate(
          # opacity = (rank(total.28day/population)/n())^2
          opacity = (as.numeric(cut(total.7day/population, c(-Inf,quantile(total.7day/population, probs = c(0.5,0.75,0.9,0.95,0.975), na.rm = TRUE),Inf)))-1)/6,
          lineCol = scales::oob_squish(gr.28day,c(-0.25,0.25))
        ) %>% filter(subgroup == sg), by="code") %>%
    left_join(popups, by="code",suffix=c(".old","")) %>%
    ungroup() 
  
    #%>%
    #left_join(sGeneColourScheme %>% select(subgroup=sGene,sGeneColour), by="subgroup")
  #pal = leaflet::colorQuantile("viridis",domain = filtered$total.28day,probs = c(0,0.25,0.5,0.75,0.9,0.95,0.975,1))
  
  
  lf = lf %>% 
    leaflet::addPolygons(
      data = filtered %>% select(lineCol,opacity,sGeneColour,popup,geometry) %>% sf::st_as_sf(),
      stroke = TRUE,
      opacity = 0.5,
      color = ~divPal(lineCol),
      weight = 3,
      fillOpacity = ~opacity,
      fillColor = ~sGeneColour,#~pal(total.28day),
      group = sg,
      popup = ~popup
    ) 
}


for (sg in unique(ltlaLineageCounts$subgroup)) {
  
  filtered = ladMap2 %>% 
    left_join(
      ltlaLineageCounts %>% ungroup() %>% 
        mutate(
          # opacity = (rank(total.28day/population)/n())^2
          opacity = (as.numeric(cut(total.28day/population, c(-Inf,quantile(total.28day/population, probs = c(0.5,0.75,0.9,0.95,0.975), na.rm = TRUE),Inf)))-1)/6,
          lineCol =  scales::oob_squish(gr.28day,c(-0.25,0.25))
        ) %>% 
        filter(subgroup == sg), by="code") %>%
    left_join(popups, by="code",suffix=c(".old","")) %>%
    ungroup() 
  
  #%>%
  #left_join(sGeneColourScheme %>% select(subgroup=sGene,sGeneColour), by="subgroup")
  # pal = leaflet::colorQuantile("viridis",domain = filtered$total.28day,probs = c(0,0.25,0.5,0.75,0.9,0.95,0.975,1))
  
  lf = lf %>% 
    leaflet::addPolygons(
      data = filtered %>% select(lineCol,opacity,variantColour,popup,geometry) %>% sf::st_as_sf(),
      stroke = TRUE,
      opacity = 0.5,
      color = ~divPal(lineCol),
      weight = 3,
      fillOpacity = ~opacity,
      fillColor = ~variantColour,#~pal(total.28day),
      group = sg,
      popup = ~popup
    ) 
}

## TODO: sequencing coverage

lf = lf %>% 
  leaflet::addPolygons(
    data = ladMap2 %>% inner_join(coverage, by="code") %>% left_join(popups, by="code",suffix=c(".old","")) %>% filter(is.finite(coverage)) %>% select(coverage,popup,geometry) %>% sf::st_as_sf(),
    stroke = TRUE,
    opacity = 0.5,
    color = "#000000",
    weight = 3,
    fillOpacity = ~coverage,
    fillColor = "#FFFFFF",#~pal(total.28day),
    group = "taqPath coverage",
    popup = ~popup
  )

lf = lf %>% 
  leaflet::addPolygons(
    data = ladMap2 %>% inner_join(coverageSeq, by="code") %>% left_join(popups, by="code",suffix=c(".old","")) %>% filter(is.finite(coverage)) %>% select(coverage,popup,geometry) %>% sf::st_as_sf(),
    stroke = TRUE,
    opacity = 0.5,
    color = "#000000",
    weight = 3,
    fillOpacity = ~coverage,
    fillColor = "#FFFFFF",#~pal(total.28day),
    group = "sequencing coverage",
    popup = ~popup
  )

lf %>%
  leaflet::addLayersControl(
    overlayGroups = c(unique(ltlaSgeneCounts3$subgroup),"taqPath coverage",unique(ltlaLineageCounts$subgroup),"sequencing coverage"),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  ) %>% 
  leaflet::hideGroup(c(unique(ltlaSgeneCounts3$subgroup),"taqPath coverage",unique(ltlaLineageCounts$subgroup),"sequencing coverage")) %>%
  leaflet::showGroup(group = "positive") %>%
  htmlwidgets::saveWidget(paste0("~/Dropbox/covid19/sa-variant/SPosLTLA-",Sys.Date(),".html"))


#https://towardsdatascience.com/a-bayesian-approach-to-estimating-revenue-growth-55d029efe2dd











# 1) group on everything by subgroup and do wilson binom on subgroup - value, total.7day and total.28day
# filter to Unknowns - gives you %age coverage

# 2) filter to S+ and S-; do wilson binom on subgroup
# filter to S+ - gives you %age S+ (S+:S- ratio)

# repeat the first step with subgroup = type (sequencing)

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
