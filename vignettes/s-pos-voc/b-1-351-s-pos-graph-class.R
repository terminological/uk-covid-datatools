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

source("./vignettes/s-pos-data.R")

# tmp3 %>% group_by(finalid) %>% filter(n() > 1) %>% arrange(finalid) %>% View()
combinedSpositives %>% select(is_variant, type, finalid, date, sequenced, exposure_type, reinfect56, ctas_status, ethnicity_final, age_group, LSOA_code, LSOA_name, x, y) %>% 
  readr::write_csv(paste0("~/Dropbox/covid19/sa-variant/input-data-",Sys.Date(),".csv"))
setwd("~/Dropbox/covid19/sa-variant/")
zip(paste0("~/Dropbox/covid19/sa-variant/input-data-",Sys.Date(),".zip"), path.expand(paste0("input-data-",Sys.Date(),".csv")))

## calculate neighbourhood network ----

# how many km is equivalent to 1 days temporal separation of cases
kmPerDay = 5
tmp4 = combinedSpositives %>% mutate(z=as.numeric(date-earliestVoc)) %>% select(x,y,z)

# 1 deg lat ~ 111km
# 1 deg long in S ~ 70 km
# 1 deg long in N ~ 63 km
# we will use 65

kmPerXUnit = 65 #tmp4 %>% sample() %>% mutate(diff = abs(x-lag(x))) %>% pull(diff) %>% mean(na.rm=TRUE)
kmPerYUnit = 111 #tmp4 %>% sample() %>% mutate(diff = abs(y-lag(y))) %>% pull(diff) %>% mean(na.rm=TRUE)



# calculate the network
data = tmp4 %>% mutate(x = x * kmPerXUnit, y=y * kmPerYUnit, z=z * kmPerDay) %>% as.matrix()
neighbours = RANN::nn2(data,k = 50)

# reformat to a tidy dataframe and convert distances to weights. 
infectors = rep(1:nrow(combinedSpositives),50)
infectees = as.vector(neighbours$nn.idx)

penalisedDistances = as.vector(neighbours$nn.dists)^
  ifelse(combinedSpositives$date[infectees]<combinedSpositives$date[infectors], 1.5, 1) # penalise negative serial intervals

map = tibble(
  distance = penalisedDistances,
  finalid.infectee = combinedSpositives$finalid[infectees],
  finalid.infector = combinedSpositives$finalid[infectors],
  serial.interval = combinedSpositives$date[infectees]-combinedSpositives$date[infectors]
)

rm(data,neighbours,tmp4)

# TODO: https://github.com/tidyverse/dtplyr



# pruning the neighbourhood map ----
# The neighbourhood map should ideally include things that are not too far away (unless the network is very sparse)
# also should not include things that have a very negative serial interval as biologically implausible
# because of some infections being sparsely connected in space and time and others being very connected so we include
# items based on a combined metric of their distance and rank - at the moment that is distance+rank*10 < 200.

# ggplot(map,aes(x=distance))+geom_density()
# indeg = map %>% group_by(finalid.infectee) %>% summarise(degree = n())
# ggplot(indeg,aes(x=degree))+geom_histogram()

ranked = map %>% group_by(finalid.infectee) %>% mutate(rank = min_rank(distance)) %>% ungroup()

# #ggplot(ranked %>% semi_join(relevantSet, by=c("finalid.infectee"="finalid")) %>% mutate(negSI = serial.interval<0),aes(x=rank,y=distance,colour=negSI))+geom_point()
# ggplot(ranked %>% semi_join(relevantSet, by=c("finalid.infectee"="finalid")),aes(x=rank,y=distance,colour=as.numeric(serial.interval)))+geom_point()+scale_colour_gradient2()

rankedMap = ranked %>% filter(distance+rank*10 < 200) %>% filter(serial.interval > -2 & serial.interval < 21)

# visualisation for the network pruning...
# relevantSet = combinedSpositives %>% filter(date > max(date)-14)
# ggplot(rankedMap %>% semi_join(relevantSet, by=c("finalid.infectee"="finalid")),aes(x=rank,y=distance,colour=as.numeric(serial.interval)))+geom_point()+scale_colour_gradient2()
# indeg = rankedMap %>% group_by(finalid.infectee) %>% summarise(degree = n())
# ggplot(indeg,aes(x=degree))+geom_histogram()
# ggplot(rankedMap,aes(x=distance))+geom_density()

# OLD NETWORK PRUNING
# # Allow infections within sensible bounds for the serial interval given negative serial intervals have been
# observed we let people be infected by those in the future. 
# this may be a terrible idea.
# map = map %>% filter(
#     serial.interval > -2 &
#     #serial.interval > 0 &
#     serial.interval < 21 # &
#     #(is.na(combinedSpositives$type[infectees]) | is.na(combinedSpositives$type[infectors]) | (combinedSpositives$type[infectees] == combinedSpositives$type[infectors]))
#   )
# at this point we prune the network 
# at the moment this cuts it down to the top 5 best links for each node.
# this means that more dense regions have to traverse further compared to those in sparser regions, although in sparse regions there is a distance penaly.
# alternatives here could be some weight based cut-off which would mean high order nodes in high density areas
# map = map %>% group_by(finalid.infectee) %>% arrange(desc(weight)) %>% filter(row_number() < 5) %>% ungroup()
# map = map %>% filter(distance < 4)

rm(map,ranked)


## label propagation ----



makePrediction = function(
  combinedSpositives,
  map,
  maxSequencedDate,
  minPredictionDate
) {

  # get the points which we knwo a sequence for
  labelled = combinedSpositives %>% filter(sequenced & date<=maxSequencedDate) %>% select(finalid,type)
  
  # the points we want to predict are those which are not labelled
  # This lets us predict labelled points
  #ends = combinedSpositives %>% filter(!sequenced) %>% filter(date > minPredictionDate) %>% select(finalid)
  ends = combinedSpositives %>% anti_join(labelled, by="finalid") %>% filter(date > minPredictionDate) %>% select(finalid)
  
  # everything else (i.e. not an end  and not a labelled point) participates passively as a node in the transmission network.
  
  # essentiallty the alogrithm starts at the ends we are interested in predicting and 
  # travererses backwards in the spatiotemporal network until it encounters a labelled node -  this is a infection with known variant status
  # each additional backwards step in the netwroks is a "stage"
  
  # This is the mapping from an "end" to all possible sequenced items at all stages
  allStages = NULL
  
  # This is the previous stage - initialised to the first backwards step from the ends
  # if there are more than 1 paths in the map they are combined
  prevStage = ends %>% 
    rename(finalid.infectee = finalid) %>% 
    inner_join(map, by="finalid.infectee") %>% 
    mutate(path=1) %>%
    group_by(finalid.infector,finalid.infectee, path) %>% 
    summarise(
      distance = ifelse(any(distance==0),0,1/sum(1/distance)),
      serial.interval = mean(serial.interval)
    ) %>% 
    ungroup()
  
  # we let the algorithm run for 10 iterations.
  for (stage in 1:10) {
    
    # if the infector is labelled it is a potential target for a given end
    thisStage = prevStage %>% semi_join(labelled, by=c("finalid.infector"="finalid"))
    allStages = allStages %>% bind_rows(thisStage)
    
    # otherwise we allow the 
    nextStage = prevStage %>% anti_join(labelled, by=c("finalid.infector"="finalid"))
    
    if(nrow(nextStage) > 1000000) {
      print("the number fo potential paths in the network grew uncontrollably and we terminated early to prevent a crash. The results are incomplete")
      # browser()
      break
    }
    # there are no more paths to follow
    if(nrow(nextStage) == 0) break
    
    #browser()
    
    # expand the paths backwards by one step
    prevStage =  map %>% rename(finalid=finalid.infectee) %>% 
      inner_join(
        nextStage %>% rename(finalid=finalid.infector), 
        by="finalid",
        suffix=c(".from",".to")
      )
    
    #prevStage %>% filter(finalid.infectee==-3252865) %>% glimpse()
    #prevStage %>% filter(finalid.infector==-3171034) %>% glimpse()
    
    # calculate the weight of each stage as the product of previous stages
    # TODO: could be distance here
    prevStage = prevStage %>% 
      # group_by(finalid.infector) %>%
      # filter(sum(weight.from) > 0.01) %>% 
      mutate(
        path=path+1, 
        distance = distance.to+distance.from, # resistors in series
        serial.interval = serial.interval.to+serial.interval.from
      ) %>%
      filter(serial.interval >= -2) %>% # This hard limit prevents serial interval from becoming very negative
      group_by(finalid.infector,finalid.infectee, path) %>% 
      summarise(
        distance = ifelse(any(distance==0),0,1/sum(1/distance)), # resistors in parallel
        serial.interval = mean(serial.interval)
      ) %>%
      group_by(finalid.infectee) %>%
      arrange(distance) %>%
      # take only the to 100 shortest resistance paths for a given end as otherwise we are likely to crash the system
      filter(row_number() < 100) %>%
      ungroup() 
    
    #prevStage %>% filter(finalid.infectee==-3252865 & finalid.infector==-3171034) %>% glimpse()
    
  }
  
  #browser()
  
  # we need to combine results that are found with different length paths
  # re-weighting contribution of a node based on its depth from the infectee as weights grow exponentially with path length due to increased number of potential routes.
  # this is demonstrated by ggplot(allStages,aes(x=path,y=weight/exp(path)))+geom_point()
  allMatched = allStages %>% 
    group_by(finalid.infector,finalid.infectee) %>% 
    summarise(
      distance = ifelse(any(distance==0),0,1/sum(1/distance)) #, # continuing the electrical analogy we combine these in parallel
      #minPath=min(path),
      #maxPath=max(path)
    ) 
  
  # assemble the full results
  # there may be some ends that were not matched - these are marked as "unknown" in the final result
  ends3 = ends %>% select(finalid.infectee = finalid) %>%
    left_join(allMatched, by="finalid.infectee") %>%
    #left_join(combinedSpositives %>% select(finalid,LSOA.infectee = LSOA_name, date.infectee = date), by=c("finalid.infectee"="finalid")) %>% # only really needed for debugging
    left_join(combinedSpositives %>% select(finalid,LSOA.infector = LSOA_name, date.infector = date, type), by=c("finalid.infector"="finalid")) %>%
    mutate(
      distance = ifelse(is.na(type),0,distance),
      type = ifelse(is.na(type),"unknown",type)
    )
  
  # combine all the matches of the same type to get a prediction score (weight and distance) for each found variant type.
  pred = ends3 %>% 
    group_by(finalid.infectee, type) %>% 
    summarise(
      # TODO: consider a type based scaling factor to account for increased infectivity?
      distance = ifelse(any(distance==0),0,1/sum(1/distance)),
      infectors = n()
    ) %>% 
    ungroup() %>%
    tidyr::complete(finalid.infectee,type,fill=list(distance=Inf,infectors=0)) 
  
  rm(ends3,allMatched)
  
  return(pred %>% ungroup())

}

## Perform prediction

maxSequencedDate = combinedSpositives %>% filter(sequenced) %>% pull(date) %>% max(na.rm = TRUE)
minPredictionDate = maxSequencedDate-14

pred = makePrediction(combinedSpositives, rankedMap, maxSequencedDate, minPredictionDate)

# mean(pred$distance[pred$distance!=Inf])
distanceToScore = function(d) ifelse(d==Inf, 0, exp( -log(2)/2*d ))

predictionComparison = function(pred, distanceToScore) {
  ## Post process prediction ----
  
  isVariant = pred %>% 
    mutate(
      is_variant = !(type %in% c("unclassified","unknown")),
      is_known = type != "unknown"
    ) %>% 
    group_by(finalid.infectee, is_variant) %>%
    summarise(
      distance = ifelse(any(distance==0),0,1/sum(1/distance, na.rm = TRUE)),
      infectors = sum(infectors)
    ) %>% ungroup() %>%
    mutate(
      score=distanceToScore(distance)
    ) %>%
    group_by(finalid.infectee) %>%
    mutate(
      variantScore = score/sum(score),
      variantScoreReliability = max(score)
    ) %>%
    ungroup() %>%
    filter(is_variant) %>% 
    select(-is_variant, -score)
  
  typePrediction = pred %>% ungroup() %>%
    tidyr::complete(finalid.infectee,type,fill=list(score=0, distance=Inf)) %>% 
    group_by(finalid.infectee) %>%
    mutate( score = distanceToScore(distance) ) %>%
    mutate(
      typeScore = score/sum(score),
      typeScoreReliability = max(score)
    ) %>%
    select(-distance, -infectors, -score) %>%
    pivot_wider(names_from=type,values_from=typeScore)
  
  comp = typePrediction %>%
    left_join(isVariant, by=c("finalid.infectee")) %>%
    inner_join(combinedSpositives %>% select(finalid,date,sequenced,sequencedType=type, exposure_type, ctas_status, asymptomatic_indicator, reinfect56,age,ethnicity_final,LSOA_code, LSOA_name, LTLA_code, LTLA_name), by=c("finalid.infectee"="finalid")) %>% 
    arrange(desc(date)) %>%
    select(finalid = finalid.infectee, date, LTLA_name, LTLA_code, variantScore, variantScoreReliability, everything()) 

  return(comp)
}

comp = predictionComparison(pred,distanceToScore)

excelOutput <- openxlsx::createWorkbook()
openxlsx::addWorksheet(excelOutput, as.character(Sys.Date()))

tmp = comp %>% select(-sequenced,-sequencedType,-exposure_type) %>% 
  select(date,finalid,LTLA_code,LTLA_name,age,ethnicity_final,asymptomatic_indicator,reinfect56) %>% #,everything()) %>%
  group_by(LTLA_name) %>%
  mutate(LTLA_symptomatics = sum(is.na(asymptomatic_indicator) | asymptomatic_indicator=="N")) %>%
  ungroup() %>%
  arrange(desc(LTLA_symptomatics,date)) 

openxlsx::writeData(excelOutput, sheet = as.character(Sys.Date()), x = tmp)
openxlsx::saveWorkbook(excelOutput, paste0("~/Dropbox/covid19/sa-variant/s-positive-potential-vocs-",Sys.Date(),".xlsx"), overwrite = TRUE)

# predict from 2 weeks ago
# maxSequencedDate14 = combinedSpositives %>% filter(sequenced) %>% pull(date) %>% max(na.rm = TRUE)-14
# minPredictionDate14 = maxSequencedDate14
# pred2 = makePrediction(combinedSpositives, rankedMap, maxSequencedDate14, minPredictionDate14)
# comp2 = predictionComparison(pred2,distanceToScore) %>% mutate(isVariant = ifelse(sequencedType != "unclassified","variant","wild-type")) %>% filter(sequenced & unknown != 1)
# roc2 = pROC::roc(response=comp2$isVariant, predictor=comp2$variantScore)
# roc2$auc
# 
# 
# # predict from 11 days ago
# maxSequencedDate7 = combinedSpositives %>% filter(sequenced) %>% pull(date) %>% max(na.rm = TRUE)-11
# minPredictionDate7 = maxSequencedDate7
# pred3 = makePrediction(combinedSpositives, rankedMap, maxSequencedDate7, minPredictionDate7)
# comp3 = predictionComparison(pred3,distanceToScore) %>% mutate(isVariant = ifelse(sequencedType != "unclassified","variant","wild-type")) %>% filter(sequenced & unknown != 1)
# roc3 = pROC::roc(response=comp3$isVariant, predictor=comp3$variantScore)
# # plot(roc)
# roc3$auc



# 
# devtools::load_all("~/Git/classifier-result/")
# cr = ClassifierResult$fromPredictions(inr.predictions,inr.obs)

# # get a best prediction for the type:
# pred2 = pred %>% 
#   group_by(finalid.infectee) %>% 
#   arrange(distance) %>% 
#   filter(row_number()==1)
# 
# # get a single score for the variant status
# pred3 = pred  %>% 
#   ungroup() %>% 
#   tidyr::complete(finalid.infectee,is_variant,fill=list(score=0, distance=Inf)) %>% 
#   group_by(finalid.infectee, is_variant, is_known) %>% 
#   summarise(
#     distance = ifelse(any(distance==0),0,1/sum(1/distance))
#   ) %>% 
#   group_by(finalid.infectee) %>%
#   mutate(
#     variantScore = weight/sum(weight), # normalised weight - not really a probability but range is 0 to 1 and high values favour variant.
#     variantProximity = (mean(distance,na.rm = TRUE)-distance)/mean(distance,na.rm = TRUE), # This is some sort of fractional difference from the mean of distance. A negative means less likely to be variant. A positive more likely
#     predictionQuality = sum(weight),
#   ) %>% 
#   filter(is_variant) %>% 
#   select(-is_variant)
# 
# # rate	0.24 ± 0.04 (0.18; 0.30)
# # shape	1.38 ± 0.24 (1.00; 1.78)
# 
# pred3 %>% 
#   inner_join(ll2, by=c("finalid.infectee"="FINALID")) %>% 
#   #mutate(infectivityRisk = 1-pgamma(as.numeric(Sys.Date()-specimen_date),rate=0.24,shape=1.38)) %>% 
#   arrange(desc(specimen_date),desc(variantScore)) %>%
#   select(finalid = finalid.infectee, specimen_date,variantScore,predictionQuality,everything()) %>% 
#   readr::write_csv(paste0("~/Dropbox/covid19/sa-variant/is-variant-prediction-",minPredictionDate,"-as-of-",Sys.Date(),".csv"))

# colour scheme

colourScheme = tribble(
  ~type, ~hue, ~sat, ~value,
  "B.1.351", 0,1,1,
  "B.1.617.x", 0.2,1,1,
  "P.1 & P.2", 0.8,1,1,
  "B.1.1.318", 0.1,1,1,
  "other VOC/VUI", 0.9,1,1,
  "B.1.1.7", 0.5,0,1,
  "B.1.525", 0.5,0,1,
  "unclassified", 2/3,1,1,
  "unknown", 0,0,0
)

pred4 = pred %>% 
  group_by(finalid.infectee) %>%
  inner_join(colourScheme, by="type") %>%
  mutate( score = distanceToScore(distance) ) %>%
  mutate(
    typeScore = score/sum(score),
    sin = sin(2*pi*hue),
    cos = cos(2*pi*hue)
  ) %>%
  arrange(desc(typeScore)) %>%
  summarise(
    wtSin = weighted.mean(sin,typeScore),
    wtCos = weighted.mean(cos,typeScore),
    sat = sqrt(wtSin^2+wtCos^2),
    hue = atan(wtSin/wtCos)/pi/2,
    value = 1,
    typePercentages = paste0(sprintf("%s: %1.0f%%",type,typeScore*100), collapse = "; "),
    typeScoreReliability = max(score)
  ) %>%
  mutate(
    # correct arctan angle on colour wheel.
    hue = case_when(
      wtCos<0 ~ 0.5+hue,
      wtSin<0 ~ 1+hue,
      TRUE ~ hue)
  )  %>% 
  mutate(
    colour = hsv(hue,sat,value)
  )


## Visualise result in leaflet ----



set.seed(101)

tmp4 = combinedSpositives %>% 
  filter(date > "2021-02-01") %>% 
  left_join(colourScheme, by="type", suffix=c("",".obs")) %>%
#  left_join(pred4,by=c("finalid"="finalid.infectee"),suffix=c("",".col")) %>%
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
dpc$unloadCaches()

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
