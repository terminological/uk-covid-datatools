## code to prepare `NHSCatchmentAreas` dataset goes here
library(ensurer)
library(tidyverse)
setwd("~/Git/uk-covid-datatools/data-raw/")
source("./rawDataFunctions.R")
devtools::load_all()

#### Wards shapefile ----

# TODO: make this a generic way of getting all demographics for random areas
wards2011 = getShapefile("Wards_December_2011_Boundaries_EW_BFE", 
                         "https://opendata.arcgis.com/datasets/f04efac388f049508ecf91cafbe70343_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D", simplify=FALSE)
wards2011 = wards2011 %>% rename(WD11CD = wd11cd, WD11NM=wd11nm)

# This is the 2018 demographics projected onto the 2011 Ward structure, at a total population level.
demogByWard2011 = UKDemographics2018$by2011Ward %>% group_by(WD11CD) %>% summarise(count = sum(count))

wards2011 = wards2011 %>% inner_join(demogByWard2011, by="WD11CD") 
  #left_join(demogByWard2011, by="WD11CD")

#View(wards2011 %>% rename(WD11CD = wd11cd) %>% anti_join(tmp, by="WD11CD") %>% as_tibble())
#View(tmp %>% anti_join(wards2011 %>% rename(WD11CD = wd11cd), by="WD11CD") %>% as_tibble())

# plot areas with max number of edges
# tmp = wards2011 %>% left_join(edges %>% group_by(from) %>% count(), by=c("objectid"="from"))
# ggplot(tmp)+geom_sf(aes(fill=n),lwd=0)

# centroids = wards2011 %>% sf::st_centroid()
# areas = wards2011 %>% sf::st_area() not required as st_areasha gives this already in m^2
# nodes = centroids %>% mutate(long = sapply(geometry,"[",1), lat=sapply(geometry,"[",2)) %>% rename(id = objectid) # %>% as_tibble() %>% select(-geometry)

# TODO: make a function out of this so I never need to do it again:
# TODO: Nightingale hospitals

# locate hospitals on the map
hospSf = sf::st_as_sf(NHSCapacity2019$hospitals, coords=c("long","lat"), crs=4326) %>% mutate(tmp_hosp_id=row_number())
wards2011 = wards2011 %>% mutate(tmp_ward_id = row_number())
# map the hospitals into 2011 wards
hospLoc = wards2011 %>% sf::st_contains(hospSf)
ward2Hosp = tibble(
  tmp_ward_id = rep(1:length(hospLoc),sapply( hospLoc, length)),
  tmp_hosp_id = unlist(hospLoc %>% purrr::flatten()) 
) %>% distinct()
ward2Hosp = ward2Hosp %>%
  left_join(wards2011 %>% as_tibble() %>% select(tmp_ward_id, WD11CD), by="tmp_ward_id") %>%
  left_join(hospSf %>% as_tibble() %>% select(tmp_hosp_id, hospitalId), by="tmp_hosp_id") %>%
  select(-tmp_ward_id, -tmp_hosp_id)

# eyeball check:
# ggplot(hospSf) + geom_sf()

# some wards have more than one hospital in them. This is often because they are the same trust
ward2Hosp %>% group_by(WD11CD) %>% count() %>% filter(n>1)

# View(nodes %>% as_tibble() %>% inner_join(ward2Hosp, by=c("id"="id")) %>% inner_join(hospSf, by=c("tmp_id"="tmp_id")))
# aggregate to level of trusts - i.e. find wards with one or more hospitals from NHS trusts in them
hospSf = hospSf %>% inner_join(NHSCapacity2019$trusts, by="trustId", suffix=c("",".rhs")) %>% 
  select(hospitalId, name, sector, pcds, trustId, trustName, hasIcu, hasAccidentAndEmergency, hasSurgicalDepartment, acuteBeds, dayBeds, icuBeds, geometry)
NHSHospSf = hospSf %>% filter(acuteBeds > 0 & sector=="NHS Sector")
icuNHSHospSf = hospSf %>% filter(icuBeds > 0 & hasIcu & sector=="NHS Sector")

# TODO: remove code duplication

ward2IcuTrust = ward2Hosp %>% inner_join(icuNHSHospSf, by="hospitalId", suffix=c("",".rhs")) %>% select(WD11CD, trustId, beds = icuBeds) %>% distinct()
ward2AcuteTrust = ward2Hosp %>% inner_join(NHSHospSf, by="hospitalId", suffix=c("",".rhs")) %>% select(WD11CD, trustId, beds = acuteBeds) %>% distinct()

# 2 wards actually have more than one ICU in them:
ward2IcuTrust %>% group_by(WD11CD) %>% mutate(n_t_per_w=n()) %>% filter(n_t_per_w>1) %>% 
  ungroup() %>% group_by(trustId) %>% mutate(n_w_per_t = n()) %>% left_join(NHSCapacity2019$trusts, by="trustId")
# this is problematic for us as the label propagation method requires only one label
# their different locations don't span ward boundaries either
# maybe this would not be a problem if we were doing this by LSOA
# as it is we'll have to merge those trusts and unmerge at the end.
# this is going to be OK as essentially these are tertiary level ITUs - e.g papworth, marsden, brompton.

# trusts frequently span wards. This is OK as there can be multiple staring points
# ward2IcuTrust %>% group_by(trustId) %>% mutate(n=n()) %>% filter(n>1) %>% left_join(nodes, by="WD11CD") %>% left_join(NHSCapacity2019$trusts, by="trustId")

# we are concatenating the labels....
ward2IcuTrust = ward2IcuTrust %>% group_by(WD11CD) %>% summarise(trustId = paste0(trustId,collapse = "|"), beds = sum(beds)) %>% distinct() %>%
  ensure_that(!any(duplicated(.$WD11CD)))
ward2AcuteTrust = ward2AcuteTrust %>% group_by(WD11CD) %>% summarise(trustId = paste0(trustId,collapse = "|"), beds = sum(beds)) %>% distinct() %>%
  ensure_that(!any(duplicated(.$WD11CD)))



# eyeball check: Icus and wards match
leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addPolygons(data=(wards2011 %>% semi_join(ward2IcuTrust,by="WD11CD"))) %>%
  leaflet::addCircleMarkers(data=icuNHSHospSf, color="#FF0000", popup = as.character(icuNHSHospSf$icuBeds))

# eyeball check: Icus and wards match
# leaflet::leaflet() %>% 
#   leaflet::addTiles() %>% 
#   leaflet::addPolygons(data=(wards2011 %>% semi_join(ward2AcuteTrust,by="WD11CD"))) %>% 
#   leaflet::addCircleMarkers(data=NHSHospSf, color="#FF0000", popup = as.character(NHSHospSf$acuteBeds))

createResourceMap = function(
  demandMap, # the demand map with density - TODO
  resourceProvider, # the supply map with capacity - TODO move this into function - to need a SF object with location only
  growthRates = function(capacityPerDemand, multiplier = 1.1) {return( rank(capacityPerDemand) / length(capacityPerDemand) * multiplier )},
  distanceModifier = function(distanceToSupply) {return(2/(1+distanceToSupply/min(0.1,mean(distanceToSupply))))}
) {
  
  if (any(duplicated(resourceProvider$WD11CD))) stop("more than one resource in a single geography. Try merging them.")
  
  areaNetwork = createNeighbourNetwork(demandMap, WD11CD) %>% 
    rename(fromAreaId = from, toAreaId = to)
  
  
  entireArea = demandMap %>% select(areaId=WD11CD, WD11NM, areaSize=st_areasha, areaDemand=count) %>% 
    semi_join(areaNetwork, by=c("areaId"="toAreaId")) %>% as_tibble()
  # ensure entire area is connected - otherwise we get problems with islands
  # browser()
   
  areasWithResource = entireArea %>% 
    inner_join(resourceProvider %>% rename(areaId = WD11CD), by="areaId") %>% 
    rename(supplierId = trustId, supplyCapacity = beds) %>% as_tibble()
  
  suppliedArea = areasWithResource %>% mutate(
    distanceToSupply = 0,
    demandDistanceToSupply = 0,
    accumulatedGrowth = 0,
    iteration = 0
  )
  
  remaining = NULL
  # loop?
  repeat {
    suppliedArea = suppliedArea %>% group_by(supplierId) %>% mutate(
      totalDemand = sum(areaDemand),
      capacityPerDemand = supplyCapacity/totalDemand
    ) %>% ungroup() 
    
    unSuppliedArea = entireArea %>% anti_join(suppliedArea, by="areaId")
    message("areas remaining: ",nrow(unSuppliedArea))
    if(nrow(unSuppliedArea) == 0) break
    remaining = c(remaining,nrow(unSuppliedArea))
    if (sum(remaining == nrow(unSuppliedArea)) > 4) {
      warning("terminating early with missing areas - it looks like ",nrow(unSuppliedArea), " areas are not connected")
      break;
    }
    # stop if unsupplied area is empty
    
    # growing areas are - A) adjoin unsupplied areas B) have accumulated enough to grow
    unSuppliedNeighbourAreas = areaNetwork %>% inner_join(unSuppliedArea, by=c("toAreaId"="areaId")) %>% rename(areaId = toAreaId)
    intoUnsupplied = unSuppliedNeighbourAreas %>% group_by(fromAreaId) %>% summarise(newAreas = n(), newAreaDemand = sum(areaDemand))
    
    # TODO: this definintion only allows growth into areas that are not already supplied
    growingArea = suppliedArea %>% rename(fromAreaId = areaId) %>% 
      inner_join(intoUnsupplied, by=c("fromAreaId"), suffix=c(".old","")) %>% 
      mutate( 
        newTotalDemand = totalDemand+newAreaDemand,
        newCapacityPerDemand = supplyCapacity/newTotalDemand
        ) %>%
      mutate(
        accumulatedGrowth = accumulatedGrowth + growthRates(newCapacityPerDemand) #*distanceModifier(distanceToSupply) #TODO: make this work
      )
    # browser()
    suppliedArea = suppliedArea %>% left_join(
      growingArea %>% select(areaId = fromAreaId, newAccumulated = accumulatedGrowth), by="areaId") %>% 
      mutate(accumulatedGrowth = ifelse(!is.na(newAccumulated), newAccumulated, accumulatedGrowth)) %>%
      select(-newAccumulated)
    
    
    newlySuppliedArea = growingArea %>% filter(accumulatedGrowth > 1) %>% inner_join(unSuppliedNeighbourAreas, by="fromAreaId", suffix=c(".from",""))
    newlySuppliedArea = newlySuppliedArea %>%
      mutate(
        distanceToSupply = distanceToSupply + sqrt(areaSize.from),
        demandDistanceToSupply = demandDistanceToSupply + areaDemand.from,
        accumulatedGrowth = accumulatedGrowth - 1,
        iteration = iteration + 1
      ) %>%
      select(-ends_with(".from"), -fromAreaId)
    
    # ensure only one supplier gets the target area
    newlySuppliedArea = newlySuppliedArea %>% group_by(areaId) %>% arrange(desc(capacityPerDemand),desc(distanceToSupply)) %>% filter(row_number() <=1) %>% ensure_that(length(unique(.$areaId)) == length(.$areaId))
    
    #browser()
    
    suppliedArea = suppressWarnings(bind_rows(suppliedArea, newlySuppliedArea)) %>% ensure_that(length(unique(.$areaId)) == length(.$areaId))
    
    
  }
  out = list(
    suppliedArea = suppliedArea %>% select(-accumulatedGrowth) %>% sf::st_as_sf(),
    notSuppliedArea = unSuppliedArea %>% sf::st_as_sf()
  )
  return(out)
  
}


icuCatchmentMap = createResourceMap(
  wards2011, 
  ward2IcuTrust)

lookupWD11toICU = icuCatchmentMap$suppliedArea %>% as_tibble() %>% select(WD11CD = areaId, WD11NM, trustId=supplierId) %>% inner_join(NHSCapacity2019$trusts %>% select(trustId, trustName), by="trustId")

icuCatchment = icuCatchmentMap$suppliedArea %>% group_by(supplierId) %>% summarise(
  population=sum(areaDemand),
  beds=first(supplyCapacity),
  bedsPer100K = 100000*first(supplyCapacity)/sum(areaDemand)
)

icu = list(
  lookupWD11CD = lookupWD11toICU,
  map = icuCatchment %>% sf::st_simplify(dTolerance=0.001),
  hospitals = icuNHSHospSf
)

# icuCatchment = icuCatchmentMap %>% group_by(areaId) %>% unionByGroup(
#   demog=sum(areaDemand),
#   beds=first(supplyCapacity),
#   bedsPer100K = 100000*first(supplyCapacity)/sum(areaDemand)
# )

leaflet::leaflet(icu$map) %>% 
  leaflet::addTiles() %>% 
  leaflet::addPolygons(popup=as.character(icu$map$bedsPer100K)) %>% 
  leaflet::addCircleMarkers(data=icu$hospitals, color="#FF0000", popup = as.character(icu$hospitals$icuBeds))

# TODO: convert icuCatchmentMap into 
# A - ward to icuTrust mapping and 
# B - shapefile (do we need to worry about naming)
# plot icuCatchmentMap as animation by iteration

acuteCatchmentMap = createResourceMap(
  wards2011, 
  ward2AcuteTrust)

lookupWD11toAcute = acuteCatchmentMap$suppliedArea %>% as_tibble() %>% select(WD11CD = areaId, WD11NM, trustId=supplierId) %>% inner_join(NHSCapacity2019$trusts %>% select(trustId, trustName), by="trustId")

acuteCatchment = acuteCatchmentMap$suppliedArea %>% group_by(supplierId) %>% summarise(
  population=sum(areaDemand),
  beds=first(supplyCapacity),
  bedsPer100K = 100000*first(supplyCapacity)/sum(areaDemand)
)

acute = list(
  lookupWD11CD = lookupWD11toAcute,
  map = acuteCatchment %>% sf::st_simplify(dTolerance=0.001),
  hospitals = NHSHospSf
)

object.size(acute)

leaflet::leaflet(acute$map) %>% 
  leaflet::addTiles() %>% 
  leaflet::addPolygons(popup=as.character(acute$map$bedsPer100K)) %>% 
  leaflet::addCircleMarkers(data=acute$hospitals, color="#FF0000", popup = as.character(acute$hospitals$acuteBeds))


NHSCatchment2019 = list(
  icu = icu,
  acute = acute
)

usethis::use_data(NHSCatchment2019,overwrite = TRUE)

# View(suppliedArea)
# 
# catchmentMap = suppliedArea %>% select(id=areaId,trustId = supplierId,demog=areaDemand,beds=supplyCapacity)
# # catchmentMap2 = catchmentMap %>% aggregate(by=catchmentMap$trustId, FUN=mean)
# catchmentMap2 = catchmentMap %>% group_by(trustId) %>% group_modify(function(d,g,...) {
#   return(tibble(
#     geometry=d %>% sf::st_union(),
#     demog=sum(d$demog),
#     beds=first(d$beds),
#     bedsPer100K = 100000*first(d$beds)/sum(d$demog)
#   )
#   )
# }) %>% sf::st_as_sf(crs=4326)
# ggplot(catchmentMap2)+geom_sf(aes(fill=bedsPer100K))
# 
# pal <- leaflet::colorNumeric(
#   palette = "Blues",
#   domain = catchmentMap2$bedsPer100K)
# leaflet::leaflet(catchmentMap2) %>% leaflet::addTiles() %>% leaflet::addPolygons(popup=as.character(catchmentMap2$bedsPer100K)) %>% leaflet::addCircleMarkers(data=icuNHSHospSf, color="#FF0000", popup = as.character(icuNHSHospSf$icuBeds))
# 


stop()



#### ----
# 
# startNodes = nodes %>% filter(wd11cd %>% stringr::str_starts("E")) %>% select(id,st_areasha,demog) %>% left_join(ward2IcuTrust %>% mutate(steps=1), by="id") %>% mutate(
#   stepDistance=0,
#   pathDistance=0,
#   score = 0,
#   # weight = 1/((1+pathDistance)^2)*beds,
#   bedsPerPerson = beds/demog
# ) %>% mutate(
#   # weight = ifelse(is.na(weight),0,weight),
#   steps = ifelse(is.na(steps),0,steps),
#   beds = ifelse(is.na(beds),0,beds),
#   bedsPerPerson = ifelse(is.na(bedsPerPerson),0,bedsPerPerson)
# ) %>% as_tibble()
# step = 1
# 
# repeat {
#   
#   maxBedsPerPerson = max(startNodes$bedsPerPerson)
#   startNodes = startNodes %>% mutate(score = ifelse(steps==step, score+bedsPerPerson/maxBedsPerPerson, score))
#   if(nrow(startNodes %>% filter(steps==step)) == 0) break
#   startNodes = startNodes %>% mutate(steps=ifelse((steps==step & score<1),steps+1,steps))
#   prevNodes = startNodes %>% filter(steps==step)
#   
#   
#   message("label propagation: step ",step)
#   nextNodes = prevNodes %>% inner_join(edges, by=c("id"="from")) %>% inner_join(startNodes, by=c("to"="id"), suffix=c("",".next"))
#   #if(nrow(nextNodes)==0) break
#   nextNodes = nextNodes %>% mutate(
#     id = to,
#     stepDistance = (sqrt(st_areasha)+sqrt(st_areasha.next))/avStepDistance,
#     steps = steps+1,
#     score = score-1
#   ) %>% mutate( 
#     pathDistance = pathDistance+stepDistance
#     # weight = beds*(1/((1+pathDistance)^2))
#   ) %>% filter(is.na(steps.next) | steps.next != 1) # prevent overriding a hospital in that ward
#   #browser()
#   nextNodes = nextNodes %>% group_by(id) %>% top_n(1,desc(bedsPerPerson)) #arrange(desc(bedsPerPerson)) %>% mutate( tmp = row_number()) %>% filter(tmp==1) %>% select(-tmp)
#   
#   nextNodes = nextNodes %>% select(-ends_with(".next"),-to)
#   
#   tmp = startNodes %>% left_join(nextNodes, by="id",suffix=c("",".new")) %>% mutate(
#     # weight.new = ifelse(is.na(weight.new),0,weight.new), 
#     bedsPerPerson.new = ifelse(is.na(bedsPerPerson.new),0,bedsPerPerson.new)
#     
#   )
#   
#   tmp2 = tmp %>% mutate(
#     score = ifelse(is.na(trustId), score.new, score),
#     beds =  ifelse(is.na(trustId), beds.new, beds),
#     steps = ifelse(is.na(trustId), steps.new, steps),
#     stepDistance = ifelse(is.na(trustId), stepDistance.new, stepDistance),
#     pathDistance = ifelse(is.na(trustId), pathDistance.new, pathDistance),
#     trustId = ifelse(is.na(trustId), trustId.new, trustId)
#     # weight = ifelse(is.na(trustId), weight.new, weight)
#   ) %>% select(-ends_with(".new"))
#   # tmp2 = tmp %>% mutate(
#   #   trustId = ifelse(weight < weight.new & bedsPerPerson < bedsPerPerson.new, trustId.new, trustId),
#   #   beds = ifelse(weight < weight.new & bedsPerPerson < bedsPerPerson.new, beds.new, beds),
#   #   steps = ifelse(weight < weight.new & bedsPerPerson < bedsPerPerson.new, steps.new, steps),
#   #   stepDistance = ifelse(weight < weight.new & bedsPerPerson < bedsPerPerson.new, stepDistance.new, stepDistance),
#   #   pathDistance = ifelse(weight < weight.new & bedsPerPerson < bedsPerPerson.new, pathDistance.new, pathDistance),
#   #   weight = ifelse(weight < weight.new & bedsPerPerson < bedsPerPerson.new, weight.new, weight)
#   # ) %>% select(-ends_with(".new"),-to)
#   
#   tmp3 = tmp2 %>% group_by(trustId) %>% mutate(bedsPerPerson = beds/sum(demog)) %>% ungroup()
#   
#   browser()
#   
#   startNodes = tmp3
#   step = step+1
# }
# 
# # for (i in 1:5) {
# #  prevNodes = startNodes
# #  message("bed density adjustment: ",i)
# #  nextNodes = prevNodes %>% inner_join(edges, by=c("id"="from")) %>% inner_join(prevNodes, by=c("to"="id"), suffix=c(".old",""))
# #  switchNodes = nextNodes %>% filter(
# #     bedsPerPerson.old < bedsPerPerson &
# #     trustId != trustId.old
# #  ) 
# #  switchNodes = switchNodes %>% group_by(id) %>% mutate(tmp_delta = bedsPerPerson-bedsPerPerson.old) %>% top_n(1, tmp_delta) %>% select(-tmp_delta) %>% ungroup()
# #  switchNodes = switchNodes %>% group_by(trustId.old,trustId) %>% mutate(tmp_path = pathDistance - pathDistance.old) %>% top_n(5, tmp_path) %>% select(-tmp_path) %>% ungroup() %>% 
# #    mutate(
# #     steps = steps+1,
# #     stepDistance = stepDistance.old,
# #     pathDistance = pathDistance+stepDistance.old) %>%
# #    select(-ends_with(".old"),-to) %>% ungroup()
# #  
# #  startNodes = prevNodes %>% anti_join(switchNodes, by="id") %>% bind_rows(switchNodes)
# #  startNodes = startNodes %>% group_by(trustId) %>% mutate(bedsPerPerson = beds/sum(demog)) %>% ungroup()
# # }
# 
# 
# 
# catchmentMap = wards2011 %>% left_join(startNodes %>% select(id,trustId,demog,beds), by=c("objectid"="id"), suffix=c("",".none"))
# # catchmentMap2 = catchmentMap %>% aggregate(by=catchmentMap$trustId, FUN=mean)
# catchmentMap2 = catchmentMap %>% group_by(trustId) %>% group_modify(function(d,g,...) {
#   return(tibble(
#     geometry=d %>% sf::st_union(),
#     demog=sum(d$demog),
#     beds=first(d$beds),
#     bedsPer100K = 100000*first(d$beds)/sum(d$demog)
#   )
#   )
# }) %>% sf::st_as_sf(crs=4326)
# ggplot(catchmentMap2)+geom_sf(aes(fill=bedsPer100K))
# 
# pal <- leaflet::colorNumeric(
#   palette = "Blues",
#   domain = catchmentMap2$bedsPer100K)
# leaflet::leaflet(catchmentMap2) %>% leaflet::addTiles() %>% leaflet::addPolygons(popup=as.character(catchmentMap2$bedsPer100K)) %>% leaflet::addCircleMarkers(data=icuNHSHospSf, color="#FF0000", popup = as.character(icuNHSHospSf$icuBeds))
# 
# 
# stop()
### N.B. NHSCapacity2019.R must be run first (or devtools::load_all())

UKWardLookup2019 <- read_csv( 
  "https://opendata.arcgis.com/datasets/e169bb50944747cd83dcfb4dd66555b1_0.csv",
  col_name=TRUE,cols(
    WD19CD = col_character(),
    WD19NM = col_character(),
    LAD19CD = col_character(),
    LAD19NM = col_character(),
    FID = col_integer()
  ))

LAD2019 = UKWardLookup2019 %>% select(LAD19CD,LAD19NM) %>% distinct()

postcodesZip <- "~/Git/uk-covid-datatools/data-raw/Postcodes/postcodes.zip"
if(!file.exists(postcodesZip)) {
  download.file("https://www.doogal.co.uk/files/postcodes.zip",postcodesZip)
  wd = getwd()
  setwd("~/Git/uk-covid-datatools/data-raw/Postcodes")
  unzip(postcodesZip,"postcodes.csv")
  setwd(wd)
}

postcodes <- read_csv("~/Git/uk-covid-datatools/data-raw/Postcodes/postcodes.csv", col_types = 
                        cols(
                          .default = col_character(),
                          Latitude = col_double(),
                          Longitude = col_double(),
                          Easting = col_double(),
                          Northing = col_double(),
                          County = col_character(),
                          Introduced = col_date(format = "%Y-%m-%d"),
                          Terminated = col_date(format = "%Y-%m-%d"),
                          Parish = col_character(),
                          `National Park` = col_character(),
                          Population = col_integer(),
                          Households = col_integer(),
                          `Built up area` = col_character(),
                          `Built up sub-division` = col_character(),
                          Region = col_character(),
                          Altitude = col_double(),
                          `London zone` = col_integer(),
                          `Local authority` = col_character(),
                          `Parish Code` = col_character(),
                          `Index of Multiple Deprivation` = col_integer(),
                          Quality = col_integer(),
                          `User Type` = col_double(),
                          `Last updated` = col_date(format = "%Y-%m-%d"),
                          `Nearest station` = col_character(),
                          `Distance to station` = col_double(),
                          `Postcode area` = col_character(),
                          `Postcode district` = col_character(),
                          `Police force` = col_character(),
                          `Water company` = col_character(),
                          `Plus Code` = col_character(),
                          `Average Income` = col_double()
                        ))




# https://geoportal.statistics.gov.uk/datasets/postcode-to-output-area-hierarchy-with-classifications-november-2019-lookup-in-the-uk     
# https://www.arcgis.com/sharing/rest/content/items/acbbb701a42146f693a158c755517a81/data
postcodes2LADZip <- "~/Git/uk-covid-datatools/data-raw/Postcodes/postcodes2Lad.zip"
if(!file.exists(postcodes2LADZip)) {
  download.file("https://www.arcgis.com/sharing/rest/content/items/acbbb701a42146f693a158c755517a81/data",postcodes2LADZip)
  wd = getwd()
  setwd("~/Git/uk-covid-datatools/data-raw/Postcodes")
  unzip(postcodes2LADZip,"NSPCL_NOV19_UK_LU.csv")
  setwd(wd)
}
postcode2LAD_tmp = read_csv("~/Git/uk-covid-datatools/data-raw/Postcodes/NSPCL_NOV19_UK_LU.csv", col_types = cols(
  pcd7 = col_character(),
  pcd8 = col_character(),
  pcds = col_character(),
  dointr = col_double(),
  doterm = col_double(),
  usertype = col_double(),
  oseast1m = col_double(),
  osnrth1m = col_character(),
  oa11cd = col_character(),
  oac11cd = col_character(),
  oac11nm = col_character(),
  wz11cd = col_character(),
  wzc11cd = col_character(),
  wzc11nm = col_character(),
  lsoa11cd = col_character(),
  lsoa11nm = col_character(),
  msoa11cd = col_character(),
  msoa11nm = col_character(),
  soac11cd = col_character(),
  soac11nm = col_character(),
  ladcd = col_character(),
  ladnm = col_character(),
  ladnmw = col_character(),
  laccd = col_character(),
  lacnm = col_character()
))
# includes OS locations

postcode2LAD_tmp


postcode2LAD = postcode2LAD_tmp %>% filter(!is.na(ladcd)) %>% left_join(postcodes %>% select(Postcode,lat=Latitude,long=Longitude,pop=Population,houses=Households,deprivation=`Index of Multiple Deprivation`) %>% distinct(), by=c("pcds"="Postcode"))
postcode2LAD = postcode2LAD %>% filter(!is.na(lat))
# all some sort of temporary test stuff in there with NA latitudes

# rm(postcodes, postcode2LAD_tmp)

# Check every LAD is in this postcodes table
LAD2019 %>% anti_join(postcode2LAD, by=c("LAD19CD"="ladcd")) %>% ensure_that(nrow(.)==0)

# quality checks
postcode2LAD %>% 
  ensure(length(unique(.$pcds)) == length(.$pcds)) %>% # pcds are unique
  ensure(all(!is.na(.$lat))) %>% # everything has a lat and a long
  ensure(all(!is.na(.$long))) %>%
  ensure(all(!is.na(.$ladcd))) %>%
  invisible()

# any(is.na(NHSCapacity2019$hospitals$long)) == FALSE
# any(is.na(NHSCapacity2019$hospitals$lat)) == FALSE


# find the nearest hospital to all post codes

# calculate the fraction of the population of a LAD that each post code represents using the population data from doogal
# if there isn't one
postcode2LAD = postcode2LAD %>% group_by(ladcd) %>% mutate(pop = ifelse(is.na(sum(pop)),1,pop)) %>% mutate(ladPop = sum(pop,na.rm=TRUE)) %>% ungroup()
postcode2LAD = postcode2LAD %>% mutate(fracPop = pop/ladPop) %>% select(-pop, -ladPop)

View(postcode2LAD %>% filter(is.na(fracPop)))

# check by LAD fracPops add up to 1.
postcode2LAD %>% group_by(ladcd) %>% summarise(fracTot = sum(fracPop)) %>% ensure_that(all.equal(rep(1,length(.$fracTot)),.$fracTot)) %>% invisible()

# aggregate to LAD, calculation fractions of population and assign them to a hospital
acuteBedHospitals = NHSCapacity2019$hospitals %>% semi_join(NHSCapacity2019$trusts %>% filter(acuteBeds>0), by="trustId")
postcode2Acute = postcode2LAD %>% ungroup() %>% tidyinfostats::findKNN(acuteBedHospitals, pcds, hospitalId, k = 1, matchVars = vars(lat,long))
LAD2Hospital_acute = postcode2LAD %>% inner_join(postcode2Acute, by="pcds") %>% group_by(ladcd, hospitalId) %>% summarise(fracPop = sum(fracPop,na.rm=TRUE))
LAD2Trust_acute = LAD2Hospital_acute %>% inner_join(acuteBedHospitals, by="hospitalId") %>% group_by(ladcd,trustId) %>% summarise(fracPop = sum(fracPop,na.rm=TRUE))

# every LAD must have at least 1 hospital and trust
LAD2019 %>% anti_join(LAD2Trust_acute, by=c("LAD19CD"="ladcd")) %>% ensure_that(nrow(.)==0) %>% invisible()
LAD2019 %>% anti_join(LAD2Hospital_acute, by=c("LAD19CD"="ladcd")) %>% ensure_that(nrow(.)==0) %>% invisible()

# aggregate hospitals to NHS trusts
icuBedHospitals = NHSCapacity2019$hospitals %>% filter(hasIcu)
postcode2ICU = postcode2LAD %>% ungroup() %>% tidyinfostats::findKNN(icuBedHospitals, pcds, hospitalId, k = 1, matchVars = vars(lat,long))
LAD2Hospital_icu = postcode2LAD %>% inner_join(postcode2ICU, by="pcds") %>% group_by(ladcd, hospitalId) %>% summarise(fracPop = sum(fracPop,na.rm=TRUE))
LAD2Trust_icu = LAD2Hospital_icu %>% inner_join(icuBedHospitals, by="hospitalId") %>% group_by(ladcd,trustId) %>% summarise(fracPop = sum(fracPop,na.rm=TRUE))

# every LAD must have at least 1 hospital and trust
LAD2019 %>% anti_join(LAD2Trust_icu, by=c("LAD19CD"="ladcd")) %>% ensure_that(nrow(.)==0) %>% invisible()
LAD2019 %>% anti_join(LAD2Hospital_icu, by=c("LAD19CD"="ladcd")) %>% ensure_that(nrow(.)==0) %>% invisible()



NHSCatchmentAreas = list(
  acuteBeds = LAD2Trust_acute %>% group_by(ladcd) %>% mutate(fracPop = fracPop/sum(fracPop)) %>% rename(fractionOfLADPopulation = fracPop) %>% filter(!is.na(ladcd)),
  icuBeds = LAD2Trust_icu %>% group_by(ladcd) %>% mutate(fracPop = fracPop/sum(fracPop)) %>% rename(fractionOfLADPopulation = fracPop) %>% filter(!is.na(ladcd))
)

usethis::use_data(NHSCatchmentAreas,overwrite = TRUE)

# NHSCatchmentAreas %>% group_by(ladcd) %>% summarise(frac = sum(fractionOfLADPopulation)) %>% filter(frac != 1) # should be empty

# TODO: lots of possible options here

rm(acuteBedHospitals, icuBedHospitals, LAD2Hospital, LAD2Hospital_acute, LAD2Hospital_icu, LAD2Trust_acute, LAD2Trust_icu, postcode2Acute, postcode2Hospital, postcode2ICU, 
   postcode2LAD, postcode2LAD_tmp, postcodes)

rm(postcodes2LADZip, postcodesZip)