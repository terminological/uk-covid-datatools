## code to prepare `UK2019Demographics` dataset goes here

library(readxl)
library(tidyverse)
devtools::load_all("~/Git/uk-covid-datatools/")
setwd("~/Git/uk-covid-datatools/data-raw/")
source("./rawDataFunctions.R")

#### High level estimates --- somewhat line up with shapefile

# url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
# destfile <- "~/Git/uk-covid-datatools/data-raw/Population/ukmidyearestimates20182019ladcodes.xls"
# if (!file.exists(destfile)) {
#   curl::curl_download(url, destfile)
# }
# ukmidyearestimates20182019ladcodes <- readxl::read_excel(path.expand(destfile), sheet="MYE2-All", skip = 4)
# ageCols = colnames(ukmidyearestimates20182019ladcodes)[!is.na(as.integer(colnames(ukmidyearestimates20182019ladcodes)))]
# tmp = ukmidyearestimates20182019ladcodes %>% 
#   tidyr::pivot_longer(cols=all_of(ageCols),names_to = "age",values_to = "count", names_ptypes = list(age=integer()))
# UKDemographics2019 = tmp %>% rename(code = Code, name=Name, aggregation = Geography1, total=`All ages`) %>% 
#   mutate(aggregation = as.factor(aggregation)) %>% filter(!is.na(count))
# 
# usethis::use_data(UKDemographics2019, overwrite = TRUE)

#### LSOA demogrphaics 2018 - 2019 ----

# # https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/wardlevelmidyearpopulationestimatesexperimental
# url2 = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2018sape21dt1a/sape21dt1amid2018on2019lalsoasyoaestimatesformatted.zip"
# destfile2 <- "~/Git/uk-covid-datatools/data-raw/Population/sape21dt1amid2018on2019lalsoasyoaestimatesformatted.zip"
# unzipDir="~/Git/uk-covid-datatools/data-raw/Population/sape21dt1amid2018on2019lalsoasyoaestimatesformatted"
# unzipFile = "~/Git/uk-covid-datatools/data-raw/Population/sape21dt1amid2018on2019lalsoasyoaestimatesformatted/SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx"
# if (!file.exists(destfile2)) { 
#   curl::curl_download(url2, destfile2)
#   if (!dir.exists(unzipDir)) dir.create(unzipDir)
#   setwd(unzipDir)
#   unzip(destfile2)
# }
# 
# demogByLSOA_M <- read_excel(path.expand(unzipFile), sheet="Mid-2018 Males", skip = 4)
# demogByLSOA_F <- read_excel(path.expand(unzipFile), sheet="Mid-2018 Females", skip = 4)
# 
# convert = function(demogByLSOA) {
#   
#   ageCols = colnames(demogByLSOA)[!is.na(as.integer(stringr::str_remove(colnames(demogByLSOA),"\\+")))]
#   tmp = demogByLSOA %>% 
#     #mutate(LA19CD = ifelse(is.na(LSOA), `Area Codes`, NA)) %>% rename(LA19NM=`LA (2019 boundaries)`) %>%
#     #fill(LA19CD, LA19NM) %>% 
#     filter(!is.na(LSOA)) %>%
#     select(-`LA (2019 boundaries)`,-`All Ages`) %>%
#     tidyr::pivot_longer(cols=all_of(ageCols),names_to = "age",values_to = "count") #, names_ptypes = list(age=integer()))
#   tmp = tmp %>% rename(LSOA11CD = `Area Codes`, LSOA11NM=LSOA) %>% #, total=`All Ages`) %>% 
#     mutate(age = as.integer(stringr::str_remove(age,"\\+")))
#   return(tmp)
# }
# 
# tmp = convert(demogByLSOA_F) %>% mutate(gender = "F") %>% bind_rows(
#   convert(demogByLSOA_M) %>% mutate(gender = "M") 
# ) %>% filter(!is.na(count))
# 
# UKDemographics2019ByLSOA = tmp
# usethis::use_data(UKDemographics2019ByLSOA, overwrite = TRUE)

#### E&W Ward demogrphaics ----

# Get teh most up to date estimates of demographics - mid year 2018 which uses WD18CD


url3 = "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fwardlevelmidyearpopulationestimatesexperimental%2fmid2018sape21dt8a/sape21dt8amid2018ward20182019lasyoaestunformatted.zip"
destfile3 <- "~/Git/uk-covid-datatools/data-raw/Population/sape21dt1amid2018on2019lalsoasyoaestimatesformatted.zip"
unzipDir2="~/Git/uk-covid-datatools/data-raw/Population/sape21dt8amid2018ward20182019lasyoaestunformatted"
unzipFile2 = "~/Git/uk-covid-datatools/data-raw/Population/sape21dt8amid2018ward20182019lasyoaestunformatted/SAPE21DT8a-mid-2018-ward-2018-on-2019-LA-syoa-estimates-unformatted.xlsx"
if (!file.exists(destfile3)) { 
  curl::curl_download(url3, destfile3)
  if (!dir.exists(unzipDir2)) dir.create(unzipDir2)
  setwd(unzipDir2)
  unzip(destfile3)
}

demogByWard_M <- read_excel(path.expand(unzipFile2), sheet="Mid-2018 Males", skip = 4)
demogByWard_F <- read_excel(path.expand(unzipFile2), sheet="Mid-2018 Females", skip = 4)

convert2 = function(demogByLSOA) {
  ageCols = colnames(demogByLSOA)[!is.na(as.integer(stringr::str_remove(colnames(demogByLSOA),"\\+")))]
  tmp = demogByLSOA %>% select(-`LA (2019 boundaries)`) %>%
    tidyr::pivot_longer(cols=all_of(ageCols),names_to = "age",values_to = "count") #, names_ptypes = list(age=integer()))
  tmp = tmp %>% rename(WD18CD = `Ward Code 1`, WD18NM=`Ward Name 1`) %>% select(-`All Ages`) %>% 
    mutate(age = as.integer(stringr::str_remove(age,"\\+"))) %>%
    filter(!is.na(WD18CD))
  return(tmp)
}

ewPopByWard = convert2(demogByWard_F) %>% mutate(gender = "F") %>% bind_rows(
  convert2(demogByWard_M) %>% mutate(gender = "M") 
) %>% filter(!is.na(count))

rm(tmp)
rm(demogByWard_F,demogByWard_M)


#### E&W Wards shapefile ----

# TODO: make this a generic way of getting all demographics for random areas
wards2011 = getShapefile("Wards_December_2011_Boundaries_EW_BFE", 
                         "https://opendata.arcgis.com/datasets/f04efac388f049508ecf91cafbe70343_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D", simplify=FALSE)

# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/wardlevelmidyearpopulationestimatesexperimental

# http://geoportal.statistics.gov.uk/datasets/a0b43fe01c474eb9a18b6c90f91664c2_0
wards2018 = getShapefile("Wards_December_2018_Full_Clipped_Boundaries_GB",
                         "https://opendata.arcgis.com/datasets/a0b43fe01c474eb9a18b6c90f91664c2_0.zip?outSR=%7B%22wkid%22%3A27700%2C%22latestWkid%22%3A27700%7D", simplify=FALSE)

#### Scotland ----

scotUrl = "https://www.nrscotland.gov.uk/files//statistics/population-estimates/mid-19/mid-year-pop-est-19-data.xlsx"
destfile <- "~/Git/uk-covid-datatools/data-raw/Population/scot_mid_year_pop_est_19_data.xlsx"
if(!file.exists(destfile)) curl::curl_download(scotUrl, destfile)
scotPopMale = read_excel(destfile, sheet = "Table 2", range = "A57:CQ107")
scotPopFemale = read_excel(destfile, sheet = "Table 2", range = "A110:CQ160")

cleanScotPop = function(df) {
  df %>% select(-`...4`,-`All Ages`) %>% rename(code = `Area code1`,name=`Area name`) %>%
    filter(!is.na(code)) %>%
    pivot_longer(cols=matches("[0-9]+\\+?"), names_to = "age", values_to = "count") %>% mutate(age = age %>% stringr::str_extract("^[0-9]+") %>% as.integer())
}

tmp = cleanScotPop(scotPopFemale) %>% mutate(gender = "F") %>% bind_rows(
  cleanScotPop(scotPopMale) %>% mutate(gender = "M") 
) %>% filter(!is.na(count))

scotPopByCouncil = tmp %>% filter(code %>% stringr::str_starts("S12")) 
scotPopByNHS = tmp %>% filter(code %>% stringr::str_starts("S08"))

scotCouncilShapes = getShapefile("pub_las",
  "https://geo.spatialhub.scot/geoserver/sh_las/wfs?authkey=b85aa063-d598-4582-8e45-e7e6048718fc&request=GetFeature&service=WFS&version=1.1.0&typeName=pub_las&outputFormat=SHAPE-ZIP", simplify=FALSE)

scotlandHealthBoardShapeFile = getShapefile("SG_NHS_HealthBoards_2019",
  "http://sedsh127.sedsh.gov.uk/Atom_data/ScotGov/ZippedShapefiles/SG_NHS_HealthBoards_2019.zip")

#### overarching shape file and single year population estimates for E, W and S ---

gbPopShapefile = rbind(
  scotCouncilShapes %>% rename(name = local_auth) %>% mutate(code = as.character(code)) %>% select(-hectares),
  wards2018 %>% filter(!stringr::str_starts(wd18cd,"S")) %>% select(code = wd18cd, name = wd18nm,geometry)
)

gbPopShapefile = suppressWarnings(gbPopShapefile %>% sf::st_simplify(dTolerance=0.001))

gbPopEstimates = rbind(
  ewPopByWard %>% rename(code = WD18CD, name=WD18NM),
  scotPopByCouncil
)

saveShapefile(gbPopShapefile,"~/Git/uk-covid-datatools/data-raw/GB_Detailed_Demographic_Map")
write.csv(gbPopEstimates, file="~/Git/uk-covid-datatools/data-raw/GB_Detailed_Demographic_Estimates")

#### Demographics by 2011 wards (for metawards model) ----

UKDemographics2018ByWard = ewPopByWard %>% mutate(ageGroup = ageToAgeCat(age)) %>% group_by(WD18CD,WD18NM,gender,ageGroup) %>% summarise(count = sum(count)) %>% ungroup()
totPop = sum(UKDemographics2018ByWard$count)

UKDemographics2018ByWard2011 = ukcovidtools::interpolateByArea(
  UKDemographics2018ByWard %>% group_by(gender,ageGroup), 
  count, 
  wards2018 %>% rename(WD18CD = wd18cd), WD18CD, 
  wards2011 %>% rename(WD11CD = wd11cd, WD11NM = wd11nm) %>% group_by(WD11CD,WD11NM)
)

#### Demographics by country and age and gender

UKByNationAndAgeAndGender = gbPopEstimates %>% mutate(nation = case_when(
  code %>% stringr::str_starts("E") ~ "England",
  code %>% stringr::str_starts("S") ~ "Scotland",
  code %>% stringr::str_starts("W") ~ "Wales",
  TRUE ~ as.character(NA)
)) %>% group_by(nation, age, gender) %>% summarise(count = sum(count))


# wards2011$originalArea = wards2011 %>% sf::st_area()
# wards2018$newArea = wards2018 %>% sf::st_area() 
# wards2011 = wards2011 %>% rename(WD11CD = wd11cd, WD11NM = wd11nm)
# wards2018 = wards2018 %>% rename(WD18CD = wd18cd, WD18NM=wd18nm)
# intersection = wards2011 %>% sf::st_intersection(wards2018)
# intersection$intersectionArea = intersection %>% sf::st_area()
# # intersection = intersection %>% rename(WD11CD = wd11cd, WD11NM = wd11nm, WD18CD = wd18cd, WD18NM=wd18nm)
# intersection = intersection %>% as_tibble() %>% mutate(
#     frac18 = intersectionArea/newArea,
#     frac11 = intersectionArea/originalArea
#   ) %>% select(WD11CD, WD11NM, frac11, frac18, WD18CD, WD18NM)
# 
# #avWardPop = mean(UKTotalDemographics2018ByWard$count)
# 
# intersection = intersection %>% #left_join(UKDemographics2018ByWard %>% select(-WD18NM) %>% rename(demog18 = count), by=c("WD18CD"))
#   inner_join(UKDemographics2018ByWard %>% select(-WD18NM) %>% rename(demog18 = count), by=c("WD18CD"))
# # there are some for which we have not demograhics. These are small area ones and will set them to 0
# # View(intersection %>% filter(is.na(demog18)))
# # intersection = intersection %>% mutate(demog18 = ifelse(is.na(demog18),0,demog18))
# # missing = intersection %>% filter(is.na(ageGroup))
# # intersection = intersection %>% filter(!is.na(ageGroup))
# 
# intersection = intersection %>% mutate(
#   demog = demog18*frac18
# )
# 
# intersection = intersection %>% as_tibble() %>% 
#   select(WD11CD, WD11NM, frac11, gender, ageGroup, demog, frac18, WD18CD, WD18NM = WD18NM.x)
# intersection = intersection %>% mutate(frac11 = as.double(frac11), frac18 = as.double(frac18), demog = as.double(demog))
# 
# any(is.na(intersection$ageGroup))
# 
# # write.csv(demographicMapping, "~/Git/uk-covid-datatools/data-raw/UKDemographicsByWard2011andWard2018.csv")
# 
# # This is the 2018 demographics projected onto the 2011 Ward structure, at a detailed population level.
# UKDemographics2018ByWard2011 = intersection %>% group_by(WD11CD, WD11NM, ageGroup, gender) %>% summarise(count = sum(demog)) %>% ungroup() %>% 
#   mutate(WD11CD = as.character(WD11CD), WD11NM = as.character(WD11NM)) %>%
#   ensurer::ensure_that(abs(sum(.$count) - totPop) < 1000) 

UKDemographics2018 = list(
  by2018Ward = UKDemographics2018ByWard,
  by2011Ward = UKDemographics2018ByWard2011,
  byNationAgeAndGender = UKByNationAndAgeAndGender
)

usethis::use_data(UKDemographics2018, overwrite = TRUE)
