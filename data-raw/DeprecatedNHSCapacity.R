## code to prepare `NHSCapacity2019` dataset goes here

library(rvest)
library(stringr)
library(readr)
library(readxl)
library(tidyverse)

#### get data about beds etc ----

conf = config::get(file="~/Dropbox/googleMaps.yaml")

# http://media.nhschoices.nhs.uk/data/foi/Hospital.csv
# although had to do a find and replace as delimiter was ¬ and not showing up properly

if (!file.exists("~/Git/uk-covid-datatools/data-raw/englandHospitals.csv")) {
  Hospital <- read_lines("http://media.nhschoices.nhs.uk/data/foi/Hospital.csv", locale = locale(encoding = "ISO-8859-15"))
  HospitalCols = Hospital[1] %>% stringr::str_split("¬")
  HospitalData = Hospital[-1] %>% stringr::str_split("¬")
  HospitalData = data.frame(do.call('rbind', HospitalData), stringsAsFactors = FALSE)
  colnames(HospitalData) = HospitalCols[[1]]
  hospitalList = HospitalData %>% mutate(Latitude = as.double(Latitude), Longitude = as.double(Longitude)) # %>% mutate(trustId = stringr::str_sub(OrganisationCode,end=3))
  
  hospitalList = hospitalList %>%
    select(
      hospitalId = OrganisationCode,
      sector = Sector,
      name = OrganisationName,
      # city = City,
      # county = County,
      pcds = Postcode,
      lat = Latitude,
      long = Longitude,
      trustId = ParentODSCode,
      trustName = ParentName
    ) %>% mutate(sector = as.factor(sector))
  
  rm(Hospital, HospitalCols, HospitalData)
  
  #### Hospitals offering each service ----
  # TODO: Independent sector hospitals
  
  url = "https://www.nhs.uk/ServiceDirectories/Pages/NHSTrustListing.aspx"
  page <- xml2::read_html(url)
  
  # Find links with "Trust" menioned
  trustLinks = (page %>% html_nodes("a"))[ page %>% html_nodes("a") %>% html_text() %>% str_ends("Trust") ]
  trustNames = trustLinks %>% html_text()
  trustUrls = paste0("https://www.nhs.uk",trustLinks %>% html_attr("href"))
  trustUrls = trustUrls %>% str_replace("Overview","Services")
  
  allItus = NULL
  allAandE = NULL
  allSurgical = NULL
  
  for (trustUrl in trustUrls) {
    
    message(trustUrl)
    page2 = xml2::read_html(trustUrl)
    hasIcu = page2 %>% html_nodes(".hidden-department-name") %>% html_text() %>% str_count("Intensive|Critical") > 0
    hasAandE = page2 %>% html_nodes(".hidden-department-name") %>% html_text() %>% str_count("Accident|Urgent") > 0
    hasSurgicalUnit = page2 %>% html_nodes(".hidden-department-name") %>% html_text() %>% str_count("General Surgery|Orthopaedics") > 0
    ituHospitals = (page2 %>% html_nodes(".hidden-department-name"))[hasIcu] %>% html_node(xpath="following-sibling::td[1]") %>% html_text() %>% str_trim() %>% str_split("(\\r\\n\\s+)+")
    aAndEHospitals = (page2 %>% html_nodes(".hidden-department-name"))[hasAandE] %>% html_node(xpath="following-sibling::td[1]") %>% html_text() %>% str_trim() %>% str_split("(\\r\\n\\s+)+")
    surgicalHospitals = (page2 %>% html_nodes(".hidden-department-name"))[hasSurgicalUnit] %>% html_node(xpath="following-sibling::td[1]") %>% html_text() %>% str_trim() %>% str_split("(\\r\\n\\s+)+")
    allSurgical = c(allSurgical,surgicalHospitals)
    allAandE = c(allAandE,aAndEHospitals)
    allItus = c(allItus,ituHospitals)
    
  }
  
  allItus = unique(Reduce(c,allItus))
  allAandE = unique(Reduce(c,allAandE))
  allSurgical = unique(Reduce(c,allSurgical))
  
  hospitalList = hospitalList %>% 
    left_join(tibble(name = as.vector(allItus), hasIcu=TRUE), by="name") %>% mutate(hasIcu = ifelse(is.na(hasIcu),FALSE,TRUE)) %>%
    left_join(tibble(name = as.vector(allAandE), hasAccidentAndEmergency=TRUE), by="name") %>% mutate(hasAccidentAndEmergency = ifelse(is.na(hasAccidentAndEmergency),FALSE,TRUE)) %>%
    left_join(tibble(name = as.vector(allSurgical), hasSurgicalDepartment=TRUE), by="name") %>% mutate(hasSurgicalDepartment = ifelse(is.na(hasSurgicalDepartment),FALSE,TRUE))
  
  # 2 hospitals don't have a location. This will cause issues and they are relatively unimportant ones (Jersey and Cygnet maidstone) - No ITU, A&E, or Surg
  hospitalList = hospitalList %>% filter(!is.na(lat))
  
  # write_csv(hospitalsWithItus, "hospitalsWithICULocations.csv")
  
  rm(page,page2,trustLinks,allItus,hasIcu, allAandE, hasAandE, allSurgical, hasSurgicalUnit, trustNames, trustUrl, trustUrls, url, surgicalHospitals,aAndEHospitals,ituHospitals)
  write.csv(hospitalList,"~/Git/uk-covid-datatools/data-raw/EnglandHospitals.csv",row.names = FALSE)
}

hospitalList = read.csv("~/Git/uk-covid-datatools/data-raw/englandHospitals.csv")

#### Trust level data ----

if (!file.exists("~/Git/uk-covid-datatools/data-raw/englandTrusts.csv")) {
  tmp = tempfile(fileext=".xlsx")
  url <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/02/Beds-Open-Overnight-Web_File-Final-Q3-2019-20-kg8gu.xlsx"
  curl::curl_download(url,destfile = tmp)
  generalBedsByTrust = read_excel(tmp, sheet = "NHS Trust by Sector",range = "B18:K218", 
                                  col_names = c("Year","Period End","Region Code",	"Org Code",	"Org Name",	"Total", "General and Acute",	"Learning Disabilities",	"Maternity",	"Mental Illness"))
  
  tmp2 = tempfile(fileext=".xlsx")
  url2 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/02/Beds-Open-Day-Only-Web_File-Final-Q3-2019-20-gjg7g.xlsx"
  curl::curl_download(url2,destfile = tmp2)
  dayBedsByTrust <- read_excel(tmp2, range = "B18:K218",  
                               col_names = c("Year","Period End","Region Code",	"Org Code",	"Org Name",	"Total", "General and Acute",	"Learning Disabilities",	"Maternity",	"Mental Illness"))
  
  rm(tmp,url,url2)
  
  icuBedsByTrust <- read_csv("https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/03/Monthly-SITREPSs-CC-and-UOC-Extracts-JANUARY-2020-oa9U1.csv")
  
  bedsByTrust = 
    generalBedsByTrust %>% select(regionId = `Region Code`, trustId = `Org Code`, trustName = `Org Name`, acuteBeds = `General and Acute`) %>% 
    left_join(dayBedsByTrust %>% select(trustId = `Org Code`, dayBeds = `General and Acute`), by="trustId") %>%
    left_join(icuBedsByTrust %>% select(trustId = `Org Code`, icuBeds = `Number of adult critical care beds open`), by="trustId") %>%
    filter(acuteBeds != 0) %>% mutate(
      acuteBeds = as.integer(acuteBeds),
      dayBeds = as.integer(dayBeds),
      icuBeds = as.integer(icuBeds)
    ) %>% mutate(icuBeds = ifelse(is.na(icuBeds),0,icuBeds))
  
  bedsByTrust = bedsByTrust %>% left_join((hospitalList %>% group_by(trustId) %>% summarise(approxLat = mean(lat, na.rm = TRUE), approxLong = mean(long, na.rm = TRUE))), by="trustId")
  
  rm(generalBedsByTrust, dayBedsByTrust, icuBedsByTrust)
  
  #TODO: https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/ae-attendances-and-emergency-admissions-2019-20/
  # info here about A&E attendances which could be useful
  
  # One community healthcare trust missing locations
  bedsByTrust = bedsByTrust %>% filter(!is.na(approxLat))
  write.csv(bedsByTrust,"~/Git/uk-covid-datatools/data-raw/englandTrusts.csv",row.names = FALSE)
}

bedsByTrust = read_csv("~/Git/uk-covid-datatools/data-raw/englandTrusts.csv")

# TODO: could expand this to include all the services listedon the NHS site

#### Wales ----
# Wales: https://statswales.gov.wales/v/Hg4K
# http://www.wales.nhs.uk/ourservices/directory/Hospitals

if (!file.exists("~/Git/uk-covid-datatools/data-raw/walesTrusts.csv")) {
  walesAll <- read_csv("~/Git/uk-covid-datatools/data-raw/source/walesAll.csv", col_name = c("code","country","LHB","hospital","hospital2","acuteBeds"), na = ".") %>% 
    mutate(hospital = ifelse(hospital==LHB, hospital2, hospital)) %>% select(-hospital2) %>% filter(code != "")
  
  walesHDU <- read_csv("~/Git/uk-covid-datatools/data-raw/source/walesHDU.csv", col_name = c("code","country","LHB","hospital","hospital2","hduBeds"), na = ".") %>% 
    mutate(hospital = ifelse(hospital==LHB, hospital2, hospital)) %>% select(-hospital2) %>% filter(code != "")
  
  walesICU <- read_csv("~/Git/uk-covid-datatools/data-raw/source/walesICU.csv", col_name = c("code","country","LHB","hospital","hospital2","icuBeds"), na = ".") %>% 
    mutate(hospital = ifelse(hospital==LHB, hospital2, hospital)) %>% select(-hospital2) %>% filter(code != "")
  
  walesOverall = walesAll %>% 
    left_join(walesHDU %>% select(code,hduBeds), by="code") %>% 
    left_join(walesICU %>% select(code,icuBeds), by="code") %>% 
    filter(hospital != "" & LHB != "" & !is.na(acuteBeds)) %>% 
    mutate(id = row_number())
  
  #TODO: filter out hospitals whose names contain:
  #"Learning disability","Cancer","Mental"
  
  tmp = mp_geocode(paste0(walesOverall$hospital,", Wales"),key=conf$key)
  tmp_sf = tmp %>% mp_get_points() %>% sf::st_as_sf()
  walesOverall2 = tmp_sf %>% inner_join(walesOverall, by="id") %>% select(-address,-location_type, -status) %>% rename(address = address_google)
  walesOverall2$long = sf::st_coordinates(walesOverall2)[,"X"]
  walesOverall2$lat = sf::st_coordinates(walesOverall2)[,"Y"]
  walesHospitals = walesOverall2 %>% as_tibble() %>% select(-pnt) %>%
    mutate(
      pcds = address %>% stringr::str_extract("[A-Z][A-Z][0-9][0-9]?( [0-9][A-Z][A-Z])?"), 
      sector = "NHS Sector",
      hasIcu = ifelse(is.na(icuBeds) & is.na(hduBeds),FALSE,TRUE),
      hasAccidentAndEmergency = ifelse(is.na(acuteBeds),FALSE,TRUE),
      hasSurgicalDepartment = ifelse(is.na(acuteBeds),FALSE,TRUE)
    ) %>%
    select(
      hospitalId = code,
      sector,
      name = hospital,
      pcds,
      lat,
      long,
      trustId = code,
      trustName = hospital,
      hasIcu, hasAccidentAndEmergency, hasSurgicalDepartment
    )
  walesTrusts = walesOverall2 %>% as_tibble() %>% select(-pnt) %>% mutate(
    regionId = code %>% stringr::str_sub(1,3),
    icuBeds = as.double(ifelse(is.na(icuBeds),0,icuBeds))+as.double(is.na(hduBeds),0,hduBeds),
    acuteBeds = as.double(ifelse(is.na(acuteBeds),0,acuteBeds)),
    dayBeds = as.double(NA)
  ) %>% select(
    trustId = code,
    trustName = hospital,
    acuteBeds,
    dayBeds,
    icuBeds,
    approxLat = lat,
    approxLong = long
  )
  write.csv(walesHospitals, "~/Git/uk-covid-datatools/data-raw/walesHospitals.csv", row.names = FALSE)
  write.csv(walesTrusts, "~/Git/uk-covid-datatools/data-raw/walesTrusts.csv", row.names = FALSE)
}

walesHospitals <- read_csv("~/Git/uk-covid-datatools/data-raw/walesHospitals.csv")
walesTrusts <- read_csv("~/Git/uk-covid-datatools/data-raw/walesTrusts.csv") %>% mutate(dayBeds=as.numeric(dayBeds))

# tmp = walesCapacity %>% sf::st_as_sf(coords = c("long","lat"), crs=4326)
# leaflet::leaflet() %>% 
#   leaflet::addTiles() %>% 
#   leaflet::addCircleMarkers(data=tmp, color="#FF0000", popup = as.character(tmp$address))



#### Combine ----

englandAndWalesHospitals = hospitalList %>% bind_rows(walesHospitals)
englandAndWalesTrusts = bedsByTrust %>% bind_rows(walesTrusts)


#### nightingale hospitals ----
# source:
# https://docs.google.com/spreadsheets/d/1SHmGnfozDyV_ZWby9WQead55GdcJKQNAG6jwsvczsmY/edit?usp=sharing

nightingales <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ8LZnTeXb-2g4rIosuJU-2X2Vy_cM7fZGxOgBtARaGYdwWRx_-7XCTWCKE2wbbtK6NS2pFGEp1C7Cn/pub?gid=0&single=true&output=csv", 
                         col_types = cols(dateOpened = col_date(format = "%Y-%m-%d")))

#### combine ----

# TODO: Scotland, Northern Ireland
# https://www.isdscotland.org/Health-Topics/Hospital-Care/Publications/2018-09-25/2018-09-25-Annual-QuarterlyAcuteActivity-Report.pdf?
# https://isdscotland.scot.nhs.uk/Health-Topics/Hospital-Care/Publications/2018-09-25/Annual-Trends-in-Available-Beds-by-Health-Board-of-Treatment-and-Hospital-Sep18.xlsx


NHSCapacity2019 = list(
  hospitals = englandAndWalesHospitals,
  trusts = englandAndWalesTrusts,
  nightingales = nightingales
)

#write.csv(NHSCapacity2019$hospitals, "~/Git/uk-covid-datatools/data-raw/hospitalCapacity.csv")
#write.csv(NHSCapacity2019$trusts, "~/Git/uk-covid-datatools/data-raw/trustCapacity.csv")

usethis::use_data(NHSCapacity2019, overwrite=TRUE)

#### TODO: eyeball tests ----

tmp = NHSCapacity2019$hospitals %>% filter(hasIcu) %>% sf::st_as_sf(coords = c("long","lat"), crs=4326)

ggplot(UKCovidMaps$reportingRegions)+geom_sf()+geom_sf(data=tmp)

leaflet::leaflet() %>% 
  leaflet::addTiles() %>% 
  leaflet::addCircleMarkers(data=tmp %>% filter(hasIcu), color="#FF0000", popup = as.character(tmp$name))

# TODO:
# These two thing should be the same but they aren't exactly 138 (via NHS website) vs 141 (via bed state spreadsheet (gold standard)):
# actually there are 13 trusts mismatched
tmp1 = glimpse(hospitalList %>% filter(hasIcu) %>% select(trustId) %>% distinct())
tmp2 = glimpse(bedsByTrust %>% filter(icuBeds > 0) %>% select(trustId) %>% distinct())
missing = tmp2 %>% anti_join(tmp1) %>% union_all(tmp1 %>% anti_join(tmp2))
hospitalList %>% semi_join(missing, by="trustId")

rm(bedsByTrust,hospitalList,missing,tmp1,tmp2)


#### Sitrep data ----

paths=list(
  chess="~/S3/encrypted/5Apr/NHS/CHESS COVID19 CaseReport 20200405.csv",
  lineList="~/S3/encrypted/5Apr/NHS/Anonymised Line List 20200405.xlsx",
  ff100="~/S3/encrypted/5Apr/NHS/FF100 Case Extract External 20200405.csv",
  chessSummary="~/S3/encrypted/5Apr/NHS/CHESS Aggregate Report 20200405.csv",
  sitrep="~/S3/encrypted/8Apr/Covid sitrep report incl CIC 20200408 FINAL.xlsx"
)

covidSitrep <- read_excel(path.expand(paths$sitrep), sheet = "Site Level Raw Data") 
covidSitrep = covidSitrep %>% filter(!is.na(`Org code`))
covidSitrep = covidSitrep %>% pivot_longer(cols = starts_with("SIT0"), names_to = "variable", values_to = "value") 
covidSitrep = covidSitrep %>% select(-starts_with("...")) %>% select(-`ADDITIONAL ISP QUESTIONS`)
covidSitrep = covidSitrep %>% mutate(
  subgroup = str_replace(variable,"^.*_([^_]+)$","\\1"),
  variable = str_replace(variable,"^(.*)_[^_]+$","\\1")
)

bedsBySite = covidSitrep %>% group_by_at(vars(-value,-subgroup)) %>% 
  summarise(subtotal = sum(as.numeric(value))) %>% 
  filter(variable %in% c("SIT032","SIT033","SIT034","SIT037")) %>% 
  pivot_wider(names_from = variable, values_from = subtotal) %>% mutate(
    ventilated = as.integer(SIT032),
    cpap = as.integer(SIT033),
    acute = as.integer(SIT034+SIT037)
  ) %>% select(-starts_with("SIT0"))

bedsExtract = read_excel(path.expand(paths$sitrep), sheet = "Beds Extract")

# View(bedsBySite %>% anti_join(NHSCapacity2019$hospitals, by=c("Site/Org Code"="hospitalId")) %>% anti_join(NHSCapacity2019$trusts, by=c("Site/Org Code"="trustId")))
# View(bedsBySite %>% anti_join(NHSCapacity2019$trusts, by=c("Org code"="trustId")))

tmp1 = bedsBySite %>% full_join(
  NHSCapacity2019$trusts %>% inner_join(NHSCapacity2019$hospitals %>% select(-trustName),by=c("trustId")) %>% mutate(trustId = ifelse(sector=="Independent Sector", hospitalId, trustId)), 
  by=c("Org code"="trustId")) 

tmp2 = tmp1 %>%
  mutate(distance = stringdist::stringdist(str_to_lower(`Site/Org Name`),str_to_lower(name), "osa")) %>% 
  mutate(distance = ifelse(is.na(distance),1000,distance)) %>% 
  group_by(`Site/Org Name`) %>% arrange(desc(distance)) %>% 
  filter(is.na(`Site/Org Name`) | row_number()<=2) %>% 
  select(-distance) %>% ungroup()

tmp3 = tmp2 %>%
  full_join(bedsExtract %>% select(!contains("Occupied")), by=c("Org code"="OrgCode")) %>%
  mutate(distance = stringdist::stringdist(str_to_lower(`Site/Org Name`),str_to_lower(`Site Name`), "osa")) %>%
  group_by(`Site/Org Name`) %>% 
  mutate(distance = ifelse(is.na(distance),1000,distance)) %>% 
  arrange(desc(distance)) %>% 
  filter(is.na(`Site/Org Name`) | row_number()==1)

write_csv(tmp, "~/Dropbox/covid19/load-sharing/NHSsitesCleanse.csv")

a = NHSCapacity2019$hospitals$name %>% stringr::str_to_upper()
b = bedsBySite$`Site/Org Name` %>% stringr::str_to_upper()
c = bedsExtract$`Site Name` %>% stringr::str_to_upper()

c(a,c)[!duplicated(c(a,c))]
c(a,b)[!duplicated(c(a,b))]

sort(c(b,c)[!duplicated(c(b,c))])



write_csv(icuBedsBySite, "~/Dropbox/covid19/load-sharing/ICUsites.csv")

occupiedIcuBedsBySite = covidSitrep %>% group_by_at(vars(-value,-subgroup)) %>% filter(variable=="SIT032" & subgroup != "Unoccupied") %>% summarise(occupied = sum(as.numeric(value))) %>% ungroup() %>% select(`Site/Org Code`,occupied)
icuBedsBySite = icuBedsBySite %>% left_join(occupiedIcuBedsBySite, by="Site/Org Code") %>% filter(`Org Type` == "Site" & subtotal > 0)



# cpapBedsBySite = covidSitrep %>% group_by_at(vars(-value,-subgroup)) %>% summarise(subtotal = sum(as.numeric(value))) %>% filter(variable=="SIT033")
# acuteBedsBySite = covidSitrep %>% group_by_at(vars(-value,-subgroup)) %>% summarise(subtotal = sum(as.numeric(value))) %>% filter(variable=="SIT034")
# mentalHealthBedsBySite = covidSitrep %>% group_by_at(vars(-value,-subgroup)) %>% summarise(subtotal = sum(as.numeric(value))) %>% filter(variable=="SIT035")
# lowDependencyBedsBySite = covidSitrep %>% group_by_at(vars(-value,-subgroup)) %>% summarise(subtotal = sum(as.numeric(value))) %>% filter(variable=="SIT036")
# otherBedsBySite = covidSitrep %>% group_by_at(vars(-value,-subgroup)) %>% summarise(subtotal = sum(as.numeric(value))) %>% filter(variable=="SIT037")
```

#### ----

tmp = NHSWales %>% 
  filter(Dataset == "Capacity and Occupancy" & Measure == "Total" & stringr::str_detect(Question,"bed|cpap")) %>% 
  select(-Section) %>% 
  pivot_wider(names_from = Question, values_from = MeasureValue) %>%
  group_by(HealthBoard, Hospital) %>%
  filter(UpdateDateTime == max(UpdateDateTime)) %>%
  mutate(
    ventilated = `Invasive ventilated beds in a hospital but outside of critical care environment` + `Invasive ventilated beds in a critical care environment`,
    beds = `Total designated Covid-19 hospital beds` + `Non designated Covid-19 hospital beds`
  ) 

tmp %>% select(HealthBoard, Hospital, ventilated, beds) %>% clipr::write_clip()

  
View(tmp)


#### ----

NHSScot <- read_excel("~/tmp/Annual-Trends-in-Available-Beds-by-Health-Board-of-Treatment-and-Hospital-Sep18.xlsx", sheet="Data")


tmp2 = NHSScot %>% filter(finyear == "2017/18p" & ind =="Average Available Staffed Beds" & specname == "All Acute Specialties")
View(tmp2)



covidSitrep <- read_excel("~/S3/encrypted/Sitreps/Covid sitrep report incl CIC 20200428 R final_v2.xlsx", sheet = "R Data") 
covidSitrep = covidSitrep %>% filter(!is.na(`Org code`))
covidSitrep = covidSitrep %>% pivot_longer(cols = starts_with("SIT0"), names_to = "variable", values_to = "value") 
covidSitrep = covidSitrep %>% select(-starts_with("...")) %>% select(-starts_with("IS")) %>% select(-starts_with("dv_"))
covidSitrep = covidSitrep %>% mutate(
  subgroup = str_replace(variable,"^.*_([^_]+)$","\\1"),
  variable = str_replace(variable,"^(.*)_[^_]+$","\\1")
)

bedsBySite = covidSitrep %>% group_by_at(vars(-value,-subgroup)) %>% 
  summarise(subtotal = sum(as.numeric(value),na.rm=TRUE)) %>% 
  filter(variable %in% c("SIT032","SIT033","SIT034","SIT037")) %>% 
  pivot_wider(names_from = variable, values_from = subtotal) %>% mutate(
    ventilated = as.integer(SIT032),
    cpap = as.integer(SIT033),
    acute = as.integer(SIT034+SIT037)
  ) %>% select(-starts_with("SIT0"))


#### ----

library(readr)
gbHospitals <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQj6X8rIlBlsD5bK-PMcBT9wjAWh60dTTJLfuczqsiKnYzYiN_4KjYAh4HWWkf4v1RH6ih7C78FhdiN/pub?gid=128715098&single=true&output=csv")
View(gbHospitals)

cfg = config::get(file="~/Dropbox/googleMaps.yaml")
doc = mapsapi::mp_geocode(
  addresses = paste0(gbHospitals %>% filter(is.na(pcds)) %>% pull(name), ", UK"),
  key = cfg$key
)
points = mapsapi::mp_get_points(doc)
points %>% filter(stringr::str_detect(address_google,"UK")) %>% mutate(
  lat = sf::st_coordinates(pnt)[,"Y"],
  long = sf::st_coordinates(pnt)[,"X"],
  pcds = stringr::str_extract(address_google, "[A-Z]+[0-9]+[A-Z]? [0-9]+[A-Z]+")
) %>% select(address,pcds,lat,long, location_type) %>% clipr::write_clip()



