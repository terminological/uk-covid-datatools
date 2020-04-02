## code to prepare `NHSCapacity2019` dataset goes here

#### get data about beds etc ----

# http://media.nhschoices.nhs.uk/data/foi/Hospital.csv
# although had to do a find and replace as delimiter was ¬ and not showing up properly

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
    city = City,
    county = County,
    pcds = Postcode,
    lat = Latitude,
    long = Longitude,
    trustId = ParentODSCode,
    trustName = ParentName
  ) %>% mutate(sector = as.factor(sector))

rm(Hospital, HospitalCols, HospitalData)

#### Hospitals offering each service ----

library(rvest)
library(stringr)

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


#### Trust level data ----

library(readxl)
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

#### combine ----

NHSCapacity2019 = list(
  hospitals = hospitalList,
  trusts = bedsByTrust
)

write.csv(NHSCapacity2019$hospitals, "~/Git/uk-covid-datatools/data-raw/hospitalCapacity.csv")
write.csv(NHSCapacity2019$trusts, "~/Git/uk-covid-datatools/data-raw/trustCapacity.csv")

usethis::use_data(NHSCapacity2019, overwrite=TRUE)

# TODO:
# These two thing should be the same but they aren't exactly 138 (via NHS website) vs 141 (via bed state spreadsheet (gold standard)):
# actually there are 13 trusts mismatched
tmp1 = glimpse(hospitalList %>% filter(hasIcu) %>% select(trustId) %>% distinct())
tmp2 = glimpse(bedsByTrust %>% filter(icuBeds > 0) %>% select(trustId) %>% distinct())
missing = tmp2 %>% anti_join(tmp1) %>% union_all(tmp1 %>% anti_join(tmp2))
hospitalList %>% semi_join(missing, by="trustId")

rm(bedsByTrust,hospitalList,missing,tmp1,tmp2)