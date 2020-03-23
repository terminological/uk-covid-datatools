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

bedsByTrust = bedsByTrust %>% left_join((hospitalList %>% group_by(trustId) %>% summarise(approxLat = mean(lat), approxLong = mean(long))), by="trustId")

rm(generalBedsByTrust, dayBedsByTrust, icuBedsByTrust)

NHSCapacity2019 = list(
  hospitals = hospitalList,
  trusts = bedsByTrust
)

usethis::use_data(NHSCapacity2019, overwrite=TRUE)
