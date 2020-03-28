## code to prepare `NHSCatchmentAreas` dataset goes here
library(ensurer)
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