## code to prepare `NHSCatchmentAreas` dataset goes here

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

postcode2LAD = postcode2LAD_tmp %>% left_join(postcodes %>% select(Postcode,lat=Latitude,long=Longitude,pop=Population,houses=Households,deprivation=`Index of Multiple Deprivation`) %>% distinct(), by=c("pcds"="Postcode"))
postcode2LAD = postcode2LAD %>% filter(!is.na(lat))
# all some sort of temporary test stuff

# length(unique(postcode2LAD$pcds)) == length(postcode2LAD$pcds)

# any(is.na(NHSCapacity2019$hospitals$long)) == FALSE
# any(is.na(NHSCapacity2019$hospitals$lat)) == FALSE
# any(is.na(postcode2LAD$lat)) == FALSE
# any(is.na(postcode2LAD$long)) == FALSE

# find the nearest hospital to all post codes
postcode2Hospital = postcode2LAD %>% ungroup() %>% tidyinfostats::findKNN(NHSCapacity2019$hospitals %>% filter(!is.na(lat)), pcds, hospitalId, k = 1, matchVars = vars(lat,long))

# calculate the fraction of the population of a LAD that each post code represents
postcode2LAD = postcode2LAD %>% group_by(ladcd) %>% mutate(ladPop = sum(pop,na.rm=TRUE)) %>% ungroup()
postcode2LAD = postcode2LAD %>% mutate(fracPop = pop/ladPop)

# aggregate to LAD, calculation fractions of population and assign them to a hospital
LAD2Hospital = postcode2LAD %>% inner_join(postcode2Hospital, by="pcds") %>% group_by(ladcd, hospitalId) %>% summarise(fracPop = sum(fracPop,na.rm=TRUE))

# aggregate hospitals to NHS trusts
LAD2Trust = LAD2Hospital %>% inner_join(NHSCapacity2019$hospitals, by="hospitalId") %>% group_by(ladcd,trustId) %>% summarise(fracPop = sum(fracPop,na.rm=TRUE))

NHSCatchmentAreas = LAD2Trust %>% rename(fractionOfLADPopulation = fracPop)

usethis::use_data(NHSCatchmentAreas)
