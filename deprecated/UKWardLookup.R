## code to prepare `UKWardLookup2011` dataset goes here

# Helpful resource to get your head around all this nonsense:
# http://infuse.mimas.ac.uk/help/definitions/2011geographies/
# also has shapefiles at the diferent resolutions

# Ward to LAD mapping file (at point of 2011 census)
# https://opendata.arcgis.com/datasets/d8070e25d9084aec8823170ff6e2da26_0.csv
# https://geoportal.statistics.gov.uk/datasets/ward-to-census-merged-ward-to-local-authority-district-december-2011-lookup-in-england-and-wales

# should be 8607 long

library(readr)
UKWardLookup2011 <- read_csv("https://opendata.arcgis.com/datasets/d8070e25d9084aec8823170ff6e2da26_0.csv", 
    col_name=TRUE,cols(
      WD11CD = col_character(),
      WD11NM = col_character(),
      WD11NMW = col_character(),
      CMWD11CD = col_character(),
      CMWD11NM = col_character(),
      CMWD11NMW = col_character(),
      IND = col_double(),
      LAD11CD = col_character(),
      LAD11NM = col_character(),
      LAD11NMW = col_character(),
      FID = col_integer()
    ))

UKWardLookup2019 <- read_csv( 
  "https://opendata.arcgis.com/datasets/e169bb50944747cd83dcfb4dd66555b1_0.csv",
  col_name=TRUE,cols(
    WD19CD = col_character(),
    WD19NM = col_character(),
    LAD19CD = col_character(),
    LAD19NM = col_character(),
    FID = col_integer()
  ))

usethis::use_data(UKWardLookup2011, overwrite = TRUE)
usethis::use_data(UKWardLookup2019, overwrite = TRUE)


getZipfile = function(filename,url) {
  wardsZip = paste0("~/Git/uk-covid-datatools/data-raw/Postcodes/",filename,".zip")
  unzipDir = paste0("~/Git/uk-covid-datatools/data-raw/Postcodes/",filename)
  if(!file.exists(wardsZip)) {
    download.file(url,wardsZip)
    wd = getwd()
    if (!dir.exists(unzipDir)) dir.create(unzipDir)
    setwd(unzipDir)
    unzip(wardsZip)
    setwd(wd)
  }
  return(paste0(unzipDir,"/",filename,".zip"))
}

# Postcode to MLOA11 / LSOA11
# https://geoportal.statistics.gov.uk/datasets/postcode-to-output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-december-2011-lookup-in-england-and-wales
# https://www.arcgis.com/sharing/rest/content/items/ef72efd6adf64b11a2228f7b3e95deea/data
# PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LU_aligned_v2
getZipfile("PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LU_aligned_v2","https://www.arcgis.com/sharing/rest/content/items/ef72efd6adf64b11a2228f7b3e95deea/data")
postcodes_to_LSOA11 <- read_csv("~/Git/uk-covid-datatools/data-raw/Postcodes/PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LU_aligned_v2/PCD11_OA11_LSOA11_MSOA11_LAD11_EW_LU_aligned_v2.csv", 
  col_types = cols(
    LAD11NMW = col_character(), 
    PCDOASPLT = col_integer()
))


# Postcode to WD11
# https://geoportal.statistics.gov.uk/datasets/postcode-to-parish-to-ward-to-local-authority-district-december-2011-lookup-in-england-and-wales
# https://www.arcgis.com/sharing/rest/content/items/c4aeb11ff5b045018b7340e807d645cb/data
# pcd11_par11_wd11_lad11_ew_lu
getZipfile("pcd11_par11_wd11_lad11_ew_lu", "https://www.arcgis.com/sharing/rest/content/items/c4aeb11ff5b045018b7340e807d645cb/data")
postcodes_to_WD11 <- read_csv("~/Git/uk-covid-datatools/data-raw/Postcodes/pcd11_par11_wd11_lad11_ew_lu/pcd11_par11_wd11_lad11_ew_lu.csv", 
  col_types = cols(
    lad11nmw = col_character(), 
    par11cd = col_character(), 
    par11nm = col_character(), 
    par11nmw = col_character(), 
    wd11nmw = col_character()
))



# WD11 -> MSOA11 to LAD19
# https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-area-2011-to-ward-to-lad-december-2019-lookup-in-england-and-wales
# https://opendata.arcgis.com/datasets/0b3c76d1eb5e4ffd98a3679ab8dea605_0.csv
MSOA11_to_LAD19 = read_csv("https://opendata.arcgis.com/datasets/0b3c76d1eb5e4ffd98a3679ab8dea605_0.csv", 
  col_types = cols(FID = col_integer()))

# WD11 -> LSOA11 to WD19 / LAD19
# https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-area-2011-to-ward-2019-lookup-in-england-and-wales
# https://opendata.arcgis.com/datasets/15299a7b8e6c498d94a08b687c75b73f_0.csv
LSOA11_to_LAD19 <- read_csv("https://opendata.arcgis.com/datasets/15299a7b8e6c498d94a08b687c75b73f_0.csv", 
  col_types = cols(FID = col_integer()))

# join on postcode to give WD11 to MLOA11 / LSOA11: many to many
WD11_to_LSOA11 = postcodes_to_WD11 %>% select(PCD7 = pcd7, PCD8 = pcd8, WD11CD = wd11cd, WD11NM = wd11nm) %>% inner_join(
  postcodes_to_LSOA11 %>% select(PCD7, PCD8, OA11CD, LSOA11CD, LSOA11NM, MSOA11CD, MSOA11NM, LAD11CD, LAD11NM), by=c("PCD7","PCD8")
) %>% select(-PCD7,-PCD8) %>% distinct()

# ?missing WD11s? - should be none
postcodes_to_WD11 %>% select(WD11CD = wd11cd) %>% anti_join(WD11_to_LSOA11,by="WD11CD") %>% ensure_that(nrow(.)==0)

# There are some missing areas that are in the city of london and the ilses of scilly - they are areas with very low resident demographics.
WD11_to_LAD19 = WD11_to_LSOA11 %>% inner_join(LSOA11_to_LAD19, by=c("LSOA11CD")) %>% select(WD11CD, WD11NM, LAD19CD, LAD19NM) %>% distinct()

missing = UKWardLookup2011 %>% anti_join(WD11_to_LAD19,by="WD11CD") %>% select(WD11CD, WD11NM,LAD11CD) %>% left_join(UKWardLookup2019, by=c("WD11NM"="WD19NM", "LAD11CD"="LAD19CD")) %>% 
  select(WD11CD, WD11NM, LAD19CD=LAD11CD, LAD19NM)

WD11_to_LAD19 = WD11_to_LAD19 %>% bind_rows(missing)

# ?missing WD11s? - should be none but there are some small population areas in cetral london and isels of scilly that are missed.
UKWardLookup2011 %>% anti_join(WD11_to_LAD19,by="WD11CD") %>% ensure_that(nrow(.)==0)
WD11_to_LAD19 %>% filter(is.na(LAD19CD)) %>%  ensure_that(nrow(.)==0)

# missing LADs in mapping - yes there are but they are all scotland and N ireland
UKWardLookup2019 %>% select(LAD19CD, LAD19NM) %>% distinct() %>% anti_join(WD11_to_LAD19,by="LAD19CD") %>% filter(!str_starts(LAD19CD,"S|N")) %>% ensure_that(nrow(.)==0) %>% invisible()
# View(UKWardLookup2019 %>% select(LAD19CD, LAD19NM) %>% distinct() %>% anti_join(WD11_to_LAD19,by="LAD19CD"))
# is it one to one?

WD11_to_LAD19 %>% group_by(WD11CD,LAD19CD) %>% count() %>% filter(n > 1) %>% ensure_that(nrow(.)==0) %>% invisible()

write.csv(WD11_to_LAD19, "~/Git/uk-covid-datatools/data-raw/WD11_to_LAD19.csv")
usethis::use_data(WD11_to_LAD19, overwrite = TRUE)

WD11_to_LSOA11 = WD11_to_LSOA11 %>% select(WD11CD,WD11NM,LSOA11CD,LSOA11NM) %>% distinct()
WD11_to_LSOA11 %>% group_by(WD11CD) %>% count() %>% filter(n > 1) %>% ensure_that(nrow(.)==0) %>% invisible()

write.csv(WD11_to_LSOA11, "~/Git/uk-covid-datatools/data-raw/WD11_to_LSOA11.csv")
usethis::use_data(WD11_to_LSOA11, overwrite = TRUE)

LSOA11_to_LAD19 = LSOA11_to_LAD19 %>% select(LSOA11CD,LSOA11NM,LAD19CD,LAD19NM) %>% distinct()
LSOA11_to_LAD19 %>% group_by(LSOA11CD) %>% count() %>% filter(n > 1) %>% ensure_that(nrow(.)==0) %>% invisible()
write.csv(WD11_to_LSOA11, "~/Git/uk-covid-datatools/data-raw/LSOA11_to_LAD19.csv")
usethis::use_data(LSOA11_to_LAD19, overwrite = TRUE)

#### load PHE mapping ----

PHE_region_to_NHS_region <- read_csv("~/Git/uk-covid-datatools/data-raw/PHE_region_to_NHS_region.csv")
usethis::use_data(PHE_region_to_NHS_region, overwrite = TRUE)
