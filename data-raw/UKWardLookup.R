## code to prepare `UKWardLookup` dataset goes here

# Ward to LAD mapping file (at point of 2011 census)
# https://opendata.arcgis.com/datasets/d8070e25d9084aec8823170ff6e2da26_0.csv
# https://geoportal.statistics.gov.uk/datasets/ward-to-census-merged-ward-to-local-authority-district-december-2011-lookup-in-england-and-wales


library(readr)
UKWardLookup <- read_csv("https://opendata.arcgis.com/datasets/d8070e25d9084aec8823170ff6e2da26_0.csv", 
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

usethis::use_data(UKWardLookup)
