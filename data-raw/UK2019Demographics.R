## code to prepare `UK2019Demographics` dataset goes here

library(readxl)
library(tidyverse)
url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
destfile <- tempfile("ukmidyearestimates20182019ladcodes",fileext = ".xls")
curl::curl_download(url, destfile)
ukmidyearestimates20182019ladcodes <- read_excel(destfile, sheet="MYE2-All", skip = 4)
ageCols = colnames(ukmidyearestimates20182019ladcodes)[!is.na(as.integer(colnames(ukmidyearestimates20182019ladcodes)))]
tmp = ukmidyearestimates20182019ladcodes %>% 
  tidyr::pivot_longer(cols=all_of(ageCols),names_to = "age",values_to = "count", names_ptypes = list(age=integer()))
UK2019Demographics = tmp %>% rename(code = Code, name=Name, aggregation = Geography1, total=`All ages`) %>% 
  mutate(aggregation = as.factor(aggregation))

usethis::use_data(UK2019Demographics)

