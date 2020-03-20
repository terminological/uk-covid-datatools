## code to prepare `UKILIdata` dataset goes here

library(tidyverse)
library(readODS)

library(rvest)
library(stringr)

urls = c(
  "https://www.gov.uk/government/publications/gp-in-hours-weekly-bulletins-for-2020",
  "https://www.gov.uk/government/publications/gp-in-hours-weekly-bulletins-for-2019",
  "https://www.gov.uk/government/publications/gp-in-hours-weekly-bulletins-for-2018",
  "https://www.gov.uk/government/publications/gp-in-hours-bulletin",
  "https://www.gov.uk/government/publications/gp-in-hours-weekly-bulletins-for-2016",
  "https://www.gov.uk/government/publications/gp-in-hours-weekly-bulletins-for-2015",
  "https://www.gov.uk/government/publications/gp-in-hours-weekly-bulletins-for-2014"
)

fileUrls = NULL
for (url in urls) {
  # url = "https://www.gov.uk/government/publications/gp-in-hours-weekly-bulletins-for-2019"
  page <- xml2::read_html(url)

  fileUrls = c(fileUrls,page %>%
    html_nodes("a") %>%       # find all links
    html_attr("href") %>%     # get the url
    str_subset("\\.xls|\\.ods"))
}

fileUrls = unique(fileUrls)
filenames = fileUrls %>% stringr::str_extract("/([^/]+)$") %>% stringr::str_remove("/")
filenames = paste0("~/Git/uk-covid-datatools/data-raw/ILI/",filenames)

missingfileUrls = fileUrls[!file.exists(filenames)]
missingfilenames = filenames[!file.exists(filenames)]

for(i in seq_along(missingfilenames)) {
    download.file(missingfileUrls[i],missingfilenames[i])
}

readILI <- function(fname) {
  message(fname)
  if(str_split(fname,"\\.",simplify = TRUE)[,2]=="xls")
  {
    #print("XLS")
    metadata = readxl::read_xls(path=fname,sheet=1,range="A6:B10", col_names = c("name","val"), col_types = "text")
    readxl::read_xls(path=fname,sheet=3,skip = 5, col_names = F)->tmp
  } else {
    metadata = readODS::read_ods(path=fname,sheet=1,range="A6:B10", col_names = FALSE, col_types = NA) %>% rename(name=A, val=B)
    readODS::read_ods(path=fname,sheet=3,skip = 5, col_names = F )->tmp
  }
  metadata = metadata %>% pivot_wider( names_from = name, values_from = val ) %>% mutate(
    DateStarting = tryCatch(
      as.Date(`Date starting`,tryFormats=c("%d/%m/%Y","%d/%m/%y")),
      error=function(e) as.Date(as.integer(`Date starting`),origin="1900-01-01")),
    DateEnding = tryCatch(
      as.Date(`Date ending`,tryFormats=c("%d/%m/%Y","%d/%m/%y")),
      error=function(e) as.Date(as.integer(`Date ending`),origin="1900-01-01"))
  )
  tmp %>%
    select(
      LACode=1,
      LAName=2,
      PHECentreName=3,
      PHECentreCode=4,
      PHERegionName =5,
      PHERegionCode =6,
      Denominator =7,
      ILI = 8,
      RatePer100000=9) %>%
    mutate(ILI = as.integer(ILI),
      Denominator = as.integer(Denominator),
      RatePer100000 = as.double(RatePer100000),
      Week = as.integer(metadata$`Week number`),
      Year = lubridate::year(metadata$DateStarting),
      DateStarting = as.Date(metadata$DateStarting),
      DateEnding = as.Date(metadata$DateEnding)
    ) %>%
    filter(row_number()<150)->tmp
  return(tmp)
}



#filename = dir(path = "~/Git/uk-covid-datatools/data-raw/",recursive = TRUE,pattern = "GP")
#weeks = filename %>% stringr::str_remove_all("_") %>% stringr::str_extract("([0-9]+)\\.") %>% stringr::str_remove("\\.") %>% as.integer()
#years = filename %>% stringr::str_remove_all("_") %>% stringr::str_extract("([0-9]{4})") %>% as.integer()
#years = ifelse(is.na(years),2017,years)

UKILIdata_toAdd = tibble(missingfilenames) %>%
  mutate(contents = map(missingfilenames,~readILI(paste0("~/Git/uk-covid-datatools/data-raw/",.))))%>%
  #mutate(Year = years) %>%
  #mutate(Week = weeks) %>%
  # select(-filename) %>%
  unnest(cols=contents)

if (nrow(UKILIdata_toAdd) > 0) {
  UKILIdata = read_csv("~/Git/uk-covid-datatools/data-raw/ilidata.csv")
  UKILIdata = bind_rows(UKILIdata,UKILIdata_toAdd)
  UKILIdata %>% write_csv("~/Git/uk-covid-datatools/data-raw/ilidata.csv")
  usethis::use_data(UKILIdata, overwrite=TRUE)
} else {
  message("Nothing to update")
}

# library(cowplot)
# ilidata %>%
#   select(-filename) %>%
#   ggplot(aes(y=RatePer100000,x=Week, colour = LACode))+
#   geom_line()+
#   facet_wrap(~Year)+
#   theme_minimal_grid()+
#   theme(legend.position = "")+
#   ylab("ILI Rate per 100 000")


