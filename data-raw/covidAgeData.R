dates = as.Date("2020-03-23"):(Sys.Date()-1)
fileUrls = paste0("https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/documentos/Actualizacion_",(dates - as.numeric(as.Date("2020-03-22")))+52,"_COVID-19.pdf")
filenames = fileUrls %>% stringr::str_extract("/([^/]+)$") %>% stringr::str_remove("/")
filenames = paste0("~/Git/uk-covid-datatools/data-raw/Spain/",filenames)

missingfileUrls = fileUrls[!file.exists(filenames)]
missingfilenames = filenames[!file.exists(filenames)]
missingdates = dates[!file.exists(filenames)]

for(i in seq_along(missingfilenames)) {
  download.file(missingfileUrls[i],missingfilenames[i])
}

extractSpainAgeTable = function(file) {
  tmp2 = tabulizer::extract_tables(file, output="data.frame")
  ageTable = tmp2[sapply(tmp2, function(d) any(d %>% pull(1) %>% stringr::str_detect("edad"), na.rm=TRUE))][[1]]
  
  cleanAgeTable = ageTable %>%
    unite(joined, everything(), sep=" ") %>% 
    mutate(joined = stringr::str_remove_all(joined," y \\+")) %>%
    mutate(joined = stringr::str_remove_all(joined,"\\.")) %>%
    mutate(joined = stringr::str_remove_all(joined,"[0-9]+,[0-9]+")) %>%
    mutate(joined = stringr::str_remove_all(joined,"NA")) %>%
    mutate(gender = ifelse(stringr::str_detect(joined,"Muj"), "Female", NA)) %>%
    mutate(gender = ifelse(stringr::str_detect(joined,"Hom"), "Male", gender)) %>%
    fill(gender) %>% mutate(gender = ifelse(is.na(gender),"Both",gender)) %>%
    separate(joined, into=c("age","cases","admitted","admittedIcu","died"),sep = "\\s+") %>%
    filter(stringr::str_starts(age,"[0-9]")) %>%
    mutate(
      admitted = as.numeric(admitted), 
      cases = as.numeric(cases),
      admittedIcu = as.numeric(admittedIcu),
      died = as.numeric(died)
    ) %>% 
    mutate(left = as.numeric(stringr::str_extract(age,"^[0-9]+"))) %>%
    filter(!is.na(left)) %>%
    group_by(gender) %>% arrange(age) %>%
    mutate(right = lead(left,default=120))
  return(cleanAgeTable)
}

if(file.exists("~/Git/uk-covid-datatools/data-raw/covid-by-age.csv")) {
  covidAgeData = read_csv("~/Git/uk-covid-datatools/data-raw/covid-by-age.csv")
} else {
  covidAgeData = tibble(filename=character())
}
processed = paste0("~/Git/uk-covid-datatools/data-raw/Spain/",unique(covidAgeData$filename))
unprocessed = !(filenames %in% processed)
unprocessedFilenames = filenames[unprocessed]
unprocessedDates = dates[unprocessed]


covidAgeData_toAdd = NULL
for (i  in seq_along(unprocessedFilenames)) {
  tmp = extractSpainAgeTable(unprocessedFilenames[i]) 
  tmp = tmp %>% mutate(
    filename=stringr::str_extract(unprocessedFilenames[i],"([^/]+$)"),
    date=as.Date(unprocessedDates[i],"1970-01-01"),
    country = "Spain"
  )
  covidAgeData_toAdd = covidAgeData_toAdd %>% bind_rows(tmp)
}
  #mutate(Year = years) %>%
  #mutate(Week = weeks) %>%
  # select(-filename) %>%
# covidAgeData_toAdd = covidAgeData_toAdd %>% mutate(
#     
#   ) %>%
#   select(-unprocessedFilenames) %>%
#   unnest(cols=contents)

if (length(unprocessedFilenames) > 0) {
  covidAgeData = bind_rows(covidAgeData,covidAgeData_toAdd)
  covidAgeData %>% write_csv("~/Git/uk-covid-datatools/data-raw/covid-by-age.csv")
  usethis::use_data(covidAgeData, overwrite=TRUE)
} else {
  message("Nothing to update")
}

#### ----

ONSurls = c(
  "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/publishedweek1620201.xlsx"
)

filenames = ONSurls %>% stringr::str_extract("/([^/]+)$") %>% stringr::str_remove("/")
filenames = paste0("~/Git/uk-covid-datatools/data-raw/ONS/",filenames)

missingfileUrls = ONSurls[!file.exists(filenames)]
missingfilenames = filenames[!file.exists(filenames)]

for(i in seq_along(missingfilenames)) {
  download.file(missingfileUrls[i],missingfilenames[i])
}

loadONS = function(file) {
  ONS <- read_excel(file,
    sheet = "Covid-19 - Weekly occurrences",
    range = "B6:BC75")
  ONS2 = ONS %>% rename(age=1) %>% mutate(gender = case_when(
    stringr::str_detect(age,"Female") ~ "Female",
    stringr::str_detect(age,"Male") ~ "Male",
    TRUE ~ as.character(NA)
  )) %>% fill(gender) %>% mutate(gender = ifelse(is.na(gender),"Both",gender)) %>%
  pivot_longer(cols = c(-age,-gender),names_to = "date",values_to = "died") %>%
  mutate(age = ifelse(age=="<1","0-1",age)) %>% filter(stringr::str_starts(age,"[0-9]") & stringr::str_starts(date,"[0-9]") & !is.na(died)) %>% mutate(date = as.Date(as.numeric(date),"1970-01-01"))
  ONS2 = ONS2 %>% group_by(gender,date) %>% mutate(left = as.numeric(stringr::str_extract(age,"^[0-9]+"))) %>% arrange(left) %>% mutate(right=lead(left,default=120))
  ONS3 = ONS2 %>% ungroup() %>% group_by(age,gender) %>% arrange(date) %>% mutate(died = cumsum(died)) %>% mutate(cases=NA,admitted=NA,admittedIcu=NA, country="E&W",filename=file %>% stringr::str_extract("/([^/]+)$") %>% stringr::str_remove("/"))
  return(ONS2)
}

loadONS(filenames[length(filenames)])


#### UK age cases ----
ageLabels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")
linelistPath = "~/S3/encrypted/2020-05-07/Anonymised Line List 20200507.xlsx"

ll = ukcovidtools::getLineList(linelistPath)
llDemog = ll %>% filter(!is.na(age) & sex != "UNKNOWN") %>% mutate(ageCat = cut(age, 
                                     breaks = c(ageLabels %>% stringr::str_extract("^[0-9]+") %>% as.integer(),Inf),
                                     labels = ageLabels,
                                     include.lowest = TRUE
), gender=stringr::str_sub(sex,1,1), specimen_date=as.Date(specimen_date)) %>% group_by(ageCat,gender,specimen_date) %>% summarise(
  incidence = n()
)

#%>% group_by(sex,ageCat,lab_report_date) %>% summarise(incidence = n())
ukDemog = UKDemographics2018$by2018Ward %>% ungroup() %>% group_by(gender,ageGroup) %>% summarise(pop = sum(count))

dateRange = tibble(specimen_date = as.Date(min(llDemog$specimen_date):max(llDemog$specimen_date),"1970-01-01")) %>% 
  crossing(ageCat=ageLabels) %>% 
  crossing(gender=c("MALE","FEMALE"))

demog2 = llDemog %>% inner_join(ukDemog,by=c("ageCat"="ageGroup","gender"="gender")) %>% group_by(ageCat,sex,specimen_date) %>% summarise(
  incidence = n(),
  incidencePer100K = n()/first(pop)*100000,
) 

# #### PHE Coronavirus cases by age ----
# # url = "https://coronavirus.data.gov.uk/"
# json_url = "https://c19downloads.azureedge.net/downloads/data/data_latest.json"
# # "https://coronavirus.data.gov.uk/downloads/json/dated/coronavirus-cases_202004292132.json"
# # "https://coronavirus.data.gov.uk/downloads/json/dated/data_latest_202004292132.json"
# tmp = jsonlite::read_json(json_url,simplifyVector = TRUE)
# 
# # glimpse(tmp$utlas$E09000002)
# # tmp3 = enframe(tmp$utlas,name = "UTLA")
# # tmp3 = tmp3 %>% unnest(cols=value)
# # tmp3 = tmp3 %>% mutate(name = map(value, ~.x$name$value))
# 
# cases = bind_rows(
#   tmp$countries$E92000001$maleCases %>% mutate(gender="Male"),
#   tmp$countries$E92000001$femaleCases %>% mutate(gender="Female") 
# )

# ONS age breakdown deaths
# https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/publishedweek172020.xlsx
# 

