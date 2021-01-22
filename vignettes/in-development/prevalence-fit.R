onsinfectionsurvey = readxl::read_excel(
  "~/Dropbox/covid19/prevalence-estimates/covid19infectionsurveydatasets2020122423122020174305.xlsx", 
  sheet="8b", skip=5, col_names=c("date",
    sapply(c("England","North East","North West","Yorkshire and The Humber","East Midlands","West Midlands","East of England","London","South East","South West"),function(x) paste0(x,c(".mean",".lower",".upper"))) %>% as.vector())
  )


onsinfectionsurvey = onsinfectionsurvey %>% mutate(pubDate = ifelse(date %>% stringr::str_starts("Publication date"),date,NA_character_))
onsinfectionsurvey = onsinfectionsurvey %>% fill(pubDate)
onsinfectionsurvey = onsinfectionsurvey %>% filter(!is.na(as.numeric(date)))
onsinfectionsurvey = onsinfectionsurvey %>% mutate(pubDate = as.Date(stringr::str_remove(pubDate,"Publication date ")))
onsinfectionsurvey = onsinfectionsurvey %>% mutate(date = as.Date(as.numeric(date)-2,"1900-01-01"))

onsinfectionsurvey = onsinfectionsurvey %>% pivot_longer(cols=c(-date,-pubDate),names_to="name",values_to="value")

onsinfectionsurvey = onsinfectionsurvey %>% separate(name,c("name","variable"),sep="\\.")
onsinfectionsurvey = onsinfectionsurvey %>% mutate(value = as.numeric(value))
onsinfectionsurvey = onsinfectionsurvey %>% pivot_wider(names_from = variable, values_from = value)