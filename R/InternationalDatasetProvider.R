#' international datasets
#' @export
InternationalDatasetProvider = R6::R6Class("InternationalDatasetProvider", inherit=DataProvider, public = list(
  
  #TODO: refactor to use caching directory
  
  initialize = function(providerController, path, ...) {
    super$initialize(providerController, ...)
  },
  
  spainAgeBreakdown = function() {
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
      covidAgeData = readr::read_csv("~/Git/uk-covid-datatools/data-raw/covid-by-age.csv")
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
  },
  
  italyTimeseries = function() {
    italyTimeseries = read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv")
    names(italyTimeseries) <- c(
      "date",	"state",	"Hospitalised with symptoms",	"intensive care",	"Total hospitalised",	
      "Home isolation",	"Total currently positive",	"New currently positive",	"discharged healed",	
      "deceased",	"Total cases",	"Tested", "note1","note2")
    return(italyTimeseries)
  }
  
))