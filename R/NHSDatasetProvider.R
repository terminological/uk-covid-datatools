#' NHS datasets
#' @export
NHSDatasetProvider = R6::R6Class("NHSDatasetProvider", inherit=CovidTimeseriesProvider, public = list(
    
    initialize = function(providerController, ...) {
      super$initialize(providerController, ...)
    },
    
    #' @description Load google mobility file
    #' @return google mobility 0 custom format
    getGoogleMobility = function(...) {
      self$getDaily("GOOGLE_MOBILITY", ..., orElse = function (...) {
        Global_Mobility_Report <- readr::read_csv(
          "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", 
          col_types = cols(
            sub_region_1 = col_character(), sub_region_2 = col_character(), iso_3166_2_code = col_character(), 
            census_fips_code = col_character(), date = col_date(format = "%Y-%m-%d"))) %>% filter(country_region_code == "GB")
        return(Global_Mobility_Report %>% dplyr::ungroup())
      })
    },
    
    #' @description Load curated significant dates file
    #' @return public 111 data including pathways and online, pre and post april. Codes are CCG20 codes.
    getSignificantDates = function(...) {
      self$getDaily("SIGNIF_DATES", ..., orElse = function (...) {
        tmp = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQyFhVOWoTycrhHYMv9HsC4DslM0vqgngRKsSIQdYBTxSS0cR6zEKz0o8f2ZAyhx5teomVBEbsETPSN/pub?gid=0&single=true&output=csv")
      })
    },
    
    #' @description Load public 111 summary file
    #' @return public 111 data including pathways and online, pre and post april. Codes are CCG20 codes.
    getPublicOneOneOne = function(...) {
      self$getDaily("PUBLIC-111", ..., orElse = function (...) covidTimeseriesFormat({
        # https://digital.nhs.uk/data-and-information/publications/statistical/mi-potential-covid-19-symptoms-reported-through-nhs-pathways-and-111-online/latest
        # Load landing page
        tmp = xml2::read_html("https://digital.nhs.uk/data-and-information/publications/statistical/mi-potential-covid-19-symptoms-reported-through-nhs-pathways-and-111-online/latest")
        links = tmp %>% rvest::html_nodes(xpath="//a") %>% rvest::html_attr(name = "href")
        url = function(str) {return(links[links %>% stringr::str_detect(str)])}
        
        #Load files matched from landing page
        pathwaysPreApril = readr::read_csv(url("NHS%20Pathways%20Covid-19%20data%20CCG%20mapped"), 
                col_types = readr::cols(SiteType = readr::col_character(),  `Call Date` = readr::col_date(format = "%d/%m/%Y")))
        pathways = readr::read_csv(url("NHS%20Pathways%20Covid-19%20data%202020"), 
                col_types = readr::cols(SiteType = readr::col_character(), `Call Date` = readr::col_date(format = "%d/%m/%Y")))
        online = readr::read_csv(url("111%20Online%20Covid-19%20data_2020"), 
                col_types = readr::cols(`journeydate` = readr::col_date(format = "%d/%m/%Y")))
        onlinePreApril = readr::read_csv(url("111%20Online%20Covid-19%20data_CCG%20mapped"), 
                col_types = readr::cols(`journeydate` = readr::col_date(format = "%d/%m/%Y")))
        
        out = bind_rows(
          # pathways spreadsheets
          pathwaysPreApril %>% 
            dplyr::select(-CCGCode, -CCGName) %>% 
            dplyr::rename(source = SiteType, date = `Call Date`, gender = `Gender`, ageCat = `AgeBand`, code = `April20 mapped CCGCode`, name=`April20 mapped CCGName`, incidence = `TriageCount`) %>%
            dplyr::group_by(source,date,gender,ageCat,code,name) %>% 
            dplyr::summarise(incidence = sum(incidence)) %>% 
            dplyr::ungroup(),
          
          pathways %>% 
            dplyr::filter(`Call Date` > max(pathwaysPreApril$`Call Date`,na.rm=TRUE)) %>% 
            dplyr::rename(source = SiteType, date = `Call Date`, gender = `Sex`, ageCat = `AgeBand`, code = `CCGCode`, name=`CCGName`, incidence = `TriageCount`) %>% 
            dplyr::ungroup(),
          
          onlinePreApril %>% 
            dplyr::select(-CCGCode, -CCGName) %>% 
            dplyr::mutate(source = "online") %>% 
            dplyr::rename(date = `journeydate`, gender = `gender`, ageCat = `ageband`, code = `April20 mapped CCGCode`, name=`April20 mappedCCGName`, incidence = `Total`) %>%
            dplyr::group_by(source,date,gender,ageCat,code,name) %>% 
            dplyr::summarise(incidence = sum(incidence)) %>% 
            dplyr::ungroup(),
          
          online %>% 
            dplyr::filter(journeydate > max(onlinePreApril$journeydate,na.rm=TRUE)) %>% dplyr::mutate(source = "online") %>% 
            dplyr::rename(date = `journeydate`, gender = `sex`, ageCat = `ageband`, code = `ccgcode`, name=`ccgname`, incidence = `Total`) %>% 
            dplyr::ungroup()
        )
        
        # standardise age categories
        out2 = out %>% dplyr::mutate(
          gender = self$normaliseGender(gender),
          ageCat = case_when(
            ageCat == "70-120 years" ~ "70+",
            ageCat == "70+ years" ~ "70+",
            ageCat == "19-69 years" ~ "19-69",
            TRUE ~ NA_character_
          ))
        out2 = out2 %>% 
          dplyr::filter(!is.na(ageCat) & code != "NULL" & name != "NULL")
        out2 = out2 %>% 
          #dplyr::group_by(gender,ageCat,code,name,source) %>% 
          #dplyr::arrange(date) %>% 
          dplyr::mutate(codeType = "CCG20", type="incidence",statistic="triage",subgroup=NA, note=NA, source=paste0(source,"-public")) %>% 
          dplyr::rename(value=incidence)
        # out2 = out2 %>% dplyr::select(-name) %>% self$codes$findNamesByCode(outputCodeTypeVar = NULL)
        return(out2 %>% self$fillAbsent() %>% self$fixDatesAndNames(0) %>% self$complete())
      }))
    },
    
    
    # TODO: https://www.health-ni.gov.uk/publications/daily-dashboard-updates-covid-19-august-2020
    # Wales: http://www2.nphs.wales.nhs.uk:8080/CommunitySurveillanceDocs.nsf/3dc04669c9e1eaa880257062003b246b/77fdb9a33544aee88025855100300cab/$FILE/Rapid%20COVID-19%20surveillance%20data.xlsx
    # Scotland: https://www.opendata.nhs.scot/dataset/covid-19-in-scotland
    
    getTomWhiteCases = function(...) {
      self$getDaily("TOM-WHITE-CASES", ..., orElse = function (...) covidTimeseriesFormat({
        walesUAtoHealthBoard = self$codes$getMappings() %>% filter(fromCodeType=="UA",toCodeType=="LHB")
        covid_19_cases_uk <- readr::read_csv("https://github.com/geeogi/covid-19-uk-data/raw/master/data/covid-19-cases-uk.csv", 
                                      col_types = readr::cols(Date = readr::col_date(format = "%Y-%m-%d")), 
                                      na = c("","NaN","NA"))
        tmp_cases_uk = covid_19_cases_uk %>% 
          dplyr::mutate(value = as.numeric(TotalCases)) %>%
          dplyr::rename(code = AreaCode, date = Date) %>%
          dplyr::mutate(code = case_when(
            !is.na(code) ~ code,
            Country == "England" ~ "E99999999",
            Country == "Scotland" ~ "S99999999",
            Country == "Wales" ~ "W99999999",
            Country == "Northern Ireland" ~ "N99999999",
            TRUE ~ NA_character_)) %>% 
          dplyr::left_join(walesUAtoHealthBoard, by=c("code"="fromCode")) %>%
          dplyr::mutate(
            code = if_else(is.na(toCode),code,toCode)
          ) %>% 
          dplyr::group_by(code,date) %>% 
          dplyr::summarise(value = sum(value, na.rm=TRUE)) %>% 
          dplyr::filter(!is.na(code)) %>% 
          self$codes$findNamesByCode() %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            statistic = "case",
            source = "covid-19-cases-uk-tom-white",
            gender = NA_character_,
            ageCat = NA_character_,
            type = "cumulative",
            subgroup = NA_character_
          ) %>%
          #dplyr::group_by(code,codeType,name,source,subgroup,statistic,gender,ageCat,type) %>% 
          #tidyr::complete(date = as.Date(min(date):max(date),"1970-01-01")) %>%
          #tidyr::fill(value) %>%
          self$complete() %>%
          dplyr::ungroup()
        return(tmp_cases_uk %>% self$fillAbsent() %>% self$fixDatesAndNames(4) %>% self$complete())
      }))
    },
      
    #browser()
    getTomWhiteIndicators = function(...) {
      self$getDaily("TOM-WHITE-INDIC", ..., orElse = function (...) covidTimeseriesFormat({
        covid_19_indicators_uk <- readr::read_csv("https://github.com/geeogi/covid-19-uk-data/raw/master/data/covid-19-indicators-uk.csv", 
                                           col_types = readr::cols(Date = readr::col_date(format = "%Y-%m-%d")))
        
        country_totals = covid_19_indicators_uk %>% 
          dplyr::filter(Date > as.Date("2020-02-12")) %>%
          dplyr::group_by(Indicator,Country) %>% 
          tidyr::complete(Date = as.Date(min(Date):max(Date),"1970-01-01")) %>%
          tidyr::fill(Value) %>% 
          dplyr::ungroup() %>%
          tidyr::pivot_wider(names_from = Country, values_from = Value) %>%
          dplyr::mutate(`England` = ifelse(is.na(`England`),`UK`-`Northern Ireland`-`Wales`-`Scotland`,`England`)) %>%
          dplyr::mutate(missing = `UK`-`England`-`Northern Ireland`-`Wales`-`Scotland`)
          
        country_totals2 = country_totals %>%
          tidyr::pivot_longer(cols=c(-Indicator,-Date), names_to = "name", values_to = "value") %>%
          dplyr::mutate(statistic = case_when(
            Indicator == "ConfirmedCases" ~ "case",
            Indicator == "Deaths" ~ "death",
            Indicator == "Tests" ~ "test",
            ),
            type = ifelse(name == "missing", "bias", "cumulative"),
            name = ifelse(name=="missing" | name=="UK","United Kingdom", name)
          ) %>%
          self$codes$findCodesByName() %>%
          dplyr::rename(date = Date) %>%
          dplyr::select(-Indicator,-name.original) %>%
          dplyr::mutate(
            source = "covid-19-indicators-uk",
            gender = NA_character_,
            ageCat = NA_character_,
            subgroup = NA_character_
          )
        
        country_totals2 %>% 
          # dplyr::group_by(code,codeType,name,source,subgroup,statistic,gender,ageCat,type) %>% 
          # tidyr::complete(date = as.Date(min(date):max(date),"1970-01-01")) %>%
          # tidyr::fill(value) %>%
          self$complete() %>%
          dplyr::ungroup()
        
        return(country_totals2 %>% self$fixDatesAndNames(4))
        
      }))
    },
  
    getNHSDeaths = function(...) {
      self$getDaily("NHS-DEATHS", ..., orElse = function (...) covidTimeseriesFormat({
        # Load landing page
        tmp = xml2::read_html("https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/")
        links = tmp %>% rvest::html_nodes(xpath="//a") %>% rvest::html_attr(name = "href")
        url = function(str) {return(links[links %>% stringr::str_detect(str)])}
        #Load files matched from landing page
        file = self$downloadDaily(id = "NHS_DEATHS",url = url("COVID-19-total-announced-deaths(?!.*-weekly-file)(.*)\\.xlsx"),type="xlsx")
        NHS_DEATHS <- suppressMessages(readxl::read_excel(file, sheet = "Tab4 Deaths by trust", skip = 15))
        NHS_DEATHS = NHS_DEATHS %>% 
          dplyr::select(-c(1,2,5)) %>%
          dplyr::rename(code=Code,name=Name) %>%
          dplyr::mutate(
            code = ifelse(name=="ENGLAND","E92000001",code),
            codeType = ifelse(name=="ENGLAND","CTRY","NHS trust")
          ) %>% 
          dplyr::filter(!is.na(code))
          
        out = NHS_DEATHS %>% 
          tidyr::pivot_longer(cols=c(-code,-codeType, -name), names_to = "date", values_to = "value") %>% 
          dplyr::mutate(date=suppressWarnings(as.Date(as.numeric(date),"1899-12-30"))) %>% 
          dplyr::filter(!is.na(date)) %>%
          dplyr::mutate(
            source = "COVID-19-total-announced-deaths",
            subgroup=NA_character_,
            statistic="death",
            type="incidence",
            gender=NA_character_,
            ageCat=NA_character_
          )
        out = out %>%  self$fillAbsent() %>% self$fixDatesAndNames(4) %>% self$complete()
        return(out)
        
      }))
    },
    
    #' @description Get UK outbreak timeseries data
    #' @return a covidTimeserisFormat data frame with several timeseries in it
    getPHEDashboard = function(...) {
      self$getDaily("PHE-DASH", ..., orElse = function (...) covidTimeseriesFormat({
        ph_cases = readr::read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv", 
                  col_types = readr::cols(`Specimen date` = readr::col_date(format = "%Y-%m-%d")))
        ph_cases = ph_cases %>% select(date = `Specimen date`, code = `Area code`, name=`Area name`,cumulative_cases = `Cumulative lab-confirmed cases`, daily_cases = `Daily lab-confirmed cases`, type=`Area type`)
        full_ph_cases = ph_cases %>% 
          tidyr::complete(date,tidyr::nesting(code,name,type)) %>% 
          dplyr::group_by(code,name,type) %>% 
          dplyr::arrange(date) %>% 
          tidyr::fill(cumulative_cases) %>% 
          dplyr::mutate(daily_cases=ifelse(is.na(daily_cases),0,daily_cases), cumulative_cases=ifelse(is.na(cumulative_cases),0,cumulative_cases)) %>% 
          dplyr::mutate(codeType = case_when(
            type == "Nation" ~ "CTRY",
            type == "Region" ~ "RGN",
            type == "Upper tier local authority" ~ "UTLA",
            type == "Lower tier local authority" ~ "LAD",
            type == "utla" ~ "UTLA",
            type == "ltla" ~ "LAD",
            type == "nation" ~ "CTRY",
            type == "region" ~ "RGN"
          )) %>%
          dplyr::ungroup() %>%
          dplyr::select(-type)
        
        # missingness2 = full_ph_cases %>% filter(type == "Nation" & name == "England") %>% select(-type) %>% 
        #   left_join(
        #     full_ph_cases %>% filter(codeType == "RGN") %>% select(-type) %>% group_by(date) %>% summarise(regions_cumulative_cases = sum(cumulative_cases), regions_daily_cases = sum(daily_cases), regions_count = n())
        #   ) %>% left_join(
        #     full_ph_cases %>% filter(codeType == "UTLA") %>% select(-type) %>% group_by(date) %>% summarise(utla_cumulative_cases = sum(cumulative_cases), utla_daily_cases = sum(daily_cases), utla_count = n())
        #   ) %>% left_join(
        #     full_ph_cases %>% filter(codeType == "LAD") %>% select(-type) %>% group_by(date) %>% summarise(ltla_cumulative_cases = sum(cumulative_cases), ltla_daily_cases = sum(daily_cases), ltla_count = n())
        #   )
        
        # full_ph_cases %>% group_by(code,name,codeType) %>% 
        #   arrange(date) %>% mutate(prev_cum =lag(cumulative_cases,default=0)) %>% filter(daily_cases != cumulative_cases-prev_cum)
        # 
        full_ph_cases = full_ph_cases %>% 
          dplyr::group_by(code,name,codeType) %>% 
          dplyr::mutate(daily_cases = cumulative_cases-lag(cumulative_cases,default=0)) %>% 
          dplyr::ungroup()
        
        LTLA_to_PHEC = readr::read_csv(self$codes$mapping$LAD19CD_PHEC19CD$url) %>% 
          dplyr::select(LAD19CD,PHEC19CD,PHEC19NM) %>% distinct()
        
        LTLA_to_NHSER = readr::read_csv(self$codes$mapping$LAD19CD_CCG19CD$url) %>% 
          dplyr::left_join(readr::read_csv(self$codes$mapping$CCG19CD_NHSER19CD$url), by="CCG19CD") %>% 
          dplyr::select(LAD19CD,NHSER19CD,NHSER19NM) %>% distinct() 
        # There are 6 LTLAS that span NHSER boundaries. Assign them to an individual one (this is minorly lossy).
        LTLA_to_NHSER = LTLA_to_NHSER %>% 
          dplyr::group_by(LAD19CD) %>% 
          dplyr::filter(row_number()==1)
        
        # zero rows
        # LTLA_to_NHSER %>% group_by(LAD19CD) %>% count() %>% filter(n>1)
        # LTLA_to_PHEC %>% group_by(LAD19CD) %>% count() %>% filter(n>1)
        # 
        # zero rows
        # england_ltla %>% anti_join(LTLA_to_NHSER, by=c("code"="LAD19CD"))
        # england_ltla %>% anti_join(LTLA_to_PHEC, by=c("code"="LAD19CD"))
        # 
        
        # add in NHSER
        tmp2 = full_ph_cases %>% 
          dplyr::filter(codeType == "LAD") %>% 
          dplyr::inner_join(LTLA_to_NHSER, by=c("code"="LAD19CD")) %>% 
          dplyr::select(-code,-name) %>% 
          dplyr::rename(code = NHSER19CD, name = NHSER19NM) %>% 
          dplyr::group_by(date,code,name) %>% 
          dplyr::summarise(cumulative_cases = sum(cumulative_cases), daily_cases = sum(daily_cases)) %>% 
          dplyr::mutate(codeType = "NHSER")
        
        # add in PHEC
        tmp4 = full_ph_cases %>% 
          dplyr::filter(codeType == "LAD") %>% 
          dplyr::inner_join(LTLA_to_PHEC, by=c("code"="LAD19CD")) %>% 
          dplyr::select(-code,-name) %>% 
          dplyr::rename(code = PHEC19CD, name = PHEC19NM) %>% 
          dplyr::group_by(date,code,name) %>% 
          dplyr::summarise(cumulative_cases = sum(cumulative_cases), daily_cases = sum(daily_cases)) %>% 
          dplyr::mutate(codeType = "PHEC")
        
        out = dplyr::bind_rows(full_ph_cases, tmp2, tmp4) %>% 
          dplyr::mutate(statistic = "case",source="PHE dashboard", subgroup=NA,ageCat = NA,gender=NA) %>% 
          dplyr::rename(incidence = daily_cases, cumulative=cumulative_cases) %>% 
          tidyr::pivot_longer(cols=c(incidence,cumulative), names_to ="type")
        
        return(out  %>% self$fillAbsent() %>% self$fixDatesAndNames(4) %>% self$complete())
      }))
    },
    
    getPHEApiNations = function(...) {
      self$getDaily("PHE-API-CTRY", ..., orElse = function (...) covidTimeseriesFormat({
        ctryCodes = dpc$codes$getCodes() %>% filter(codeType=="CTRY" & status=="live") %>% pull(code)
        tmp = bind_rows(lapply(ctryCodes, FUN=function(x) self$getPHEApi(areaType = "nation", areaCode=x)))
        return(tmp)
      }))
    },
    
    getPHEApiNHSRegions = function(...) {
      self$getDaily("PHE-API-NHSER", ..., orElse = function (...) covidTimeseriesFormat({
        nhserCodes = dpc$codes$getCodes() %>% filter(codeType=="NHSER" & status=="live") %>% pull(code)
        tmp = bind_rows(lapply(nhserCodes, FUN=function(x) self$getPHEApi(areaType = "nhsRegion", areaCode=x)))
        return(tmp)
      }))
    },
    
    getPHEApi = function(areaType = "nation", areaName= NULL, areaCode = NULL, ...) {
      
      endpoint <- "https://api.coronavirus.data.gov.uk/v1/data"
      
      # Create filters:
      filters = sprintf("areaType=%s", areaType)
      if(!identical(areaName,NULL)) filters = c(filters, sprintf("areaName=%s", areaName))
      if(!identical(areaCode,NULL)) filters = c(filters, sprintf("areaCode=%s", areaCode))
      
      # Create the structure as a list or a list of lists:
      structure <- list(
        date = "date", 
        codeType = "areaType", 
        name = "areaName", 
        code = "areaCode", 
        case = "newCasesBySpecimenDate",
        death = "newDeaths28DaysByDeathDate",
        admission = "newAdmissions"
      )
      
      # The "httr::GET" method automatically encodes 
      # the URL and its parameters:
      httr::GET(
        # Concatenate the filters vector using a semicolon.
        url = endpoint,
        
        # Convert the structure to JSON (ensure 
        # that "auto_unbox" is set to TRUE).
        query = list(
          filters = paste(filters, collapse = ";"),
          structure = jsonlite::toJSON(structure, auto_unbox = TRUE)
        ),
        
        # The API server will automatically reject any
        # requests that take longer than 10 seconds to 
        # process.
        httr::timeout(10)
      ) -> response
      
      # Handle errors:
      if (response$status_code >= 400) {
        url <- response$url
        print(url)
        err_msg = httr::http_status(response)
        stop(err_msg)
      }
      
      # Convert response from binary to JSON:
      json_text <- httr::content(response, "text")
      data = jsonlite::fromJSON(json_text)
      
      # Store the encoded URL for inspection:
      # url <- response$url
      # print(url)
      
      ts = data$data
      
      ts = ts %>% pivot_longer(cols=c("case","death","admission"),names_to = "statistic",values_to = "value") %>%
        mutate(
          type = "incidence",
          codeType = case_when(
            codeType == "nation" ~ "CTRY",
            codeType == "region" ~ "RGN",
            codeType == "utla" ~ "UTLA",
            codeType == "ltla" ~ "LAD",
            TRUE ~ NA_character_
          ),
          source = "phe api",
          subgroup = NA,
          ageCat = NA,
          gender = NA,
          note = NA,
          statistic = ifelse(statistic == "admission","hospital admission",statistic),
          date = as.Date(date,"%Y-%m-%d")
        )
      return(covidTimeseriesFormat(ts %>% filter(!is.na(value))) %>% self$fillAbsent() %>% self$fixDatesAndNames(4) %>% self$complete())
    },
    
    ####TODO: ----
    # BBC contact tracing matrix data:
    # https://www.medrxiv.org/content/10.1101/2020.02.16.20023754v2.supplementary-material
    
    # getPHETests = function() {
    #   # TODO: https://www.gov.uk/guidance/coronavirus-covid-19-information-for-the-public
    # },
    
    # getGoogleTrends = function(...) {
    #   https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf
    # statistic = information seekinh
    # },
    
    # PHE Coronavirus cases by age
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
    
    # # TODO: This has not yet been integrated
    # ONS age breakdown deaths
    # https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales
    # https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/publishedweek172020.xlsx
    # getONSDeaths = function(...) {
    #   ONSurls = c(
    #     "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/publishedweek1620201.xlsx"
    #   )
    #   
    #   filenames = ONSurls %>% stringr::str_extract("/([^/]+)$") %>% stringr::str_remove("/")
    #   filenames = paste0("~/Git/uk-covid-datatools/data-raw/ONS/",filenames)
    #   
    #   missingfileUrls = ONSurls[!file.exists(filenames)]
    #   missingfilenames = filenames[!file.exists(filenames)]
    #   
    #   for(i in seq_along(missingfilenames)) {
    #     download.file(missingfileUrls[i],missingfilenames[i])
    #   }
    #   
    #   loadONS = function(file) {
    #     ONS <- read_excel(file,
    #                       sheet = "Covid-19 - Weekly occurrences",
    #                       range = "B6:BC75")
    #     ONS2 = ONS %>% rename(age=1) %>% mutate(gender = case_when(
    #       stringr::str_detect(age,"Female") ~ "Female",
    #       stringr::str_detect(age,"Male") ~ "Male",
    #       TRUE ~ as.character(NA)
    #     )) %>% fill(gender) %>% mutate(gender = ifelse(is.na(gender),"Both",gender)) %>%
    #       pivot_longer(cols = c(-age,-gender),names_to = "date",values_to = "died") %>%
    #       mutate(age = ifelse(age=="<1","0-1",age)) %>% filter(stringr::str_starts(age,"[0-9]") & stringr::str_starts(date,"[0-9]") & !is.na(died)) %>% mutate(date = as.Date(as.numeric(date),"1970-01-01"))
    #     ONS2 = ONS2 %>% group_by(gender,date) %>% mutate(left = as.numeric(stringr::str_extract(age,"^[0-9]+"))) %>% arrange(left) %>% mutate(right=lead(left,default=120))
    #     ONS3 = ONS2 %>% ungroup() %>% group_by(age,gender) %>% arrange(date) %>% mutate(died = cumsum(died)) %>% mutate(cases=NA,admitted=NA,admittedIcu=NA, country="E&W",filename=file %>% stringr::str_extract("/([^/]+)$") %>% stringr::str_remove("/"))
    #     return(ONS2)
    #   }
    #   
    #   loadONS(filenames[length(filenames)])
    # },
    
    # getUkILIData = function(...) {
    #   urls = c(
    #     "https://www.gov.uk/government/publications/gp-in-hours-weekly-bulletins-for-2020",
    #     "https://www.gov.uk/government/publications/gp-in-hours-weekly-bulletins-for-2019",
    #     "https://www.gov.uk/government/publications/gp-in-hours-weekly-bulletins-for-2018",
    #     "https://www.gov.uk/government/publications/gp-in-hours-bulletin",
    #     "https://www.gov.uk/government/publications/gp-in-hours-weekly-bulletins-for-2016",
    #     "https://www.gov.uk/government/publications/gp-in-hours-weekly-bulletins-for-2015",
    #     "https://www.gov.uk/government/publications/gp-in-hours-weekly-bulletins-for-2014"
    #   )
    #   
    #   fileUrls = NULL
    #   for (url in urls) {
    #     # url = "https://www.gov.uk/government/publications/gp-in-hours-weekly-bulletins-for-2019"
    #     page <- xml2::read_html(url)
    #     
    #     fileUrls = c(fileUrls,page %>%
    #                    html_nodes("a") %>%       # find all links
    #                    html_attr("href") %>%     # get the url
    #                    str_subset("\\.xls|\\.ods"))
    #   }
    #   
    #   fileUrls = unique(fileUrls)
    #   filenames = fileUrls %>% stringr::str_extract("/([^/]+)$") %>% stringr::str_remove("/")
    #   filenames = paste0("~/Git/uk-covid-datatools/data-raw/ILI/",filenames)
    #   
    #   missingfileUrls = fileUrls[!file.exists(filenames)]
    #   missingfilenames = filenames[!file.exists(filenames)]
    #   
    #   for(i in seq_along(missingfilenames)) {
    #     download.file(missingfileUrls[i],missingfilenames[i])
    #   }
    #   
    #   readILI <- function(fname) {
    #     message(fname)
    #     if(str_split(fname,"\\.",simplify = TRUE)[,2]=="xls")
    #     {
    #       #print("XLS")
    #       metadata = readxl::read_xls(path=fname,sheet=1,range="A6:B10", col_names = c("name","val"), col_types = "text")
    #       readxl::read_xls(path=fname,sheet=3,skip = 5, col_names = F)->tmp
    #     } else {
    #       metadata = readODS::read_ods(path=fname,sheet=1,range="A6:B10", col_names = FALSE, col_types = NA) %>% rename(name=A, val=B)
    #       readODS::read_ods(path=fname,sheet=3,skip = 5, col_names = F )->tmp
    #     }
    #     metadata = metadata %>% pivot_wider( names_from = name, values_from = val ) %>% mutate(
    #       DateStarting = tryCatch(
    #         as.Date(`Date starting`,tryFormats=c("%d/%m/%Y","%d/%m/%y")),
    #         error=function(e) as.Date(as.integer(`Date starting`),origin="1900-01-01")),
    #       DateEnding = tryCatch(
    #         as.Date(`Date ending`,tryFormats=c("%d/%m/%Y","%d/%m/%y")),
    #         error=function(e) as.Date(as.integer(`Date ending`),origin="1900-01-01"))
    #     )
    #     tmp %>%
    #       select(
    #         LACode=1,
    #         LAName=2,
    #         PHECentreName=3,
    #         PHECentreCode=4,
    #         PHERegionName =5,
    #         PHERegionCode =6,
    #         Denominator =7,
    #         ILI = 8,
    #         RatePer100000=9) %>%
    #       mutate(ILI = as.integer(ILI),
    #              Denominator = as.integer(Denominator),
    #              RatePer100000 = as.double(RatePer100000),
    #              Week = as.integer(metadata$`Week number`),
    #              Year = lubridate::year(metadata$DateStarting),
    #              DateStarting = as.Date(metadata$DateStarting),
    #              DateEnding = as.Date(metadata$DateEnding)
    #       ) %>%
    #       filter(row_number()<150)->tmp
    #     return(tmp)
    #   }
    #   
    #   
    #   
    #   #filename = dir(path = "~/Git/uk-covid-datatools/data-raw/",recursive = TRUE,pattern = "GP")
    #   #weeks = filename %>% stringr::str_remove_all("_") %>% stringr::str_extract("([0-9]+)\\.") %>% stringr::str_remove("\\.") %>% as.integer()
    #   #years = filename %>% stringr::str_remove_all("_") %>% stringr::str_extract("([0-9]{4})") %>% as.integer()
    #   #years = ifelse(is.na(years),2017,years)
    #   
    #   UKILIdata = readr::read_csv("~/Git/uk-covid-datatools/data-raw/ilidata.csv")
    #   processed = paste0("~/Git/uk-covid-datatools/data-raw/ILI/",unique(UKILIdata$filename))
    #   unprocessed = !(filenames %in% processed)
    #   unprocessedFilenames = filenames[unprocessed]
    #   
    #   UKILIdata_toAdd = tibble(unprocessedFilenames) %>%
    #     mutate(contents = map(unprocessedFilenames,~readILI(.))) %>%
    #     #mutate(Year = years) %>%
    #     #mutate(Week = weeks) %>%
    #     # select(-filename) %>%
    #     mutate(filename=stringr::str_extract(unprocessedFilenames,"([^/]+$)")) %>%
    #     select(-unprocessedFilenames) %>%
    #     unnest(cols=contents)
    #   
    #   if (length(unprocessedFilenames) > 0) {
    #     UKILIdata = bind_rows(UKILIdata,UKILIdata_toAdd)
    #     UKILIdata %>% write_csv("~/Git/uk-covid-datatools/data-raw/ilidata.csv")
    #     usethis::use_data(UKILIdata, overwrite=TRUE)
    #   } else {
    #     message("Nothing to update")
    #   }
    # },
    
    #### Everything ----
    
    getTheFireHose = function(...) {
      self$getDaily("FIRE-HOSE", ..., orElse = function (...) covidTimeseriesFormat({
        bind_rows(
          self$getPublicOneOneOne(...),
          self$getNHSDeaths(...),
          self$getPHEDashboard(...),
          self$getTomWhiteCases(...),
          self$getTomWhiteIndicators(...)
        )
      }))
    }
))







