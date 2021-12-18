#' NHS datasets
#' @export
NHSDatasetProvider = R6::R6Class("NHSDatasetProvider", inherit=CovidTimeseriesProvider, public = list(
    
    initialize = function(providerController, ...) {
      super$initialize(providerController, ...)
    },
    
    #' @description Load google mobility file aligned with LAD codes
    #' @return google mobility 0 custom format
    getGoogleMobility = function(...) {
      self$getDaily("GOOGLE_MOBILITY", ..., orElse = function (...) {
        Global_Mobility_Report <- readr::read_csv(
          "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", 
          col_types = cols(
            sub_region_1 = col_character(), sub_region_2 = col_character(), iso_3166_2_code = col_character(), metro_area = col_character(),
            census_fips_code = col_character(), date = col_date(format = "%Y-%m-%d"))) %>% filter(country_region_code == "GB")
        mobility_to_LAD = readr::read_csv("https://raw.githubusercontent.com/datasciencecampus/google-mobility-reports-data/master/geography/google_mobility_lad_lookup_200903.csv")
        out = Global_Mobility_Report %>% inner_join(mobility_to_LAD, by=c("country_region_code","sub_region_1","sub_region_2"))
        return(out %>% rename(code = lad19cd, name=la_name) %>% mutate(codeType = ifelse(flag_2018,"LAD18","LAD19")) %>% select(-place_id,-census_fips_code,-metro_area,-country_region_code,-country_region) )
      })
    },
    
    #' @description Load curated significant dates file
    #' @return public 111 data including pathways and online, pre and post april. Codes are CCG20 codes.
    getSignificantDates = function(signif = Inf, ...) {
      return(ukcovidtools::ukCovidDates %>% filter(Significance <= signif))
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
          dplyr::mutate(codeType = "CCG20", type="incidence",statistic="triage",subgroup=NA_character_, note=NA_character_, source=paste0(source,"-public")) %>% 
          dplyr::rename(value=incidence)
        # out2 = out2 %>% dplyr::select(-name) %>% self$codes$findNamesByCode(outputCodeTypeVar = NULL)
        return(out2 %>% self$fillAbsent() %>% self$fixDatesAndNames(0) %>% self$complete())
      }))
    },
    
    
    # TODO: https://www.health-ni.gov.uk/publications/daily-dashboard-updates-covid-19-august-2020
    # Wales: http://www2.nphs.wales.nhs.uk:8080/CommunitySurveillanceDocs.nsf/3dc04669c9e1eaa880257062003b246b/77fdb9a33544aee88025855100300cab/$FILE/Rapid%20COVID-19%20surveillance%20data.xlsx
    # Scotland: https://www.opendata.nhs.scot/dataset/covid-19-in-scotland
    
    getTomWhiteCases = function(truncate=4,...) {
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
        return(tmp_cases_uk %>% self$fillAbsent() %>% self$fixDatesAndNames(truncate) %>% self$complete())
      }))
    },
      
    #browser()
    getTomWhiteIndicators = function(truncate=4,...) {
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
        
        return(country_totals2 %>% self$fixDatesAndNames(truncate))
        
      }))
    },
  
    getNHSDeaths = function(truncate = 7,...) {
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
        out = out %>%  self$fillAbsent() %>% self$fixDatesAndNames(truncate) %>% self$complete()
        return(out)
        
      }))
    },
    
    getNHSAdmissions = function(truncate = 0,...) {
      self$getDaily("NHS-ADMISSIONS", ..., orElse = function (...) covidTimeseriesFormat({
        # Load landing page
        tmp = xml2::read_html("https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/")
        links = tmp %>% rvest::html_nodes(xpath="//a") %>% rvest::html_attr(name = "href")
        url = function(str) {return(links[links %>% stringr::str_detect(str)])}
        #Load files matched from landing page
        file = self$downloadDaily(id = "NHS_ADMISSIONS",url = url("Weekly-covid-admissions-and-beds-publication-([0-9]+)\\.xlsx"),type="xlsx")
        
        doTab = function(tab,statistic,type,subgroup) {
          
          NHS_ADMISSIONS <- suppressMessages(readxl::read_excel(file, sheet = tab, skip = 14))
          NHS_ADMISSIONS = NHS_ADMISSIONS %>% 
            dplyr::select(-c(2)) %>%
            dplyr::rename(code=Code,name=Name,type1=`Type 1 Acute?`) %>%
            dplyr::filter(!is.na(code) & name!="ENGLAND") %>%
            dplyr::mutate(codeType = "NHS Trust")
          
        
          out = NHS_ADMISSIONS %>% 
            tidyr::pivot_longer(cols=c(-type1,-code,-codeType, -name), names_to = "date", values_to = "value") %>% 
            dplyr::mutate(date=suppressWarnings(as.Date(as.numeric(date),"1899-12-30"))) %>% 
            dplyr::filter(!is.na(date)) %>%
            dplyr::mutate(
              source = "Weekly-covid-admissions-and-beds",
              subgroup= subgroup,
              statistic=statistic,
              type=type,
              gender=NA_character_,
              ageCat=NA_character_,
              note = tab
            ) 
          
          return(out)
        }
        
        out2 = bind_rows(
          doTab("Hosp ads & diag","hospital admission","incidence","Pillar 1+2"),
          doTab("New hosp cases","hospital admission","incidence","Pillar 1"),
          doTab("Hosp ads from comm","hospital admission","incidence","Pillar 2"),
          doTab("Care home ads and diags","hospital admission","incidence","Care home Pillar 1+2"),
          doTab("All beds COVID","hospital admission","prevalence","All"),
          doTab("MV beds COVID","icu admission","prevalence","All"),
          doTab("Adult G&A Beds Occupied COVID","hospital admission","prevalence","Adult G&A"),
          doTab("Adult G&A Bed Occupied NonCOVID","non covid hospital admission","prevalence","Adult G&A"),
          doTab("Adult G&A Beds Unoccupied","hospital capacity","background","Adult G&A"),
          doTab("Adult CC Beds Occupied COVID","icu admission","prevalence","Adult ICU"),
          doTab("Adult CC Bed Occupied NonCOVID","non covid icu admission","prevalence","Adult ICU"),
          doTab("Adult CC Beds Unoccupied","icu capacity","background","Adult ICU"),
        )
        out2 = out2 %>%  self$fillAbsent() %>% self$fixDatesAndNames(truncate) %>% self$complete()
        out2 = out2 %>% bind_rows(
          out2 %>% covidStandardDateGrouping(code,name,codeType) %>% group_by(type1,.add=TRUE) %>% summarise(value = sum(value)) %>% dplyr::mutate(
            code = "E92000001",
            name = "England",
            codeType = "CTRY")
          )
        
        return(out2)
        
      }))
    },
    
    getNHSCapacityEstimate = function(window=28) {
      adm = self$getNHSAdmissions()
      
      tmp = bind_rows(
        adm %>% filter(statistic == "hospital admission" & type == "prevalence" & subgroup == "Adult G&A"),
        adm %>% filter(statistic == "non covid hospital admission" & type == "prevalence" & subgroup == "Adult G&A"),
        adm %>% filter(statistic == "hospital capacity" & type == "background" & subgroup == "Adult G&A")
      ) %>% 
        mutate(statistic = "hospital capacity", type = "background", subgroup = "Adult G&A") %>%
        covidStandardDateGrouping() %>%
        group_by(type1,.add=TRUE) %>%
        summarise(value = sum(value)) %>% 
        covidStandardGrouping() %>%
        group_by(type1,.add=TRUE) %>%
        #mutate(Smooth.value = predict(loess(value~as.integer(date), span=pmin(window/n(),1),degree=1), newdata=as.integer(date)))
        mutate(Smooth.value = slider::slide_dbl(value,.f = mean,na.rm=TRUE,.before=window/2,.after=window/2,.complete = FALSE))
      
      tmp2 = bind_rows(
        adm %>% filter(statistic == "icu admission" & type == "prevalence" & subgroup == "Adult ICU"),
        adm %>% filter(statistic == "non covid icu admission" & type == "prevalence" & subgroup == "Adult ICU"),
        adm %>% filter(statistic == "icu capacity" & type == "background" & subgroup == "Adult ICU")
      ) %>% 
        mutate(statistic = "icu capacity", type = "background", subgroup = "Adult ICU") %>%
        covidStandardDateGrouping() %>%
        group_by(type1,.add=TRUE) %>%
        summarise(value = sum(value)) %>% 
        covidStandardGrouping() %>%
        group_by(type1,.add=TRUE) %>%
        #mutate(Smooth.value = predict(loess(value~as.integer(date), span=pmin(window/n(),1),degree=1), newdata=as.integer(date)))
        mutate(Smooth.value = slider::slide_dbl(value,.f = mean,na.rm=TRUE,.before=window/2,.after=window/2,.complete = FALSE))
      
      return(bind_rows(tmp,tmp2))
      
    },
    
    # https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/09/Weekly-covid-admissions-and-beds-publication-210916.xlsx
    
    # getPHEAdmissions = function(...) {
    #   csv = "https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsTrust&metric=cumAdmissions&metric=newAdmissions&format=csv"
    #   # https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=nhstrust;areaName=Alder%2520Hey%2520Children%27s%2520NHS%2520Foundation%2520Trust&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22hospitalCases%22:%22hospitalCases%22%7D&format=csv
    #   tmp = readr::read_csv(csv)
    #   tmp = tmp %>%
    #     dplyr::rename(code = areaCode, name = areaName, code) %>%
    #     dplyr::select(-areaType) %>%
    #     tidyr::complete(date,tidyr::nesting(code,name,type)) %>% 
    #     
    # }
    getPHELADCases = function(...) {
      self$getDaily("PHE-LAD-CASES", ..., orElse=function(...) {
        csv = readr::read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDate&format=csv")
        ph_cases = csv %>% select(date, code = `areaCode`, name=`areaName`,value = `newCasesBySpecimenDate`) %>% mutate(codeType="LAD")
        full_ph_cases = ph_cases %>% 
          tidyr::complete(date,tidyr::nesting(code,name,codeType), fill=list(value=0)) %>%
          mutate(
            statistic="case",
            type="incidence",
            source = "PHE api",
            subgroup = NA_character_,
            ageCat = NA_character_,
            gender = NA_character_,
            note = NA_character_,
          )
        return(full_ph_cases)
      })
    },
    
    #' @description Get UK outbreak timeseries data
    #' @return a covidTimeserisFormat data frame with several timeseries in it
    getPHEDashboard = function(truncate = 4,...) {
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
        
        return(out  %>% self$fillAbsent() %>% self$fixDatesAndNames(truncate) %>% self$complete())
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
    
    getPHEApiNHSTrusts = function(...) {
      self$getDaily("PHE-API-NHS-TRUST", ..., orElse = function (...) covidTimeseriesFormat({
        #nhserCodes = dpc$codes$getCodes() %>% filter(codeType=="NHS trust" & status=="live") %>% pull(code)
        tmp = self$getPHEApi(areaType = "nhsTrust")
        return(tmp)
      }))
    },
    
    
    #' Extracts paginated data by requesting all of the pages
    #' and combining the results.
    #'
    #' @param filters    API filters. See the API documentations for 
    #'                   additional information.
    #'                   
    #' @param structure  Structure parameter. See the API documentations 
    #'                   for additional information.
    #'                   
    #' @return list      Comprehensive list of dictionaries containing all 
    #'                   the data for the given ``filter`` and ``structure`.`
    getPHEPaginatedData = function (filters, structure) {
      
      endpoint     <- "https://api.coronavirus.data.gov.uk/v1/data"
      results      <- list()
      current_page <- 1
      
      repeat {
        
        httr::GET(
          url   = endpoint,
          query = list(
            filters   = paste(filters, collapse = ";"),
            structure = jsonlite::toJSON(structure, auto_unbox = TRUE),
            page      = current_page
          ),
          httr::timeout(20)
        ) -> response
        
        # Handle errors:
        if ( response$status_code >= 400 ) {
          err_msg = httr::http_status(response)
          stop(err_msg)
        } else if ( response$status_code == 204 ) {
          break
        }
        
        # Convert response from binary to JSON:
        json_text <- httr::content(response, "text")
        dt        <- jsonlite::fromJSON(json_text)
        results   <- rbind(results, dt$data)
        
        if ( is.null( dt$pagination$`next` ) ){
          break
        }
        
        current_page <- current_page + 1;
        
      }
      
      return(results)
      
    },
    
    
    getPHEApi = function(areaType = "nation", areaName= NULL, areaCode = NULL, truncate=4, ...) {
      
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
        admission = "newAdmissions",
        cumAdmission = "cumAdmissions",
        hospitalCases = "hospitalCases",
        icuCases = "covidOccupiedMVBeds"
      )
      
      ## https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=nhstrust;areaName=Alder%2520Hey%2520Children%27s%2520NHS%2520Foundation%2520Trust&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newAdmissions%22:%22newAdmissions%22,%22cumAdmissions%22:%22cumAdmissions%22%7D&format=csv
      ## https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=nhstrust;areaName=Alder%2520Hey%2520Children%27s%2520NHS%2520Foundation%2520Trust&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22hospitalCases%22:%22hospitalCases%22%7D&format=csv
      ## https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=nhstrust;areaName=Alder%2520Hey%2520Children%27s%2520NHS%2520Foundation%2520Trust&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22covidOccupiedMVBeds%22:%22covidOccupiedMVBeds%22%7D&format=csv
      
      ts = self$getPHEPaginatedData(filters,structure)
      
      ts = ts %>% pivot_longer(cols=c("case","death","admission","cumAdmission","hospitalCases","icuCases"),names_to = "label",values_to = "value") %>%
        mutate(
          type = case_when(
            label == "cumAdmission" ~ "cumulative",
            label == "hospitalCases" ~ "prevalence",
            label == "icuCases" ~ "prevalence",
            TRUE ~ "incidence"
          ),
          codeType = case_when(
            codeType == "overview" ~ "UK",
            codeType == "nation" ~ "CTRY",
            codeType == "region" ~ "RGN",
            codeType == "utla" ~ "UTLA",
            codeType == "ltla" ~ "LAD",
            codeType == "msoa" ~ "MSOA",
            codeType == "nhsRegion" ~ "NHSER",
            codeType == "nhsTrust" ~ "NHS trust",
            TRUE ~ NA_character_
          ),
          source = "phe api",
          subgroup = NA_character_,
          ageCat = NA_character_,
          gender = NA_character_,
          note = NA_character_,
          statistic = case_when(
            label == "admission" ~ "hospital admission",
            label == "case" ~ "case",
            label == "death" ~ "death",
            label == "cumAdmission" ~ "hospital admission",
            label == "hospitalCases" ~ "hospital admission",
            label == "icuCases" ~ "icu admission",
            TRUE ~ NA_character_
          ),
          date = as.Date(date,"%Y-%m-%d")
        ) %>% select(-label)
      return(covidTimeseriesFormat(ts %>% filter(!is.na(value) & !is.na(statistic)) %>% self$fillAbsent() %>% self$fixDatesAndNames(truncate) %>% self$complete()))
    },
    
    getCLIMB = function(...) {
      self$getDaily("CLIMB", ..., orElse = function (...) {
        tmp = readr::read_csv("https://cog-uk.s3.climb.ac.uk/phylogenetics/latest/cog_metadata.csv")
        return(tmp)
      })
    },
    
    getLTLALineages = function(window=3, lineagesExpr=5, fitPoisson = NULL, ...) {
      lineagesExpr = enexpr(lineagesExpr)
      out = self$getDaily("SANGER", params=list(window, lineagesExpr, fitPoisson), ..., orElse = function(...) {
        tsv = readr::read_tsv("https://covid-surveillance-data.cog.sanger.ac.uk/download/lineages_by_ltla_and_week.tsv")
        if(is.numeric(lineagesExpr)) {
          lineages = tsv %>% filter(WeekEndDate == max(WeekEndDate)) %>% group_by(Lineage) %>% summarise(Count = sum(Count)) %>% arrange(desc(Count)) %>% pull(Lineage) %>% head(lineages)
          lineagesExpr = expr(ifelse(Lineage %in% lineages, Lineage, "Other"))
        }
        tsv = tsv %>% mutate(.x = Lineage, Lineage = !!lineagesExpr) %>% select(-.x) %>% group_by(Lineage,WeekEndDate,LTLA) %>% summarise(Count = sum(Count)) %>% ungroup() 
        if (is.null(fitPoisson)) fitPoisson = length(unique(tsv$Lineage))<10
        
        tsv = tsv %>% tidyr::complete( LTLA,Lineage,WeekEndDate, fill=list(Count=0))
        dates = as.Date((min(tsv$WeekEndDate)-6):max(tsv$WeekEndDate),"1970-01-01")
        tsvRoll = tsv %>% 
          rename(date = WeekEndDate) %>% 
          tidyr::complete( LTLA,Lineage,date = dates, fill=list(Count=0)) %>% 
          group_by(LTLA,Lineage) %>% arrange(date) %>% 
          mutate(value = slider::slide_dbl(.x = Count,.f = sum,.after = 6,.complete = FALSE)/7) %>%
          mutate(Roll.value = slider::slide_dbl(.x = value,.f = mean,.before=3, .after = 3,.complete = FALSE))
        
        if(fitPoisson) {
          tsv2 = tsv %>% group_by(LTLA,Lineage) %>% group_modify(function(d,g,...) {
            #d = tsv %>% filter(Lineage=="AY.4") %>% group_by(LTLA,Lineage) %>% filter(cur_group_id()==7)
            #d = tsv %>% filter(LTLA=="E06000007",Lineage=="B.1.1.44")
  
            d = d %>% mutate(time = as.numeric(WeekEndDate-Sys.Date()-4))
  
            timepoints = dates-Sys.Date()
            
            if(sum(na.omit(d$Count) != 0) < 3) {
  
              modelled = tibble(date = dates, Est.value = 0, Est.value.se=NA_real_)
  
            } else {
  
              tmp_alpha_2 = min(window/nrow(d),1)
  
              suppressWarnings({
              #tryCatch({
                #ev = seq(min(d$time)-1,max(d$time)+1,length.out = floor(2*nrow(d)/window+1))
                tmp_intercept_model = locfit::locfit(Count ~ locfit::lp(time, nn=tmp_alpha_2, deg=1), d, family="qpois", link="log")
                tmp_intercept = predict(tmp_intercept_model, band="global", se.fit=TRUE, newdata = timepoints)
                modelled = tibble(date = dates, Est.value = tmp_intercept$fit/7, Est.value.se=tmp_intercept$se.fit/7)
              #},error = browser)
              })
  
            }
            # ggplot(modelled, aes(x=date,y=value))+geom_line()+geom_point(data=d,aes(x=WeekEndDate-4,y=Count/7))
            return(modelled)
          })
          tsv3 = tsvRoll %>% left_join(tsv2, by=c("Lineage","LTLA","date"))
        } else {
          tsv3 = tsvRoll
        }
      })
      out = out %>% rename(lineage = Lineage, code=LTLA) %>% self$codes$findNamesByCode()
      return(out)
    },
    
    getCOGUK = function(...) {
      self$getDaily("COG", ..., orElse = function (...) {
        cogDate = Sys.Date()
        cogData = NULL
        while (identical(cogData,NULL)) {
          tryCatch({
            #cogData = readr::read_csv(paste0("http://cog-uk-microreact.s3.climb.ac.uk/",cogDate,"/cog_metadata_microreact_public.csv"))
            cogData = readr::read_csv(paste0("http://cog-uk-microreact.s3.climb.ac.uk/",cogDate,"/cog_metadata_microreact_geocodes_only.csv"),col_types = cols(
              .default = col_character(),
              sample_date = col_date(format = "%Y-%m-%d"),
              epi_week = col_double(),
              pillar_2 = col_logical(),
              lineage_support = col_double()
            ))
            message("Most recent COG microreact build: ",cogDate)
          }, error = function(e) {
            #message(cogDate)
            cogDate <<- cogDate-1
          })
        }
        cogData = cogData %>% mutate(publish_date = cogDate)
        return(cogData)
      })
    },
    
    getTiers = function(...) {
      self$getDaily("TIERS", ..., orElse = function (...) {
        # Tier data
        fpath = system.file("data-raw", "NPI_dataset_full_extract_03_11_2020.xlsx", package="ukcovidtools")
        prevTiers = readxl::read_xlsx(fpath)
        
        tidyTiers = prevTiers %>% 
          mutate(local_lockdown = residents_cannot_leave_the_local_area) %>%
          select(date,code = ltla20cd, name = ltla20nm, local_lockdown, tier_1, tier_2, tier_3, national_lockdown) %>% 
          mutate(local_lockdown = ifelse(tier_1+tier_2+tier_3+national_lockdown==0, local_lockdown, 0)) %>%
          pivot_longer(cols=c(local_lockdown,tier_1,tier_2,tier_3,national_lockdown),values_to = "present",names_to = "tier") %>% 
          filter(present==1) %>% 
          mutate(date = as.Date(date), tier = case_when(
            tier=="local_lockdown" ~ "local",
            tier=="tier_1" ~ "one",
            tier=="tier_2" ~ "two",
            tier=="tier_3" ~ "three",
            TRUE ~ "lockdown"
          ), codeType="LAD20") %>%
          select(-present)
        revert19to20 = tidyTiers %>% inner_join(tibble(
          code="E06000060",
          oldCode=c("E07000004","E07000005","E07000006","E07000007"),
          oldName=c("Aylesbury Vale","Chiltern","South Bucks","Wycombe")),by="code") %>% 
          select(-code,-name) %>%
          rename(code = oldCode,name = oldName) %>% mutate(codeType="LAD")
        
        tidyTiers = tidyTiers %>% filter(code != "E06000060") %>% bind_rows(revert19to20) %>% mutate(codeType="LAD")
        
        fromDate = max(tidyTiers$date)+1
        #https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=alertLevel&format=csv
        decTiers = readr::read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=alertLevel&format=csv")
        
        tidyTiers2 = decTiers %>% select(code = areaCode, name = areaName,FROM = date, alertLevel, alertLevelName) %>% 
          mutate(tier = case_when(
            alertLevel ==1 ~ "one+",
            alertLevel ==2 ~ "one+", #Scot 
            alertLevel ==3 ~ "two+",
            alertLevel ==4 ~ "three+",
            alertLevel ==5 ~ "four+",
            alertLevel ==-99 ~ "lockdown",
          ), codeType="LAD") %>%
          group_by(code,name) %>% arrange(FROM) %>% mutate(TO = lead(FROM,default = NA)) %>%
          mutate(TO = as.Date(ifelse(is.na(TO),Sys.Date(),TO-1),"1970-01-01")) %>% 
          ungroup() %>%
          group_by(across()) 
        
        
        tidyTiers2 = tidyTiers2 %>% group_modify(function(d,g,...) 
          tibble(date = as.Date(g$FROM:g$TO,"1970-01-01"))) %>% ungroup() %>% select(-TO,-FROM)
        
        tidyTiers3 = bind_rows(tidyTiers %>% anti_join(tidyTiers2, by=c("code","date")), tidyTiers2)
        tidyOut = tidyTiers3 %>% tidyr::complete(nesting(code,name,codeType),date=seq(as.Date("2020-01-01"),Sys.Date(),1), fill=list(tier="none"))
        return(tidyOut)
      })
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







