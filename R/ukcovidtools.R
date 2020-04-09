

#' Calculates a join list
#' 
#' @param df - a df which may be grouped
#' @param groupVars - the grouping for which we want to create a label as a list of columns quoted by vars(...)
#' @param defaultJoin - if there is no grouping we need one column to join by.
#' @import dplyr
#' @return a join List
#' @export
joinList = function(df,groupVars=NULL,defaultJoin=NULL) {
  grps = df %>% groups()
  joinList = c()
  if (!identical(defaultJoin,NULL)) {
    joinList = c(joinList,defaultJoin)
  }
  if (length(grps)!=0) {
    joinList = c(joinList, sapply(grps,as.character))
  }
  if (!identical(groupVars,NULL)) {
    joinList = c(joinList, as.vector(sapply(groupVars,as_label)))
  }
  return(joinList)
}


#' capture a data fram for an error message
#'
#' @param x a dataframe
print_and_capture <- function(x)
{
  paste(capture.output(print(x)), collapse = "\n")
}

#' Calculates a survival R(t) curve on grouped data
#' 
#' N.B. EpiEstim has a bug affecting 0.25 quantiles which is being looked at
#' 
#' @param groupedDf - a df which may be grouped, of timeseries data including an incidence column with continuous positive non NA values. 
#' @param config An object of class estimate_R_config, as returned by function EpiEstim::make_config.
#' @param dateVar - the variable containing the seqence of dates
#' @param incidenceVar - the sequence of daily incidence
#' @param window - the width of the smoothing function applied (default 2)
#' @return a dataframe with groupwise Rt estimates
#' @import dplyr
#' @export
tidyEstimateRt = function(groupedDf, config, dateVar = "date", incidenceVar = "incidence", window=2,...) {
  grps = groups(groupedDf)
  dateVar = ensym(dateVar)
  incidenceVar = ensym(incidenceVar)
  joinBy = joinList(groupedDf, defaultJoin = as_label(dateVar))
  
  groupedDf = groupedDf %>% filter(!is.na(!!incidenceVar)) %>% mutate(check = !!incidenceVar>=0)
  if (any(groupedDf$check==FALSE)) {
    message("Negative daily incidence in...",print_and_capture(groupedDf %>% filter(check==FALSE)))
    stop()
  }
  tmp = groupedDf %>% select(!!!grps, dates = !!dateVar,I=!!incidenceVar) 
  # tmp starts on first non zero value of I in group
  tmp2 = tmp %>% group_modify(function(d,g) {
    siConfig = config
    if(nrow(d) >= 2+window) {
      d = d %>% arrange(dates) %>% mutate(seq_id=row_number())
      siConfig$t_start = c(2:(nrow(d)-window))
      siConfig$t_end = siConfig$t_start+window
      #browser()
      tmp4 = suppressWarnings(EpiEstim::estimate_R(d,method="parametric_si",config=siConfig,...))
      tmp5 = tmp4$R %>% mutate(seq_id=t_end)
      tmp6 = d %>% left_join(tmp5, by="seq_id")
      return(tmp6 %>% select(-seq_id))
    } else {
      # not enough data
      return(d %>% mutate(
        t_start = NA,
        t_end = NA,
        `Mean(R)` = NA,                    
        `Std(R)` = NA,
        `Quantile.0.025(R)` = NA,
        `Quantile.0.05(R)` = NA,
        `Quantile.0.25(R)` = NA,
        `Median(R)` = NA,
        `Quantile.0.75(R)` = NA,
        `Quantile.0.95(R)` = NA,
        `Quantile.0.975(R)` = NA
      ))
    }
  }) %>% rename(date=dates)
  return(groupedDf %>% left_join(tmp2, by=joinBy) %>% select(-I,-t_start,-t_end,-check))
}

#' Get UK outbreak timeseries data
#' 
#' data held in google sheets:
#' https://docs.google.com/spreadsheets/d/1snb-vYuH7fVpTuyoQrM8zWiABYoXbSrnn44w-zlhM90/edit?usp=sharing
#' 
#' @import dplyr
#' @return a data frame with several timeseries in it
#' @export
getUKCovidTimeseries = function() {
  UKregional=readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQod-HdDk4Nl8BFcunG5P-QA2CuKdIXCfK53HJDxcsaYlOov4FFc-yQciJyQFrqX5_n_ixz56S7uNBh/pub?gid=163112336&single=true&output=csv", 
                    col_types = readr::cols(date = readr::col_date(format = "%Y-%m-%d")))
  
  englandNHS=readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQod-HdDk4Nl8BFcunG5P-QA2CuKdIXCfK53HJDxcsaYlOov4FFc-yQciJyQFrqX5_n_ixz56S7uNBh/pub?gid=0&single=true&output=csv", 
                      col_types = readr::cols(date = readr::col_date(format = "%Y-%m-%d")))
  
  scotlandHealthBoard = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQod-HdDk4Nl8BFcunG5P-QA2CuKdIXCfK53HJDxcsaYlOov4FFc-yQciJyQFrqX5_n_ixz56S7uNBh/pub?gid=490497042&single=true&output=csv")
  walesHealthBoard = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQod-HdDk4Nl8BFcunG5P-QA2CuKdIXCfK53HJDxcsaYlOov4FFc-yQciJyQFrqX5_n_ixz56S7uNBh/pub?gid=762770891&single=true&output=csv")
  northernIreland = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQod-HdDk4Nl8BFcunG5P-QA2CuKdIXCfK53HJDxcsaYlOov4FFc-yQciJyQFrqX5_n_ixz56S7uNBh/pub?gid=1217212942&single=true&output=csv")
  englandUnitAuth=readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQod-HdDk4Nl8BFcunG5P-QA2CuKdIXCfK53HJDxcsaYlOov4FFc-yQciJyQFrqX5_n_ixz56S7uNBh/pub?gid=796246456&single=true&output=csv")
  
  englandUnitAuth2NHSregion=readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQod-HdDk4Nl8BFcunG5P-QA2CuKdIXCfK53HJDxcsaYlOov4FFc-yQciJyQFrqX5_n_ixz56S7uNBh/pub?gid=1933702254&single=true&output=csv")
  
  
  # tidy England unitary authority region
  tmp = englandUnitAuth %>% 
    tidyr::pivot_longer(cols=starts_with("20"),names_to = "date",values_to = "cumulative_cases", values_ptypes = list(cumulative_cases=integer())) %>%
    mutate(date = as.Date(as.character(date),"%Y-%m-%d"))
  tmp = tmp %>% left_join(UKregional %>% select(date,daily_total=england_cumulative_cases), by="date")
  tidyEnglandUnitAuth = tmp %>% group_by(date) %>% mutate(daily_unknown = daily_total-sum(cumulative_cases,na.rm = TRUE)) %>%
    ungroup() %>% group_by(CTYUA19CD, CTYUA19NM)
  
  # tidy Scotland health board
  tmp = scotlandHealthBoard %>% 
    tidyr::pivot_longer(cols=starts_with("20"),names_to = "date",values_to = "cumulative_cases", values_ptypes = list(cumulative_cases=integer())) %>%
    mutate(date = as.Date(as.character(date),"%Y-%m-%d"))
  tmp = tmp %>% left_join(UKregional %>% select(date,daily_total=scotland_cumulative_cases), by="date")
  tidyScotlandHealthBoard = tmp %>% group_by(date) %>% mutate(daily_unknown = daily_total-sum(cumulative_cases,na.rm = TRUE)) %>%
    ungroup() %>% group_by(HB16CD, HB16NM)
  
  # tidy Wales health board
  tmp = walesHealthBoard %>% 
    tidyr::pivot_longer(cols=starts_with("20"),names_to = "date",values_to = "cumulative_cases", values_ptypes = list(cumulative_cases=integer())) %>%
    mutate(date = as.Date(as.character(date),"%Y-%m-%d"))
  tmp = tmp %>% left_join(UKregional %>% select(date,daily_total=wales_cumulative_cases), by="date")
  tidyWalesHealthBoard = tmp %>% group_by(date) %>% mutate(daily_unknown = daily_total-sum(cumulative_cases,na.rm = TRUE)) %>%
    ungroup() %>% group_by(LHB16CD, LHB16NM)
  
  # tidy Wales health board
  tmp = northernIreland %>% 
    tidyr::pivot_longer(cols=starts_with("20"),names_to = "date",values_to = "cumulative_cases", values_ptypes = list(cumulative_cases=integer())) %>%
    mutate(date = as.Date(as.character(date),"%Y-%m-%d"))
  tmp = tmp %>% left_join(UKregional %>% select(date,daily_total=northern_ireland_cumulative_cases), by="date")
  tidyNorthernIreland = tmp %>% group_by(date) %>% mutate(daily_unknown = daily_total-sum(cumulative_cases,na.rm = TRUE)) %>%
    ungroup() %>% group_by(LGD14CD, LGD14NM)
  
  # combined regional
  tidyCombinedUK = bind_rows(
    tidyEnglandUnitAuth %>% rename(code=CTYUA19CD, name=CTYUA19NM),
    tidyScotlandHealthBoard %>% rename(code=HB16CD, name=HB16NM),
    tidyWalesHealthBoard %>% rename(code=LHB16CD, name=LHB16NM)#,
    #tidyNorthernIreland %>% rename(code=LGD14CD, name=LGD14NM)
  )
  
  # tidy England NHS region
  tmp = englandNHS %>% tidyr::pivot_longer(cols=!date,names_to = "england_nhs_region",values_to = "cumulative_cases", values_ptypes = list(cumulative_cases=integer()))
  tmp = tmp %>% left_join(UKregional %>% select(date,daily_total=england_cumulative_cases), by="date")
  tidyEnglandNHS = tmp %>% group_by(date) %>% mutate(daily_unknown = daily_total-sum(cumulative_cases,na.rm = TRUE)) %>%
    ungroup() %>% group_by(england_nhs_region)
  
  # tidy UK regional
  tidyUKRegional = UKregional %>% select(date,england_cumulative_cases,scotland_cumulative_cases,wales_cumulative_cases,northern_ireland_cumulative_cases,daily_total=uk_cumulative_cases) %>% 
    tidyr::pivot_longer(cols=ends_with("cumulative_cases"),names_to = "uk_region",values_to = "cumulative_cases", values_ptypes = list(cumulative_cases=integer())) %>%
    filter(!is.na(cumulative_cases)) %>%
    mutate(uk_region = stringr::str_remove(uk_region,"_cumulative_cases")) %>%
    mutate(uk_region = stringr::str_replace(uk_region,"_"," "))  %>% 
    group_by(date) %>% 
    mutate(daily_unknown = daily_total-sum(cumulative_cases,na.rm = TRUE)) %>%
    ungroup() %>% group_by(uk_region)
  
  
  return(list(
    UKregional=UKregional,
    englandNHS=englandNHS,
    englandUnitAuth=englandUnitAuth,
    scotlandHealthBoard=scotlandHealthBoard,
    walesHealthBoard=walesHealthBoard,
    northernIrelandLocalGovernmentDistrict=northernIreland,
    englandUnitAuth2NHSregion=englandUnitAuth2NHSregion,
    tidyUKRegional=tidyUKRegional,
    tidyEnglandNHS=tidyEnglandNHS,
    tidyEnglandUnitAuth=tidyEnglandUnitAuth,
    tidyScotlandHealthBoard=tidyScotlandHealthBoard,
    tidyWalesHealthBoard=tidyWalesHealthBoard,
    tidyNorthernIrelandLocalGovernmentDistrict=tidyNorthernIreland,
    tidyCombinedUK=tidyCombinedUK
  ))
}

#' Cleanse UK outbreak timeseries data
#' 
#' Unofficial UK timeseries data has lots of data quality issues.
#' Various things done to fix:
#' * scales cumulative cases by unknown/total+1 across the board 
#' * calculates incidence and makes sure no negative incidences recorded
#' * updated cumulative_cases to reflect cleaned incidences
#' 
#' @param groupedDf the tidy dataframe grouped by regional geographical areas whatever you have
#' @param dateVar the column containing the date
#' @param cumulativeCasesVar 
#' @param totalVar the total over all the whole geography
#' @param unknownVar 
#' @import dplyr
#' @export
normaliseAndCleanse = function(groupedDf, dateVar = "date", cumulativeCasesVar = "cumulative_cases", totalVar = "daily_total", unknownVar = "daily_unknown", adjustUnknowns=TRUE) {
  grps = groups(groupedDf)
  if (identical(grps,NULL)) warning("there input data is not grouped - this is probably a mistake")
  dateVar = ensym(dateVar)
  cumulativeCasesVar = ensym(cumulativeCasesVar) 
  totalVar = ensym(totalVar)
  unknownVar = ensym(unknownVar)
  tmp = groupedDf %>% select(
    !!!grps,
    date = !!dateVar, 
    cumulative_cases= !!cumulativeCasesVar,
    total = !!totalVar,
    unknown = !!unknownVar
  ) %>% filter(!is.na(cumulative_cases))
  # clear out NA's if there are any
  if(adjustUnknowns) {
    tmp2 = tmp %>% mutate(adj_cumulative_cases = cumulative_cases*(1+unknown/total))# %>% mutate(adj_cumulative_cases = cummax(adj_cumulative_cases))
  } else {
    tmp2 = tmp %>% mutate(adj_cumulative_cases = cumulative_cases)# %>% mutate(adj_cumulative_cases = cummax(adj_cumulative_cases))
  }
  # calculate incidece
  tmp3 = tmp2 %>% group_by(!!!grps) %>% arrange(date) %>%
    mutate(incidence = as.integer(round(lead(adj_cumulative_cases))-round(adj_cumulative_cases))) %>%
    mutate(incidence = ifelse(is.na(incidence) & cumulative_cases==1,1,incidence)) # this was the index case otherwise we simply don't know incidence
  # make sure incidence positive or zero
  tmp4 = tmp3 %>% group_by(!!!grps) %>% arrange(date) %>%
    mutate(incidence = ifelse(incidence<0,0,incidence)) 
  tmp5 = tmp4 %>% 
    group_by(!!!grps) %>% arrange(date) %>%
    mutate(cumulative_cases = cumsum(ifelse(is.na(incidence),0,incidence))-incidence+min(cumulative_cases)) %>% select(-total,-unknown,-adj_cumulative_cases) %>% 
    rename(!!dateVar := date) %>% filter(!is.na(cumulative_cases)) # make sure date is same as input.
  return(tmp5)
}


getDemographics = function() {
  library(readxl)
  url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
  destfile <- "ukmidyearestimates20182019ladcodes.xls"
  curl::curl_download(url, destfile)
  ukmidyearestimates20182019ladcodes <- read_excel(destfile)
  View(ukmidyearestimates20182019ladcodes)
}
