#' General covid  processing
#' @import ggplot2
#' @import msm
#' @export
CovidTimeseriesProvider = R6::R6Class("CovidTimeseriesProvider", inherit=DataProvider, public = list(
  
  initialize = function(providerController, ...) {
    super$initialize(providerController, ...)
  },
  
  trimNAs = function(r0Timeseries) {
    covidTimeseriesFormat(r0Timeseries) %>% 
      dplyr::ungroup() %>%
      dplyr::group_by(statistic,type,code,codeType,source,subgroup,ageCat,gender) %>% 
      dplyr::group_modify(function(d,g,...) {
        startDate = suppressWarnings(min(d$date[!is.na(d$value)]))
        endDate = suppressWarnings(max(d$date[!is.na(d$value)]))
        return(d %>% filter(date <= endDate & date >= startDate))
      }) %>% 
      dplyr::ungroup()
  },
  
  complete = function(covidTimeseries, infer = TRUE) {
    tmp = covidTimeseriesFormat(covidTimeseries)
    tmp = tmp %>% 
      dplyr::ungroup() %>%
      dplyr::group_by(codeType,source,type,statistic) %>% 
      dplyr::group_modify(function(d,g,...) {
        minDate = suppressWarnings(min(d$date,na.rm = TRUE))
        maxDate = suppressWarnings(max(d$date,na.rm = TRUE))
        if (!is.infinite(minDate) & !is.infinite(maxDate)) {
          ageCats = d %>% select(ageCat) %>% distinct()
          genders = d %>% select(gender) %>% distinct()
          subgroups = d %>% select(subgroup) %>% distinct()
          dates = tibble(date = as.Date(minDate:maxDate,"1970-01-01"))
          codesAndNames = d %>% select(code,name) %>% distinct()
          combinations = tidyr::crossing(ageCats,genders,subgroups,dates,codesAndNames)
          d = combinations %>% dplyr::left_join(d, by=c("ageCat","gender","subgroup","date","code","name"))
          if (infer) {
            if (g$type == "cumulative") {
              d = d %>% dplyr::arrange(code,name,date) %>% dplyr::mutate(Inferred = is.na(value)) %>% tidyr::fill(value)
            } else if (g$type == "incidence") {
              d = d %>% dplyr::mutate(Inferred = is.na(value)) %>% dplyr::mutate(value = ifelse(is.na(value),0,value))
            } else {
              # bias or prevalence
              # leave as NA
              d = d %>% dplyr::mutate(Inferred = FALSE)
            }
          } else {
            d = d %>% dplyr::mutate(Inferred = FALSE)
          }
          return(d)
        } else {
          return(tibble())
        }
      }) %>%
      dplyr::ungroup() 
    tmp = tmp %>% self$trimNAs()
    return(tmp)
  },
  
  fixDatesAndNames = function(tmp5, truncate, naIsZero = FALSE) {
    if(!("note" %in% colnames(tmp5))) tmp5 = tmp5 %>% dplyr::mutate(note=NA)
    tmp5 = tmp5 %>% 
      dplyr::select(-name) %>%
      self$codes$findNamesByCode( outputCodeTypeVar = "lookupCodeType" ) %>% 
      dplyr::select(-lookupCodeType) %>% 
      dplyr::group_by(code,codeType,name,source,subgroup,statistic,gender,ageCat,type) %>% 
      dplyr::filter(date <= max(date)-truncate) %>%
      dplyr::ungroup()
    tmp5 = self$complete(tmp5, infer = naIsZero)
    return(tmp5)
  },
  
  #' @description Check timeseries conforms
  timeseriesQA = function(covidTimeseries) {
    stop("This needs testing")
    covidTimeseriesFormat(covidTimeseries) %>%
      dplyr::group_by(code,codeType,name,source,subgroup,statistic,gender,ageCat,type) %>% 
      dplyr::arrange(date) %>% 
      dplyr::mutate(duplicatedDates = (isTRUE(lead(date)==date) | isTRUE(lag(date)==date))) %>%
      dplyr::mutate(missingDates = !(isTRUE(lead(date)==date+1) & isFALSE(lag(date)==date-1))) %>%
      dplyr::mutate(naIncidence = (statistic != "incidence" | !is.na(value))) %>%
      dplyr::mutate(negativeIncidence = (statistic != "incidence" | value>0)) %>%
      dplyr::filter(duplicatedDates | missingDates | naIncidence | negativeIncidence)
  },
  
  describeErrors = function(covidTimeseries, valueVar = "value") {
    valueVar = ensym(valueVar)
    groupedDf = covidTimeseriesFormat(covidTimeseries) %>%
      covidStandardGrouping() %>%
      dplyr::mutate(tmp_value = !!valueVar)
    
    groupedDf = groupedDf %>% group_modify(function(d,g,...) {
      errs = NULL
      if(any(duplicated(d$date))) errs = c(errs,"Duplicate dates")
      if(length(d$date) < max(d$date)-min(d$date)) errs = c(errs,"Missing dates")
      if(any(is.na(d$tmp_value))) errs = c(errs,"NAs in values")
      if(any(g$type == "incidence" & d$tmp_value<0)) errs = c(errs,"Negative values in incidence figures")
      
      if (identical(errs,NULL)) errs = NA 
      else errs = paste0(errs,collapse="; ")
      
      return(d %>% dplyr::mutate(errors = errs))
    })
    if (!all(is.na(groupedDf$errors))) warning("Validation errors found in data sets - check \"errors\" column for details") 
    return(groupedDf %>% dplyr::select(-tmp_value) %>% dplyr::ungroup())
  },
  
  describe = function(covidTimeseries,...) {
    covidTimeseriesFormat(covidTimeseries) %>% 
      dplyr::group_by(statistic,type,codeType,source,subgroup,ageCat,gender) %>% 
      dplyr::filter(...) %>%
      dplyr::summarise(
        areas = n_distinct(code), totalValue=sum(value,na.rm = TRUE),
        from = as.Date(min(date),"1970-01-01"),
        to = as.Date(max(date),"1970-01-01")
      )
  }
  
))

