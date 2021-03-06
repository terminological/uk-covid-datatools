#' General covid  processing
#' @import ggplot2
#' @import msm
#' @export
CovidTimeseriesProvider = R6::R6Class("CovidTimeseriesProvider", inherit=DataProvider, public = list(
  
  truncation = list(
    "case" = 4,
    "hospital admission" = 4,
    "icu admission" = 4,
    "death" = 10
  ),
  
  #' @description New timeseries provider pipeline
  #' @param providerController the provider controller
  #' @param ... for compatibility
  #' @return the provider
  initialize = function(providerController, ...) {
    super$initialize(providerController, ...)
  },
  
  setTruncation = function(listDays) {
    self$truncation = listDays
  },
  
  trimNAs = function(r0Timeseries) {
    covidTimeseriesFormat(r0Timeseries) %>% 
      dplyr::ungroup() %>%
      dplyr::group_by(statistic,type,code,codeType,source,subgroup,ageCat,gender) %>% 
      dplyr::group_modify(function(d,g,...) {
        # rowAny <- function(x) rowSums(x) > 0
        # tmp = d %>% filter(rowAny(across(-date,~!is.na(.))))
        # startDate = suppressWarnings(min(tmp$date))
        # endDate = suppressWarnings(max(d$date))
        # return(d %>% filter(date <= endDate & date >= startDate))
        startDate = suppressWarnings(min(d$date[!is.na(d$value)]))
        endDate = suppressWarnings(max(d$date[!is.na(d$value)]))
        return(d %>% filter(date <= endDate & date >= startDate))
      }) %>% 
      dplyr::ungroup()
  },
  
  #' @description Take a set of timeseries and add in missing dates as NAs. Preserves source, type, statistic differences in timeseries length. 
  #' intially Fills all regions to be the same length but with trim trailing NAs
  #' @return a covidTimeseriesFormat dataframe
  complete = function(covidTimeseries) {
    tmp = covidTimeseriesFormat(covidTimeseries)
    tmp = tmp %>% 
      dplyr::ungroup() %>%
      dplyr::group_by(source,type,statistic) %>% 
      dplyr::group_modify(function(d,g,...) {
        minDate = suppressWarnings(min(d$date,na.rm = TRUE))
        maxDate = suppressWarnings(max(d$date,na.rm = TRUE))
        if (!is.infinite(minDate) & !is.infinite(maxDate)) {
          ageCats = d %>% select(ageCat) %>% distinct()
          genders = d %>% select(gender) %>% distinct()
          subgroups = d %>% select(subgroup) %>% distinct()
          dates = tibble(
            date = as.Date(minDate:maxDate,"1970-01-01")
          )
          codesAndNames = d %>% select(code,name,codeType) %>% distinct()
          combinations = #tidyr::crossing(ageCats,genders,subgroups,dates,codesAndNames)
            dates %>% left_join(ageCats, by=character()) %>% left_join(genders, by=character()) %>% left_join(subgroups, by=character()) %>% left_join(codesAndNames, by=character())
          d = combinations %>% dplyr::left_join(d, by=c("ageCat","gender","subgroup","date","code","name","codeType"))
          if("Implicit" %in% colnames(d)) d = d %>% mutate(Implicit = ifelse(is.na(Implicit),FALSE,Implicit))
          return(d)
        } else {
          return(tibble())
        }
      }) %>%
      dplyr::ungroup() 
    tmp = tmp %>% self$trimNAs()
    return(tmp)
  },
  
  #' @description Take a set of timeseries and add in missing dates as NAs. Preserves source, type, statistic differences in timeseries length. 
  #' intially Fills all regions to be the same length but with trim trailing NAs
  #' @return a covidTimeseriesFormat dataframe
  completeAllRegions = function(covidTimeseries) {
    tmp = covidTimeseriesFormat(covidTimeseries)
    tmp = tmp %>% 
      dplyr::ungroup() %>%
      dplyr::group_by(source,type,statistic) %>% 
      dplyr::group_modify(function(d,g,...) {
        minDate = suppressWarnings(min(d$date,na.rm = TRUE))
        maxDate = suppressWarnings(max(d$date,na.rm = TRUE))
        if (!is.infinite(minDate) & !is.infinite(maxDate)) {
          ageCats = d %>% select(ageCat) %>% distinct()
          genders = d %>% select(gender) %>% distinct()
          subgroups = d %>% select(subgroup) %>% distinct()
          dates = tibble(
            date = as.Date(minDate:maxDate,"1970-01-01")
          )
          codeTypes = d %>% filter(!is.na(codeType)) %>% pull(codeType) %>% unique()
          codesAndNames = self$codes$getCodes() %>% filter(codeType %in% codeTypes & status == "live") 
          combinations = #tidyr::crossing(ageCats,genders,subgroups,dates,codesAndNames)
            dates %>% left_join(ageCats, by=character()) %>% left_join(genders, by=character()) %>% left_join(subgroups, by=character()) %>% left_join(codesAndNames, by=character())
          d = combinations %>% dplyr::left_join(d, by=c("ageCat","gender","subgroup","date","code","name","codeType"))
          if("Implicit" %in% colnames(d)) d = d %>% mutate(Implicit = ifelse(is.na(Implicit),FALSE,Implicit))
          return(d)
        } else {
          return(tibble())
        }
      }) %>%
      dplyr::ungroup() 
    tmp = tmp %>% self$trimNAs()
    return(tmp)
  },
  
  fillAbsent = function(covidTimeseries, completeDates=FALSE) {
    tmp = covidTimeseriesFormat(covidTimeseries)
    tmp = tmp %>% 
      dplyr::ungroup() %>%
      dplyr::group_by(source,type,statistic) %>% 
      dplyr::group_modify(function(d,g,...) {
        if (nrow(d) > 0) {
          ageCats = d %>% select(ageCat) %>% distinct()
          genders = d %>% select(gender) %>% distinct()
          subgroups = d %>% select(subgroup) %>% distinct()
          
          seenDates = unique(d$date)
          dates = tibble(date = as.Date(min(d$date):max(d$date),"1970-01-01"))
          
          codesAndNames = d %>% select(code,name,codeType) %>% distinct()
          combinations = #tidyr::crossing(ageCats,genders,subgroups,dates,codesAndNames)
            dates %>% left_join(ageCats, by=character()) %>% left_join(genders, by=character()) %>% left_join(subgroups, by=character()) %>% left_join(codesAndNames, by=character())
          d = combinations %>% dplyr::left_join(d, by=c("ageCat","gender","subgroup","date","code","name","codeType"))
          if (g$type == "cumulative") {
            d = d %>% dplyr::group_by(ageCat,gender,subgroup,code,name,codeType) %>% dplyr::arrange(date) %>% dplyr::mutate(Implicit = is.na(value)) %>% tidyr::fill(value) %>% dplyr::ungroup()
          } else if (g$type == "incidence") {
            d = d %>% dplyr::mutate(Implicit = is.na(value)) %>% dplyr::mutate(value = ifelse(is.na(value),0,value))
          } else {
            # bias or prevalence
            # leave as NA
            d = d %>% dplyr::mutate(Implicit = FALSE)
          }
          if (!completeDates) {
            d = d %>% mutate(
              value = ifelse(!(date %in% seenDates), NA_real_, value),
              Implicit = ifelse(!(date %in% seenDates), FALSE, Implicit)
            )
          }
          return(d)
        } else {
          return(tibble())
        }
      }) %>%
      dplyr::ungroup() 
    return(tmp)
  },
  
  #' @description Take a set of regional timeseries, finds the full list of regions and the whole timeseries and fills any missing values with zero (if incidence) or the previous value (if cumulative)
  #' explicitly filling in missing values. This is suitable for use for a incidence derived from a line list where some dates and regions will not be observed
  #' @return a covidTimeseriesFormat dataframe
  fillAbsentAllRegions = function(covidTimeseries) {
    tmp = covidTimeseriesFormat(covidTimeseries)
    tmp = tmp %>% 
      dplyr::ungroup() %>%
      dplyr::group_by(source,type,statistic) %>% 
      dplyr::group_modify(function(d,g,...) {
        if (nrow(d) > 0) {
          ageCats = d %>% select(ageCat) %>% distinct()
          genders = d %>% select(gender) %>% distinct()
          subgroups = d %>% select(subgroup) %>% distinct()
          dates = tibble(date = unique(d$date))
          codeTypes = d %>% filter(!is.na(codeType)) %>% pull(codeType) %>% unique()
          codesAndNames = self$codes$getCodes() %>% filter(codeType %in% codeTypes & status == "live") 
          combinations = #tidyr::crossing(ageCats,genders,subgroups,dates,codesAndNames)
            dates %>% left_join(ageCats, by=character()) %>% left_join(genders, by=character()) %>% left_join(subgroups, by=character()) %>% left_join(codesAndNames, by=character())
          d = combinations %>% dplyr::left_join(d, by=c("ageCat","gender","subgroup","date","code","name","codeType"))
          if (g$type == "cumulative") {
            d = d %>% dplyr::group_by(ageCat,gender,subgroup,code,name,codeType) %>% dplyr::arrange(date) %>% dplyr::mutate(Implicit = is.na(value)) %>% tidyr::fill(value) %>% dplyr::ungroup()
          } else if (g$type == "incidence") {
            d = d %>% dplyr::mutate(Implicit = is.na(value)) %>% dplyr::mutate(value = ifelse(is.na(value),0,value))
          } else {
            # bias or prevalence
            # leave as NA
            d = d %>% dplyr::mutate(Implicit = FALSE)
          }
          return(d)
        } else {
          return(tibble())
        }
      }) %>%
      dplyr::ungroup() 
    return(tmp)
  },
  
  #' @description Take a set of timeseries and fills any missing values with zero (if incidence) or the previous value (if cumulative)
  #' explicitly filling in zeros for missing values. This is suitable for use for a incidence derived from a line list where some dates will not be
  #' observed. It does not enforce that something has been observed in all regions.
  #' @return a covidTimeseriesFormat dataframe
  fillAbsentByRegion = function(covidTimeseries) {
    tmp = covidTimeseriesFormat(covidTimeseries)
    tmp = tmp %>% 
      dplyr::ungroup() %>%
      dplyr::group_by(source,type,statistic,code,name,codeType) %>% 
      dplyr::group_modify(function(d,g,...) {
        if (nrow(d) > 0) {
          ageCats = d %>% select(ageCat) %>% distinct()
          genders = d %>% select(gender) %>% distinct()
          subgroups = d %>% select(subgroup) %>% distinct()
          dates = tibble(date = unique(d$date))
          combinations = tidyr::crossing(ageCats,genders,subgroups,dates)
          d = combinations %>% dplyr::left_join(d, by=c("ageCat","gender","subgroup","date"))
          if (g$type == "cumulative") {
            d = d %>% dplyr::group_by(ageCat,gender,subgroup) %>% dplyr::arrange(date) %>% dplyr::mutate(Implicit = is.na(value)) %>% tidyr::fill(value) %>% dplyr::ungroup()
          } else if (g$type == "incidence") {
            d = d %>% dplyr::mutate(Implicit = is.na(value)) %>% dplyr::mutate(value = ifelse(is.na(value),0,value))
          } else {
            # bias or prevalence
            # leave as NA
            d = d %>% dplyr::mutate(Implicit = FALSE)
          }
          return(d)
        } else {
          return(tibble())
        }
      }) %>%
      dplyr::ungroup() 
    return(tmp)
  },
  
  #' @description Take a set of timeseries and fixes any non standard names, truncates time series by truncate days. 
  #' intially Fills all regions to be the same length but with trim trailing NAs
  #' @return a covidTimeseriesFormat dataframe
  fixDatesAndNames = function(covidTimeseries, truncate=NULL) {
    tmp5 = covidTimeseriesFormat(covidTimeseries)
    if(!("note" %in% colnames(tmp5))) tmp5 = tmp5 %>% dplyr::mutate(note=NA_character_)
    
    truncations = tibble::tibble(
      statistic = names(self$truncation),
      tmpTrunc = unlist(self$truncation))
    
    tmp5 = tmp5 %>% 
      dplyr::ungroup() %>%
      # dplyr::select(-name) %>%
      dplyr::left_join(truncations,by="statistic") %>%
      dplyr::mutate(tmpTrunc = ifelse(is.null(truncate), ifelse(is.na(tmpTrunc),0,tmpTrunc), truncate)) %>%
      self$codes$findNamesByCode( outputCodeTypeVar = "lookupCodeType" ) %>% 
      dplyr::select(-lookupCodeType) %>% 
      dplyr::mutate(name = ifelse(is.na(name),name.original,name)) %>%
      dplyr::group_by(code,codeType,name,source,subgroup,statistic,gender,ageCat,type) %>% 
      dplyr::filter(date <= max(date-tmpTrunc)) %>%
      dplyr::select(-tmpTrunc) %>%
      dplyr::ungroup()
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
      if(any(g$type == "incidence" & (!is.na(d$tmp_value) & d$tmp_value<0))) errs = c(errs,"Negative values in incidence figures")
      
      if (identical(errs,NULL)) errs = NA_character_ 
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

