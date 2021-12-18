#' General timeseries processing
#' @import ggplot2
#' @export
TimeseriesProcessingPipeline = R6::R6Class("TimeseriesProcessingPipeline", inherit=CovidTimeseriesProvider, public = list(
  
  serial = NULL,
  
  
  initialize = function(providerController, ...) {
    super$initialize(providerController, ...)
    self$serial = providerController$serial #SerialIntervalProvider$default(providerController)
  },
  
  #### type conversion functions ----
  
  incidenceFromCumulative = function(covidTimeseries, ...) {
    self$getHashCached(object = covidTimeseries, operation = "INC-FROM-CUM", ... , orElse = function (ts, ...) {covidTimeseriesFormat %def% {
      dplyr::bind_rows(
        ts %>% dplyr::filter(type == "incidence"),
        ts %>% 
          dplyr::filter(type == "cumulative") %>% 
          covidStandardGrouping() %>%
          dplyr::mutate(value = value-lag(value)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            value = ifelse(value<0, NA, value), 
            type = "incidence",
            source = paste0(source," (from cumulative)")
          ) 
      )
    }})
  },
  
  cumulativeFromIncidence = function(covidTimeseries, ...) {
    self$getHashCached(object = covidTimeseries, operation = "CUM-FROM-INC", ... , orElse = function (ts, ...) {covidTimeseriesFormat %def% {
      dplyr::bind_rows(
        ts %>% dplyr::filter(type == "cumulative"),
        ts %>% 
          dplyr::filter(type == "incidence") %>% 
          covidStandardGrouping() %>%
          dplyr::mutate(value = cumsum(value)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(
            type = "cumulative",
            source = paste0(source," (from incidence)")
          )
      )
    }})
  },
  
  #### aggregation functions ----
  
  aggregateRagged = function(groupedDf, originalVar, aggregateVars, ..., fn = sum, dateVar = "date", valueVars = vars(value)) {
    dateVar = ensym(dateVar)
    originalVar = ensym(originalVar)
    grps = groupedDf %>% groups()
    
    output = groupedDf %>% select(!!!grps,!!originalVar,!!!aggregateVars) %>% distinct()
    cross = groupedDf %>% select(!!!grps,!!dateVar) %>% distinct()
    complete = output %>% inner_join(cross, by=unlist(sapply(grps,as_label)))
    
    join = sapply(c(grps,aggregateVars,originalVar,dateVar), as_label)
    output2 = complete %>% left_join(groupedDf %>% select(!!!grps,!!!aggregateVars,!!originalVar,!!dateVar, !!!valueVars), by=join)
    
    tmpFn = function(x) fn(x,...)
    
    output3 = output2 %>% group_by(!!!grps,!!!aggregateVars,!!dateVar) %>% 
      dplyr::summarise(
        across(sapply(valueVars,as_label), tmpFn)
        #!!(paste0("Count.",as_label(originalVar))) := sum()
      ) %>% 
      dplyr::mutate(across(sapply(valueVars,as_label), ~ifelse(is.nan(.x),NA,.x)))
    
    return(output3 %>% dplyr::ungroup())
  },
  
  aggregateAge = function(covidTimeseries, fn = sum, ...) {covidTimeseriesFormat %def% {
    tmp = covidTimeseriesFormat(covidTimeseries)
    
    errors = tmp %>% 
      covidStandardDateGrouping(ageCat) %>%
      dplyr::summarise(mixed = any(is.na(ageCat)) & any(!is.na(ageCat))) %>%
      dplyr::filter(mixed==TRUE)
    if(nrow(errors) > 0) warning("aggregating by age, but some groups have mixed NAs and values. You maybe wanted to filter out the NAs:\n", paste(capture.output(print(errors)), collapse = "\n"))
    
    tmp = tmp %>% mutate(oldAgeCat = ageCat, ageCat = NA_character_) %>% covidStandardGrouping(ageCat)
    
    if ("population" %in% colnames(tmp)) {
      tmp = tmp %>% 
        self$aggregateRagged(originalVar = oldAgeCat, aggregateVars = vars(ageCat), ..., fn = fn, valueVars = vars(value,population))
    } else {
      tmp = tmp %>% 
        self$aggregateRagged(originalVar = oldAgeCat, aggregateVars = vars(ageCat), ..., fn = fn, valueVars = vars(value))
    }
    tmp = tmp %>% self$trimNAs()
    return(tmp %>% dplyr::ungroup())
  }},
  
  aggregateGender = function(covidTimeseries, fn = sum, ...) {covidTimeseriesFormat %def% {
    tmp = covidTimeseriesFormat(covidTimeseries)
    
    errors = tmp %>% 
      covidStandardDateGrouping(gender) %>% 
      dplyr::summarise(mixed = any(is.na(gender)) & any(!is.na(gender))) %>%
      dplyr::filter(mixed==TRUE)
    if(nrow(errors) > 0) warning("aggregating by gender, but some groups have mixed NAs and values. You maybe wanted to filter out the NAs:\n", paste(capture.output(print(errors)), collapse = "\n"))
    
    tmp= tmp %>% dplyr::mutate(oldGender=gender, gender=NA_character_) %>% covidStandardGrouping(gender)
    if ("population" %in% colnames(tmp)) {
      tmp = tmp %>% 
        self$aggregateRagged(originalVar = oldGender, aggregateVars = vars(gender), ..., fn = fn, valueVars = vars(value,population))
    } else {
      tmp = tmp %>% 
        self$aggregateRagged(originalVar = oldGender, aggregateVars = vars(gender), ..., fn = fn, valueVars = vars(value))
    }
    
    tmp = tmp %>% self$trimNAs()
    return(tmp %>% dplyr::ungroup())
  }},
  
  aggregateSubgroup = function(covidTimeseries, fn=sum, ...) {covidTimeseriesFormat %def% {
    tmp = covidTimeseriesFormat(covidTimeseries)
    
    errors = tmp %>% 
      covidStandardDateGrouping(subgroup) %>%
      dplyr::summarise(mixed = any(is.na(subgroup)) & any(!is.na(subgroup))) %>%
      dplyr::filter(mixed==TRUE)
    if(nrow(errors) > 0) warning("aggregating by subgroup, but not all items have a subgroup. You maybe wanted to filter out the NAs:\n", paste(capture.output(print(errors)), collapse = "\n"))
  
    tmp= tmp %>% dplyr::mutate(oldSubgroup=subgroup, subgroup=NA_character_) %>% covidStandardGrouping(subgroup)
    if ("population" %in% colnames(tmp)) {
      tmp = tmp %>% 
        self$aggregateRagged(originalVar = oldSubgroup, aggregateVars = vars(subgroup), ..., fn = fn, valueVars = vars(value,population))
    } else {
      tmp = tmp %>% 
        self$aggregateRagged(originalVar = oldSubgroup, aggregateVars = vars(subgroup), ..., fn = fn, valueVars = vars(value))
    }
    tmp = tmp %>% self$trimNAs()
    return(tmp %>% dplyr::ungroup())
  }},
  
  aggregateSource = function(covidTimeseries, namedListOfSources = list("All sources"=unique(covidTimeseries$source)), fn=sum, ...) {covidTimeseriesFormat %def% {
    tmp = covidTimeseriesFormat(covidTimeseries)
    tmp$oldSource = tmp$source
    for (name in names(namedListOfSources)) {
      tmp$source[tmp$oldSource %in% namedListOfSources[[name]]] = name; 
    }
    
    tmp= tmp %>% covidStandardGrouping(source)
    if ("population" %in% colnames(tmp)) {
      tmp = tmp %>% 
        self$aggregateRagged(originalVar = oldSource, aggregateVars = vars(source), ..., fn = fn, valueVars = vars(value,population))
    } else {
      tmp = tmp %>% 
        self$aggregateRagged(originalVar = oldSource, aggregateVars = vars(source), ..., fn = fn, valueVars = vars(value))
    }
    tmp = tmp %>% self$trimNAs()
    return(tmp %>% dplyr::ungroup())
  }},
  
  #' @param completeness should the mapping be complete? if the mapping is "source" complete it will only be successful if all source codes are present when mapping to a higher region. if the mapping is target, then only if all the target codes are represented. Or both if the mapping must be complete at both ends.
  aggregateGeography = function(covidTimeseries, targetCodeTypes, completeness = "source", fn=sum, ...) {covidTimeseriesFormat %def% {
    tmp = covidTimeseriesFormat(covidTimeseries)
    
    mapping = self$codes$getTransitiveClosure() %>% 
      dplyr::filter(toCodeType %in% targetCodeTypes) %>%
      dplyr::semi_join(tmp, by=c("fromCode" = "code")) %>%
      dplyr::select(fromCode,toCode) %>%
      self$codes$findNamesByCode(codeVar = toCode, outputNameVar = toName, outputCodeTypeVar = toCodeType) %>%
      dplyr::distinct()
    if(nrow(mapping)==0) stop("no route to target code types in transitive closure.")
    
    tmp2 = tmp %>% 
      dplyr::inner_join(mapping, by=c("code" = "fromCode"))
    tmp3 = tmp2 %>%
      dplyr::rename(fromCode=code,fromCodeType = codeType, fromName = name) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        note = paste0(fromCodeType,"->",toCodeType),
        source = paste0(source," (aggr ",fromCodeType,")")
      ) %>%
      dplyr::group_by(source,statistic,type,fromCodeType,toCodeType)
    
    tmp3 = tmp3 %>%
      dplyr::group_modify(function(d,g,...) {
          # targets with no sources in the map:
          nosource = mapping %>% dplyr::filter(toCodeType == g$toCodeType) %>% dplyr::anti_join(d, by="toCode")
          # sources with no targets in the map:
          notarget = mapping %>% dplyr::filter(toCodeType == g$toCodeType) %>% dplyr::anti_join(d, by="fromCode")
          mapped = d %>% 
            dplyr::filter(!is.na(fromCode) & !is.na(toCode)) %>% 
            dplyr::ungroup() %>%
            dplyr::mutate(codeType=g$toCodeType) %>%
            dplyr::rename(code = toCode, name = toName) %>%
            dplyr::select(-fromName) %>%
            dplyr::group_by(subgroup,gender,ageCat) 
          
          if("population" %in% colnames(d)) {
            mapped = mapped %>% 
              self$aggregateRagged(originalVar = fromCode, aggregateVars = vars(code,name,codeType), ..., fn = fn,valueVars = vars(value,population))
          } else {
            mapped = mapped %>% 
              self$aggregateRagged(originalVar = fromCode, aggregateVars = vars(code,name,codeType), ..., fn = fn,valueVars = vars(value))
          }
          
          if("source"==completeness) { 
            if(nrow(notarget) == 0) {
              # complete mapping
              return(mapped)
            } else {
              stop("Cannot map sources:\n",printDataframeToString(notarget))
              #return(tibble())
            }
          } else if("target"==completeness) {
            if(nrow(nosource) == 0) {
              # complete mapping
              return(mapped)
            } else {
              stop("Cannot map targets:\n",printDataframeToString(nosource))
              #return(tibble())
            }
          } else if(completeness %in% c("all","both")) {
            if(nrow(notarget) == 0 & nrow(nosource) == 0) {
              # complete mapping
              return(mapped)
            } else {
              stop("Cannot map targets:\n",printDataframeToString(nosource),"\n","Cannot map sources:\n",printDataframeToString(notarget))
              return(tibble())
            }
          } else {
            # incomplete mapping
            mapped = mapped %>% dplyr::mutate(mappingComplete = TRUE)
            if(nrow(notarget) > 0) {
              warning("no targetCodeType mapping found for: ",paste0(unique(notarget$toName), collapse="; ")," - aggregation will lose some cases")
              mapped = mapped %>% dplyr::mutate(mappingComplete = FALSE)
            }
            if(nrow(nosource) > 0) {
              warning("no sourceCodeType mapping found for: ",paste0(unique(nosource$toName), collapse="; ")," - mapping is incomplete")
              mapped = mapped %>% dplyr::mutate(mappingComplete = FALSE)
            }
            return(mapped)
          }
      })
    tmp3 = tmp3 %>% dplyr::ungroup() %>% dplyr::select(-fromCodeType, -toCodeType) %>% self$trimNAs()
    return(tmp3 %>% dplyr::ungroup())
  }},
  
  #### smoothing and imputing ----
  
  #' @description Low tech smoothing and missing value imputation, based on a log1p transform and a sgolayfilter to preserve ends of timeseries. This also does anomaly detection and is the one stop shop to minimally clean a timeseries.
  #' @param window - the window length for the rolling average
  #' @return - the timeseries with an Imputed.value, a RollMean.value column and an Imputed flag.
  imputeAndWeeklyAverage = function(covidTimeseries, window=7, ...) {
    if ("RollMean.value" %in% colnames(covidTimeseries)) {
      return(covidTimeseries)
    }
    ts=covidTimeseries
    self$getHashCached(object = covidTimeseries, operation="IMPUTE", ... , orElse = function (ts, ...) {covidTimeseriesFormat %def% {
      tmp = ts %>%
        covidStandardGrouping() %>%
        self$completeAndRemoveAnomalies(valueVar = value, originalValueVar = value.original) %>%
        dplyr::mutate(
          logValue1 = log(value+1)) %>%
        covidStandardGrouping() %>%
        dplyr::group_modify(function(d,g,...) {
          d = d %>% arrange(date)
          if (sum(!is.na(d$logValue1)) < 2) {
            d$logValue1 = rep(NA,length(d$logValue1))
          } else {
            d$logValue1 = forecast::na.interp(d$logValue1)
          }
          if (length(d$logValue1) > window) {
            d$logValue2 = signal::sgolayfilt(d$logValue1,p=1,n=window)
          } else {
            d$logValue2 = rep(NA,length(d$logValue1))
          }
          return(d) 
        }) %>%
        dplyr::mutate(
          Imputed.value = ifelse(logValue1 < 0,0,exp(logValue1)-1),
          Imputed = is.na(value) & !is.na(Imputed.value),
          RollMean.value = ifelse(logValue2 < 0,0,exp(logValue2)-1),
          Window.RollMean.value = window
        ) %>%
        dplyr::select(-logValue1, -logValue2) %>%
        dplyr::ungroup() 
      #browser(expr = self$debug)
      return(tmp)
    }})
  },
  
  #' @description Identify and make NA any days for which the timeseries contains no non zero values
  #' @param valueVar - the value to look at
  #' @return - the same timeseries with totally missing days replaced with NA
  #' TODO: do we want to store the original somewhere?
  removeZeroDays = function(r0Timeseries, valueVar = "value") {
    valueVar = ensym(valueVar)
    r0Timeseries %>% 
      group_by(date) %>% 
      mutate(.anyNonZero = any(!!valueVar!=0)) %>% 
      mutate(!!valueVar := ifelse(.anyNonZero,!!valueVar,NA_real_)) %>%
      select(-.anyNonZero) %>%
      return()
  },
  
  #' @description Ensure completeness timeseries and remove anomalies. Anomolies are detected as improbable observations based on a an approximate log normal of surrounding obs.
  #' @param r0timeseries a time series includeing incidence totals
  #' @param smoothExpr an expression to evaluate in the context of the dataframe to generate a variable to be smoothed (e.g. log(value+1))
  #' @param ... - not used
  #' @param window - the window over which to check anomaly status (defaults to 9)
  #' @param p - how unlikely is an oservation before removed.
  #' @param min - the minimum SD allowed (to prevent runs of identical values causing issues)
  #' @param valueVar - the column to screen
  #' @param originalValueVar - the column to save to originals to
  #' @param allowZeroDays - whether to identify dates for which there are zero counts across all groups. This helps in the detection of totally missing days.
  #' @return an timeseries with Anomaly column 
  completeAndRemoveAnomalies = function(r0Timeseries, window=9, p = 0.999, min = 1, valueVar = "value", originalValueVar = "value.original", allowZeroDays=FALSE) {covidTimeseriesFormat %def% {
    
    # TODO: generate more tests for this from manual review of:
    # View(symptomaticTimeseries %>% filter(is.na(stats::filter(value,rep(1,9)))))
    
    valueVar = ensym(valueVar)
    originalValueVar = ensym(originalValueVar)
    if ("Anomaly" %in% colnames(r0Timeseries)) {
      # detection already performed
      return(r0Timeseries)
    }
    # trim tailing NAs
    tmp = self$trimNAs(r0Timeseries)
    groups = tmp %>% covidStandardGrouping() %>% n_groups()
    if (!allowZeroDays & groups>1) tmp = tmp %>% self$removeZeroDays()
    
    # the minimum value of lambda is set so that 0 is within the confidence limits.
    # usually this will allow noise at low levels
    minLambda = -log(2*(1-p))
    
    tmp = tmp %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!originalValueVar := !!valueVar) %>%
      self$complete() %>%
      covidStandardGrouping() %>%
      
      dplyr::group_modify(function(d,g,...) {
        
        d = d %>% mutate(Anomaly = FALSE)
        
        fn = function(e) {
          y_orig = e %>% pull(!!valueVar)
          # #TODO: make this a more generic way of doing windowing & tidyify it
          y = log(y_orig+1)
          i = 1:(length(y))
          w2 = floor(window/2)
          # set anomalous values to NA
          y[d$Anomaly] = NA
          # add in head and tail
          v = c(rep(NA,w2), y, rep(NA,w2))
          # turn time series into matrix with columns for each window
          m = sapply(i,function(j) {v[j:(j+w2*2)]})
          # get rid of central value
          m[w2+1,] = NA
          
          intercept = function(y) {
            #browser()
            x = (-w2:w2)[!is.na(y)]
            y = y[!is.na(y)]
            n = length(y)
            m = (n*sum(x*y)-sum(x)*sum(y))/(n*sum(x^2)-sum(x)^2)
            c = (sum(y) - m*sum(x))/n
            
            return(c)
          }
          
          m_mean = apply(m, 2, intercept) #2 here means apply mean col-wise
          m_sd = pmax(apply(m, 2, sd, na.rm=TRUE),log(min+1)) #2 here means apply mean col-wise
          
          # m_lambda = pmax(exp(m_mean)-1, minLambda)
          # m_p = 1-m_mean/m_sd^2
          # m_r = m_mean^2/(m_sd^2-m_mean)
          # p_obs = abs(ppois(y_orig, m_lambda)-0.5)*2
          p_obs = abs(pnorm(y, mean = m_mean,sd = m_sd)-0.5)*2
          Anomaly = p_obs > p | !is.finite(p_obs) #& !(m_lambda==0 & y_orig==0) 
          # browser()
          return(Anomaly)
        }
        
        i=1
        repeat {
          newAnomaly = fn(d)
          if (i>5) {
            warning("anomaly detection did not converge")
            break;
          }
          if (all(newAnomaly==d$Anomaly)) break;
          d$Anomaly=newAnomaly
          i=i+1
        }
        # d$Anomaly = fn(d)
        # browser()
        # d$Anomaly = fn(d)
        # #TODO: repeat the anomaly removal stage to detect anything ?
        
        d = d %>% dplyr::mutate(!!valueVar := ifelse(Anomaly,NA,!!valueVar), Anomaly = Anomaly & !is.na(!!originalValueVar))
        # browser()
        return(d)
      }) %>% 
      dplyr::ungroup()
    return(tmp)
  }}, 
  
  #' @description Calculate an estimate of central value and rate of change of a value using a loess smoother. This makes no assumptions and can be used for any statistic.
  #' @param r0timeseries a timeseries including a date
  #' @param smoothExpr an expression to evaluate in the context of the dataframe to generate a variable to be smoothed (e.g. log(value+1))
  #' @param ... - not used
  #' @param window - the smoothing window (defaults to 21)
  #' @return an timeseries augmented with estimates of the smoothExpr and first derivative of the smoothExpr.
  smoothAndSlopeTimeseries = function(r0Timeseries, smoothExpr, ..., window = 21) {covidTimeseriesFormat %def% {
    smoothExpr = enexpr(smoothExpr)
    smoothLabel = as_label(smoothExpr)
    if (smoothLabel == "value") message("Smoothing value directly does not account for its exponential nature - you maybe want logIncidenceStats?")
    if (paste0("Est.",smoothLabel) %in% colnames(r0Timeseries)) {
      warning(smoothLabel," has already been estimated. aborting smooth and slope.")
      return(r0Timeseries)
    }
    covidTimeseriesFormat(r0Timeseries) %>% 
      dplyr::ungroup() %>%
      # TODO: does this make sense here as the anomaly detection is focussed around exponentially growing quantities?
      self$completeAndRemoveAnomalies() %>%
      dplyr::mutate(
        y = !!smoothExpr
      ) %>%
      dplyr::mutate(
        x = as.integer(date-min(date)),
      ) %>%
      covidStandardGrouping() %>%
      dplyr::group_modify(function(d,g,...) {
        tmp_alpha = min(window/nrow(d),1)
        tmp_alpha_2 = min(window*2/nrow(d),1)
        tmp_ts = d %>% 
          dplyr::mutate(
            y = forecast::na.interp(y), # TODO: locfit does not need NAs to be interpolated. This is probably extraneous.
            window = window
          ) %>%
          dplyr::select(date,x,y,Anomaly,window) 
        
        if(nrow(tmp_ts) == 0) {
          warning("No non NA values to smooth in ",paste0(g$statistic,g$type,g$codeType,g$source,g$subgroup,g$ageCat,g$gender,collapse = "; "))
          return(d)
        }
        # calculate derivative using a loess method
        # tmp = locfit::locfit(log(value+1) ~ day,loessTest,deg = 2,alpha=0.25,deriv=1)
        # if (leftSided) {
        #   tmp_intercept_model = locfit::locfit(y ~ locfit::left(x, nn=tmp_alpha_2, deg=1), tmp_ts)#, family="qgamma")
        #   tmp_slope_model = locfit::locfit(y ~ locfit::left(x, nn=tmp_alpha_2, deg=1), tmp_ts, deriv=1)#, family="qgamma")
        #   #tmp_intercept_model = locfit::locfit(y ~ locfit::left(x, h=window, deg=1), tmp_ts)#, family="qgamma")
        #   #tmp_slope_model = locfit::locfit(y ~ locfit::left(x, h=window, deg=1), tmp_ts, deriv=1)#, family="qgamma")
        # } else {
        # TODO: look at changing this in line with evaluation points in estimateGrowthRate(...)
          tmp_intercept_model = locfit::locfit(y ~ locfit::lp(x, nn=tmp_alpha_2, deg=1), tmp_ts)#, family="qgamma")
          tmp_slope_model = locfit::locfit(y ~ locfit::lp(x, nn=tmp_alpha_2, deg=1), tmp_ts, deriv=1)#, family="qgamma")
          #tmp_intercept_model = locfit::locfit(y ~ locfit::lp(x, h=window/2, deg=1), tmp_ts)#, family="qgamma")
          #tmp_slope_model = locfit::locfit(y ~ locfit::lp(x, h=window/2, deg=1), tmp_ts, deriv=1)#, family="qgamma")
        # }
        #tmp_intercept_model = locfit::locfit(y ~ x, tmp_ts, deg=1, alpha=tmp_alpha)
        #tmp_slope_model = locfit::locfit(y ~ x, tmp_ts, deg=1, alpha=tmp_alpha, deriv=1)
        tmp_intercept = predict(tmp_intercept_model, tmp_ts$x, band="global")
        tmp_slope = predict(tmp_slope_model, tmp_ts$x, band="global")
        tmp_ts[[paste0("Est.",smoothLabel)]] = ifelse(tmp_intercept$fit < 0, 0, tmp_intercept$fit) # prevent negative smoothing.
        tmp_ts[[paste0("Est.SE.",smoothLabel)]] = tmp_intercept$se.fit
        #tmp_ts[[paste0("Est.RMSE.",smoothLabel)]] = sqrt(mean(residuals(tmp_intercept_model,tmp_ts)^2))
        tmp_ts[[paste0("Slope.",smoothLabel)]] = tmp_slope$fit
        tmp_ts[[paste0("Slope.SE.",smoothLabel)]] = tmp_slope$se.fit
        # browser()
        # tmp_ts[[paste0("Slope.RMSE.",smoothLabel)]] = sqrt(mean(residuals(tmp_slope_model,tmp_ts)^2))
        
        # # https://en.wikipedia.org/wiki/Ratio_distribution#Uncorrelated_noncentral_normal_ratio
        mu_x = tmp_slope$fit
        mu_y = ifelse(tmp_intercept$fit < 0, 0, tmp_intercept$fit)
        sig_x = tmp_slope$se.fit
        sig_y = tmp_intercept$se.fit
        
        tmp_ts[[paste0("Ratio.",smoothLabel)]] = ifelse(mu_y==0,0,mu_x/mu_y)
        tmp_ts[[paste0("Ratio.SE.",smoothLabel)]] = sqrt((mu_x^2/mu_y^2)*(sig_x^2/mu_x^2+sig_y^2/mu_y^2))
        tmp_ts[[paste0("Window.",smoothLabel)]] = tmp_ts$window
        # #  Díaz-Francés, E. & Rubio, F. J. On the existence of a normal approximation to the distribution of the ratio of two independent normal random variables. Statist. Papers 54, 309–323 (2013)
        
        browser(expr = self$debug)
        return(d %>% select(-x,-y) %>% dplyr::full_join(tmp_ts %>% select(-x,-y, -Anomaly, -window), by="date"))
      }) %>% 
      dplyr::ungroup()
  }},

  #### calculate baskets of stats ----
  #' @description Calculates a basket of stats based on 
  #' @param covidTimeseries a covid timeseries data frame
  #' @param window - the length of the smoothing window (default 21)
  #' @return a dataframe with groupwise growth rate estimates
  logIncidenceStats = function(covidTimeseries, valueVar = "value", growthRateWindow = 7,smoothingWindow = 14,earliestPossibleDate = "2020-02-01", ...) {
    valueVar = ensym(valueVar)
    message("This function is deprecated: switch logIncidenceStats() to estimateGrowthRate()")
    self$getHashCached(object = covidTimeseries, operation="LOG-INCIDENCE", params=list(as_label(valueVar),growthRateWindow,smoothingWindow), ... , orElse = function (...) {covidTimeseriesFormat %def% {
      
      tmp = covidTimeseriesFormat(covidTimeseries)  %>% 
        ensurer::ensure_that(any(.$type == "incidence") ~ "dataset must have incidence totals") %>%
        dplyr::filter(type=="incidence")
      
      logExpr = expr(log(!!valueVar+1))
      
      lbl = function(pref) return(paste0(pref,".",as_label(logExpr)))
      lblV = function(pref) return(paste0(pref,".",as_label(valueVar)))
      
      if (lblV("Growth") %in% colnames(covidTimeseries)) {
        # this has already been done
        return(covidTimeseries) 
      }
      
      # The following does anomaly detection (but not imputation)
      tmp = tmp %>% self$smoothAndSlopeTimeseries(!!logExpr,window = smoothingWindow,...)
      
      tmp[[lblV("Growth")]] = tmp[[lbl("Slope")]]
      tmp[[lblV("Growth.SE")]] =  tmp[[lbl("Slope.SE")]]
      # tmp[[lblV("Growth.RMSE")]] =  tmp[[lbl("Slope.RMSE")]]
      tmp[[lblV("Growth.ProbPos")]] = 1-pnorm(0,mean=tmp[[lblV("Growth")]],sd=tmp[[lblV("Growth.SE")]])
      
      tmp$interceptDate = as.Date(tmp$date - tmp[[lbl("Est")]]/tmp[[lbl("Slope")]])
      tmp$interceptDate = ifelse(tmp$interceptDate<earliestPossibleDate, NA, tmp$interceptDate)
      
      tmp$doublingTime = log(2)/tmp[[lblV("Growth")]]
      tmp$doublingTime.Quantile.0.025 = log(2)/qnorm(mean=tmp[[lblV("Growth")]],sd=tmp[[lblV("Growth.SE")]],p = 0.975)
      tmp$doublingTime.Quantile.0.25 = log(2)/qnorm(mean=tmp[[lblV("Growth")]],sd=tmp[[lblV("Growth.SE")]],p = 0.75)
      tmp$doublingTime.Quantile.0.75 = log(2)/qnorm(mean=tmp[[lblV("Growth")]],sd=tmp[[lblV("Growth.SE")]],p = 0.25)
      tmp$doublingTime.Quantile.0.975 = log(2)/qnorm(mean=tmp[[lblV("Growth")]],sd=tmp[[lblV("Growth.SE")]],p = 0.025)
      
      meanLbl = lbl("Est")
      sdLbl = lbl("Est.SE")
      q = qnorm(c(0.025,0.25,0.5,0.75,0.975))
      fn = function(i) return(exp(tmp[[meanLbl]]+q[i]*tmp[[sdLbl]])-1)
      
      tmp[[lblV("Est")]] = exp(tmp[[meanLbl]])-exp(-1)
      tmp[[lblV("Est.Quantile.0.025")]] = fn(1)
      tmp[[lblV("Est.Quantile.0.25")]] = fn(2)
      tmp[[lblV("Est.Quantile.0.5")]] = fn(3)
      tmp[[lblV("Est.Quantile.0.75")]] = fn(4)
      tmp[[lblV("Est.Quantile.0.975")]] = fn(5)
      
      tmp[["tmp_gv"]] = tmp[[lblV("Growth")]]
      tmp[["tmp_gv.se"]] = tmp[[lblV("Growth.SE")]]
      
      tmp = tmp %>% 
        covidStandardGrouping() %>% 
        dplyr::mutate(
            !!lblV("Growth.windowed") := stats::filter(x = tmp_gv, filter=rep(1/growthRateWindow,growthRateWindow)),
            !!lblV("Growth.windowed.SE") := stats::filter(x = tmp_gv.se, filter=rep(1/growthRateWindow,growthRateWindow)) / sqrt(growthRateWindow),
            !!lblV("Growth.windowed.Window") := growthRateWindow
        ) %>% 
        dplyr::select(-tmp_gv, -tmp_gv.se)
      
      tmp$doublingTime.windowed = log(2)/tmp[[lblV("Growth.windowed")]]
      tmp$doublingTime.windowed.Quantile.0.025 = log(2)/qnorm(mean=tmp[[lblV("Growth.windowed")]],sd=tmp[[lblV("Growth.windowed.SE")]],p = 0.975)
      tmp$doublingTime.windowed.Quantile.0.25 = log(2)/qnorm(mean=tmp[[lblV("Growth.windowed")]],sd=tmp[[lblV("Growth.windowed.SE")]],p = 0.75)
      tmp$doublingTime.windowed.Quantile.0.75 = log(2)/qnorm(mean=tmp[[lblV("Growth.windowed")]],sd=tmp[[lblV("Growth.windowed.SE")]],p = 0.25)
      tmp$doublingTime.windowed.Quantile.0.975 = log(2)/qnorm(mean=tmp[[lblV("Growth.windowed")]],sd=tmp[[lblV("Growth.windowed.SE")]],p = 0.025)
      
      tmp[[lblV("Growth.windowed.ProbPos")]] = 1-pnorm(0,mean=tmp[[lblV("Growth.windowed")]],sd=tmp[[lblV("Growth.windowed.SE")]])
      
      # calculate imputed value as close to actual value as possible
      tmp$Imputed = is.na(tmp$value) & !is.na(tmp[[lblV("Est.Quantile.0.5")]])
      tmp$Imputed.value = ifelse(is.na(tmp$value),tmp[[lblV("Est.Quantile.0.5")]],tmp$value)
        
      return(tmp %>% ungroup())
    }})
  },
  
  #' @description Calculates a growth rate timeseries using a fitted quasipoisson model fitted using sliding windows on the assumption growth is constant over the window this is for comparison and estmiateGrowthRate(...) is almost always better
  #' @param covidTimeseries a covid timeseries data frame
  #' @param window - the length of the smoothing window (default 21)
  #' @return a dataframe with groupwise growth rate estimates
  estimatePoissonGrowthRate = function(covidTimeseries, window = 21, ...) {
    self$getHashCached(object = covidTimeseries, operation="ESTIM-LITTLE-R", params=list(window), ... , orElse = function (ts, ...) {covidTimeseriesFormat %def% {

      w2 = floor(window/2)
      
      groupedDf = covidTimeseriesFormat(covidTimeseries) %>%
        ensurer::ensure_that(any(.$type == "incidence") ~ "dataset must contain incidence figures") %>%
        dplyr::filter(type == "incidence") %>%
        self$completeAndRemoveAnomalies() %>%
        dplyr::mutate(I = value, errors=NA_character_)

      
      fitPois = function(d) {
        if(sum(d$I > 0,na.rm = TRUE) == 0) {
          result = tibble(
            Est.value = 0,
            Est.SE.value = NA_real_,
            Est.dispersion = 0,
            Est.error = 0,
            Growth.value = 0,
            Growth.SE.value = NA_real_,
            Growth.N = nrow(d)
          )
        } else {
          
          ltrunc = (d$row[1] == 1 & length(d$row) < (w2*2+1)) * ((w2*2+1)-length(d$row))
          rtrunc = (d$row[1] != 1 & length(d$row) < (w2*2+1)) * ((w2*2+1)-length(d$row))
          # if(leftSided) {
          #   d = d %>% mutate(time = -(w2*2-ltrunc):0)
          # } else {
          d = d %>% mutate(time = -(w2-ltrunc):(w2-rtrunc))
          # }
          model = suppressWarnings(glm(I ~ time, family = quasipoisson(), data = d))
          # browser()
          model_sum <- summary(model)
          result = tibble(
            Est.value = ifelse(model$df.residual < w2-1, NA_real_, tryCatch(model_sum$family$linkinv(model_sum$coefficients[1,1]), error = function(e) 0)),
            Est.SE.value = ifelse(model$df.residual < w2-1, NA_real_, tryCatch(model_sum$family$linkinv(model_sum$coefficients[1,2]), error = function(e) NA)),
            Est.dispersion = ifelse(model$df.residual < w2-1, NA_real_, model_sum$dispersion),
            Est.error = ifelse(model$df.residual < w2-1, NA_real_, mean(model$residuals,na.rm = TRUE)),
            Growth.value = ifelse(model$df.residual < w2-1, NA_real_, tryCatch(model_sum$coefficients[2,1], error = function(e) NA)),
            Growth.SE.value = ifelse(model$df.residual < w2-1, NA_real_, tryCatch(model_sum$coefficients[2,2], error = function(e) NA)),
            Growth.N = model$df.residual
            # `Growth.Fit.poisson` = (model$null.deviance - model$deviance) / (model$null.deviance)
          )
          # browser()
        }
        return(result)
      }         

      groupedDf = groupedDf %>% covidStandardGrouping()

      groupedDf = groupedDf %>% group_modify(function(d,g,...) {
        #browser()
        message(".",appendLF = FALSE)
        if (nrow(d) < 2+window) d$errors = paste0(ifelse(is.na(d$errors),"",paste0(d$errors,"; ")),"Not enough data to calculate growth rates")
        if (any(!is.na(d$errors))) {return(d)}
        # if(leftSided) {
        #   d$regression = slider::slide(d %>% mutate(row = row_number()), fitPois, .before=w2*2,.complete = FALSE)
        # } else {
        d$regression = slider::slide(d %>% mutate(row = row_number()), fitPois, .before=w2,.after=w2,.complete = FALSE)
        # }
        # browser()
        d = d %>% tidyr::unnest(cols = regression)
        return(d)
      })
      
      # browser()
      groupedDf = groupedDf %>%
        covidStandardGrouping() %>% 
        dplyr::mutate(
          # estimate second derivative with sgolay filter
          # TODO: this seemed to produce unexpected NA values 
          Growth.derivative = signal::sgolayfilt(Growth.value, p = 1, n = floor(window/2)*2+1, m = 1),
          Growth.SE.derivative = NA_real_
        ) %>% 
        ungroup() %>% 
        mutate(
          Growth.method = "GLM",
          Growth.ProbPos = 1-pnorm(0,mean=Growth.value,sd=Growth.SE.value),
          
          Est.Quantile.0.025.value = qpois(0.025,Est.value),
          Est.Quantile.0.25.value = qpois(0.25,Est.value),
          Est.Quantile.0.5.value = qpois(0.5,Est.value),
          Est.Quantile.0.75.value = qpois(0.75,Est.value),
          Est.Quantile.0.975.value = qpois(0.975,Est.value),
          
          Growth.Quantile.0.025.value = qnorm(0.025, Growth.value, Growth.SE.value),
          Growth.Quantile.0.25.value = qnorm(0.25, Growth.value, Growth.SE.value),
          Growth.Quantile.0.5.value = qnorm(0.5, Growth.value, Growth.SE.value),
          Growth.Quantile.0.75.value = qnorm(0.75, Growth.value, Growth.SE.value),
          Growth.Quantile.0.975.value = qnorm(0.975, Growth.value, Growth.SE.value),
          
          # calculate imputed value as close to actual value as possible
          Imputed = is.na(value) & !is.na(Est.Quantile.0.5.value),
          Imputed.value = ifelse(is.na(value),Est.Quantile.0.5.value,value)
          
        )
        
      return(groupedDf %>% dplyr::select(-I))

    }})
  },
  
  #' @description Calculates a growth rate timeseries using a fitted quasipoisson model and locally fitted polynomials
  #' @param covidTimeseries a covid timeseries data frame
  #' @param window - the length of the smoothing window (default 21)
  #' @param weekendEffect - the downweighting of figures from Saturday, Sunday and Monday to account for weekend delay
  #' @param polynomialDegree - improves fit at risk of overfitting. default 1 (linear)
  #' @return a dataframe with groupwise growth rate estimates
  estimateGrowthRate2 = function(covidTimeseries, window = 14, ...) {
    message("This function is deprecated: switch estimateGrowthRate2() to estimateGrowthRate() or estimatePoissonGrowthRate()")
    self$estimateGrowthRate(covidTimeseries, window, ...)
  },
  
  #' @description Calculates a growth rate timeseries using a fitted quasipoisson model and locally fitted polynomials
  #' @param covidTimeseries a covid timeseries data frame
  #' @param window - the length of the smoothing window (default 21)
  #' @param weekendEffect - the downweighting of figures from Saturday, Sunday and Monday to account for weekend delay
  #' @param polynomialDegree - improves fit at risk of overfitting. default 1 (linear)
  #' @return a dataframe with groupwise growth rate estimates
  estimateGrowthRate = function(covidTimeseries, window = 14, weekendEffect = 0.75, polynomialDegree = 1, nearestNeigbours = FALSE, ... ) {
    self$getHashCached(object = covidTimeseries, operation="ESTIM-LITTLE-R-2", params=list(window, weekendEffect, polynomialDegree), ... , orElse = function (ts, ...) {covidTimeseriesFormat %def% {
      
      groupedDf = covidTimeseriesFormat(covidTimeseries) %>%
        ensurer::ensure_that(any(.$type == "incidence") ~ "dataset must contain incidence figures") %>%
        dplyr::filter(type == "incidence") %>%
        self$completeAndRemoveAnomalies() %>%
        dplyr::mutate(errors=NA_character_, Growth.method = "Local regression", Growth.N = window)
      
      
      groupedDf = groupedDf %>% covidStandardGrouping()
      
      groupedDf = groupedDf %>% group_modify(function(d,g,...) {
        #browser()
        d = d %>% mutate(time = as.numeric(date-max(date)))
        tmp = d %>% mutate(
          weights = case_when(
            weekdays(date) %in% c("Tuesday","Wednesday","Thursday","Friday") ~ (7-3*weekendEffect)/4,
            TRUE ~ weekendEffect)
        )
        
        if(sum(na.omit(tmp$value) != 0) < polynomialDegree) {
          
          d$Est.value = 0
          d$Est.SE.value = NA_real_
          d$Est.error = 0
          d$Est.dispersion = NA_real_
          d$Growth.value = 0
          d$Growth.SE.value = NA_real_
          d$Growth.derivative = 0
          d$Growth.SE.derivative = NA_real_
          
        } else {
          
          tmp_alpha = min(window/nrow(d),1)
          tmp_alpha_2 = min(window*2/nrow(d),1)
          lpParams = list(
            nn = if( nearestNeigbours ) tmp_alpha_2 else tmp_alpha, # this is given in fraction of total observations
            h = if( !nearestNeigbours ) window else 0, # this is given in units of X
            deg = polynomialDegree
          )
          lpParamsText = paste(names(lpParams),lpParams,sep="=",collapse=", ")
          lpFormula = as.formula(paste0(c("value ~ locfit::lp(time, ",lpParamsText,")")))
          
          suppressWarnings({
            #ev = seq(min(d$time)-1,max(d$time)+1,length.out = floor(2*nrow(d)/window+1))
            
            tryCatch({
            tmp_intercept_model = locfit::locfit(lpFormula, tmp, family="qpois", link="log", weights = tmp$weights, ev=d$time)
            tmp_intercept = predict(tmp_intercept_model, band="global", se.fit=TRUE, where="fitp")
            }, error = browser)
            
            # if(any(is.na(tmp_intercept$fit))) 
            #   tmp_intercept = predict(tmp_intercept_model, d$time, band="global", se.fit=TRUE)
            if(any(is.na(tmp_intercept$fit))) 
              browser()
            
            tmp_slope_model = locfit::locfit(lpFormula, tmp, family="qpois", link="log", deriv=1, weights = tmp$weights, ev=d$time)
            tmp_slope = predict(tmp_slope_model, band="global", se.fit=TRUE, where="fitp")
            
            # if(any(is.na(tmp_slope$fit))) 
            #   tmp_slope = predict(tmp_slope_model, d$time, band="global", se.fit=TRUE)
            if(any(is.na(tmp_slope$fit))) 
              browser()
            
            # browser()
            # https://stats.stackexchange.com/questions/62006/definition-of-dispersion-parameter-for-quasipoisson-family
            
            absRes = d$value - tmp_intercept$fit
            d$Est.error = (absRes %>% slider::slide_dbl( .f = mean, .before = floor(window/2), .after = floor(window/2), .complete = FALSE, na.rm=TRUE))/tmp_intercept$fit
            # Pearson residuals assuming poisson
            # pearson = sqrt(absRes^2/tmp_intercept$fit)
            # d$Est.dispersion = pearson %>% slider::slide_dbl( .f = mean, .before = floor(window/2), .after = floor(window/2), .complete = FALSE, na.rm=TRUE)
            # Deviance residuals
            
            # https://stats.stackexchange.com/questions/99065/why-are-pearsons-residuals-from-a-poisson-regression-so-large
            # Deviance residuals
            deviance = d$value * log(d$value/tmp_intercept$fit) + (tmp_intercept$fit-d$value)
            d$Est.dispersion = deviance %>% slider::slide_dbl( .f = mean, .before = floor(window/2), .after = floor(window/2), .complete = FALSE, na.rm=TRUE)
            # possibly this is not accounting for df and should be multiplied by 
            
            
            # https://statweb.stanford.edu/~jtaylo/courses/stats306b/restricted/notebooks/quasilikelihood.pdf
            
            d$Est.value = ifelse(tmp_intercept$fit < 0, 0, tmp_intercept$fit) # prevent negative smoothing.
            d$Est.SE.value = tmp_intercept$se.fit
            
            d$Growth.value = tmp_slope$fit #ifelse(tmp_intercept$fit < 0, 0, tmp_slope$fit) # prevent growth when case numbers are zero.
            d$Growth.SE.value = tmp_slope$se.fit
            
            if(polynomialDegree > 1) {
              # if the locfit model supports second derivative then grab it
              tmp_slope2_model = locfit::locfit(lpFormula, tmp, family="qpois", link="log", deriv=c(1,1), weights = tmp$weights, ev=d$time)
              tmp_slope2 = predict(tmp_slope2_model, band="global", se.fit=TRUE, where="fitp")
              d$Growth.derivative = tmp_slope2$fit #ifelse(tmp_intercept$fit < 0, 0, tmp_slope$fit) # prevent growth when case numbers are zero.
              d$Growth.SE.derivative = tmp_slope2$se.fit
            } else {
              # otherwise estimate directly
              d$Growth.derivative = signal::sgolayfilt(d$Growth.value, p = 1, n = floor(window/2)*2+1, m = 1)
              d$Growth.SE.derivative = NA_real_
            }
            
          })
          
        }
        
        d$Growth.ProbPos = 1-pnorm(0,mean=d$Growth.value,sd=d$Growth.SE.value)
        
        d$Est.Quantile.0.025.value = qpois(0.025,d$Est.value)
        d$Est.Quantile.0.25.value = qpois(0.25,d$Est.value)
        d$Est.Quantile.0.5.value = qpois(0.5,d$Est.value)
        d$Est.Quantile.0.75.value = qpois(0.75,d$Est.value)
        d$Est.Quantile.0.975.value = qpois(0.975,d$Est.value)
        
        d$Growth.Quantile.0.025.value = qnorm(0.025, d$Growth.value, d$Growth.SE.value)
        d$Growth.Quantile.0.25.value = qnorm(0.25, d$Growth.value, d$Growth.SE.value)
        d$Growth.Quantile.0.5.value = qnorm(0.5, d$Growth.value, d$Growth.SE.value)
        d$Growth.Quantile.0.75.value = qnorm(0.75, d$Growth.value, d$Growth.SE.value)
        d$Growth.Quantile.0.975.value = qnorm(0.975, d$Growth.value, d$Growth.SE.value)
        
        # calculate imputed value column as close to actual value as possible
        d$Imputed = is.na(d$value) & !is.na(d$Est.Quantile.0.5.value)
        d$Imputed.value = ifelse(is.na(d$value),d$Est.Quantile.0.5.value,d$value)
        
        #browser()
        return(d)
      
    })
    
      return(groupedDf %>% ungroup())
    }})
  },
  
  #' @description Calculates a growth rate timeseries using a fitted quasipoisson model and locally fitted polynomials
  #' @param covidTimeseries a covid timeseries data frame
  #' @param facets 
  #' @param window - the length of the smoothing window (default 21)
  #' @param weekendEffect - the downweighting of figures from Saturday, Sunday and Monday to account for weekend delay
  #' @param polynomialDegree - improves fit at risk of overfitting. default 1 (linear)
  #' @return a dataframe with groupwise growth rate estimates
  estimateBaselineGrowthRate = function(covidTimeseries, facets = vars(name,code,codeType), window = 14, weekendEffect = 0.75, polynomialDegree = 1, nearestNeigbours = FALSE, ... ) {
    tmp = covidTimeseries
    # browser()
    for(facet in facets) {
       tmp = tmp %>% mutate(!!facet := "all")
    }
    
    if ("population" %in% colnames(covidTimeseries)) {
      tmp = tmp %>% covidStandardDateGrouping() %>% summarise(value = sum(value), Baseline.population = sum(population))
    } else {
      tmp = tmp %>% covidStandardDateGrouping() %>% summarise(value = sum(value))
    }
    tmp = tmp %>% self$estimateGrowthRate(window = window,weekendEffect = weekendEffect, polynomialDegree = polynomialDegree, nearestNeigbours = nearestNeigbours, ...)
    tmp = tmp %>% rename_with(.cols = starts_with("Growth"), .fn = ~ paste0("Baseline.",.x))
    tmp = tmp %>% rename_with(.cols = starts_with("Est"), .fn = ~ paste0("Baseline.",.x))
    tmp = tmp %>% covidStandardSelect(starts_with("Baseline"))
    tmp = tmp %>% rename(Baseline.value = value)
    for(facet in facets) {
      tmp = tmp %>% select(-!!facet)
    }
    tmp = covidTimeseries %>% inner_join(tmp, by=covidStandardJoins(sapply(facets,as_label)))
    return(tmp)
  },
  
  
  
  ## growth rate advantage ----
  # TODO convert to use standard format instead of all estimates...
  # doAdvantage = function(
  #   allEstimates,
  #   gtAssumptions,
  #   names = unname(unlist(nationalClusters)),
  #   sources="infection episodes",
  #   datasets="Symptomatic",
  #   methods = unique(allEstimates$method),
  #   gtSource = unique(gtAssumptions$source),
  #   ...
  # ) {
  #   
  #   # browser()
  #   
  #   tsp$getSaved(id = "ADVANTAGE",params = list(allEstimates,gtAssumptions,names,sources,datasets,methods,gtSource), ..., function(...) {
  #     
  #     gtAssumptions = gtAssumptions %>% filter(source %in% gtSource)
  #     
  #     filteredTS = allEstimates %>% 
  #       filter(source %in% sources) %>%
  #       filter(dataset %in% datasets) %>%
  #       filter(method %in% methods) %>% 
  #       filter(name %in% names) %>% 
  #       mutate(name = name %>% ordered(names)) %>%
  #       filter(!is.na(Growth) & !is.na(Growth.SE))
  #     
  #     # filteredTS = filteredTS %>% group_by(source,date,subgroup,dataset,method,name)
  #     # browser()
  #     
  #     discGRToR = function(r, d) {
  #       
  #       d$p = purrr::map2(d$a,d$y,function(a,y) y/(a - lag(a,default=0)))
  #       
  #       R = pmap(list(r,d$a,d$y,d$p), function(r,a,y,p) {
  #         # tmp = r/sum(y*(exp(-r*lag(a,default=0))-exp(-r*a))/(a - lag(a,default=0)))
  #         tmp = r/sum(p*(exp(-r*lag(a,default=0))-exp(-r*a)))
  #         # if(!is.finite(tmp)) browser()
  #         return(tmp)
  #       })
  #       # browser()
  #       return(unlist(R))
  #     }
  #     
  #     grpComparison = gtAssumptions %>% rename(gtSource = source) %>% group_by(subgroup, gtSource) %>% group_modify(function(d,g,...) {
  #       
  #       subgroupfilteredTS = filteredTS %>% filter(subgroup==g$subgroup) %>% select(-subgroup) %>% mutate(
  #         # this is the num / denominator of the fractions above. 
  #         # and is probably an R estimate
  #         # keep this as a list column for now
  #         bootstrap = map2( Growth, Growth.SE, function(mean,sd) {
  #           tibble(
  #             id = 1:nrow(d),
  #             # alpha = gtAssumptions$alpha,
  #             # beta = gtAssumptions$beta,
  #             sample_r = rnorm(nrow(d),mean,sd), 
  #             R = ( 1 + sample_r/d$beta ) ^ d$alpha,
  #             R_disc = discGRToR(sample_r, d)
  #           )
  #         })
  #         # ) %>% mutate(
  #         #   R.mean = purrr::map_dbl(bootstrap, function(.x) mean(.x$R)),
  #         #   R.sd = purrr::map_dbl(bootstrap, function(.x) sd(.x$R))
  #       )
  #       
  #       return(subgroupfilteredTS)
  #     })  
  #     
  #     # browser()
  #     
  #     disadv = grpComparison %>% filter(subgroup == "negative")
  #     adv = grpComparison %>% filter(subgroup == "positive")
  #     
  #     compare = adv %>% inner_join(disadv, by=c("gtSource","source","date","dataset","method","name"), suffix=c(".adv",".disadv"))
  #     
  #     # calculate the ratio for each bootstrap.
  #     compare2 = compare %>% mutate(bootstrap = purrr::map2(bootstrap.adv, bootstrap.disadv, function(adv,disadv) {
  #       adv %>% inner_join(disadv, by="id", suffix=c(".adv",".disadv")) %>% mutate(
  #         transadv_cont = R.adv/R.disadv,
  #         transadv = R_disc.adv/R_disc.disadv
  #       )
  #     })) %>% select(-bootstrap.adv, -bootstrap.disadv)
  #     
  #     return(compare2)
  #   })
  # }
  # 
  # summariseAdvantage = function(compare, combineDays=FALSE,mergeModels = FALSE, combineGt = FALSE, completeDominance=NULL, zeroSpos=NULL) {
  #   # Options are:
  #   
  #   if(combineDays) {
  #     compare2 = compare %>% 
  #       mutate(name = as.character(name)) %>% 
  #       inner_join(completeDominance %>% select(name=area,date), by="name",suffix=c("",".after")) %>% 
  #       inner_join(zeroSpos %>% select(name=area,date), by="name",suffix=c("",".before")) %>% 
  #       filter(date > date.after & date < date.before)
  #   } else {
  #     compare2 = compare %>%
  #       mutate(
  #         date.after = min(date)-1,
  #         date.before = max(date)+1
  #       )
  #   }
  #   
  #   compare2 = compare2 %>% unnest(bootstrap)
  #   
  #   # Summarise bootstraps for each day
  #   compare2 = compare2 %>% group_by(source,dataset,name)
  #   # merge methods
  #   if (!combineGt) compare2 = compare2 %>% group_by(gtSource, .add=TRUE)
  #   if (!mergeModels) compare2 = compare2 %>% group_by(method, .add=TRUE)
  #   # Summarise bootstraps for all days
  #   if (!combineDays) {
  #     compare2 = compare2 %>% group_by(date, .add=TRUE)
  #   } else {
  #     compare2 = compare2 %>% group_by(date.after,date.before, .add=TRUE)
  #   }
  #   
  #   # browser()
  #   # grps == compare2 %>% groups()
  #   # compare2 = compare2 %>% summarise(
  #   #     bootstrap = bind_rows(bootstrap),
  #   #     bootstrap.adv = bind_rows(bootstrap.adv),
  #   #     bootstrap.disadv = bind_rows(bootstrap.disadv),
  #   # )
  #   
  #   out = compare2 %>% summarise(
  #     transadv.mean = mean(transadv),
  #     transadv.sd = sd(transadv),
  #     
  #     transadv.Quantile.0.025 = quantile(transadv,0.025),
  #     transadv.Quantile.0.25 = quantile(transadv,0.25),
  #     transadv.Quantile.0.5 = quantile(transadv,0.5),
  #     transadv.Quantile.0.75 = quantile(transadv,0.75),
  #     transadv.Quantile.0.975 = quantile(transadv,0.975),
  #     
  #     R.adv.mean = mean(R_disc.adv),
  #     R.adv.sd = sd(R_disc.adv),
  #     R.disadv.mean = mean(R_disc.disadv),
  #     R.disadv.sd = sd(R_disc.disadv)
  #   )
  #   
  #   return(out)
  #   
  # }
  
  ## reproduction number ----
  
  estimateRtFromGrowthRate = function(
    covidTimeseries, 
    bootstraps=1000,
    growthVar = "Growth.value", 
    growthSEVar = "Growth.SE.value", 
    serialIntervals = self$serial,
    joinBy = character(),
    rawEstimates = FALSE,
    quantiles = c(0.025,0.25,0.5,0.75,0.975),
    ...) {
    
    growthVar = ensym(growthVar)
    growthSEVar =  ensym(growthSEVar)
    
    df = covidTimeseries %>% dplyr::filter(type=="incidence") 
    si = serialIntervals$getInfectivityProfile()
    
    if (!as_label(growthVar) %in% colnames(df)) {
      df = df %>% self$estimateGrowthRate(...)
    }
    
    collision = intersect(setdiff(colnames(df),names(joinBy)),setdiff(colnames(si),joinBy))
    if(length(collision) > 0) {
      stop("Naming collision detected. Did you mis-specify joinBy? Affected columns were: ",collision)
    }
    if(length(joinBy) > 0) {
      message("Differentially applying serial interval by facets: ",joinBy)
    }
    
    self$getHashCached(object = df, operation="ESTIM-RT-FROM-GR", params=list(bootstraps, si, growthVar, growthSEVar, joinBy, rawEstimates, quantiles), ... , orElse = function (ts, ...) {
      
      # TODO: detect name collisions
      si %>% group_modify(function(d1,g1,...) {
        
        # Iterate over the serial interval distributions types in the provider
        
        # d1 = si %>% ungroup() %>% filter(row_number()==1) %>% select(y,a)
        # g1 = si %>% ungroup() %>% filter(row_number()==1) %>% select(-y,-a)
        message("Serial interval: ", g1 %>% unite(m, everything(), sep=", ", na.rm=TRUE) %>% pull(m))
        
        # grab the y matrix from the list column
        y_cols = d1$y[[1]]
        a = d1$a[[1]]
        # figure out how many bootstraps we need:
        bootsPerInf = max(c(bootstraps %/% dim(y)[2],1))
        # lose the zero values in y and a, if present (which they will be):
        if (a[1]==0) {
          y_cols = y_cols[-1,]
          a = a[-1]
        }
        # get the infectivity profiles as a list of vectors, each bootstrap profile will be a vector.
        ys = asplit(y_cols, MARGIN=2)
        
        # filter df items that match something in the serial interval provider. 
        # the default cross join setting joinBy=character() should let everything through (tested - good to know this works)
        tmpDf = df %>% semi_join(g1, joinBy)
        # If nothing matches use unfiltered
        # if (nrow(tmpDf) == 0) tmpDf = df
        
        tmpDf %>% covidStandardGrouping() %>% group_modify(function(d2,g2,...) {
          
          message("Timeseries: ", g2 %>% unite(m, everything(), sep=", ", na.rm=TRUE) %>% pull(m))
          
          # Iterate over the different timeseries in the filtered covid timeseries
          #g2 = tmpDf %>% covidStandardGrouping() %>% group_data() %>% filter(row_number()==1) %>% select(-.rows)
          #d2 = tmpDf %>% covidStandardGrouping() %>% semi_join(g2) %>% ungroup() %>% select(-covidStandardJoins())
          
          d3 = d2 %>% mutate(R = map2(!!growthVar, !!growthSEVar, function(mean_r,sd_r) {
            
            r_samples = rnorm(bootsPerInf*length(ys),mean_r,sd_r)
            rs = asplit(matrix(r_samples,nrow=length(ys)), MARGIN=1)
            # browser()
            out = map2(rs,ys,function(r10,y) {
              # browser()
              R10 = sapply(r10, function(r) {
                # browser()
                R = r/sum(y*(exp(-r*lag(a,default=0))-exp(-r*a))/(a - lag(a,default=0)))
              })
            })
            R_out = as.vector(sapply(out,c))
            if(rawEstimates) return(R_out)
            R_q = quantile(R_out, quantiles)
            names(R_q) = paste0("Rt.Quantile.",quantiles)
            R_summ = enframe(R_q) %>% pivot_wider() %>% mutate(Rt.value = mean(R_out), Rt.SE.value = sd(R_out))
            return(R_summ)
          }))
          
          if(rawEstimates) return(d3)
          return(d3 %>% unnest(R))
          
        }) %>% return()
        # This is the part of the timeseries which is relevant to the given group of serial intervals
        
      }) %>% return()
      #This is combined now with serial interval metadata. The details of the serial interval can be got from the serial interval provider.
      
    }) %>% return()
  },
  
  estimateRtJepidemic = function(
    covidTimeseries, 
    window = 7,
    valueVar = "Imputed.value", 
    serialIntervals = self$serial,
    joinBy = intersect(colnames(df),colnames(si)),
    valueIsPoissonRate = (valueVar %in% c("RollMean.value","Est.value")),
    priorR0 = 1, priorR0Sd=2, adaptivePrior = 1.25, minIncidence = 100, bootstraps=1000, minWindow = 7, maxWindow = max(c(window,21)),
    # quantiles = c(0.025,0.25,0.5,0.75,0.975), jepidemic does not support custon quantiles although this is possible in the collector.
    ...) {
    
    df = covidTimeseries %>% dplyr::filter(type=="incidence") 
    
    # Fill in missing values.
    if(!valueVar %in% colnames(covidTimeseries)) {
      if (valueVar %in% c("RollMean.value","Imputed.value")) {
        df = df %>% self$imputeAndWeeklyAverage(...)
      } else if (valueVar == "Est.value") {
        df = df %>% self$estimateGrowthRate(window=14,...)
      }
    } 
    
    df = df %>% covidStandardGrouping()
    
    si = serialIntervals$getInfectivityProfile()
    # valueVar = ensym(valueVar)
    
    collision = intersect(setdiff(colnames(df),names(joinBy)),setdiff(colnames(si),joinBy))
    if(length(collision) > 0) {
      stop("Naming collision detected. Did you mis-specify joinBy? Affected columns were: ",collision)
    }
    if(length(joinBy) > 0) {
      message("Differentially applying serial interval by facets: ",joinBy)
    }
    
    self$getHashCached(object = df, operation="ESTIM-RT-JEPI", params=list(bootstraps, si, valueVar, joinBy, maxWindow, valueIsPoissonRate, priorR0, priorR0Sd, adaptivePrior, minIncidence, window), ... , orElse = function (ts, ...) {
      
      if(minIncidence > 0) {
        estim2 = J$CoriEstimator$new(r0Mean = priorR0,r0SD = priorR0Sd,maxWindow = maxWindow)
        estim2$selectAdaptiveWindow(incidenceSum = 100,minWindow = minWindow)
      } else {
        estim2 = J$CoriEstimator$new(r0Mean = priorR0,r0SD = priorR0Sd,maxWindow = window)
        estim2$selectSpecificWindow(window = window)
      }
      if (adaptivePrior < 1) {
        estim2$withDefaultPrior()
      } else {
        estim2$withAdaptivePrior(factor = adaptivePrior)
      }
      
      estim2$collectMixtureQuantiles()
      estim2$inMiddleOfTimeseries()
      
      si %>% group_modify(function(d1,g1,...) {
        
        # Iterate over the serial interval distributions types in the provider
        message("Serial interval: ", g1 %>% unite(m, everything(), sep=", ", na.rm=TRUE) %>% pull(m))
        
        # grab the y matrix from the list column
        y_cols = d1$y[[1]]
        a = d1$a[[1]]
        if (any(a-lag(a,default=-1) != 1)) stop("Cori estimator needs daily spaced infectivity profiles but these are irregular.")
        estim2$withInfectivityProfileMatrix(y_cols)
        bootsPerInf = max(c(bootstraps %/% dim(y_cols)[2],1))
        
        # filter df items that match something in the serial interval provider. 
        # the default cross join setting joinBy=character() should let everything through (tested - good to know this works)
        tmpDf = df %>% semi_join(g1, joinBy)

        if (valueIsPoissonRate) {
          d3 = estim2$estimateRtFromRates(tmpDf,dateColName = "date", rateColName = valueVar, samplesPerProfile = bootsPerInf)
        } else {
          d3 = estim2$estimateRt(tmpDf,dateColName = "date", incidenceColName = valueVar)    
        }
        
        return(d3)
        
      }) -> allRt
      #This is combined now with serial interval metadata. The details of the serial interval can be got from the serial interval provider.
      
      rm(estim2)
      return(allRt)
      
    }) %>% return()
  },
  
  #' @description Calculates a survival R(t) curve on grouped data
  #' @param covidTimeseries a covid timeseries data frame
  #' @param valueVar - the column to calculate an R(t) for - usually "RollMean.value"
  #' @param config An object of class estimate_R_config, as returned by function EpiEstim::make_config.
  #' @param window - the width of the smoothing function applied (default 2)
  #' @return a dataframe with groupwise Rt estimates
  estimateRtQuick = function(covidTimeseries, valueVar = "RollMean.value", window = 7,...) {
    valueVar = ensym(valueVar)
    self$estimateRt(covidTimeseries, valueVar = !!valueVar, window = window, quick=TRUE,...) %>%
      dplyr::select(-`Quantile.0.025(R)`,-`Quantile.0.05(R)`,-`Quantile.0.25(R)`,-`Quantile.0.75(R)`,-`Quantile.0.95(R)`,-`Quantile.0.975(R)`)
  },
  
  #' @description Calculates a survival R(t) curve on grouped data
  #' @param covidTimeseries a covid timeseries data frame
  #' @param valueVar - the column to calculate an R(t) for - usually "RollMean.value"
  #' @param config An object of class estimate_R_config, as returned by function EpiEstim::make_config.
  #' @param dateVar - the variable containing the seqence of dates
  #' @param incidenceVar - the sequence of daily incidence
  #' @param window - the width of the smoothing function applied (default 7)
  #' @return a dataframe with groupwise Rt estimates
  estimateRt = function(covidTimeseries, valueVar = "RollMean.value", window = 7, priorR0=1, priorR0Sd=2, quick=FALSE, serialIntervalProvider = self$serial, ...) {
    valueVar = ensym(valueVar)
    tmpConfig = serialIntervalProvider$getBasicConfig(quick=quick, priorR0=priorR0, priorR0Sd=priorR0Sd)
    
    self$getHashCached(object = covidTimeseries, operation="ESTIM-RT", params=list(as_label(valueVar), window, quick, priorR0, priorR0Sd, tmpConfig), ... , orElse = function (ts, ...) {covidTimeseriesFormat %def% {
      
      if(!(as_label(valueVar) %in% colnames(covidTimeseries))) {
        if (as_label(valueVar) %in% c("RollMean.value","Imputed.value")) {
          covidTimeseries = covidTimeseries %>% self$imputeAndWeeklyAverage()
        } else if (as_label(valueVar) == "Est.value") {
          covidTimeseries = covidTimeseries %>% self$estimateGrowthRate()
        } else {
          stop("can't estimate Rt, ",as_label(valueVar)," is not defined")
        }
      }
      
      groupedDf = covidTimeseriesFormat(covidTimeseries) %>%
        ensurer::ensure_that(any(.$type == "incidence") ~ "dataset must contain incidence figures") %>%
        dplyr::filter(type == "incidence") %>%
        dplyr::mutate(I = !!valueVar) %>%
        self$describeErrors(I) 
      
      groupedDf = groupedDf %>% covidStandardGrouping()
        
      # tmp starts on first non zero value of I in group
      tmp2 = groupedDf %>% group_modify(function(d,g,...) {
        
        message(".",appendLF = FALSE)
        config = serialIntervalProvider$getBasicConfig(quick=quick, priorR0=priorR0, priorR0Sd=priorR0Sd) #, statistic = g$statistic)
        siConfig = config$cfg
        method = config$method
        siSample = config$si_sample
        
        tmp = d %>% dplyr::select(dates=date,I)
        if (nrow(d) < 2+window) errors = paste0(ifelse(is.na(errors),"",paste0(errors,"; ")),"Not enough data to calculate R(t)")
        if (any(!is.na(d$errors))) {return(d)}

        d = d %>% dplyr::mutate(seq_id=row_number())

        siConfig$t_start = c(2:(nrow(tmp)-window))
        siConfig$t_end = siConfig$t_start+window
        warn = NA

        #TODO: https://cran.rstudio.com/web/packages/tibbletime/vignettes/TT-03-rollify-for-rolling-analysis.html
        tmp4 = #suppressWarnings(EpiEstim::estimate_R(d,method = method,config=siConfig,...))
          withCallingHandlers(
            tryCatch(EpiEstim::estimate_R(tmp, method = method,config=siConfig,si_sample = siSample), error = stop), warning= function(w) {
            warn <<- w$message
            invokeRestart("muffleWarning")
          })
        tmp5 = tmp4$R %>% mutate(seq_id=t_end, errors=NA, `Window.R`=window) #warn)
        tmp6 = d %>% dplyr::select(-errors) %>% dplyr::left_join(tmp5, by="seq_id")
        # browser()
        return(tmp6 %>% select(-seq_id))
      })
      if(all(!is.na(tmp2$errors))) warning("Rt estimation failed - see errors column")
      if(any(!is.na(tmp2$errors))) warning("Rt estimation completed with errors")
      return(tmp2 %>% mutate(Anomaly.R=Anomaly) %>% select(-I)) 
      
      #TODO: 
      # R_to_growth(2.18, 4, 1)  
      # R_to_growth <- function(R, gamma_mean, gamma_sd) {
      #   k <- (gamma_sd / gamma_mean)^2
      #   
      #   r <- (R^k - 1) / (k * gamma_mean)
      #   
      #   return(r)
      # } 
      
    }})
  },
  
  ## Correct outputs for delays ---
  
  defaultOffsetAssumptions = function() {
    return(
      tribble(
        ~to, ~statistic,
        "admission", "hospital admission",
        "admission", "icu admission",
        "onset", "symptom",
        "onset", "triage",
        "test", "case",
        "death", "death",
        NA, "serology",
        NA, "test",
        NA, "information seeking"
      ) %>% left_join(ukcovidtools::ukCovidObservationDelays,by="to") %>% select(statistic,mean) %>% tibble::deframe()
    )
    # return(list(
    #   "case"=8.22,
    #   "death"=18.14,
    #   "hospital admission"=14.6,
    #   "icu admission"=15.6,
    #   "symptom"=5.45,
    #   "triage"=5.45,
    #   "serology"=NA,
    #   "test"=NA,
    #   "information seeking"=NA
    # )),
    
  },

  defaultCorrectionFactor = function() {
    return(list(
      "case"=0.75,
      "death"=0.65,
      "hospital admission"=0.67,
      "icu admission"=0.67,
      "symptom"=0.86,
      "triage"=0.86,
      "serology"=NA,
      "test"=NA,
      "information seeking"=NA
    ))
  },
  
  defaultR0Assumptions = function() {
    return(tibble::tribble(
      ~startDate, ~Mean.Prior.R0, ~SE.Prior.R0, ~Adj.Mean.SerialInterval, ~Adj.SD.SerialInterval,
      as.Date("2020-01-01"), 2.5, 2, 1.0, 1.0,
      as.Date("2020-03-23"), 1.0, 2, 1.0, 1.0
      #as.Date("2020-07-04"), 1.2, 0.1, 1.0, 1.5,
    ))
  },
  
  adjustRtConfidence = function(covidRtResult, sdMultiplier, predicate=NULL) {
    predicate = enexpr(predicate)
    doQuant = function(meanVec,sdVec,p) {
      ifelse(sdVec>meanVec,NA, qgamma(shape = meanVec^2/sdVec^2, rate = meanVec/sdVec^2,p=p))
    }
    if(!identical(predicate,NULL)) {
      covidRtResult %>% mutate(
        `Quantile.0.025(R)` = ifelse(!!predicate, doQuant(`Mean(R)`,`Std(R)` * sdMultiplier,0.025),`Quantile.0.025(R)`),
        `Quantile.0.05(R)` = ifelse(!!predicate, doQuant(`Mean(R)`,`Std(R)` * sdMultiplier,0.05),`Quantile.0.05(R)`),
        `Quantile.0.25(R)` = ifelse(!!predicate, doQuant(`Mean(R)`,`Std(R)` * sdMultiplier,0.25),`Quantile.0.25(R)`),
        `Median(R)` = ifelse(!!predicate, doQuant(`Mean(R)`,`Std(R)` * sdMultiplier,0.5),`Median(R)`),
        `Quantile.0.75(R)` = ifelse(!!predicate, doQuant(`Mean(R)`,`Std(R)` * sdMultiplier,0.75),`Quantile.0.75(R)`),
        `Quantile.0.95(R)` = ifelse(!!predicate, doQuant(`Mean(R)`,`Std(R)` * sdMultiplier,0.95),`Quantile.0.95(R)`),
        `Quantile.0.975(R)` = ifelse(!!predicate, doQuant(`Mean(R)`,`Std(R)` * sdMultiplier,0.975),`Quantile.0.975(R)`),
        `Std(R)` = ifelse(!!predicate, `Std(R)` * sdMultiplier,`Std(R)`)
      )
    } else {
      covidRtResult %>% mutate(
        `Quantile.0.025(R)` = doQuant(`Mean(R)`,`Std(R)` * sdMultiplier,0.025),
        `Quantile.0.05(R)` = doQuant(`Mean(R)`,`Std(R)` * sdMultiplier,0.05),
        `Quantile.0.25(R)` = doQuant(`Mean(R)`,`Std(R)` * sdMultiplier,0.25),
        `Median(R)` = doQuant(`Mean(R)`,`Std(R)` * sdMultiplier,0.5),
        `Quantile.0.75(R)` = doQuant(`Mean(R)`,`Std(R)` * sdMultiplier,0.75),
        `Quantile.0.95(R)` = doQuant(`Mean(R)`,`Std(R)` * sdMultiplier,0.95),
        `Quantile.0.975(R)` = doQuant(`Mean(R)`,`Std(R)` * sdMultiplier,0.975),
        `Std(R)` = `Std(R)` * sdMultiplier
      )
    }
  },
  
  adjustRtDates = function(covidRtResult, window=0, offsetAssumptions = self$defaultOffsetAssumptions(), extraCols=NULL) {
    
    # TODO: integrate Jepidemic output 
    
    adjustable = c(
      "Mean(R)","Std(R)","Quantile.0.025(R)","Quantile.0.05(R)","Quantile.0.25(R)","Median(R)","Quantile.0.75(R)","Quantile.0.95(R)","Quantile.0.975(R)",
      "Window.R","Anomaly.R","Est.Mean.SerialInterval","SE.Mean.SerialInterval","Est.SD.SerialInterval","SE.SD.SerialInterval","Prior.Mean.R0","Prior.SD.R0",
      extraCols
    )
    
    tmp7 = covidRtResult %>% group_by(statistic) %>% group_modify(function(d,g,...) {
      #browser()
      offset = round(window/2+offsetAssumptions[[g$statistic]])
      tmp = d %>% covidStandardGrouping(statistic) %>% group_modify(function(d2,g2,...) {
      
        d2 = tibble(date = as.Date((min(d2$date)-offset):(min(d2$date)-1),"1970-01-01")) %>% bind_rows(d2)
        cols = colnames(d2)[colnames(d2) %in% adjustable]
        for (col in cols) {
          col = as.symbol(col)
          if(offset < 0) {
            d2 = d2 %>% mutate(!!col := lag(!!col,n = -offset))
          } else {
            d2 = d2 %>% mutate(!!col := lead(!!col,n = offset))
          }
        }
        return(d2)
        
      })
      return(tmp)
      
    })
    
    return(tmp7 %>% ungroup())
  },
  
  adjustGrowthRateDates = function(covidRtResult, window=0, offsetAssumptions = self$defaultOffsetAssumptions(), extraCols=NULL) {
    
    #TODO: check this is up to date.
    
    adjustable = c(
      "Growth.value","Growth.SE.value","Growth.ProbPos",
      "Growth.windowed.value","Growth.windowed.SE.value","Growth.windowed.ProbPos.value",
      "Growth.Quantile.0.025.value","Growth.Quantile.0.25.value","Growth.Quantile.0.5.value","Growth.Quantile.0.75.value","Growth.Quantile.0.975.value",
      "doublingTime.Quantile.0.025","doublingTime.Quantile.0.25","doublingTime","doublingTime.Quantile.0.75","doublingTime.Quantile.0.975",
      "doublingTime.windowed.Quantile.0.025","doublingTime.windowed.Quantile.0.25","doublingTime.windowed","doublingTime.windowed.Quantile.0.75","doublingTime.windowed.Quantile.0.975",
      extraCols
    )
    
    tmp7 = covidRtResult %>% group_by(statistic) %>% group_modify(function(d,g,...) {
      #browser()
      offset = round(window/2+offsetAssumptions[[g$statistic]])
      tmp = d %>% covidStandardGrouping(statistic) %>% group_modify(function(d2,g2,...) {
        
        d2 = tibble(date = as.Date((min(d2$date)-offset):(min(d2$date)-1),"1970-01-01")) %>% bind_rows(d2)
        cols = colnames(d2)[colnames(d2) %in% adjustable]
        for (col in cols) {
          col = as.symbol(col)
          if(offset < 0) {
            d2 = d2 %>% mutate(!!col := lag(!!col,n = -offset))
          } else {
            d2 = d2 %>% mutate(!!col := lead(!!col,n = offset))
          }
        }
        return(d2)
        
      })
      return(tmp)
      
    })
    
    return(tmp7 %>% ungroup())
  },
  
  # Deprecated - to remove
  adjustRtCorrFac = function(covidRtResult, window=7, correctionFactor = self$defaultCorrectionFactor(), extraCols=NULL) {
    
    adjustable = c(
      "Mean(R)","Std(R)","Quantile.0.025(R)","Quantile.0.05(R)","Quantile.0.25(R)","Median(R)","Quantile.0.75(R)","Quantile.0.95(R)","Quantile.0.975(R)",
      extraCols
    )
    
    tmp7 = covidRtResult %>% group_by(statistic) %>% group_modify(function(d,g,...) {
      #browser()
      corrFac = correctionFactor[[g$statistic]]
      cols = colnames(d)[colnames(d) %in% adjustable]
      for (col in cols) {
        col = as.symbol(col)
        d = d %>% mutate(!!col := !!col*corrFac)
      }
      return(d %>% mutate(`correctionFactor(R)` = corrFac))
    })
    
    return(tmp7)
  },
  
  estimateRtWithAssumptions = function(covidTimeseries, 
              valueVar = "RollMean.value", 
              window = 7,
              period = 28,
              quick=FALSE,
              r0Assumptions = self$defaultR0Assumptions(),
              serialIntervalProvider = self$serial,
              dateRange = as.Date(c(min(covidTimeseries$date),max(covidTimeseries$date)),"1970-01-01"), 
              ...) {
    
    valueVar = ensym(valueVar)
    tmpConfig = serialIntervalProvider$getBasicConfig(quick=quick, priorR0=NA, priorR0Sd=NA)
    
    self$getHashCached(object = covidTimeseries, operation="ESTIM-RT-ASSUM", params=list(as_label(valueVar), window, period, r0Assumptions, dateRange, quick, tmpConfig), ... , orElse = function (ts, ...) {
      
      if(!(as_label(valueVar) %in% colnames(covidTimeseries))) {
        if (as_label(valueVar) == "RollMean.value") {
          covidTimeseries = covidTimeseries %>% self$imputeAndWeeklyAverage()
        } else if (as_label(valueVar) == "Est.value") {
          covidTimeseries = covidTimeseries %>% self$logIncidenceStats()
        } else {
          stop("can't estimate Rt, ",as_label(valueVar)," is not defined")
        }
      }
      
      groupedDf = covidTimeseriesFormat(covidTimeseries) %>%
        ensurer::ensure_that(any(.$type == "incidence") ~ "dataset must contain incidence figures") %>%
        dplyr::filter(type == "incidence") %>%
        dplyr::mutate(
          I = !!valueVar
        ) %>%
        self$describeErrors(I) 
      
      r0Assumptions = r0Assumptions  %>% 
        dplyr::arrange(startDate) %>% 
        dplyr::mutate(
          endDate = lead(startDate, default = as.Date("2030-01-01"))-1
        ) %>% 
        dplyr::mutate(
          endDate = as.Date(ifelse(endDate > dateRange[[2]], dateRange[[2]], endDate),"1970-01-01"),
          startDate = as.Date(ifelse(startDate < dateRange[[1]], dateRange[[1]], startDate),"1970-01-01")
        ) %>% 
        dplyr::filter(
          endDate > dateRange[[1]],
          startDate < dateRange[[2]]
        ) 
      
      #browser()
      configs = serialIntervalProvider$getCustomConfigs(quick=quick,priorR0=NA,priorR0Sd=NA,period=period)
      basicConfig = self$serial$getBasicConfig(quick=quick, priorR0=NA, priorR0Sd=NA)
      #TODO: calculate period based on SD of distributions?
      #browser()
      #r0Assumptions2 = r0Assumptions %>% left_join(configs,by=character())
      
      
      
      # r0Assumptions2 = r0Assumptions2 %>% group_by(across(-config)) %>% group_modify(function(d,g,..) {
      #   tmp = d$config[[1]]
      #   tmp$cfg$mean_prior = g$Mean.Prior.R0
      #   tmp$cfg$std_prior = g$SE.Prior.R0
      #   return(tibble(config = list(tmp)))  
      # })
      
      groupedDf2 = groupedDf %>% covidStandardGrouping() %>% inner_join(r0Assumptions, by=character()) %>% filter(date >= startDate & date <= endDate)
      groupedDf2 %>% covidStandardGrouping()
      allTs = groupedDf2 %>% group_modify(function(d,g,...) {
          
        tmpConfig = NULL
        
        if ("statistic" %in% names(configs)) {  
          tmp = configs %>% filter(statistic==g$statistic)
          if(nrow(tmp) == 1) tmpConfig = tmp$config[[1]]
          if(nrow(tmp) > 1) d = d %>% mutate(errors = paste0("Multiple configurations for ",g$statistic))
        }
        
        if(is.null(tmpConfig)) {
          d = d %>% mutate(warnings = paste0("No config for ",g$statistic," using default basic config"))
          tmpConfig = basicConfig
        }   
        
        siConfig = tmpConfig$cfg
        method = tmpConfig$method
        si_sample = tmpConfig$si_sample
          
          
        d = d %>% dplyr::arrange(date) %>% dplyr::mutate(seq_id=row_number())
        data = d %>% dplyr::select(dates=date,I,seq_id)
        if (nrow(d) < 2+window) d$errors = paste0(ifelse(is.na(d$errors),"",paste0(d$errors,"; ")),"Not enough data to calculate R(t)")
        if (any(!is.na(d$errors))) {return(d)}
        
        groupedTs = d %>% group_by(startDate, endDate, Mean.Prior.R0, SE.Prior.R0) %>% group_modify(function(d2,g2,...) {
          
          tmpStartDate = g2$startDate
          tmpEndDate = g2$endDate
          if(tmpStartDate < min(d$date)+1+window) tmpStartDate = min(d$date)+1+window
          if(tmpEndDate > max(d$date)) tmpEndDate = max(d$date)
          
          t_end = d2 %>% pull(seq_id)
          t_start = t_end-window
          t_end = t_end[t_start >= 2]
          t_start = t_start[t_start >= 2]
          
          if (length(t_end) == 0) {
            return(tibble())
            
          } else {
            siConfig$t_end = t_end
            siConfig$t_start = t_start
            siConfig$mean_prior = g2$Mean.Prior.R0
            siConfig$std_prior = g2$SE.Prior.R0
            
            warn = NA
            
            #TODO: https://cran.rstudio.com/web/packages/tibbletime/vignettes/TT-03-rollify-for-rolling-analysis.html
            tmp4 = #suppressWarnings(EpiEstim::estimate_R(d,method = method,config=siConfig,...))
              withCallingHandlers(
                tryCatch(EpiEstim::estimate_R(data, method=method, config=siConfig, si_sample = si_sample), error = stop), warning= function(w) {
                  warn <<- w$message
                  invokeRestart("muffleWarning")
                })
            tmp5 = tmp4$R %>% mutate(
              seq_id=t_end, 
              Window.R=window,
              Est.Mean.SerialInterval = siConfig$mean_si,
              SE.Mean.SerialInterval = siConfig$std_mean_si,
              Est.SD.SerialInterval = siConfig$std_si, 
              SE.SD.SerialInterval = siConfig$std_std_si,
              warnings = warn
            )
            tmp6 = d2 %>% dplyr::inner_join(tmp5, by="seq_id") %>% select(-seq_id)
            return(tmp6)
          
          }
          
          
        }) 
        
        return(groupedTs %>% ungroup() %>% select(-startDate,-endDate))
        
      })
      
      if(quick) allTs = allTs %>% select(-`Quantile.0.025(R)`,-`Quantile.0.05(R)`,-`Quantile.0.25(R)`,-`Quantile.0.75(R)`,-`Quantile.0.95(R)`,-`Quantile.0.975(R)`)
      
      # TODO: this truncates timeseries into values that only have an R_t this is different behaviour from others. 
      return(allTs %>% mutate(Anomaly.R=Anomaly) )
    })
  },
  
  #' @description calculate a volatility statistic for the timeseries based on previous N days
  #' @details 
  #' 
  #' $x+y=z$
  #'  
  #' @param df a df containing a timeseries
  #' @param valueVar - value for which the volatility being calculated
  #' @param window - The number of time points to consider
  estimateVolatilty = function(covidTimeseries, valueVar, outputVar=NULL, window=28) {
    valueVar = ensym(valueVar)
    outputVar = tryCatch(ensym(outputVar),error=function(e) return(as.symbol(paste0("Volatility.",window,".",as_label(valueVar)))))
    # https://stats.stackexchange.com/questions/309483/measure-of-volatility-for-time-series-data
    tmp = covidTimeseries %>% dplyr::mutate(tmp_x = !!valueVar) %>% covidStandardGrouping() 
    tmp = tmp %>% dplyr::mutate(tmp_delta_x = abs(tmp_x-lag(tmp_x)))
    tmp = tmp %>% dplyr::mutate(
      tmp_delta_x_roll = stats::filter(tmp_delta_x, filter = rep(1,(window-1))/(window-1), sides = 1),
      tmp_abs_x_roll = stats::filter(abs(tmp_x), filter = rep(1,window)/window, sides = 1)
    ) %>% dplyr::mutate(
      !!outputVar := ifelse(tmp_abs_x_roll==0,0,tmp_delta_x_roll/tmp_abs_x_roll)
    ) %>% dplyr::select(
      -tmp_x, -tmp_delta_x, -tmp_delta_x_roll, -tmp_abs_x_roll
    )
    return(tmp %>% ungroup())
  },
  
  #### plot data ----
  
  epiestimCols = c("Quantile.0.025(R)","Quantile.0.25(R)","Median(R)","Quantile.0.75(R)","Quantile.0.975(R)"),
  jepidemicCols = c("Rt.Quantile.0.025","Rt.Quantile.0.25","Rt.Quantile.0.5","Rt.Quantile.0.75","Rt.Quantile.0.975"),
  
  #' @description Plot the EpiEstim output in a standard way
  #' 
  #' @param df a df containing an Rt timeseries, including a date and a `Median(R)` column from EpiEstim
  #' @param group - the colour aesthetic
  #' @param dateVar - the name of the date column
  #' @param facetVars - the facetting variables
  #' @param features - 
  #' @param rtlim - the max and min or Rt to display
  #' @param dates - the min (and optionally max) dates to display as a YYYY-MM-DD character (or anything that can be coerced to a Date)
  plotRt = function(covidRtTimeseries, colour=NULL, events=self$datasets$getSignificantDates(1), rtlim=c(0.5,2.5), dates=NULL, 
                    jepidemicMode = any(self$jepidemicCols %in% colnames(covidRtTimeseries)),
                    ribbons = TRUE,
                    ...) {
    df = covidRtTimeseries
    cols = if(jepidemicMode) self$jepidemicCols else self$epiestimCols
    #browser()
    
    if (!any(cols %in% colnames(covidRtTimeseries))) {
      warning("estimating median Rt with default parameters")
      df = df %>% self$estimateRtQuick()
      cols = self$epiestimCols
      ribbons = FALSE
    }
    
    if (!ribbons) {
      q5RtVar = as.symbol(cols[3])
      
      df = df %>% rename(
        q5Rt = !!q5RtVar
      )
      
    } else {
    
      q025RtVar = as.symbol(cols[1])
      q25RtVar = as.symbol(cols[2])
      q5RtVar = as.symbol(cols[3])
      q75RtVar = as.symbol(cols[4])
      q975RtVar = as.symbol(cols[5])
      
      df = df %>% rename(
        q025Rt = !!q025RtVar,
        q25Rt = !!q25RtVar,
        q5Rt = !!q5RtVar,
        q75Rt = !!q75RtVar,
        q975Rt = !!q975RtVar
      )
    }
    
    if (!"Anomaly.R" %in% colnames(covidRtTimeseries)) {
      df = df %>% tsp$completeAndRemoveAnomalies() %>% mutate(Anomaly.R = Anomaly)
    }
    
    colour = tryCatch(ensym(colour),error = function(e) NULL)
    
    p2 = self$plotDefault(df,events,dates, ylim=rtlim) + ylab(latex2exp::TeX("$R_t$"))
    
    if(identical(colour,NULL)) {
      
      if(ribbons) {
        p2 = p2 + 
          geom_ribbon(data=df,mapping=aes(x=date, ymin=q25Rt, ymax=q75Rt,...),alpha=0.05,fill="black",show.legend = FALSE)+
          geom_ribbon(data=df,mapping=aes(x=date, ymin=q025Rt, ymax=q975Rt,...),alpha=0.065,fill="black",show.legend = FALSE)
      }
      p2 = p2 + 
        geom_line(data=df,mapping=aes(x=date,y=q5Rt,...))+
        geom_point(data=df %>% filter(`Anomaly.R`),mapping=aes(x=date,y=q5Rt),colour="red",size=0.5, alpha=1, shape=16,show.legend = FALSE)
      
    } else {
      
      if(ribbons) {
        p2 = p2 + 
          geom_ribbon(data=df,mapping=aes(x=date, ymin=q25Rt, ymax=q75Rt,group=!!colour,  ...),alpha=0.05,fill="black",show.legend = FALSE)+
          geom_ribbon(data=df,mapping=aes(x=date, ymin=q025Rt, ymax=q975Rt,group=!!colour, ...),alpha=0.065,fill="black",show.legend = FALSE)
          
      }
      p2 = p2 +
        geom_line(data=df,mapping=aes(x=date,y=q5Rt,colour=!!colour,...))+
        geom_point(data=df %>% filter(`Anomaly.R`),mapping=aes(x=date,y=q5Rt),colour="red",size=0.5, alpha=1, shape=16,show.legend = FALSE)
      
    }
    p2 = p2 + geom_hline(yintercept = 1,colour="grey50")
    return(p2)
  },
  
  #' @description Plot the growth rate
  #' @param df a df containing an Rt timeseries, including a date and a `Median(R)` column from EpiEstim
  #' @param colour - the colour aesthetic
  #' @param rlim - the max and min or Rt to display
  #' @param dates - the min (and optionally max) dates to display as a YYYY-MM-DD character (or anything that can be coerced to a Date)
  #' @param ribbons - display the confidence limit as ribbons
  plotGrowthRate = function(covidRtTimeseries, colour=NULL, events=self$datasets$getSignificantDates(1), 
                            rlim=c(-0.25,0.25), dates=NULL, ribbons=TRUE, 
                            growthVar = "Growth.value", growthLowerVar = "Growth.Quantile.0.025.value", growthHigherVar = "Growth.Quantile.0.975.value", growthSEVar = NULL, ...) {
    
    growthVar = ensym(growthVar)
    growthLowerVar =  ensym(growthLowerVar)
    growthHigherVar =  ensym(growthHigherVar)
    growthSEVar =  tryCatch(ensym(growthSEVar),error=function(x) NULL)
    
    df = covidRtTimeseries %>% 
      dplyr::filter(type=="incidence") 
    
    if (!as_label(growthVar) %in% colnames(df)) {
      df = df %>% self$estimateGrowthRate(...)
    }
    
    if (!as_label(growthLowerVar) %in% colnames(df)) df = df %>% mutate(!!growthLowerVar := !!growthVar-1.96*!!growthSEVar)
    if (!as_label(growthHigherVar) %in% colnames(df)) df = df %>% mutate(!!growthHigherVar := !!growthVar+1.96*!!growthSEVar)
    
    colour = enexpr(colour)
    
    p2 = self$plotDefault(df, events,dates, ylim=rlim,...) + ylab(latex2exp::TeX("$r$")) + scale_y_continuous(
      sec.axis = dup_axis( breaks = log(2)/c(2,3,5,7,14,Inf,-14,-7,-5,-3,-2), labels = c(2,3,5,7,14,Inf,-14,-7,-5,-3,-2), name="doubling time")
    )
    p2 = p2 + geom_hline(yintercept = 0,colour="grey50")
    
    # TODO: change here to rename colums appropriately add in 0.25 quantiles also
    # if (identical(rlim,NULL)) {
    #   #tmp = max(c(quantile(subsetDf$growth,0.99,na.rm = TRUE),0.1))*1.2
    #   tmp = max(c(df$growth,0.1),na.rm = TRUE)*1.1
    #   tmp = if (tmp>0.20) 0.20 else if (tmp<0.1) 0.1 else tmp
    #   rlim = c(-tmp,tmp)
    # }
    
    if(identical(colour,NULL)) {
      if(ribbons) p2 = p2 + geom_ribbon(aes(ymin=!!growthLowerVar,ymax=!!growthHigherVar,...),alpha=0.065,fill = "black")
      p2 = p2 + geom_line(aes(y=!!growthVar,...))
    } else if (class(colour) == "character") {
      if(ribbons) p2 = p2 + geom_ribbon(aes(ymin=!!growthLowerVar,ymax=!!growthHigherVar,...),alpha=0.065,fill = "black")
      p2 = p2 + geom_line(aes(y=!!growthVar, ...),colour=!!colour)
    } else {
      if(ribbons) p2 = p2 + geom_ribbon(aes(ymin=!!growthLowerVar,ymax=!!growthHigherVar, group=!!colour,...),alpha=0.065,fill = "black")
      p2 = p2 + geom_line(aes(y=!!growthVar,colour=!!colour, ...))
    }
    if ("Anomaly" %in% colnames(df)) {
      p2 = p2 + geom_point(data=df %>% filter(Anomaly),mapping=aes(x=date,y=!!growthVar),colour="red",size=0.5, alpha=1, shape=16,show.legend = FALSE)
    }
    
    return(p2)
  },
  
  minMaxDate = function(groupedCovidRtTimeseries, buffer = 0) {
    return(as.Date(groupedCovidRtTimeseries %>% summarise(p = max(date,na.rm = TRUE)) %>% pull(p) %>% min(na.rm = TRUE) - buffer, "1970-01-01"))
  },
  
  plotGrowthIncidence = function(
      groupedCovidRtTimeseries, 
      plotDates = NULL, timespan=15, colour=NULL, shape=NULL, 
      populationDenominatorExpr = population/1000000,
      populationAdj = TRUE, 
      showConfInt = TRUE, 
      showHistorical = TRUE, maxAlpha=0.6, rlim=NULL, ilim=NULL, size=1, 
      labellingFilterExpr = Est.value/population > quantile(Est.value/population,0.75),
      labellingCriteriaExpr = Est.value/population*1000000*exp(Growth.value), 
      labels = 0,
      labellingExpr = shortLabel(name),
      highlightExpr = NULL,
      tableText=6,
      ...
    ) {
    
    populationDenominatorExpr = enexpr(populationDenominatorExpr)
    colour = enexpr(colour)
    shape = enexpr(shape)
    labellingFilterExpr = enexpr(labellingFilterExpr)
    labellingCriteriaExpr = enexpr(labellingCriteriaExpr)
    labellingExpr = enexpr(labellingExpr)
    highlightExpr = enexpr(highlightExpr)
    
    if (identical(highlightExpr,NULL)) {
      if (labels>0) {highlightExpr = as.symbol("isLabelled")}
      else {highlightExpr = TRUE}
    }
    
    if(length(maxAlpha==1)) maxAlpha = c(maxAlpha,maxAlpha)
    
    grps = groupedCovidRtTimeseries %>% groups()
    if (length(grps)==0) grps = list(as.symbol("code"))
    
    if (identical(plotDates, NULL)) {
      plotDates = self$minMaxDate(groupedCovidRtTimeseries)
    } else {
      if (all(plotDates <= 0)) plotDates = self$minMaxDate(groupedCovidRtTimeseries) + plotDates
    }
    plotDates = as.Date(plotDates)
    
    if (
        !all(c("Est.value", "Est.Quantile.0.025.value", "Est.Quantile.0.975.value","Growth.value", "Growth.Quantile.0.025.value", "Growth.Quantile.0.975.value") %in% colnames(groupedCovidRtTimeseries))
    ) {
      df = covidTimeseriesFormat(groupedCovidRtTimeseries)  %>% 
        dplyr::filter(type=="incidence") %>%
        self$estimateGrowthRate(...)
    } else {
      df = covidTimeseriesFormat(groupedCovidRtTimeseries)  %>% 
        dplyr::filter(type=="incidence")
    }
    
    
    if(populationAdj & any(all.names(populationDenominatorExpr)=="population") & !("population" %in% colnames(df))) {
      df = df %>% self$demog$findDemographics()
    }
    
    
    subsetDf = bind_rows(lapply(plotDates, function(plotDate) {
      if (max(df$date) < plotDate) stop("Max supported plot date is: ",max(df$date))
      return(df %>% filter(date <= plotDate & date > plotDate-timespan) %>% mutate(
        plotDate = plotDate, 
        timeOffset = as.numeric(plotDate-date), 
        fade=(timespan-as.numeric(plotDate-date))/timespan))
    }))
    
    
    
    #browser()
    subsetDf$tmpGrpId = subsetDf %>% group_by(!!!grps, plotDate) %>% group_indices()
    
    if (populationAdj) {
      subsetDf = subsetDf %>%
        mutate(
          incidence = Est.value/!!populationDenominatorExpr,
          incidenceLow = Est.Quantile.0.025.value/!!populationDenominatorExpr,
          incidenceHi = Est.Quantile.0.975.value/!!populationDenominatorExpr,
          growth = Growth.Quantile.0.5.value,
          growthLow = Growth.Quantile.0.025.value,
          growthHi = Growth.Quantile.0.975.value
        )
    } else {
      
      subsetDf = subsetDf %>%
        mutate(
          incidence = Est.value,
          incidenceLow = Est.Quantile.0.025.value,
          incidenceHi = Est.Quantile.0.975.value,
          growth = Growth.Quantile.0.5.value,
          growthLow = Growth.Quantile.0.025.value,
          growthHi = Growth.Quantile.0.975.value
        )
    }
    
    if (identical(ilim,NULL)) {
      ilim = c(0,max(subsetDf$incidence)*1.2)
    }
    
    if (identical(rlim,NULL)) {
      #tmp = max(c(quantile(subsetDf$growth,0.99,na.rm = TRUE),0.1))*1.2
      tmp = max(c(subsetDf$growth,0.1),na.rm = TRUE)*1.1
      tmp = if (tmp>0.20) 0.20 else if (tmp<0.1) 0.1 else tmp
      rlim = c(-tmp,tmp)
    }
    
    # TODO: clipped data points:
    # https://stackoverflow.com/questions/30131061/plot-points-outside-grid-as-arrows-pointing-to-data-with-ggplot2-in-r
    
    fixed = list()
    variable = list()
    
    if(!identical(colour,NULL)) {
      if (class(colour) == "character") {
        fixed$colour = colour
      } else {
        variable$colour = colour
      }
    }
    
    if(!identical(shape,NULL)) {
      if (class(shape) == "character") {
        fixed$shape = shape
      } else {
        variable$shape = shape
      }
    }
    
    fullDf = subsetDf
    
    # Just the final point of each path 
    
    points = subsetDf %>% filter(timeOffset==0)
    # The points with labels
    labPoint = points %>% 
      filter(!!labellingFilterExpr) %>% 
      mutate(
        label = !!labellingExpr,
        criteria = !!labellingCriteriaExpr
      ) %>% 
      arrange(desc(criteria)) %>% 
      filter(row_number()<=labels)
    subsetDf$isLabelled = subsetDf$tmpGrpId %in% labPoint$tmpGrpId
    
    subsetDf = subsetDf %>% filter(!!highlightExpr)
    isHighlighted = (nrow(fullDf) != nrow(subsetDf))
    
    p2 = ggplot()
    
    p2 = p2 +
      geom_vline(xintercept = 0, colour="grey40")
    if(showConfInt) p2 = p2 +  
      geom_errorbar(data = points,mapping=aes(x=growth, ymin=incidenceLow, ymax=incidenceHi),colour="grey50",size=0.5,alpha=maxAlpha[1],width=0)+
      geom_errorbar(data = points,mapping=aes(y =incidence,xmin=growthLow, xmax= growthHi),colour="grey50",size=0.5,alpha=maxAlpha[1],width=0)
    #aes(x=growth, y=incidence, group=tmpGrpId, !!!variable), !!!fixed
    if (isHighlighted) {
      # TODO: grey out background select out points by some criteria. plot the path with a fixed grey value
      # plot the selected paths as below
      
      p2 = p2+
        geom_path(data=fullDf, mapping=aes(x=growth, y=incidence, group=tmpGrpId, alpha=fade*maxAlpha[2]*2/3),size = size*0.5*2/3, linejoin = "round",lineend = "round",colour="grey70")
      tmpFixed = fixed
      tmpVariable = variable
      tmpFixed$colour = "grey70"
      tmpVariable$colour = NULL
      # browser()
      if(showHistorical) {
        #p2 = p2 + geom_point(data=fullDf, mapping=aes(x=growth, y=incidence, group=tmpGrpId, alpha=fade*maxAlpha[1], !!!tmpVariable),size=size,stat="identity",position="identity",!!! tmpFixed)
        p2 = p2 + rlang::exec(
          geom_point,
          !!!c(list(
            data=fullDf, 
            mapping=aes(x=growth, y=incidence, group=tmpGrpId, alpha=fade*maxAlpha[1]*2/3, !!!tmpVariable),
            size=size*0.5,
            stat="identity",
            position="identity"), tmpFixed))
      } else {
        #p2 = p2 + geom_point(data=fullDf %>% filter(timeOffset==0), mapping=aes(x=growth, y=incidence, group=tmpGrpId, alpha=maxAlpha[1], !!!tmpVariable),size=size,stat="identity",position="identity", !!! tmpFixed)
        p2 = p2 + rlang::exec(
          geom_point, 
          !!!c(list(
            data=fullDf %>% filter(timeOffset==0), 
            mapping=aes(x=growth, y=incidence, group=tmpGrpId, alpha=maxAlpha[1]*2/3, !!!tmpVariable),
            size=size*0.5,
            stat="identity",
            position="identity"), tmpFixed))
      }
    } 
    
    # this highlighted subset gets plotted regardless
    p2 = p2+rlang::exec(
      geom_path,
      !!!c(list(
        data=subsetDf, 
        mapping=aes(x=growth, y=incidence, group=tmpGrpId, alpha=fade*maxAlpha[2], !!!variable), 
        size=size*2/3,
        stat="identity",
        position="identity", 
        linejoin = "round", 
        lineend = "round"), fixed))
    
    if(showHistorical) {
      # p2 = p2 + geom_point(data=subsetDf, mapping=aes(x=growth, y=incidence, group=tmpGrpId, alpha=fade*maxAlpha[1], !!!variable),size=size,stat="identity",position="identity",!!! fixed)
      p2 = p2 + rlang::exec(
        geom_point,
        !!!c(list(
          data=subsetDf, 
          mapping=aes(x=growth, y=incidence, group=tmpGrpId, alpha=fade*maxAlpha[1], !!!variable),
          size=size,
          stat="identity",
          position="identity"), fixed))
    } else {
      #p2 = p2 + geom_point(data=subsetDf %>% filter(timeOffset==0), mapping=aes(x=growth, y=incidence, group=tmpGrpId, alpha=maxAlpha[1], !!!variable),size=size,stat="identity",position="identity",!!! fixed)
      p2 = p2 + rlang::exec(
        geom_point,
        !!!c(list(
          data=subsetDf %>% filter(timeOffset==0), 
          mapping=aes(x=growth, y=incidence, group=tmpGrpId, alpha=maxAlpha[1], !!!variable),
          size=size,
          stat="identity",
          position="identity"), fixed))
    }
    
    p2 = p2 +
      scale_y_continuous(trans="log1p", breaks=ukcovidtools::breaks_log1p())+
      scale_x_continuous(sec.axis = dup_axis( breaks = log(2)/c(2,3,5,7,14,Inf,-14,-7,-5,-3,-2), labels = c(2,3,5,7,14,Inf,-14,-7,-5,-3,-2), name="doubling time"))+
      scale_alpha_identity(guide="none")+
      coord_cartesian(xlim=rlim, ylim=ilim)
    
    if (populationAdj) p2 = p2+ylab("incidence/1M")
    
    p2=p2+facet_wrap(vars(plotDate))
    
    
    
    if (labels>0) {
      
      p2=p2+ggrepel::geom_label_repel(
        data = labPoint,
        mapping=aes(x=growth,y=incidence,label=label),
        inherit.aes = FALSE,
        min.segment.length = 0, segment.colour = "blue",colour="blue",fill="#F0F0F0A0",size=2,segment.size=0.25,nudge_x = +0.05,nudge_y = +0.025)
      
      p3 = standardPrintOutput::simpleFigureTable(labPoint %>% select(label,name) %>% distinct() %>% arrange(label),pts = tableText)
      out = list(
        plot = p2,
        legend = p3
      )
      
    } else {
      
      out = p2
      
    }
    
    #browser()  
    return(out)
  },
  
  gogPlot = function(...) self$plotGrowthIncidence(...),
  
  plotEvents = function(events,labelSize=7,labelY=Inf,...) {
    rects = events %>% filter(!is.na(`End date`))
    lines = events %>% filter(is.na(`End date`))
    return(list(
      geom_rect(data=rects,mapping=aes(xmin=`Start date`,xmax=`End date`),inherit.aes = FALSE,ymin=-Inf,ymax=Inf,fill="grey90",colour="grey90",alpha=0.5),
      geom_vline(data=lines,mapping=aes(xintercept = `Start date`),linetype="dashed",colour="grey50",show.legend = FALSE),
      ggrepel::geom_text_repel(
          aes(x=`Start date`, y=labelY, label=`Label`),data=events, hjust=0,vjust=1, angle=90, show.legend = FALSE,box.padding=0.05,inherit.aes = FALSE,
          size=(labelSize/ggplot2:::.pt/(96/72)))
    ))
  },
  
  plotDefault = function(data, events = NULL, dates=NULL, ylim=NULL, labelSize = 7,...) {
    p = ggplot(data, aes(x=date))
    if (identical(dates,NULL)) {
      dates = as.Date(c(min(data$date),max(data$date)),"1970-01-01")
    } else {
      dates = as.Date(dates)
      if (length(dates) == 1) dates = c(dates,max(data$date))
    }
    if (!identical(events,NULL)) {
      events = events %>% filter((is.na(`End date`) | `End date` < max(dates)) & `Start date` > min(dates) & `Start date` < max(dates))
      if (nrow(events) > 1) {
        rects = events %>% filter(!is.na(`End date`)) # %>% mutate(labelDate = as.Date(`Start date`+floor((`End date`-`Start date`)/2),"1970-01-01"))
        p = p + geom_rect(data=rects,mapping=aes(xmin=`Start date`,xmax=`End date`),inherit.aes = FALSE,ymin=-Inf,ymax=Inf,fill="grey90",colour="grey90")
        lines = events %>% filter(is.na(`End date`)) %>% mutate(labelDate = `Start date`)
        p = p + geom_vline(data=lines,mapping=aes(xintercept = `Start date`),linetype="dashed",colour="grey50",show.legend = FALSE) 
        labels = bind_rows(rects,lines)
        p = p +
          ggrepel::geom_text_repel(
            aes(x=labelDate, y=Inf, label=`Label`),data=labels, hjust=0,vjust=1, angle=90, show.legend = FALSE,box.padding=0.05,inherit.aes = FALSE,
            size=(labelSize/ggplot2:::.pt/(96/72)))
      }
    }
    p = p + scale_x_date(date_breaks = "1 month", date_labels = "%d-%m")
    
    if (!identical(ylim,NULL)) {
      p =p+coord_cartesian(xlim=dates, ylim=ylim)
    } else {
      p =p+coord_cartesian(xlim=dates)
    }
    return(p+theme(
      axis.text.x = element_text(angle=90,vjust=0.5)
    ))
  },

  plotIncidenceQuantiles = function(covidTimeseries, denominatorExpr=NULL, colour=NULL, events = self$datasets$getSignificantDates(1), dates=NULL, ribbons=TRUE, ylim=c(0,NA), pointSize=0.25, ...) {
    
    if (!all(c("Est.value", "Est.Quantile.0.025.value", "Est.Quantile.0.975.value", "Est.Quantile.0.25.value", "Est.Quantile.0.75.value") %in% colnames(covidTimeseries))) {
      df = covidTimeseriesFormat(covidTimeseries)  %>% 
        dplyr::filter(type=="incidence") %>%
        self$estimateGrowthRate(...)
    } else {
      df = covidTimeseriesFormat(covidTimeseries)  %>% 
        dplyr::filter(type=="incidence")
    }
    
    
    denominatorExpr = enexpr(denominatorExpr)
    
    if (identical(denominatorExpr, NULL)) {
      df = df %>% mutate(
        x = date,
        y = value,
        yMid = Est.value,
        yMin1 = Est.Quantile.0.025.value,
        yMax1 = Est.Quantile.0.975.value,
        yMin2 = Est.Quantile.0.25.value,
        yMax2 = Est.Quantile.0.75.value,
      )
    } else {
      df = df %>% mutate(
        x = date,
        denom = !!denominatorExpr,
        y = value/denom,
        yMid = Est.value/denom,
        yMin1 = Est.Quantile.0.025.value/denom,
        yMax1 = Est.Quantile.0.975.value/denom,
        yMin2 = Est.Quantile.0.25.value/denom,
        yMax2 = Est.Quantile.0.75.value/denom,
      )
    }
    
    colour = tryCatch(ensym(colour),error = function(e) NULL)
    p2 = self$plotDefault(df,events,dates, ylim=ylim,...)+ylab("Incidence")
    
    if(!identical(colour,NULL)) {
      if(ribbons) p2 = p2 + 
        # geom_ribbon(aes(ymin=yMin1, ymax=yMax1, fill=!!colour, ...),colour = NA,alpha=0.1,show.legend = FALSE) + 
        # geom_ribbon(aes(ymin=yMin2, ymax=yMax2, fill=!!colour,...), colour = NA,alpha=0.15,show.legend = FALSE)
          geom_ribbon(aes(ymin=yMin1, ymax=yMax1, group=!!colour, ...),colour = NA,fill="black",alpha=0.05,show.legend = FALSE) +
          geom_ribbon(aes(ymin=yMin2, ymax=yMax2, group=!!colour, ...),colour = NA,fill="black",alpha=0.065,show.legend = FALSE)
      p2 = p2 + 
        geom_point(aes(y=y,colour=!!colour),size=pointSize, alpha=0.5, shape=16,show.legend = FALSE)+
        geom_point(data=df %>% filter(Anomaly),mapping=aes(x=date,y=y),colour="red",size=0.5, alpha=1, shape=16,show.legend = FALSE) +
        geom_line(aes(y=yMid,colour=!!colour,...))
    } else {
      if(ribbons) p2 = p2 + 
        # geom_ribbon(aes(ymin=yMin1, ymax=yMax1, ...),colour = NA,alpha=0.1,show.legend = FALSE) + 
        # geom_ribbon(aes(ymin=yMin2, ymax=yMax2, ...),colour = NA,alpha=0.15,show.legend = FALSE)
        geom_ribbon(aes(ymin=yMin1, ymax=yMax1, ...),colour = NA,fill="black",alpha=0.05,show.legend = FALSE) +
        geom_ribbon(aes(ymin=yMin2, ymax=yMax2, ...),colour = NA,fill="black",alpha=0.065,show.legend = FALSE)
      p2 = p2 + 
        geom_point(aes(y=y),colour="black",size=pointSize, alpha=0.5, shape=16,show.legend = FALSE)+
        geom_point(data=df %>% filter(Anomaly),mapping=aes(x=date,y=y),colour="red",size=0.5, alpha=1, shape=16,show.legend = FALSE) +
        geom_line(data=df,mapping=aes(x=date,y=yMid,...))
    }
    return(p2)
  },
  
  plotIncidenceRollmean = function(covidTimeseries, denominatorExpr=NULL, events = self$datasets$getSignificantDates(1), dates=NULL, ylim=c(0,NA), ...
  ) {
    df = covidTimeseriesFormat(covidTimeseries)  %>% 
      dplyr::filter(type=="incidence") %>%
      self$completeAndRemoveAnomalies() %>%
      self$imputeAndWeeklyAverage()
    
    denominatorExpr = enexpr(denominatorExpr)
    
    if (identical(denominatorExpr,NULL)) {
      df = df %>% mutate(
        x = date,
        denom = !!denominatorExpr,
        y = value,
        yMid = RollMean.value
      )
    } else {
      df = df %>% mutate(
        x = date,
        denom = !!denominatorExpr,
        y = value/denom,
        yMid = RollMean.value/denom
      )
    }
      
    p2 = self$plotDefault(df,events,dates,ylim=ylim,...)+ylab("Incidence")
    p2 = p2+
      geom_point(aes(y=y,...),alpha=0.5,size=0.25)+
      geom_point(data=df %>% filter(Imputed),mapping=aes(y=y),colour="blue",size=0.5, alpha=1, shape=16,show.legend = FALSE) +
      geom_point(data=df %>% filter(Anomaly),mapping=aes(y=y),colour="red",size=0.5, alpha=1, shape=16,show.legend = FALSE) +
      geom_line(aes(y=yMid,...),alpha=1)
    return(p2)
  },
  
  
  
  ## MAP PLOT ----
  
  
  plotLabelledMap = function(map, 
     fillExpr, fillLimit = c(NA,NA), fillName="incidence/1M", hardFillLimit=c(TRUE,FALSE),
     labellingFilterExpr = Est.value/population > quantile(Est.value/population,0.75),
     labellingCriteriaExpr = Est.value/population*1000000*exp(Growth.value), 
     labels = 6,
     labellingExpr = shortLabel(name),
     insetMapShape = self$geog$getMap("NHSER20") %>% filter(name=="London"),
     insetTrim = FALSE,
     insetPos.x = Inf, 
     insetPos.y = Inf,
     insetWidth = 0.4,
     insetVjust = 1,
     insetHjust = 1,
     fillFunction = scale_fill_viridis_c, fillBreaks=waiver(), fillLabels=waiver(),
     facets = vars(date),
     tableText=6,
     ... 
  ) {
    
    fillDots = rlang::list2(...)
    
    fillExpr = enexpr(fillExpr)
    labellingFilterExpr = enexpr(labellingFilterExpr)
    labellingCriteriaExpr = enexpr(labellingCriteriaExpr)
    labellingExpr = enexpr(labellingExpr)
    
    map = map %>% mutate(fillValue = scales::squish(!!fillExpr,fillLimit)) %>% ungroup() %>% sf::st_as_sf()
    limits = range(fillLimit[hardFillLimit],map$fillValue,na.rm = TRUE)
    
    shapeFilter = insetMapShape %>% ungroup() %>% summarise() %>% sf::st_buffer(-0.01)
    lonMap = map %>% filter(sf::st_intersects(shapeFilter,.,sparse = FALSE))
    if (insetTrim) lonMap = lonMap %>% sf::st_intersection(insetMapShape)
    #ggplot(lonMap)+geom_sf()+geom_sf(data=lonNHSER,fill=NA,colour="black",size=0.5)
    
    
    if(nrow(lonMap) > 0) {
      insPlots = lonMap %>% group_by(!!!facets) %>% group_modify(function(d,g, ...) {
        
        p2a_ins = ggplot(d) + 
          geom_sf(aes(fill=fillValue),size = 0.05,colour="grey") + 
          geom_sf(data=insetMapShape, fill=NA,size=0.1,colour="white") +
          theme_void() + 
          do.call(fillFunction,args = c(list(breaks=fillBreaks, labels=fillLabels, limit=limits, guide="none"), fillDots))
        
        # x range
        xr = ggplot_build(p2a_ins)$layout$panel_params[[1]]$x_range
        # N.b. should be x.range but not for sf objects
        
        # y range
        yr = ggplot_build(p2a_ins)$layout$panel_params[[1]]$y_range
        
        return(tibble(label=list(p2a_ins), aspect = (xr[2]-xr[1])/(yr[2]-yr[1])))
      })
    } else {
      insPlots = NULL
    }
    
    asp = min(insPlots$aspect)
    
    if(is.null(insPlots)) {
      p2a = ggplot()
    } else {
      p2a = ggplot() + 
        # Add in inset
        ggpp::geom_plot(data=insPlots, mapping=aes(label=label), x=insetPos.x, y=insetPos.y,vp.width = insetWidth,vp.height = insetWidth/asp, vjust=insetVjust,hjust=insetHjust)
    }
    p2a = p2a +
      geom_sf(data = map, mapping=aes(fill=fillValue),size = 0.05,colour="grey") + 
      standardPrintOutput::defaultMapLayout() + 
      fillFunction(breaks=fillBreaks, labels=fillLabels, limit=limits, name=fillName, ...) + 
      standardPrintOutput::smallLegend(textSize = 6,spaceLegend = 1) + 
      theme(legend.text = element_text(angle = 30, vjust = 1, hjust=1))
    
    if (labels>0) {
      
      labPoint = map %>% 
        covidStandardDateGrouping(name,code) %>% 
        filter(!!labellingFilterExpr) %>% 
        mutate(criteria = !!labellingCriteriaExpr) %>% 
        arrange(desc(criteria)) %>% 
        filter(row_number()<=labels)
      # browser()
      mapLabs = labPoint %>% ungroup() %>%
        sf::st_centroid() %>%
        mutate(
          x=sf::st_coordinates(.)[,"X"], 
          y=sf::st_coordinates(.)[,"Y"],
          label = !!labellingExpr
        ) %>% 
        as_tibble()
      
      p2b = p2a + ggrepel::geom_label_repel(
        data = mapLabs,
        mapping=aes(x=x,y=y,label=label),
        inherit.aes = FALSE,
        min.segment.length = 0, segment.colour = "blue",colour="blue",fill="#F0F0F0A0",size=2,segment.size=0.25,nudge_x = -1)
      p2 = standardPrintOutput::simpleFigureTable(mapLabs %>% select(label,name) %>% distinct() %>% arrange(label),pts = tableText)
      
    } else {
      
      p2b = p2a
      p2 = NA
      mapLabs = NA
      
    }
    
    
    
    return(list(plot = p2b, legend=p2, labelDf = mapLabs))
    
  }
  
  
  
  # TODO: convert to sts
  # toSts = function(covidTimeseries, valueExpr) {
  #   
  # }
  
  
))



