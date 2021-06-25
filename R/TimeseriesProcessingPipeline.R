#' General timeseries processing
#' @import ggplot2
#' @import msm
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
          d$logValue1 = forecast::na.interp(d$logValue1)
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
      Imputed = is.na(value) & !is.na(tmp[[lblV("Est.Quantile.0.5")]])
      Imputed.value = ifelse(is.na(value),tmp[[lblV("Est.Quantile.0.5")]],value)
        
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
  estimateGrowthRate2 = function(covidTimeseries, window = 21, ...) {
    message("This function is deprecated: switch estimateGrowthRate2() to estimateGrowthRate() or estimatePoissonGrowthRate()")
    self$estimateGrowthRate(covidTimeseries, window, ...)
  },
  
  #' @description Calculates a growth rate timeseries using a fitted quasipoisson model and locally fitted polynomials
  #' @param covidTimeseries a covid timeseries data frame
  #' @param window - the length of the smoothing window (default 21)
  #' @param weekendEffect - the downweighting of figures from Saturday, Sunday and Monday to account for weekend delay
  #' @param polynomialDegree - improves fit at risk of overfitting. default 1 (linear)
  #' @return a dataframe with groupwise growth rate estimates
  estimateGrowthRate = function(covidTimeseries, window = 21, weekendEffect = 0.75, polynomialDegree = 1, ...) {
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
        
        if(all(na.omit(tmp$value) == 0)) {
          
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
          
          suppressWarnings({
            #ev = seq(min(d$time)-1,max(d$time)+1,length.out = floor(2*nrow(d)/window+1))
            
            tmp_intercept_model = locfit::locfit(value ~ locfit::lp(time, nn=tmp_alpha_2, deg=polynomialDegree), tmp, family="qpois", link="log", weights = tmp$weights, ev=d$time)
            tmp_intercept = predict(tmp_intercept_model, band="global", se.fit=TRUE, where="fitp")
            
            # if(any(is.na(tmp_intercept$fit))) 
            #   tmp_intercept = predict(tmp_intercept_model, d$time, band="global", se.fit=TRUE)
            if(any(is.na(tmp_intercept$fit))) 
              browser()
            
            tmp_slope_model = locfit::locfit(value ~ locfit::lp(time, nn=tmp_alpha_2, deg=polynomialDegree), tmp, family="qpois", link="log", deriv=1, weights = tmp$weights, ev=d$time)
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
              tmp_slope2_model = locfit::locfit(value ~ locfit::lp(time, nn=tmp_alpha_2, deg=polynomialDegree), tmp, family="qpois", link="log", deriv=c(1,1), weights = tmp$weights, ev=d$time)
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
        
        #TODO: quasi-poisson model must have some overdispertion parameters in there somewhere which we are not surfacing.
        #also makes confidence interval calculations below questionable.
        
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
    if (!cols[3] %in% colnames(covidRtTimeseries)) {
      warning("estimating median Rt with default parameters")
      df = df %>% self$estimateRtQuick()
      cols = self$epiestimCols
      ribbons = FALSE
    
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
  
  plotGrowthIncidence = function(groupedCovidRtTimeseries, plotDates = NULL, timespan=15, colour=NULL, shape=NULL, populationAdj = TRUE, showConfInt = TRUE, showHistorical = TRUE, maxAlpha=0.6, rlim=c(-0.15,0.15), ilim=c(0,NA), maxSize=6, ...) {
    colour = enexpr(colour)
    shape = enexpr(shape)
    
    grps = groupedCovidRtTimeseries %>% groups()
    if (length(grps)==0) grps = list(as.symbol("code"))
    
    if (identical(plotDates, NULL)) plotDates = (groupedCovidRtTimeseries %>% summarise(p = max(date,na.rm = TRUE)) %>% pull(p) %>% min(na.rm = TRUE)) - 3
    plotDates = as.Date(plotDates)
    
    if (!all(c("Est.value", "Est.Quantile.0.025.value", "Est.Quantile.0.975.value","Growth.value", "Growth.Quantile.0.025.value", "Growth.Quantile.0.975.value") %in% colnames(groupedCovidRtTimeseries))) {
      df = covidTimeseriesFormat(groupedCovidRtTimeseries)  %>% 
        dplyr::filter(type=="incidence") %>%
        self$estimateGrowthRate(...)
    } else {
      df = covidTimeseriesFormat(groupedCovidRtTimeseries)  %>% 
        dplyr::filter(type=="incidence")
    }
    
    
    if(populationAdj & !("population" %in% colnames(df))) {
      df = df %>% self$demog$findDemographics()
    }
    
    
    subsetDf = bind_rows(lapply(plotDates, function(plotDate) {
      if (max(df$date) < plotDate) stop("Max supported plot date is: ",max(df$date))
      return(df %>% filter(date <= plotDate & date > plotDate-timespan) %>% mutate(plotDate = plotDate, timeOffset = as.numeric(plotDate-date), fade=(timespan-as.numeric(plotDate-date))/timespan))
    }))
    
    #browser()
    subsetDf$tmpGrpId = subsetDf %>% group_by(!!!grps, plotDate) %>% group_indices()
    
    if (populationAdj) {
      subsetDf = subsetDf %>%
        mutate(
          incidence = Est.value/population*1000000,
          incidenceLow = Est.Quantile.0.025.value/population*1000000,
          incidenceHi = Est.Quantile.0.975.value/population*1000000,
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
    
    p2 = ggplot(subsetDf, aes(x=growth, y=incidence, group=tmpGrpId, !!!variable), !!!fixed)
    
    points = subsetDf %>% filter(timeOffset==0)
    p2 = p2 +
      geom_vline(xintercept = 0, colour="grey40")
    if(showConfInt) p2 = p2 +  
      geom_errorbar(data = points,mapping=aes(ymin=incidenceLow, ymax=incidenceHi),colour="grey50",size=0.5,alpha=0.5,width=0)+
      geom_errorbar(data = points,mapping=aes(xmin=growthLow, xmax= growthHi),colour="grey50",size=0.5,alpha=0.5,width=0)
    p2 = p2+
      geom_path(aes(alpha=fade),linejoin = "round",lineend = "round")
      
    if(showHistorical) {
      p2 = p2 + geom_point(data=subsetDf, mapping=aes(x=growth, y=incidence, group=tmpGrpId,alpha=fade,!!!variable),stat="identity",position="identity",!!!fixed)
    } else {
      p2 = p2 + geom_point(data=points, mapping=aes(x=growth, y=incidence, group=tmpGrpId,!!!variable),stat="identity",position="identity",!!!fixed)
    }
    
    p2 = p2 +
      scale_y_continuous(trans="log1p", breaks=ukcovidtools::breaks_log1p())+
      scale_x_continuous(sec.axis = dup_axis( breaks = log(2)/c(2,3,5,7,14,Inf,-14,-7,-5,-3,-2), labels = c(2,3,5,7,14,Inf,-14,-7,-5,-3,-2), name="doubling time"))+
      scale_alpha_continuous(range=c(0,maxAlpha),guide="none")+
      coord_cartesian(xlim=rlim, ylim=ilim)
    
    if (populationAdj) p2 = p2+ylab("incidence/1M")
    
    #browser()  
    return(p2)
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

  plotIncidenceQuantiles = function(covidTimeseries, denominatorExpr=NULL, colour=NULL, events = self$datasets$getSignificantDates(1), dates=NULL, ribbons=TRUE, ylim=c(0,NA), ...) {
    
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
        geom_point(aes(y=y,colour=!!colour),size=0.25, alpha=0.5, shape=16,show.legend = FALSE)+
        geom_point(data=df %>% filter(Anomaly),mapping=aes(x=date,y=y),colour="red",size=0.5, alpha=1, shape=16,show.legend = FALSE) +
        geom_line(aes(y=yMid,colour=!!colour,...))
    } else {
      if(ribbons) p2 = p2 + 
        # geom_ribbon(aes(ymin=yMin1, ymax=yMax1, ...),colour = NA,alpha=0.1,show.legend = FALSE) + 
        # geom_ribbon(aes(ymin=yMin2, ymax=yMax2, ...),colour = NA,alpha=0.15,show.legend = FALSE)
        geom_ribbon(aes(ymin=yMin1, ymax=yMax1, ...),colour = NA,fill="black",alpha=0.05,show.legend = FALSE) +
        geom_ribbon(aes(ymin=yMin2, ymax=yMax2, ...),colour = NA,fill="black",alpha=0.065,show.legend = FALSE)
      p2 = p2 + 
        geom_point(aes(y=y),colour="black",size=0.25, alpha=0.5, shape=16,show.legend = FALSE)+
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
  }#,
  
  # ## convert to sts ----
  # toSts = function(covidTimeseries, valueExpr) {
  #   
  # }
  
  
))



# completeTimeseries = ensurer::ensures_that(
#   .$
#   . %>% covidStandardGrouping() %>% dplyr::mutate(check = lead(date)==date) %>% dplyr::pull(check) %>% all(na.rm=TRUE) ~ "Dates must be unique"
#   . %>% covidStandardGrouping() %>% dplyr::mutate(check = lead(date)==date+1) %>% dplyr::pull(check) %>% all(na.rm=TRUE) ~ "Dates must be contiguous"
# )
# 
# nonNegativeIncidence = ensurer::ensures_that(
#   . %>% dplyr::mutate(check = (statistic != "incidence" | !is.na(value))) %>% pull(check) %>% all() ~ "Incidence values cannot be NA",
#   . %>% dplyr::mutate(check = (statistic != "incidence" | value >=0)) %>% pull(check) %>% all() ~ "Incidence values cannot be negative."
# )
# 
# covidTimeseriesFormat = ensurer::ensures_that(
#   +covidTimeseriesFormat,
#   +completeTimeseries,
#   +nonNegativeIncidence
# )