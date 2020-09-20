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
  
  
  
  aggregateAge = function(covidTimeseries, fn = sum, ...) {covidTimeseriesFormat %def% {
    tmp = covidTimeseriesFormat(covidTimeseries)
    
    errors = tmp %>% 
      covidStandardDateGrouping(ageCat) %>%
      dplyr::summarise(mixed = any(is.na(ageCat)) & any(!is.na(ageCat))) %>%
      dplyr::filter(mixed==TRUE)
    if(nrow(errors) > 0) warning("aggregating by age, but some groups have mixed NAs and values. You maybe wanted to filter out the NAs:\n", paste(capture.output(print(errors)), collapse = "\n"))
    
    #TODO: check unequal lengths or non overlapping timeseries dates
    
    tmp= tmp %>% dplyr::mutate(ageCat=NA)
    tmp = tmp %>% 
      covidStandardDateGrouping() %>%
      dplyr::summarise(value = fn(value, ...)) %>%
      dplyr::mutate(value = ifelse(is.nan(value),NA,value))
    return(tmp %>% dplyr::ungroup())
  }},
  
  aggregateGender = function(covidTimeseries, fn = sum, ...) {covidTimeseriesFormat %def% {
    tmp = covidTimeseriesFormat(covidTimeseries)
    
    errors = tmp %>% 
      covidStandardDateGrouping(gender) %>% 
      dplyr::summarise(mixed = any(is.na(gender)) & any(!is.na(gender))) %>%
      dplyr::filter(mixed==TRUE)
    if(nrow(errors) > 0) warning("aggregating by gender, but some groups have mixed NAs and values. You maybe wanted to filter out the NAs:\n", paste(capture.output(print(errors)), collapse = "\n"))
    
    tmp= tmp %>% dplyr::mutate(gender=NA)
    tmp = tmp %>% 
      covidStandardDateGrouping() %>% 
      dplyr::summarise(value = fn(value, ...)) %>%
      dplyr::mutate(value = ifelse(is.nan(value),NA,value))
    return(tmp %>% dplyr::ungroup())
  }},
  
  aggregateSubgroup = function(covidTimeseries, fn=sum, ...) {covidTimeseriesFormat %def% {
    tmp = covidTimeseriesFormat(covidTimeseries)
    
    errors = tmp %>% 
      covidStandardDateGrouping(subgroup) %>%
      dplyr::summarise(mixed = any(is.na(subgroup)) & any(!is.na(subgroup))) %>%
      dplyr::filter(mixed==TRUE)
    if(nrow(errors) > 0) warning("aggregating by subgroup, but not all items have a subgroup. You maybe wanted to filter out the NAs:\n", paste(capture.output(print(errors)), collapse = "\n"))
  
    tmp= tmp %>% dplyr::mutate(subgroup=NA)
    tmp = tmp %>% 
      covidStandardDateGrouping() %>% 
      dplyr::summarise(value = fn(value, ...)) %>%
      dplyr::mutate(value = ifelse(is.nan(value),NA,value))
    return(tmp %>% dplyr::ungroup())
  }},
  
  aggregateSource = function(covidTimeseries, namedListOfSources = list("All sources"=unique(covidTimeseries$source)), fn=sum, ...) {covidTimeseriesFormat %def% {
    tmp = covidTimeseriesFormat(covidTimeseries)
    tmp$newSource = tmp$source
    for (name in names(namedListOfSources)) {
      tmp$newSource[tmp$source %in% namedListOfSources[[name]]] = name; 
    }
    # filter out dates where there are non equal numbers of sources
    
    tmp = tmp %>% 
      covidStandardGrouping(source) %>% 
      dplyr::group_modify(function(d,g,...) {
        # check each date has the same number of elements and introduce NAs for those dates where not every source is represented
        out = tidyr::crossing(
          d %>% select(source,newSource) %>% distinct(),
          tibble(date = as.Date(min(d$date):max(d$date),"1970-01-01"))
        ) %>% left_join(d, by=c("date","source","newSource"))
        out = out %>% dplyr::group_by(newSource,date) %>% dplyr::summarise(value = fn(value,...)) %>% rename(source = newSource)
      })
    return(tmp %>% dplyr::ungroup() %>% self$trimNAs())
  }},
  
  #' @param completeness should the mapping be complete? if the mapping is "source" complete it will only be successful if all source codes are present when mapping to a higher region. if the mapping is target, then only if all the target codes are represented. Or both if the mapping must be complete at both ends.
  aggregateGeography = function(covidTimeseries, targetCodeTypes, completeness = "source", fn=sum, keepOriginal = TRUE, ...) {covidTimeseriesFormat %def% {
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
      dplyr::mutate(fromCode=code,fromCodeType = codeType, fromName = name) %>%
      dplyr::ungroup() 
    tmp3 = tmp3 %>%
      dplyr::select(-code,-codeType,-name) %>%
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
          #TODO: There are lots of edge cases here.
          mapped = d %>% 
            dplyr::filter(!is.na(fromCode) & !is.na(toCode)) %>% 
            dplyr::ungroup() %>%
            dplyr::mutate(codeType=g$toCodeType) %>%
            dplyr::rename(code = toCode, name = toName) %>%
            dplyr::group_by(subgroup,gender,ageCat,date,code,codeType,name) %>% 
            dplyr::summarise(value = fn(value,...))
         
          if("source"==completeness) { 
            if(nrow(notarget) == 0) {
              # complete mapping
              return(mapped)
            } else {
              return(tibble())
            }
          } else if("target"==completeness) {
            if(nrow(nosource) == 0) {
              # complete mapping
              return(mapped)
            } else {
              return(tibble())
            }
          } else if("all"==completeness) {
            if(nrow(notarget) == 0 & nrow(nosource) == 0) {
              # complete mapping
              return(mapped)
            } else {
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
    tmp3 = tmp3 %>% dplyr::ungroup() %>% dplyr::select(-fromCodeType, -toCodeType)
    if(keepOriginal) return(covidTimeseries %>% dplyr::bind_rows(tmp3))
    else return(tmp3)
  }},
  
  #### smoothing and imputing
  
  imputeAndWeeklyAverage = function(covidTimeseries, window=7, ...) {
    if ("RollMean.value" %in% colnames(covidTimeseries)) {
      # message("time series has already been averaged")
      return(covidTimeseries)
    }
    ts=covidTimeseries
    #self$getHashCached(object = covidTimeseries, operation="IMPUTE", ... , orElse = function (ts, ...) {covidTimeseriesFormat %def% {
      tmp = ts %>%
        covidStandardGrouping() %>%
        dplyr::mutate(
          logValue1 = log(value+1)) %>%
        self$completeAndRemoveAnomalies(valueVar = logValue1, originalValueVar = logValue1.original) %>%
        covidStandardGrouping() %>%
        dplyr::group_modify(function(d,g,...) {
         d = d %>%
          dplyr::mutate(
            logValue1 = forecast::na.interp(logValue1)
          ) %>% 
          dplyr::mutate(
            #logValue1 = stats::filter(logValue1,rep(1,7)/7)
            logValue2 = signal::sgolayfilt(logValue1,p=1,n=window) 
          )
        }) %>%
        dplyr::mutate(
          Imputed.value = ifelse(logValue1 < 0,0,exp(logValue1)-1),
          Imputed = is.na(value) & !is.na(Imputed.value),
          RollMean.value = ifelse(logValue2 < 0,0,exp(logValue2)-1),
          Window.RollMean.value = window
        ) %>%
        dplyr::select(-logValue1.original, -logValue1, -logValue2) %>%
        dplyr::ungroup() 
      #browser(expr = self$debug)
      return(tmp)
    #}})
  },
  
  completeAndRemoveAnomalies = function(r0Timeseries, outlier_sd = 5, window=9, valueVar = "value", originalValueVar = "value.original", precision=0.00001) {covidTimeseriesFormat %def% {
    valueVar = ensym(valueVar)
    originalValueVar = ensym(originalValueVar)
    if (as_label(originalValueVar) %in% colnames(r0Timeseries)) {
      # detection already performed
      return(r0Timeseries)
    }
    # trim tailing NAs
    tmp = self$trimNAs(r0Timeseries)
    
    tmp = tmp %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!originalValueVar := !!valueVar) %>%
      self$complete() %>%
      covidStandardGrouping() %>%
      dplyr::group_modify(function(d,g,...) {
        
        tmp_ts = d %>% dplyr::mutate(tmp_y = !!valueVar)
        # #TODO: make this a more generic way of doing windowing & tidyify it
        i = 1:(nrow(tmp_ts))
        w2 = floor(window/2)
        v = c(rep(NA,w2),tmp_ts$tmp_y, rep(NA,w2))
        # turn time series into matrix with columns for each window
        m = sapply(i,function(j) {v[j:(j+w2*2+1)]})
        # get rid of central value
        m[w2+1,] = NA
        m_mean = apply(m, 2, mean, na.rm=TRUE) #2 here means apply mean col-wise
        m_sd = apply(m, 2, sd, na.rm=TRUE)
        m_sd[m_sd < precision] = precision
        m_low = m_mean-outlier_sd*m_sd
        m_high = m_mean+outlier_sd*m_sd
        
        tmp_ts = tmp_ts %>% dplyr::mutate(
          m_mean = m_mean,
          m_sd = m_sd,
          m_low = m_low,
          m_high = m_high,
          tmp_y = ifelse(tmp_y < m_low | tmp_y > m_high, NA, tmp_y),
        ) %>% dplyr::mutate(
          Anomaly = is.na(tmp_y)
        )
        
        #TODO: repeat the anomaly removal stage to detect anything ?
        
        tmp_ts = tmp_ts %>% dplyr::select(-!!valueVar, -m_mean, -m_sd, -m_low, -m_high) %>% dplyr::rename(!!valueVar := tmp_y) 
        #browser()
        return(tmp_ts)
      }) %>% 
      dplyr::ungroup()
    return(tmp)
  }}, 
  
  #' @description Calculate an estimate of rate of change of Rt using a loess
  #' 
  #' @param R0timeseries a grouped df contianing R0 timeseries including a date and a `Median(R)` column from EpiEstim
  
  smoothAndSlopeTimeseries = function(r0Timeseries, smoothExpr, window = 14) {covidTimeseriesFormat %def% {
    smoothExpr = enexpr(smoothExpr)
    smoothLabel = as_label(smoothExpr)
    if (smoothLabel == "value") warning("Smoothing value directly does not account for its exponential nature - you maybe want logIncidenceStats?")
    if (paste0("Est.",smoothLabel) %in% colnames(r0Timeseries)) {
      warning(smoothLabel," has already been estimated. aborting smooth and slope.")
      return(r0Timeseries)
    }
    covidTimeseriesFormat(r0Timeseries) %>% 
      dplyr::ungroup() %>%
      dplyr::mutate(
        y = !!smoothExpr
      ) %>%
      self$completeAndRemoveAnomalies(valueVar=y, originalValueVar = y_orig) %>%
      dplyr::mutate(
        x = as.integer(date-min(date)),
      ) %>%
      covidStandardGrouping() %>%
      dplyr::group_modify(function(d,g,...) {
        tmp_alpha = min(window/nrow(d),1)
        tmp_ts = d %>% 
          dplyr::mutate(
            y = forecast::na.interp(y),
            window = window
          ) %>%
          dplyr::select(date,x,y,Anomaly,window) 
        
        if(nrow(tmp_ts) == 0) {
          warning("No non NA values to smooth in ",paste0(g$statistic,g$type,g$codeType,g$source,g$subgroup,g$ageCat,g$gender,collapse = "; "))
          return(d)
        }
        # calculate derivative using a loess method
        # tmp = locfit::locfit(log(value+1) ~ day,loessTest,deg = 2,alpha=0.25,deriv=1)
        tmp_intercept_model = locfit::locfit(y ~ x, tmp_ts, deg=1, alpha=tmp_alpha)
        tmp_slope_model = locfit::locfit(y ~ x, tmp_ts, deg=1, alpha=tmp_alpha, deriv=1)
        tmp_intercept = predict(tmp_intercept_model, tmp_ts$x, band="local")
        tmp_slope = predict(tmp_slope_model, tmp_ts$x, band="local")
        tmp_ts[[paste0("Est.",smoothLabel)]] = ifelse(tmp_intercept$fit < 0, 0, tmp_intercept$fit) # prevent negative smoothing.
        tmp_ts[[paste0("Est.SE.",smoothLabel)]] = tmp_intercept$se.fit
        tmp_ts[[paste0("Slope.",smoothLabel)]] = tmp_slope$fit
        tmp_ts[[paste0("Slope.SE.",smoothLabel)]] = tmp_slope$se.fit
        
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
        return(d %>% select(-x,-y,-y_orig) %>% dplyr::full_join(tmp_ts %>% select(-x,-y, -Anomaly, -window), by="date"))
      }) %>% 
      dplyr::ungroup()
  }},
  
  
  
  # TODO: gamma distribution weighted interpolation:
  # shape = (self$rtConfig$mean_si^2)/(self$rtConfig$std_si^2)
  # rate = self$rtConfig$mean_si/(self$rtConfig$std_si^2)
  # tmp = pgamma(1:31,shape,rate)-pgamma(0:30,shape,rate)
  # EpiEstim::discr_si does something different
  # in theory this could be bootstrapped
  # sds = qgamma(seq(5,195,5)/200,cfg$$std_si, cfg$std_std_si)
  # means = qgamma(seq(5,195,5)/200,cfg$mean_si, cfg$std_mean_si)
  # relevant code in metaward tools for applying a set of bootstrapped parameterised convolutions
  # but in this case we want the convolution filter to be used as weights for a linear model.

  #### calculate baskets of stats ----
  
  logIncidenceStats = function(covidTimeseries, valueVar = "value", growthRateWindow = 7,smoothingWindow = 14,...) {
    valueVar = ensym(valueVar)
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
      
      tmp = tmp %>% self$smoothAndSlopeTimeseries(!!logExpr,window = smoothingWindow)
      
      tmp[[lblV("Growth")]] = tmp[[lbl("Slope")]]
      tmp[[lblV("Growth.SE")]] =  tmp[[lbl("Slope.SE")]]
      tmp[[lblV("Growth.ProbPos")]] = 1-pnorm(0,mean=tmp[[lblV("Growth")]],sd=tmp[[lblV("Growth.SE")]])
      
      tmp$interceptDate = as.Date(tmp$date - tmp[[lbl("Est")]]/tmp[[lbl("Slope")]])
      
      tmp$doublingTime = log(2)/tmp[[lblV("Growth")]]
      tmp$doublingTime.Quantile.0.025 = log(2)/qnorm(mean=tmp[[lblV("Growth")]],sd=tmp[[lblV("Growth.SE")]],p = 0.975)
      tmp$doublingTime.Quantile.0.25 = log(2)/qnorm(mean=tmp[[lblV("Growth")]],sd=tmp[[lblV("Growth.SE")]],p = 0.75)
      tmp$doublingTime.Quantile.0.75 = log(2)/qnorm(mean=tmp[[lblV("Growth")]],sd=tmp[[lblV("Growth.SE")]],p = 0.25)
      tmp$doublingTime.Quantile.0.975 = log(2)/qnorm(mean=tmp[[lblV("Growth")]],sd=tmp[[lblV("Growth.SE")]],p = 0.025)
      
      meanLbl = lbl("Est")
      sdLbl = lbl("Est.SE")
      q = qnorm(c(0.025,0.25,0.5,0.75,0.975))
      fn = function(i) return(exp(tmp[[meanLbl]]+q[i]*tmp[[sdLbl]])-1)
      
      tmp[[lblV("Est")]] = exp(tmp[[meanLbl]])-1
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
            !!lblV("Growth.windowed") := stats::filter(x = tmp_gv, filter=rep(1/growthRateWindow,growthRateWindow), sides = 1),
            !!lblV("Growth.windowed.SE") := stats::filter(x = tmp_gv.se, filter=rep(1/growthRateWindow,growthRateWindow), sides = 1) / sqrt(growthRateWindow),
            !!lblV("Growth.windowed.Window") := growthRateWindow
        ) %>% 
        dplyr::select(-tmp_gv, -tmp_gv.se)
      
      tmp$doublingTime.windowed = log(2)/tmp[[lblV("Growth.windowed")]]
      tmp$doublingTime.windowed.Quantile.0.025 = log(2)/qnorm(mean=tmp[[lblV("Growth.windowed")]],sd=tmp[[lblV("Growth.windowed.SE")]],p = 0.975)
      tmp$doublingTime.windowed.Quantile.0.25 = log(2)/qnorm(mean=tmp[[lblV("Growth.windowed")]],sd=tmp[[lblV("Growth.windowed.SE")]],p = 0.75)
      tmp$doublingTime.windowed.Quantile.0.75 = log(2)/qnorm(mean=tmp[[lblV("Growth.windowed")]],sd=tmp[[lblV("Growth.windowed.SE")]],p = 0.25)
      tmp$doublingTime.windowed.Quantile.0.975 = log(2)/qnorm(mean=tmp[[lblV("Growth.windowed")]],sd=tmp[[lblV("Growth.windowed.SE")]],p = 0.025)
      
      tmp[[lblV("Growth.windowed.ProbPos")]] = 1-pnorm(0,mean=tmp[[lblV("Growth.windowed")]],sd=tmp[[lblV("Growth.windowed.SE")]])
        
      return(tmp %>% ungroup())
    }})
  },
  
  estimateGrowthRate = function(covidTimeseries, window = self$rtWindow, ...) {
    self$getHashCached(object = covidTimeseries, operation="ESTIM-LITTLE-R", params=list(window), ... , orElse = function (ts, ...) {covidTimeseriesFormat %def% {

      groupedDf = covidTimeseriesFormat(covidTimeseries) %>%
        ensurer::ensure_that(any(.$type == "incidence") ~ "dataset must contain incidence figures") %>%
        dplyr::filter(type == "incidence") %>%
        self$completeAndRemoveAnomalies() %>%
        dplyr::mutate(I = value) %>%
         

      groupedDf = groupedDf %>% covidStandardGrouping()

      groupedDf = groupedDf %>% group_modify(function(d,g,...) {
        if (nrow(d) < 2+window) errors = paste0(ifelse(is.na(errors),"",paste0(errors,"; ")),"Not enough data to calculate growth rates")
        if (any(!is.na(d$errors))) {return(d)}
        
        result = NULL
        # https://cran.rstudio.com/web/packages/tibbletime/vignettes/TT-03-rollify-for-rolling-analysis.html
        # https://rdrr.io/cran/tsibble/man/slide.html
        for(i in window:nrow(d)) {
          sample = d %>% dplyr::filter(row_number() > i-window & row_number() <= i) %>% mutate(time=row_number())
          # browser()
          model <- glm(I ~ time, family = quasipoisson(), data = sample)
          model_sum <- summary(model)
          result = result %>% bind_rows(tibble::tibble(
            date = d$date[i],
            `Growth.poisson` = model_sum$coefficients[2, 1],
            `Growth.SE.poisson` = model_sum$coefficients[2, 2],
            `Growth.Fit.poisson` = (model$null.deviance - model$deviance) / (model$null.deviance)
          ))
        }
        return(d %>% dplyr::left_join(result, by="date"))
      })

      return(groupedDf %>% dplyr::select(-I) %>% mutate(`Window.Growth.poisson` = window))

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
        dplyr::mutate(I = !!valueVar) %>%
        self$describeErrors(I) 
      
      groupedDf = groupedDf %>% covidStandardGrouping()
        
      # tmp starts on first non zero value of I in group
      tmp2 = groupedDf %>% group_modify(function(d,g,...) {
        
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
  
  adjustRtDates = function(covidRtResult, window=0, offsetAssumptions = self$defaultOffsetAssumptions(), extraCols=NULL) {
    
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
          d2 = d2 %>% mutate(!!col := lead(!!col,n = offset))
        }
        return(d2)
        
      })
      return(tmp)
      
    })
    
    return(tmp7)
  },
  
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
  #' This will not work well for timeseries with NAs
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
  
  #' @description Plot the EpiEstim output in a standard way
  #' 
  #' @param df a df containing an Rt timeseries, including a date and a `Median(R)` column from EpiEstim
  #' @param group - the colour aesthetic
  #' @param dateVar - the name of the date column
  #' @param facetVars - the facetting variables
  #' @param features - 
  #' @param rtlim - the max and min or Rt to display
  #' @param dates - the min (and optionally max) dates to display as a YYYY-MM-DD character (or anything that can be coerced to a Date)
  plotRt = function(covidRtTimeseries, colour=NULL, events=self$datasets$getSignificantDates() %>% filter(Significance==1), rtlim=c(0.5,2.5), dates=NULL, 
                    ribbons=("Quantile.0.025(R)" %in% colnames(covidRtTimeseries)), ...) {
    df = covidRtTimeseries
    if (!("Median(R)" %in% colnames(covidRtTimeseries))) {
      warning("estimating median Rt with default parameters")
      df = df %>% self$estimateRtQuick()
    }
    colour = tryCatch(ensym(colour),error = function(e) NULL)
    
    p2 = self$plotDefault(df,events,dates, ylim=rtlim) + ylab(latex2exp::TeX("$R_t$"))
    
    if(identical(colour,NULL)) {
      
      if(ribbons) {
        p2 = p2 + 
          geom_ribbon(data=df,mapping=aes(x=date, ymin=`Quantile.0.25(R)`, ymax=`Quantile.0.75(R)`,...),alpha=0.05,fill="black",show.legend = FALSE)+
          geom_ribbon(data=df,mapping=aes(x=date, ymin=`Quantile.0.025(R)`, ymax=`Quantile.0.975(R)`,...),alpha=0.065,fill="black",show.legend = FALSE)
      }
      p2 = p2 + 
        geom_line(data=df,mapping=aes(x=date,y=`Median(R)`,...))+
        geom_point(data=df %>% filter(`Anomaly.R`),mapping=aes(x=date,y=`Median(R)`),colour="red",size=1, alpha=1, shape=16,show.legend = FALSE)
      
    } else {
      
      if(ribbons) {
        p2 = p2 + 
          # geom_ribbon(data=df,mapping=aes(x=date, ymin=`Quantile.0.25(R)`, ymax=`Quantile.0.75(R)`, fill=!!colour,...),alpha=0.1,show.legend = FALSE)+
          # geom_ribbon(data=df,mapping=aes(x=date, ymin=`Quantile.0.025(R)`, ymax=`Quantile.0.975(R)`, fill=!!colour,...),alpha=0.15,show.legend = FALSE)
          geom_ribbon(data=df,mapping=aes(x=date, ymin=`Quantile.0.25(R)`, ymax=`Quantile.0.75(R)`,group=!!colour,  ...),alpha=0.05,fill="black",show.legend = FALSE)+
          geom_ribbon(data=df,mapping=aes(x=date, ymin=`Quantile.0.025(R)`, ymax=`Quantile.0.975(R)`,group=!!colour, ...),alpha=0.065,fill="black",show.legend = FALSE)
          
      }
      p2 = p2 +
        geom_line(data=df,mapping=aes(x=date,y=`Median(R)`,colour=!!colour,...))+
        geom_point(data=df %>% filter(`Anomaly.R`),mapping=aes(x=date,y=`Median(R)`),colour="red",size=1, alpha=1, shape=16,show.legend = FALSE)
      
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
  plotGrowthRate = function(covidRtTimeseries, colour=NULL, events=self$datasets$getSignificantDates() %>% filter(Significance==1), rlim=c(-0.25,0.25), dates=NULL, ribbons=TRUE, ...) {
    df = covidRtTimeseries %>% 
      dplyr::filter(type=="incidence") %>%
      self$logIncidenceStats()
    colour = tryCatch(ensym(colour),error = function(e) NULL)
    
    p2 = self$plotDefault(df, events,dates, ylim=rlim) + ylab(latex2exp::TeX("$r$"))
    p2 = p2 + geom_hline(yintercept = 0,colour="grey50")
    
    if(identical(colour,NULL)) {
      if(ribbons) p2 = p2 + plotRibbons(`Growth.value`,`Growth.SE.value`,colourExpr = "black")
      else p2 = p2 + geom_line(aes(y=`Growth.value`,...))
    } else {
      if(ribbons) p2 = p2 + plotRibbons(`Growth.value`,`Growth.SE.value`,colourExpr = !!colour)
      else p2 = p2 + geom_line(aes(y=`Growth.value`,colour=!!colour, ...))
    }
    p2 = p2 + geom_point(data=df %>% filter(Anomaly),mapping=aes(x=date,y=`Growth.value`),colour="red",size=1, alpha=1, shape=16,show.legend = FALSE)
    
    return(p2)
  },
  
  #' @description Plot the growth rate
  #' @param df a df containing an Rt timeseries, including a date and a `Median(R)` column from EpiEstim
  #' @param colour - the colour aesthetic
  #' @param rlim - the max and min or Rt to display
  #' @param dates - the min (and optionally max) dates to display as a YYYY-MM-DD character (or anything that can be coerced to a Date)
  #' @param ribbons - display the confidence limit as ribbons
  plotWindowedGrowthRate = function(covidRtTimeseries, colour=NULL, events=self$datasets$getSignificantDates() %>% filter(Significance==1), rlim=c(-0.25,0.25), dates=NULL, ribbons=TRUE, ...) {
    df = covidRtTimeseries %>% 
      dplyr::filter(type=="incidence") %>%
      self$logIncidenceStats()
    colour = tryCatch(ensym(colour),error = function(e) NULL)
    
    p2 = self$plotDefault(df, events,dates, ylim=rlim) + ylab(latex2exp::TeX("$r$"))
    p2 = p2 + geom_hline(yintercept = 0,colour="grey50")
    
    if(identical(colour,NULL)) {
      if(ribbons) p2 = p2 + plotRibbons(`Growth.windowed.value`,`Growth.windowed.SE.value`,colourExpr = "black")
      else p2 = p2 + geom_line(aes(y=`Growth.windowed.value`,...))
    } else {
      if(ribbons) p2 = p2 + plotRibbons(`Growth.windowed.value`,`Growth.windowed.SE.value`,colourExpr = !!colour)
      else p2 = p2 + geom_line(aes(y=`Growth.windowed.value`,colour=!!colour, ...))
    }
    p2 = p2 + geom_point(data=df %>% filter(Anomaly),mapping=aes(x=date,y=`Growth.windowed.value`),colour="red",size=1, alpha=1, shape=16,show.legend = FALSE)
    
    return(p2)
  },
  
  plotDefault = function(data, events = self$datasets$getSignificantDates() %>% filter(Significance==1), dates=NULL, ylim=NULL) {
    p = ggplot(data, aes(x=date))
    if (identical(dates,NULL)) {
      dates = as.Date(c(min(data$date),max(data$date)),"1970-01-01")
    } else {
      dates = as.Date(dates)
      if (length(dates) == 1) dates = c(dates,Sys.Date())
    }
    rects = events %>% filter(!is.na(`End date`))
    p = p + geom_rect(data=rects,mapping=aes(xmin=`Start date`,xmax=`End date`),inherit.aes = FALSE,ymin=-Inf,ymax=Inf,fill="grey90",colour="grey90")
    lines = events %>% filter(is.na(`End date`))
    p = p + geom_vline(data=lines,mapping=aes(xintercept = `Start date`),linetype="dashed",colour="grey50",show.legend = FALSE) 
    p = p +
      ggrepel::geom_text_repel(
        aes(x=`Start date`, y=Inf, label=`Label`),data=events, hjust=0,vjust=1, angle=90, show.legend = FALSE,box.padding=0.05,inherit.aes = FALSE,
        size=(7/ggplot2:::.pt/(96/72)))+
      scale_x_date(date_breaks = "2 week", date_labels = "%d-%m")
    if (!identical(ylim,NULL)) {
      p =p+coord_cartesian(xlim=dates, ylim=ylim)
    } else {
      p =p+coord_cartesian(xlim=dates)
    }
    return(p+theme(
      axis.text.x = element_text(angle=90,vjust=0.5)
    ))
  },

  plotIncidenceQuantiles = function(covidTimeseries, denominatorExpr=NA, colour=NULL, events = self$datasets$getSignificantDates() %>% filter(Significance==1), dates=NULL, ribbons=TRUE, ylim=NULL, ...) {
    df = covidTimeseriesFormat(covidTimeseries)  %>% 
      dplyr::filter(type=="incidence") %>%
      self$logIncidenceStats()
    
    denominatorExpr = enexpr(denominatorExpr)
    
    df = df %>% mutate(
      x = date,
      denom = !!denominatorExpr,
      y = ifelse(!is.na(denom), value/denom, value),
      yMid = ifelse(!is.na(denom), Est.Quantile.0.5.value/denom, Est.Quantile.0.5.value),
      yMin1 = ifelse(!is.na(denom), Est.Quantile.0.025.value/denom, Est.Quantile.0.025.value),
      yMax1 = ifelse(!is.na(denom), Est.Quantile.0.975.value/denom, Est.Quantile.0.975.value),
      yMin2 = ifelse(!is.na(denom), Est.Quantile.0.25.value/denom, Est.Quantile.0.25.value),
      yMax2 = ifelse(!is.na(denom), Est.Quantile.0.75.value/denom, Est.Quantile.0.75.value),
    )
    
    colour = tryCatch(ensym(colour),error = function(e) NULL)
    p2 = self$plotDefault(df,events,dates, ylim=ylim)+ylab("Incidence")
    
    if(!identical(colour,NULL)) {
      if(ribbons) p2 = p2 + 
        # geom_ribbon(aes(ymin=yMin1, ymax=yMax1, fill=!!colour, ...),colour = NA,alpha=0.1,show.legend = FALSE) + 
        # geom_ribbon(aes(ymin=yMin2, ymax=yMax2, fill=!!colour,...), colour = NA,alpha=0.15,show.legend = FALSE)
          geom_ribbon(aes(ymin=yMin1, ymax=yMax1, group=!!colour, ...),colour = NA,fill="black",alpha=0.05,show.legend = FALSE) +
          geom_ribbon(aes(ymin=yMin2, ymax=yMax2, group=!!colour, ...),colour = NA,fill="black",alpha=0.065,show.legend = FALSE)
      p2 = p2 + 
        geom_line(aes(y=yMid,colour=!!colour,...)) +
        geom_point(aes(y=y,colour=!!colour),size=0.5, alpha=0.5, shape=16,show.legend = FALSE)+
        geom_point(data=df %>% filter(Anomaly),mapping=aes(x=date,y=y),colour="red",size=1, alpha=1, shape=16,show.legend = FALSE)
    } else {
      if(ribbons) p2 = p2 + 
        # geom_ribbon(aes(ymin=yMin1, ymax=yMax1, ...),colour = NA,alpha=0.1,show.legend = FALSE) + 
        # geom_ribbon(aes(ymin=yMin2, ymax=yMax2, ...),colour = NA,alpha=0.15,show.legend = FALSE)
        geom_ribbon(aes(ymin=yMin1, ymax=yMax1, ...),colour = NA,fill="black",alpha=0.05,show.legend = FALSE) +
        geom_ribbon(aes(ymin=yMin2, ymax=yMax2, ...),colour = NA,fill="black",alpha=0.065,show.legend = FALSE)
      p2 = p2 + 
        geom_line(data=df,mapping=aes(x=date,y=yMid,...)) +
        geom_point(aes(y=y),colour="black",size=0.5, alpha=0.5, shape=16,show.legend = FALSE)+
        geom_point(data=df %>% filter(Anomaly),mapping=aes(x=date,y=y),colour="red",size=1, alpha=1, shape=16,show.legend = FALSE)
    }
    return(p2)
  },
  
  plotIncidenceRollmean = function(covidTimeseries, denominatorExpr=NA, events = self$datasets$getSignificantDates() %>% filter(Significance==1), dates=NULL, ylim=NULL, ...
  ) {
    df = covidTimeseriesFormat(covidTimeseries)  %>% 
      dplyr::filter(type=="incidence") %>%
      self$imputeAndWeeklyAverage()
    
    denominatorExpr = enexpr(denominatorExpr)
    
    df = df %>% mutate(
      x = date,
      denom = !!denominatorExpr,
      y = ifelse(!is.na(denom), value/denom, value),
      yMid = ifelse(!is.na(denom), RollMean.value/denom, RollMean.value)
    )
      
    p2 = self$plotDefault(df,events,dates,ylim=ylim)+ylab("Incidence")
    p2 = p2+
      geom_point(aes(y=y,...),alpha=0.5,size=0.5)+
      geom_line(aes(y=yMid,...),alpha=0.5)+
      geom_point(data=df %>% filter(Anomaly),mapping=aes(y=y),colour="red",size=1, alpha=1, shape=16,show.legend = FALSE)
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