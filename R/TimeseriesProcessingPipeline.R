#' General timeseries processing
#' @import ggplot2
#' @import msm
#' @export
TimeseriesProcessingPipeline = R6::R6Class("TimeseriesProcessingPipeline", inherit=CovidTimeseriesProvider, public = list(
  
  rtConfig = NULL,
  rtMethod = NULL,
  rtSources = NULL,
  rtWindow = NULL,
  
  
  initialize = function(providerController, ...) {
    super$initialize(providerController, ...)
    self$setSerialInterval()
  },
  

  
  #### type conversion functions ----
  
  incidenceFromCumulative = function(covidTimeseries, ...) {
    self$getHashCached(object = covidTimeseries, operation = "INC-FROM-CUM", ... , orElse = function (ts, ...) {covidTimeseriesFormat %def% {
      dplyr::bind_rows(
        ts %>% dplyr::filter(type == "incidence"),
        ts %>% 
          dplyr::filter(type == "cumulative") %>% 
          dplyr::group_by(code,codeType,name,source,subgroup,statistic,gender,ageCat) %>% 
          dplyr::arrange(date) %>%
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
          dplyr::group_by(code,codeType,name,source,subgroup,statistic,gender,ageCat) %>% 
          dplyr::arrange(date) %>%
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
  
  standardGrouping = function(ts, ...) {
    vars = ensyms(...)
    grps = sapply(c("code","codeType","name","source","subgroup","statistic","gender","ageCat","type"),as.symbol)
    grps = grps[!(names(grps) %in% sapply(vars,as_label))]
    return(ts %>% dplyr::group_by(!!!grps))
  },
  
  aggregateAge = function(covidTimeseries, fn = sum, ...) {covidTimeseriesFormat %def% {
    tmp = covidTimeseriesFormat(covidTimeseries)
    
    errors = tmp %>% 
      self$standardGrouping(ageCat) %>%
      #dplyr::group_by(code,codeType,name,source,gender,statistic,subgroup,type) %>%
      dplyr::summarise(mixed = any(is.na(ageCat)) & any(!is.na(ageCat))) %>%
      dplyr::filter(mixed==TRUE)
    if(nrow(errors) > 0) warning("aggregating by age, but some groups have mixed NAs and values. You maybe wanted to filter out the NAs:\n", paste(capture.output(print(errors)), collapse = "\n"))
    
    
    tmp= tmp %>% dplyr::mutate(ageCat=NA)
    tmp = tmp %>% 
      dplyr::group_by(code,codeType,name,source,subgroup,statistic,gender,ageCat,type,date) %>% 
      dplyr::summarise(value = fn(value, ...)) %>%
      dplyr::mutate(value = ifelse(is.nan(value),NA,value))
    return(tmp %>% dplyr::ungroup())
  }},
  
  aggregateGender = function(covidTimeseries, fn = sum, ...) {covidTimeseriesFormat %def% {
    tmp = covidTimeseriesFormat(covidTimeseries)
    
    errors = tmp %>% 
      dplyr::group_by(code,codeType,name,source,subgroup,statistic,ageCat,type) %>%
      dplyr::summarise(mixed = any(is.na(gender)) & any(!is.na(gender))) %>%
      dplyr::filter(mixed==TRUE)
    if(nrow(errors) > 0) warning("aggregating by gender, but some groups have mixed NAs and values. You maybe wanted to filter out the NAs:\n", paste(capture.output(print(errors)), collapse = "\n"))
    
    tmp= tmp %>% dplyr::mutate(gender=NA)
    tmp = tmp %>% 
      dplyr::group_by(code,codeType,name,source,subgroup,statistic,gender,ageCat,type,date) %>% 
      dplyr::summarise(value = fn(value, ...)) %>%
      dplyr::mutate(value = ifelse(is.nan(value),NA,value))
    return(tmp %>% dplyr::ungroup())
  }},
  
  aggregateSubgroup = function(covidTimeseries, fn=sum, ...) {covidTimeseriesFormat %def% {
    tmp = covidTimeseriesFormat(covidTimeseries)
    
    errors = tmp %>% 
      dplyr::group_by(code,codeType,name,source,gender,statistic,ageCat,type) %>%
      dplyr::summarise(mixed = any(is.na(subgroup)) & any(!is.na(subgroup))) %>%
      dplyr::filter(mixed==TRUE)
    if(nrow(errors) > 0) warning("aggregating by subgroup, but not all items have a subgroup. You maybe wanted to filter out the NAs:\n", paste(capture.output(print(errors)), collapse = "\n"))
  
    tmp= tmp %>% dplyr::mutate(subgroup=NA)
    tmp = tmp %>% 
      dplyr::group_by(code,codeType,name,source,subgroup,statistic,gender,ageCat,type,date) %>% 
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
      dplyr::group_by(code,codeType,name,subgroup,gender,statistic,ageCat,type) %>%
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
  
  #' @param completeness should the mapping be complete? if the mapping is "source" complete it will only be successfull if all source codes are present when mapping to a higher region. if the mapping is target, then only if all the target codes are represented. Or both if the mapping must be complete at both ends.
  aggregateGeography = function(covidTimeseries, targetCodeTypes, completeness = "source", fn=sum, keepOriginal = TRUE, ...) {covidTimeseriesFormat %def% {
    tmp = covidTimeseriesFormat(covidTimeseries)
    mapping = self$codes$getTransitiveClosure() %>% 
      dplyr::filter(toCodeType %in% targetCodeTypes) %>%
      dplyr::semi_join(tmp, by=c("fromCode" = "code")) %>%
      dplyr::select(fromCode,fromCodeType, toCode,-toCodeType) %>%
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
          nosource = mapping %>% dplyr::filter(fromCodeType==g$fromCodeType & toCodeType == g$toCodeType) %>% dplyr::anti_join(d, by="toCode")
          # sources with no targets in the map:
          notarget = mapping %>% dplyr::filter(fromCodeType==g$fromCodeType & toCodeType == g$toCodeType) %>% dplyr::anti_join(d, by="fromCode")
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
    self$getHashCached(object = covidTimeseries, operation="IMPUTE", ... , orElse = function (ts, ...) {covidTimeseriesFormat %def% {
      tmp = ts %>%
        dplyr::group_by(code,codeType,name,source,subgroup,statistic,gender,ageCat,type) %>% 
        dplyr::arrange(date) %>%
        dplyr::mutate(
          logValue1 = log(value+1)) %>%
        self$completeAndRemoveAnomalies(valueVar = logValue1, originalValueVar = logValue1.original) %>%
        dplyr::group_by(code,codeType,name,source,subgroup,statistic,gender,ageCat,type) %>% 
        dplyr::group_modify(function(d,g,...) {
         d = d %>%
          dplyr::mutate(
            logValue1 = forecast::na.interp(logValue1)
          ) %>% 
          dplyr::mutate(
            #logValue1 = stats::filter(logValue1,rep(1,7)/7)
            logValue1 = signal::sgolayfilt(logValue1,p=1,n=window) 
          )
        }) %>%
        dplyr::mutate(
          RollMean.value = ifelse(logValue1 < 0,0,exp(logValue1)-1),
          Window.RollMean.value = window
        ) %>%
        dplyr::select(-logValue1.original, -logValue1) %>%
        dplyr::ungroup() 
      #browser(expr = self$debug)
      return(tmp)
    }})
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
      self$complete(infer=FALSE) %>%
      dplyr::group_by(statistic,type,code,codeType,source,subgroup,ageCat,gender) %>% 
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
        # browser()
        return(tmp_ts)
      }) %>% 
      dplyr::ungroup()
    return(tmp)
  }}, 
  
  #' @description Calculate an estimate of rate of change of Rt using a loess
  #' 
  #' @param R0timeseries a grouped df contianing R0 timeseries including a date and a `Median(R)` column from EpiEstim
  
  smoothAndSlopeTimeseries = function(r0Timeseries, smoothExpr, window = 14,...) {covidTimeseriesFormat %def% {
    smoothExpr = enexpr(smoothExpr)
    smoothLabel = as_label(smoothExpr)
    if (paste0("Est.",smoothLabel) %in% colnames(r0Timeseries)) {
      warning(smoothLabel," has already been estimated. aborting smooth and slope.")
      return(r0Timeseries)
    }
    covidTimeseriesFormat(r0Timeseries) %>% 
      dplyr::ungroup() %>%
      dplyr::mutate(
        y = !!smoothExpr
      ) %>%
      self$completeAndRemoveAnomalies(valueVar=y, originalValueVar = y_orig, ...) %>%
      dplyr::mutate(
        x = as.integer(date-min(date)),
      ) %>%
      dplyr::group_by(statistic,type,code,codeType,source,subgroup,ageCat,gender) %>% 
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
  
  logIncidenceStats = function(covidTimeseries, valueVar = "value", growthRateWindow = 7,...) {covidTimeseriesFormat %def% {
    valueVar = ensym(valueVar)
    
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
    
    tmp = tmp %>% self$smoothAndSlopeTimeseries(!!logExpr,...)
    
    tmp[[lblV("Growth")]] = tmp[[lbl("Slope")]]
    tmp[[lblV("Growth.SE")]] =  tmp[[lbl("Slope.SE")]]
    
    tmp$interceptDate = as.Date(tmp$date - tmp[[lbl("Est")]]/tmp[[lbl("Slope")]])
    tmp$doublingTime = log(2)/tmp[[lblV("Growth")]]
    
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
      self$standardGrouping() %>% 
      dplyr::arrange(date) %>%
      dplyr::mutate(
          !!lblV("Growth.windowed") := stats::filter(x = tmp_gv, filter=rep(1/growthRateWindow,growthRateWindow), sides = 1),
          !!lblV("Growth.windowed.SE") := stats::filter(x = tmp_gv.se, filter=rep(1/growthRateWindow,growthRateWindow), sides = 1) / sqrt(growthRateWindow),
          !!lblV("Growth.windowed.Window") := growthRateWindow
      ) %>% 
      dplyr::select(-tmp_gv, -tmp_gv.se)
      
    return(tmp %>% ungroup())
  }},
  
  estimateGrowthRate = function(covidTimeseries, window = self$rtWindow, ...) {
    self$getHashCached(object = covidTimeseries, operation="ESTIM-LITTLE-R", params=list(window), ... , orElse = function (ts, ...) {covidTimeseriesFormat %def% {

      groupedDf = covidTimeseriesFormat(covidTimeseries) %>%
        ensurer::ensure_that(any(.$type == "incidence") ~ "dataset must contain incidence figures") %>%
        dplyr::filter(type == "incidence") %>%
        self$completeAndRemoveAnomalies() %>%
        dplyr::mutate(I = value) %>%
         

      groupedDf = groupedDf %>%
        dplyr::group_by(code,codeType,name,source,subgroup,statistic,gender,ageCat,type) %>%
        dplyr::arrange(date) 

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
  estimateRtQuick = function(covidTimeseries, valueVar = "RollMean.value", window = self$rtWindow, config = self$rtConfig, ...) {
    valueVar = ensym(valueVar)
    self$estimateRt(covidTimeseries, valueVar = !!valueVar, window = window, config = config, method = "parametric_si",...) %>%
      dplyr::select(-`Quantile.0.025(R)`,-`Quantile.0.25(R)`,-`Quantile.0.75(R)`,-`Quantile.0.975(R)`)
  },
  
  #' @description Calculates a survival R(t) curve on grouped data
  #' @param covidTimeseries a covid timeseries data frame
  #' @param valueVar - the column to calculate an R(t) for - usually "RollMean.value"
  #' @param config An object of class estimate_R_config, as returned by function EpiEstim::make_config.
  #' @param dateVar - the variable containing the seqence of dates
  #' @param incidenceVar - the sequence of daily incidence
  #' @param window - the width of the smoothing function applied (default 2)
  #' @return a dataframe with groupwise Rt estimates
  estimateRt = function(covidTimeseries, valueVar = "RollMean.value", window = self$rtWindow, config = self$rtConfig, method = self$rtMethod, ...) {
    valueVar = ensym(valueVar)
    self$getHashCached(object = covidTimeseries, operation="ESTIM-RT", params=list(as_label(valueVar), window, config, method), ... , orElse = function (ts, ...) {covidTimeseriesFormat %def% {
      
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
      
      groupedDf = groupedDf %>%
        dplyr::group_by(code,codeType,name,source,subgroup,statistic,gender,ageCat,type) %>%
        dplyr::arrange(date) 
        
      # tmp starts on first non zero value of I in group
      tmp2 = groupedDf %>% group_modify(function(d,g) {
        tmp = d %>% dplyr::select(dates=date,I)
        if (nrow(d) < 2+window) errors = paste0(ifelse(is.na(errors),"",paste0(errors,"; ")),"Not enough data to calculate R(t)")
        if (any(!is.na(d$errors))) {return(d)}

        siConfig = config
        d = d %>% dplyr::mutate(seq_id=row_number())

        siConfig$t_start = c(2:(nrow(tmp)-window))
        siConfig$t_end = siConfig$t_start+window
        warn = NA

        #TODO: https://cran.rstudio.com/web/packages/tibbletime/vignettes/TT-03-rollify-for-rolling-analysis.html
        tmp4 = #suppressWarnings(EpiEstim::estimate_R(d,method = method,config=siConfig,...))
          withCallingHandlers(
            tryCatch(EpiEstim::estimate_R(tmp, method = method,config=siConfig), error = stop), warning= function(w) {
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
      return(tmp2 %>% select(-I))
      
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
  
  defaultUKAssumptions = function() {
    return(tibble::tribble(
      ~startDate, ~Mean.Prior.R0, ~SE.Prior.R0, ~Adj.Mean.SerialInterval, ~Adj.SD.SerialInterval,
      "2020-01-01", 2.5, 0.5, 1.0, 1.0,
      "2020-03-23", 0.7, 0.1, 1.0, 0.75,
      "2020-07-04", 1.2, 0.1, 1.0, 1.5,
    ) %>% dplyr::mutate(startDate = as.Date(startDate)))
  },
  
  estimateRtWithAssumptions = function(covidTimeseries, 
              valueVar = "RollMean.value", window = self$rtWindow, config = self$rtConfig, method = self$rtMethod,
              assumptions = self$defaultUKAssumptions(), 
              dateRange = as.Date(c(min(covidTimeseries$date),max(covidTimeseries$date)),"1970-01-01"), 
              ...) {
    
    valueVar = ensym(valueVar)
    self$getHashCached(object = covidTimeseries, operation="ESTIM-RT-ASSUM", params=list(as_label(valueVar), window, config, method, assumptions, dateRange), ... , orElse = function (ts, ...) {covidTimeseriesFormat %def% {
      
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
      
      wtSIs = config
      assumptions = assumptions  %>% 
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
      
      
      tmp7 = assumptions %>% rowwise() %>% do({
        
        siConfig = EpiEstim::make_config(config = list(
            mean_si = wtSIs$mean_si * .$Adj.Mean.SerialInterval, 
            std_mean_si = wtSIs$std_mean_si * .$Adj.Mean.SerialInterval,
            min_mean_si = wtSIs$min_mean_si * .$Adj.Mean.SerialInterval, 
            max_mean_si = wtSIs$max_mean_si * .$Adj.Mean.SerialInterval,
            std_si = wtSIs$std_si * .$Adj.SD.SerialInterval, 
            std_std_si = wtSIs$std_std_si * .$Adj.SD.SerialInterval, 
            min_std_si = wtSIs$min_std_si* .$Adj.SD.SerialInterval, 
            max_std_si = wtSIs$max_std_si* .$Adj.SD.SerialInterval, 
            mean_prior = .$Mean.Prior.R0,
            std_prior = .$SE.Prior.R0,
            n1 = 100), method=method)
        
        
        startDate = .$startDate
        endDate = .$endDate
        
        groupedDf = groupedDf %>%
          self$standardGrouping() %>%
          dplyr::arrange(date)
        
        tmp2 = groupedDf %>% group_modify(function(d,g,...) {
          d = d %>% dplyr::arrange(date) %>% dplyr::mutate(seq_id=row_number())
          tmp = d %>% dplyr::select(dates=date,I,seq_id)
          if (nrow(d) < 2+window) errors = paste0(ifelse(is.na(errors),"",paste0(errors,"; ")),"Not enough data to calculate R(t)")
          if (any(!is.na(d$errors))) {return(d)}
          
          tmpStartDate = startDate
          tmpEndDate = endDate
          if(tmpStartDate < min(d$date)+1+window) tmpStartDate = min(d$date)+1+window
          if(tmpEndDate > max(d$date)) tmpEndDate = max(d$date)
          
          t_end = tmp %>% filter(dates >= tmpStartDate & dates <= tmpEndDate) %>% pull(seq_id)
          t_start = t_end-window
          tmp6 = tibble()
          
          
          if (length(t_end) > 0) {
          
            siConfig$t_end = t_end
            siConfig$t_start = t_start
            
            warn = NA
            
            #TODO: https://cran.rstudio.com/web/packages/tibbletime/vignettes/TT-03-rollify-for-rolling-analysis.html
            tmp4 = #suppressWarnings(EpiEstim::estimate_R(d,method = method,config=siConfig,...))
              withCallingHandlers(
                tryCatch(EpiEstim::estimate_R(tmp, method=method, config=siConfig), error = browser), warning= function(w) {
                  warn <<- w$message
                  invokeRestart("muffleWarning")
                })
            tmp5 = tmp4$R %>% mutate(
              seq_id=t_end, 
              errors=NA, 
              `Window.R`=window,
              Est.Mean.SerialInterval = siConfig$mean_si,
              SE.Mean.SerialInterval = siConfig$std_mean_si,
              Est.SD.SerialInterval = siConfig$std_si, 
              SE.SD.SerialInterval = siConfig$std_std_si,
              Prior.Mean.R0 = siConfig$mean_prior,
              Prior.SD.R0 = siConfig$std_prior
              ) #warn)
            tmp6 = d %>% dplyr::select(-errors) %>% dplyr::inner_join(tmp5, by="seq_id") %>% select(-seq_id)
          }
          # browser()
          return(tmp6)
        })
      })
      # TODO: this truncates timeseries into values that only have an R_t this is different behaviour from others. 
      return(tmp7)
    }})
  },
  
  
  summariseVolatilty = function(covidTimeseries, valueVar) {
    
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
  plotRt = function(covidRtTimeseries, colour=NULL, events=self$datasets$getSignificantDates() %>% filter(Significance==1), rtlim=c(0.5,2.5), dates="2020-03-01", 
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
          geom_ribbon(data=df,mapping=aes(x=date, ymin=`Quantile.0.25(R)`, ymax=`Quantile.0.75(R)`,...),alpha=0.1,show.legend = FALSE)+
          geom_ribbon(data=df,mapping=aes(x=date, ymin=`Quantile.0.025(R)`, ymax=`Quantile.0.975(R)`,...), alpha=0.15,show.legend = FALSE)
      }
      p2 = p2 + 
        geom_line(data=df,mapping=aes(x=date,y=`Median(R)`,...))+
        geom_point(data=df %>% filter(Anomaly),mapping=aes(x=date,y=`Median(R)`),colour="red",size=1, alpha=1, shape=16,show.legend = FALSE)
      
    } else {
      
      if(ribbons) {
        p2 = p2 + 
          geom_ribbon(data=df,mapping=aes(x=date, ymin=`Quantile.0.25(R)`, ymax=`Quantile.0.75(R)`, fill=!!colour,...),alpha=0.1,show.legend = FALSE)+
          geom_ribbon(data=df,mapping=aes(x=date, ymin=`Quantile.0.025(R)`, ymax=`Quantile.0.975(R)`, fill=!!colour,...),alpha=0.15,show.legend = FALSE)
      }
      p2 = p2 +
        geom_line(data=df,mapping=aes(x=date,y=`Median(R)`,colour=!!colour,...))+
        geom_point(data=df %>% filter(Anomaly),mapping=aes(x=date,y=`Median(R)`),colour="red",size=1, alpha=1, shape=16,show.legend = FALSE)
      
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
  plotGrowthRate = function(covidRtTimeseries, colour=NULL, events=self$datasets$getSignificantDates() %>% filter(Significance==1), rlim=c(-0.25,0.25), dates="2020-03-01", ribbons=TRUE, ...) {
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
  plotWindowedGrowthRate = function(covidRtTimeseries, colour=NULL, events=self$datasets$getSignificantDates() %>% filter(Significance==1), rlim=c(-0.25,0.25), dates="2020-03-01", ribbons=TRUE, ...) {
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
  
  plotDefault = function(data, events = dpc$datasets$getSignificantDates() %>% filter(Significance==1), dates="2020-03-01", ylim=NULL) {
    p = ggplot(data, aes(x=date))
    dates = as.Date(dates)
    if (length(dates) == 1) dates = c(dates,Sys.Date())
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

  plotIncidenceQuantiles = function(covidTimeseries, denominatorExpr=NA, colour=NULL, events = dpc$datasets$getSignificantDates() %>% filter(Significance==1), dates="2020-03-01", ribbons=TRUE, ylim=NULL, ...) {
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
        geom_ribbon(aes(ymin=yMin1, ymax=yMax1, fill=!!colour, ...),colour = NA,alpha=0.1,show.legend = FALSE) + 
        geom_ribbon(aes(ymin=yMin2, ymax=yMax2, fill=!!colour,...), colour = NA,alpha=0.15,show.legend = FALSE)
      p2 = p2 + 
        geom_line(aes(y=yMid,colour=!!colour,...)) +
        geom_point(aes(y=y,colour=!!colour),size=0.5, alpha=0.5, shape=16,show.legend = FALSE)+
        geom_point(data=df %>% filter(Anomaly),mapping=aes(x=date,y=y),colour="red",size=1, alpha=1, shape=16,show.legend = FALSE)
    } else {
      if(ribbons) p2 = p2 + 
        geom_ribbon(aes(ymin=yMin1, ymax=yMax1, ...),colour = NA,alpha=0.1,show.legend = FALSE) + 
        geom_ribbon(aes(ymin=yMin2, ymax=yMax2, ...),colour = NA,alpha=0.15,show.legend = FALSE)
      p2 = p2 + 
        geom_line(data=df,mapping=aes(x=date,y=yMid,...)) +
        geom_point(aes(y=y),colour="black",size=0.5, alpha=0.5, shape=16,show.legend = FALSE)+
        geom_point(data=df %>% filter(Anomaly),mapping=aes(x=date,y=y),colour="red",size=1, alpha=1, shape=16,show.legend = FALSE)
    }
    return(p2)
  },
  
  plotIncidenceRollmean = function(covidTimeseries, denominatorExpr=NA, events = dpc$datasets$getSignificantDates() %>% filter(Significance==1), dates="2020-03-01", ylim=NULL, ...
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
  },
  
  #### serial intervals from the literature ----
  
  setSerialInterval = function(cfg = self$defaultSerialInterval(), epiEstimConfig = cfg$config, method = cfg$method, window = cfg$window, sources = cfg$sources) {
    self$rtConfig = epiEstimConfig
    self$rtMethod = method
    self$rtWindow = window
    self$rtSources = sources
  },
  
  printSerialInterval = function(si = self$defaultSerialInterval(confint)$config, confint = c(0.1,0.9)) {
    tdp = function(w,x,y,z) sprintf("%1.2f \U00B1 %1.2f (%1.2f; %1.2f)", w, x ,y, z)
    #TODO: adjust this for non uncertain SI. Add in info about distributions...
    wtSIs = si
    ci = floor((confint[2]-confint[1])*100)
    return(list(
      mean = paste0("Serial interval mean plus ",ci,"% credible interval: ",tdp(wtSIs$mean_si, wtSIs$std_mean_si, wtSIs$min_mean_si, wtSIs$max_mean_si)),
      sd = paste0("Serial interval standard deviation plus ",ci,"% credible interval: ",tdp(wtSIs$std_si, wtSIs$std_std_si, wtSIs$min_std_si, wtSIs$max_std_si))
    ))
  },
  
  printSerialIntervalSources = function(serialIntervals = self$defaultSerialInterval()$sources) {
    unk=function(x) ifelse(is.na(x),"unk",sprintf("%1.2f",x))
    conf=function(x,xmin,xmax) return(paste0(unk(x),"\n(",unk(xmin),"-",unk(xmax),")"))
    SItable1 = serialIntervals %>% mutate(
      `Mean\n(95% CrI) days`=conf(mean_si_estimate,mean_si_estimate_low_ci,mean_si_estimate_high_ci),
      `Std\n(95% CrI) days`=conf(std_si_estimate,std_si_estimate_low_ci,std_si_estimate_high_ci),
    ) %>% select(-contains("si_estimate")) %>% select(
      `Reference`=source,
      `Statistic`=estimate_type,
      `Mean\n(95% CrI) days`,
      `Std\n(95% CrI) days`,
      `N`=sample_size,
      `Distribution` = assumed_distribution,
      `Population`=population
    )
  },
  
  defaultSerialInterval = function(...) {
    return(self$resampledSerialInterval(...))
  },
  
  resampledSerialInterval = function(confint=c(0.025,0.975),...) {
    
    bootstrap = self$getSaved("SERIAL-INTERVAL",...,orElse = function() {
      serialIntervals = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRdVV2wm6CcqqLAGymOLGrb8JXSe5muEOotE7Emq9GHUXJ1Fu2Euku9d2LhIIK5ZvrnGsinH11ejnUt/pub?gid=0&single=true&output=csv")
      out = list()
      out$sources = serialIntervals
      
      boot.samples = NULL
      set.seed(101)
      bootIterations = 250
      
      for (iteration in 1:bootIterations) {
        samples = NULL
        message(".",appendLF = FALSE)
        if (iteration %% 50==0) message(iteration)
        serialIntervals = serialIntervals %>% filter(estimate_type %>% stringr::str_starts("serial"))
        suppressWarnings(suppressMessages({
          for (i in 1:nrow(serialIntervals)) {
            
            dist = serialIntervals$assumed_distribution[[i]]
            if(dist != "empirical") {
              #browser()
              mean = serialIntervals$mean_si_estimate[[i]]
              mean_sd = (serialIntervals$mean_si_estimate_high_ci[[i]]-serialIntervals$mean_si_estimate_low_ci[[i]])/(1.96*2)
              if(is.na(mean_sd) | mean_sd < 0) mean_sd=0
              sd = serialIntervals$std_si_estimate[[i]]
              sd_sd = (serialIntervals$std_si_estimate_high_ci[[i]]-serialIntervals$std_si_estimate_low_ci[[i]])/(1.96*2)
              if(is.na(sd_sd) | sd_sd < 0) sd_sd=0
              
              N = serialIntervals$sample_size[[i]]
              
              boot_mean = rnorm(1,mean,mean_sd) %>% scales::squish(range=c(serialIntervals$mean_si_estimate_low_ci[[i]],serialIntervals$mean_si_estimate_high_ci[[i]]))
              #boot_sd = rnorm(1,sd,sd_sd) %>% scales::squish(range=c(serialIntervals$std_si_estimate_low_ci[[i]],serialIntervals$std_si_estimate_high_ci[[i]]))
              # http://www.milefoot.com/math/stat/samp-variances.htm
              boot_sd = (sd*sqrt(rchisq(1, N-1)/(N-1))) %>% scales::squish(range=c(serialIntervals$std_si_estimate_low_ci[[i]],serialIntervals$std_si_estimate_high_ci[[i]]))
              #browser()
              if (dist == "normal") {
                samples = c(samples,rnorm(N*10,boot_mean,boot_sd))
              } else if (dist == "log normal") {
                # reparametereise
                lmean = log(boot_mean/sqrt(1+(boot_sd^2)/(boot_mean^2)))
                lsd = sqrt(log(1+(boot_sd^2)/(boot_mean^2)))
                samples = c(samples,rlnorm(N*10,lmean,lsd))
              } else if (dist == "gamma") {
                scale = (boot_sd^2) / boot_mean
                shape = (boot_mean^2) / (boot_sd^2)
                samples = c(samples,rgamma(N*10,shape = shape,scale=scale))
              }
            }
          }
          #browser()
          samples = samples[samples > 0 & !is.na(samples) & samples<21]
          fit.gamma <- fitdistrplus::fitdist(samples, distr = "gamma", method = "mle")
          #summary(fit.gamma)
          fit.shape = fit.gamma$estimate[[1]]
          fit.rate = fit.gamma$estimate[[2]]
          fit.mean = fit.shape/fit.rate
          fit.sd = sqrt(fit.shape/(fit.rate)^2)
          fir.aic = fit.gamma$aic
          boot.samples = boot.samples %>% bind_rows(tibble(shape = fit.shape,rate = fit.rate,mean = fit.mean,sd=fit.sd))
        }))
      }
      out$estimates = boot.samples
      return(out)
    })
    
    sd_lowfit = min(bootstrap$estimates$sd)
    sd_highfit = max(bootstrap$estimates$sd)
    fit_data = bootstrap$estimates$sd
    
    #find the mode
    tmp_max = which.max(density(bootstrap$estimates$sd)$y)
    sd_mode = density(bootstrap$estimates$sd)$x[tmp_max]
    
    fit.std_as_norm = suppressWarnings(fitdistrplus::fitdist(fit_data, distr = "tnorm", fix.arg=
      list(
        lower=sd_lowfit,
        upper=sd_highfit,
        mean=sd_mode
      ),
      start = list(
        sd = sd(bootstrap$estimates$sd)
      ), lower=c(0), method="mle"))
    #summary(fit.gamma)
    fit.std_as_norm.sd = fit.std_as_norm$estimate[[1]]
    
    cfg = EpiEstim::make_config(list(
      mean_si = mean(bootstrap$estimates$mean), 
      std_mean_si = sd(bootstrap$estimates$mean), 
      min_mean_si = quantile(x = bootstrap$estimates$mean,confint[[1]])[[1]],
      max_mean_si = quantile(x = bootstrap$estimates$mean,confint[[2]])[[1]],
      std_si = sd_mode, #mean(bootstrap$estimates$sd), or could in theory use the mode
      std_std_si = fit.std_as_norm.sd, #sd(bootstrap$estimates$sd), 
      # The problem here is the skew of the bootstrap distribution of SD. If we use this then because epiestim resamples using truncated normal it will tend to over estimate 
      # this is now fixed by fitting normal distribution to bootstrapped sds, so these will be assymetrically distributed around fitted mean which is what we want.
      min_std_si = quantile(x = bootstrap$estimates$sd,confint[[1]])[[1]],
      max_std_si = quantile(x = bootstrap$estimates$sd,confint[[2]])[[1]],
      # min_std_si = mean(bootstrap$estimates$sd)+qnorm(confint[[1]])*sd(bootstrap$estimates$sd),
      # max_std_si = mean(bootstrap$estimates$sd)+qnorm(confint[[2]])*sd(bootstrap$estimates$sd),
      mean_prior = 1,
      std_prior = 0.5,
      n1 = 100), method="uncertain_si")
    
    bootstrap$config = cfg
    bootstrap$method = "uncertain_si"
    bootstrap$window = 7
    # Calculate the mean serial intervals
    return(bootstrap)
  },
  
  midmarketSerialInterval = function(...) {
    self$getSaved("SERIAL-INTERVAL-MIDMARKET",...,orElse = function() {
      serialIntervals = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRdVV2wm6CcqqLAGymOLGrb8JXSe5muEOotE7Emq9GHUXJ1Fu2Euku9d2LhIIK5ZvrnGsinH11ejnUt/pub?gid=0&single=true&output=csv")
      
      unk=function(x) ifelse(is.na(x),"unk",sprintf("%1.2f",x))
      conf=function(x,xmin,xmax) return(paste0(unk(x),"\n(",unk(xmin),"-",unk(xmax),")"))
      
      # Calculate the mean serial intervals
      wtSIs = serialIntervals %>% filter(assumed_distribution == "gamma" & estimate_type %>% stringr::str_starts("serial")) %>% summarise(
        mean_si = weighted.mean(mean_si_estimate,sample_size,na.rm = TRUE),
        min_mean_si = weighted.mean(mean_si_estimate_low_ci,sample_size,na.rm = TRUE),
        max_mean_si = weighted.mean(mean_si_estimate_high_ci,sample_size,na.rm = TRUE),
        std_si  = weighted.mean(ifelse(is.na(std_si_estimate_low_ci),NA,1)*std_si_estimate,sample_size,na.rm = TRUE),
        min_std_si  = weighted.mean(std_si_estimate_low_ci,sample_size,na.rm = TRUE),
        max_std_si  = weighted.mean(std_si_estimate_high_ci,sample_size,na.rm = TRUE)
        #total = sum(sample_size)
      ) %>% mutate(
        std_mean_si = (max_mean_si - min_mean_si) / 3.92, 
        std_std_si = (max_std_si - min_std_si) / 3.92
      )
      
      # SD should be distributed as chsqd which is a gamma with scale=2
      fit.sd.gamma = suppressWarnings(nls(y ~ qgamma(x, shape=shape, scale=2), data = tibble( x=c(0.025,0.5,0.975), y=c(wtSIs$min_std_si, wtSIs$std_si, wtSIs$max_std_si)) ))
      sd_shape = summary(fit.sd.gamma)$parameters[["shape",1]]
      std_std_si = sqrt(sd_shape*4) # shape*scale^2
      # ultimately this is going to be modelled by epiestim as a normal.
      
      cfg = EpiEstim::make_config(list(
        mean_si = wtSIs$mean_si, 
        std_mean_si = wtSIs$std_mean_si,
        min_mean_si = wtSIs$min_mean_si, 
        max_mean_si = wtSIs$max_mean_si,
        std_si = wtSIs$std_si, 
        std_std_si = std_std_si, # from chisquared fit of SD - #wtSIs$std_std_si - from normal fit of SD,
        min_std_si = wtSIs$min_std_si, 
        max_std_si = wtSIs$max_std_si,
        mean_prior = 1,
        std_prior = 0.5,
        n1 = 100), method="uncertain_si")
      
      bootstrap = list()
      bootstrap$sources = serialIntervals
      bootstrap$config = cfg
      bootstrap$method = "uncertain_si"
      bootstrap$window = 7
      return(bootstrap)
    })
  }
  
))



# completeTimeseries = ensurer::ensures_that(
#   .$
#   . %>% dplyr::group_by(code,codeType,name,source,subgroup,statistic,gender,ageCat,type) %>% dplyr::arrange(date) %>% dplyr::mutate(check = lead(date)==date) %>% dplyr::pull(check) %>% all(na.rm=TRUE) ~ "Dates must be unique"
#   . %>% dplyr::group_by(code,codeType,name,source,subgroup,statistic,gender,ageCat,type) %>% dplyr::arrange(date) %>% dplyr::mutate(check = lead(date)==date+1) %>% dplyr::pull(check) %>% all(na.rm=TRUE) ~ "Dates must be contiguous"
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



# defaultCovidSI = function() {
#   
#   
#   serialIntervals = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRdVV2wm6CcqqLAGymOLGrb8JXSe5muEOotE7Emq9GHUXJ1Fu2Euku9d2LhIIK5ZvrnGsinH11ejnUt/pub?gid=0&single=true&output=csv")
#   unk=function(x) ifelse(is.na(x),"unk",sprintf("%1.2f",x))
#   conf=function(x,xmin,xmax) return(paste0(unk(x),"\n(",unk(xmin),"-",unk(xmax),")"))
# 
#   #TODO: remove this from here
#   SItable1 = serialIntervals %>% mutate(
#     `Mean\n(95% CrI) days`=conf(mean_si_estimate,mean_si_estimate_low_ci,mean_si_estimate_high_ci),
#     `Std\n(95% CrI) days`=conf(std_si_estimate,std_si_estimate_low_ci,std_si_estimate_high_ci),
#   ) %>% select(-contains("si_estimate")) %>% select(
#     `Reference`=source,
#     `Statistic`=estimate_type,
#     `Mean\n(95% CrI) days`,
#     `Std\n(95% CrI) days`,
#     `N`=sample_size,
#     `Distribution` = assumed_distribution,
#     `Population`=population
#   )
#   
#   # Calculate the mean serial intervals
#   
#   wtSIs = serialIntervals %>% filter(assumed_distribution == "gamma") %>% summarise(
#     mean_si = weighted.mean(mean_si_estimate,sample_size,na.rm = TRUE),
#     min_mean_si = weighted.mean(mean_si_estimate_low_ci,sample_size,na.rm = TRUE),
#     max_mean_si = weighted.mean(mean_si_estimate_high_ci,sample_size,na.rm = TRUE),
#     std_si  = weighted.mean(ifelse(is.na(std_si_estimate_low_ci),NA,1)*std_si_estimate,sample_size,na.rm = TRUE),
#     min_std_si  = weighted.mean(std_si_estimate_low_ci,sample_size,na.rm = TRUE),
#     max_std_si  = weighted.mean(std_si_estimate_high_ci,sample_size,na.rm = TRUE)
#     #total = sum(sample_size)
#   ) %>% mutate(
#     std_mean_si = (max_mean_si - min_mean_si) / 3.92, # TODO: fit gamma
#     std_std_si = (max_std_si - min_std_si) / 3.92
#   )
#   
#   ### TODO: Think some more about prior from Rt
#   ### 
#   
#   cfg = EpiEstim::make_config(list(
#     mean_si = wtSIs$mean_si, 
#     std_mean_si = wtSIs$std_mean_si,
#     min_mean_si = wtSIs$min_mean_si, 
#     max_mean_si = wtSIs$max_mean_si,
#     std_si = wtSIs$std_si, 
#     std_std_si = wtSIs$std_si,
#     min_std_si = wtSIs$min_std_si, 
#     max_std_si = wtSIs$max_std_si,
#     mean_prior = 1,
#     std_prior = 0.5), method="uncertain_si")
#   
#   return(list(
#     config=cfg,
#     sources = SItable1,
#     method = "uncertain_si",
#     window = 7
#   ))
# 
# }
