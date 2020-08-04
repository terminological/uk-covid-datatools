#' General timeseries processing
#' @import ggplot2
#' @import msm
#' @export
SerialIntervalProvider = R6::R6Class("SerialIntervalProvider", inherit=CovidTimeseriesProvider, public = list(
  
  offset = 0,
  
  initialize = function(providerController, offset = 0, ...) {
    super$initialize(providerController, ...)
    super$offset = offset
  },
  
  # TODO: implement this as an EpiEstimConfigBuilder.
  # getConfig = function(quick=FALSE, statistic="cases", dates=NULL) {stop("abstract definition")},
  # getMethod = function(quick=FALSE) {return(self$getConfig()$method)},
  
  
  getSummary = function(confint = c(0.025,0.975)) {stop("abstract definition")},
  isUncertain = function() {stop("abstract definition")},
  
  printSerialInterval = function(confint = c(0.025,0.975)) {
    ci = floor((confint[2]-confint[1])*100)
    sum = self$getSummary(confint)
    if(self$isUncertain()) {
      return(paste0(
        sum$distName, 
        sprintf(" distribution, with a mean plus %1d%% credible interval of %1.2f \U00B1 %1.2f (%1.2f; %1.2f), ",ci, 
                sum$meanOfMean, sum$sdOfMean, sum$minOfMean, sum$maxOfMean),
        sprintf("and a standard deviation of %1.2f \U00B1 %1.2f (%1.2f; %1.2f)",
                sum$meanOfSd, sum$sdOfSd, sum$minOfSd, sum$maxOfSd)
      ))
    } else {
      return(sprintf("The mean of the serial interval is %1.2f, and the standard deviation is %1.2f", sum$meanOfMean, sum$meanOfSd))
    }
  },
  
  adjustDateOutputs = function(covidRtResult, offset = self$offset) {
    return(covidRtResult %>% covidStandardGrouping() %>% mutate(
      # Shift Rt result backwards by self$offset days
      # TODO: Check exists first?
      `Mean(R)` = lead(`Mean(R)`,n = offset),
      `Std(R)` = lead(`Std(R)`,n = offset),
      `Quantile.0.025(R)` = lead(`Quantile.0.025(R)`,n = offset),
      `Quantile.0.05(R)` = lead(`Quantile.0.05(R)`,n = offset),
      `Quantile.0.25(R)` = lead(`Quantile.0.25(R)`,n = offset),
      `Median(R)` = lead(`Median(R)`,n = offset),
      `Quantile.0.75(R)` = lead(`Quantile.0.75(R)`,n = offset),
      `Quantile.0.95(R)` = lead(`Quantile.0.95(R)`,n = offset),
      `Quantile.0.975(R)` = lead(`Quantile.0.975(R)`,n = offset),
    ))
  }
  
  
  
))

FittedSerialIntervalProvider = R6::R6Class("FittedSerialIntervalProvider", inherit=SerialIntervalProvider, public = list(
  
  dfit = NULL,
  summary = NULL,
  confint = NULL,
  
  initialize = function(providerController, offset=0, dfit, ...) {
    super$initialize(providerController, offset, ...)
    if(dfit$shifted !=0) stop("Cannot handle shifted distributions")
    self$dfit = dfit
  },
  
  getSummary = function(confint = c(0.025,0.975)) {
    if (identical(self$summary,NULL) | any(self$confint != confint)) {
      self$summary = self$dfit$printDistributionSummary(confint = confint) %>% filter(dist == "gamma")
      self$confint = confint
    }
    tmp = self$summary
    return(list(
      distName = "gamma",
      meanOfMean = tmp %>% filter(param=="mean") %>% pull(mean), 
      sdOfMean = tmp %>% filter(param=="mean") %>% pull(sd),
      minOfMean = tmp %>% filter(param=="mean") %>% pull(lower),
      maxOfMean = tmp %>% filter(param=="mean") %>% pull(upper),
      meanOfSd = tmp %>% filter(param=="sd") %>% pull(mean), 
      sdOfSd = tmp %>% filter(param=="sd") %>% pull(sd), 
      minOfSd = tmp %>% filter(param=="sd") %>% pull(lower), 
      maxOfSd = tmp %>% filter(param=="sd") %>% pull(upper)
    ))
  },
  
  getBasicConfig = function(priorR0=1, priorR0Sd=2, quick=TRUE,...) {
    tmp = self$getSummary(...)
    if(!self$isUncertain() | quick) {
      cfg = EpiEstim::make_config(list(
        mean_si = tmp$meanOfMean, 
        std_si = tmp$meanOfSd, 
        mean_prior = priorR0,
        std_prior = priorR0Sd), method= "parametric_si")
      method= "parametric_si"
    } else {
      cfg = EpiEstim::make_config(list(
        mean_si = tmp$meanOfMean, 
        std_mean_si = tmp$sdOfMean, 
        min_mean_si = tmp$minOfMean, 
        max_mean_si = tmp$maxOfMean, 
        std_si = tmp$meanOfSd, 
        std_std_si = tmp$sdOfSd, 
        min_std_si = tmp$minOfSd, 
        max_std_si = tmp$maxOfSd, 
        mean_prior = priorR0,
        std_prior = priorR0Sd,
        n1 = 100),method = "uncertain_si")
      method = "uncertain_si"
    }
    return(list(config = cfg, method = method))
  },
  
  isUncertain = function() TRUE
  
))

#### Parametric SI provider ----

#' General timeseries processing
#' @import ggplot2
#' @import msm
#' @export
ParametricSerialIntervalProvider = R6::R6Class("ParametricSerialIntervalProvider", inherit=FittedSerialIntervalProvider, public = list(
  
  dist = NULL,
  paramDf = NULL,
  uncertain = NULL,
  
  initialize = function(providerController, dist, paramDf, offset=0, epiestimMode = TRUE, ...) {
    fitter = DistributionFit$new()
    #stop("TODO: add in constrint that sigma < mu or shape>1")
    fitter$withSingleDistribution(dist = dist, paramDf = parameterDefinition(paramDf), epiestimMode = epiestimMode, ...)
    shapeFilter = fitter$bootstraps %>% filter(dist=="gamma" & param=="shape" & value > 1) %>% pull(bootstrapNumber)
    fitter$bootstraps = fitter$bootstraps %>% filter(bootstrapNumber %in% shapeFilter)
    self$paramDf = parameterDefinition(paramDf)
    self$dist = dist
    # self$dfit = fitter
    super$initialize(providerController, offset = offset, dfit= fitter, ...)
    self$uncertain = any(!is.na(c(self$paramDf$sd,self$paramDf$lower,self$paramDf$upper)))
  },

  isUncertain = function() {self$uncertain},
  
  getSummary = function(confint = c(0.025,0.975)) {
    if (identical(self$summary,NULL) | any(self$confint != confint)) {
      self$summary = self$dfit$printDistributionSummary(confint = confint) %>% filter(dist == dist)
      self$confint = confint
    }
    
    tmp = self$paramDf %>% mutate(pri=1) %>% bind_rows(self$summary %>% mutate(pri=2)) %>% arrange(pri) %>% group_by(param) %>% summarise(
      mean = first(na.omit(mean)),
      sd = first(na.omit(sd)),
      lower = first(na.omit(lower)),
      upper = first(na.omit(upper))
    )
    
    return(list(
      distName = self$dist,
      meanOfMean = tmp %>% filter(param=="mean") %>% pull(mean), 
      sdOfMean = tmp %>% filter(param=="mean") %>% pull(sd),
      minOfMean = tmp %>% filter(param=="mean") %>% pull(lower),
      maxOfMean = tmp %>% filter(param=="mean") %>% pull(upper),
      meanOfSd = tmp %>% filter(param=="sd") %>% pull(mean), 
      sdOfSd = tmp %>% filter(param=="sd") %>% pull(sd), 
      minOfSd = tmp %>% filter(param=="sd") %>% pull(lower), 
      maxOfSd = tmp %>% filter(param=="sd") %>% pull(upper)
    ))
  }
  
))

#### Non-parametric SI provider ----

#' General timeseries processing
#' @import ggplot2
#' @import msm
#' @export
NonParametricSerialIntervalProvider = R6::R6Class("NonParametricSerialIntervalProvider", inherit=FittedSerialIntervalProvider, public = list(
  
  bootstrapSamples = NULL,
  uncertain = NULL,
  
  initialize = function(providerController, samples, offset=0,...) {
    
    if(!is.data.frame(samples)) stop("must be dataframe")
    if(!"bootstrapNumber" %in% colnames(samples)) stop("must have a bootstrapNumber column")
    if(!"value" %in% colnames(samples)) stop("must have a value column")
    
    dfit = DistributionFit$new(distributions = c("norm","gamma"), shifted = 0) #offset)
    dfit$models$gamma$start$shape = 1.1
    dfit$models$gamma$lower$shape = 1
    dfit$fromBootstrappedData(samples %>% ungroup() %>% select(bootstrapNumber, value))
    shapeFilter = dfit$bootstraps %>% filter(dist=="gamma" & param=="shape" & value > 1) %>% pull(bootstrapNumber)
    dfit$bootstraps = dfit$bootstraps %>% filter(bootstrapNumber %in% shapeFilter)
    #browser()
    super$initialize(providerController,offset,dfit,...)
    self$uncertain = length(unique(samples$bootstrapNumber))>1
  },
  
  isUncertain = function() {self$uncertain}
  
))



  
SerialIntervalProvider$printSerialIntervalSources = function() {
  serialIntervals = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRdVV2wm6CcqqLAGymOLGrb8JXSe5muEOotE7Emq9GHUXJ1Fu2Euku9d2LhIIK5ZvrnGsinH11ejnUt/pub?gid=0&single=true&output=csv")
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
  ) %>% group_by(Reference, Population)
  return(SItable1)
}
  

SerialIntervalProvider$resampledSerialInterval = function(providerController, shortestCredibleSI = -7, longestCredibleSI = 28, samples=100, ...) {
  
  out = providerController$getSaved("SERIAL-INTERVAL-RESAMPLE",...,orElse = function() {
    serialIntervals = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRdVV2wm6CcqqLAGymOLGrb8JXSe5muEOotE7Emq9GHUXJ1Fu2Euku9d2LhIIK5ZvrnGsinH11ejnUt/pub?gid=0&single=true&output=csv")
    boot.samples = NULL
    set.seed(101)
    # bootIterations = 250
    
    dfit = DistributionFit$new()
    
    parameterisedSIs = serialIntervals %>% 
      filter(
        estimate_type %>% stringr::str_starts("serial") &
        !assumed_distribution %in% c("empirical","unknown"),
      ) %>% 
      group_by(i = row_number()) %>% 
      group_modify(function(d,g,...) {
        paramDf = tribble(
          ~param, ~mean, ~sd, ~lower, ~upper,
          "mean", d$mean_si_estimate, NA, d$mean_si_estimate_low_ci, d$mean_si_estimate_high_ci,
          "sd", d$std_si_estimate, NA, d$std_si_estimate_low_ci, d$std_si_estimate_high_ci
        )
        dfit$withSingleDistribution(dist = d$assumed_distribution, paramDf = paramDf, bootstraps = samples, N=d$sample_size, source = g$i)
        tibble()
        # side effect in group modify. This deserves 100 Hail Mary's
    })
    
    dfit$generateSamples(sampleExpr = N)
    sampleDf = dfit$samples %>% select(bootstrapNumber, value)
    
    # include raw data for Xu et al, who did not produce parameterised estimates - only empirical distribution.
    # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7276042/bin/73153-2020.03.02.20029868-1.xlsx
    xuXls = DataProvider$new(providerController)$download(id = "XU_SERIAL_INTERVAL",url = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7276042/bin/73153-2020.03.02.20029868-1.xlsx", type = "xlsx")
    xuData = readxl::read_excel(xuXls)
    xuSamples = suppressWarnings(xuData %>% mutate(dt = as.integer(`p_Date of onset`)-as.integer(`s_Date of onset`)) %>% filter(!is.na(dt)) %>% pull(dt))
    xuSrc = max(sampleDf$source)+1
    N = length(xuSamples)*0.9
    for(i in 1:samples) {
        # raw data also available from Du et al
        # https://github.com/MeyersLabUTexas/COVID-19/blob/master/Table%20S5%20medrxiv.xlsx?raw=true
        # their analysis is included though as resulted in normal distribution
        # N.B. it is probably the same data as Xu
        sampleDf = sampleDf %>% bind_rows(tibble(
          bootstrapNumber = i, 
          value = sample(xuSamples, size=N),
          N = N,
          source = xuSrc
        ))
    }
    
    # We can now: 
    # use this to directly estimate a parameterised distribution for the serial interval with or without shift
    # Or create a set of discretised empirical distributions for epiestim to explore.
    return(sampleDf %>% group_by(N,source))
  })
  out = out %>% filter(value > shortestCredibleSI & value <= longestCredibleSI)
  npsip = NonParametricSerialIntervalProvider$new(providerController = providerController,samples = out) 
    # including an offset in here causes issues.
    #,offset = (-shortestCredibleSI))
  return(npsip)
}
          

  
SerialIntervalProvider$midmarketSerialInterval = function(providerController, epiestimMode = TRUE, ...) {
    #out = providerController$getSaved("SERIAL-INTERVAL-MIDMARKET",...,orElse = function() {
      serialIntervals = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRdVV2wm6CcqqLAGymOLGrb8JXSe5muEOotE7Emq9GHUXJ1Fu2Euku9d2LhIIK5ZvrnGsinH11ejnUt/pub?gid=0&single=true&output=csv")
      
      unk=function(x) ifelse(is.na(x),"unk",sprintf("%1.2f",x))
      conf=function(x,xmin,xmax) return(paste0(unk(x),"\n(",unk(xmin),"-",unk(xmax),")"))
      
      # Estimate the mean serial intervals,
      wtSIs = serialIntervals %>% filter(assumed_distribution == "gamma" & estimate_type %>% stringr::str_starts("serial")) %>% summarise(
        mean_si = weighted.mean(mean_si_estimate,sample_size,na.rm = TRUE),
        min_mean_si = weighted.mean(mean_si_estimate_low_ci,sample_size,na.rm = TRUE),
        max_mean_si = weighted.mean(mean_si_estimate_high_ci,sample_size,na.rm = TRUE),
        std_si  = weighted.mean(ifelse(is.na(std_si_estimate_low_ci),NA,1)*std_si_estimate,sample_size,na.rm = TRUE),
        min_std_si  = weighted.mean(std_si_estimate_low_ci,sample_size,na.rm = TRUE),
        max_std_si  = weighted.mean(std_si_estimate_high_ci,sample_size,na.rm = TRUE)
      )
      
      # SD should be distributed as chsqd which is a gamma with scale=2
      # fit.sd.gamma = suppressWarnings(nls(y ~ qgamma(x, shape=shape, scale=2), data = tibble( x=c(0.025,0.5,0.975), y=c(wtSIs$min_std_si, wtSIs$std_si, wtSIs$max_std_si)) ))
      # sd_shape = summary(fit.sd.gamma)$parameters[["shape",1]]
      # std_std_si = sqrt(sd_shape*4) # shape*scale^2
      # # ultimately this is going to be modelled by epiestim as a normal.
      
      std_mean_si = (wtSIs$max_mean_si-wtSIs$min_mean_si)/3.92 #confidence intervals all 95%
      
      paramDf = tribble(
        ~param, ~mean, ~sd, ~lower, ~upper,
        "mean", wtSIs$mean_si, NA, wtSIs$min_mean_si, wtSIs$max_mean_si,
        "sd", wtSIs$std_si, NA, wtSIs$min_std_si, wtSIs$max_std_si
      )
      
      #paramDf2 = DistributionFit$convertParameters(paramDf = paramDf)
      
      #dfit = DistributionFit$new(distributions = "gamma")
      #dfit$models$gamma$lower$shape = 1
      #dfit$withSingleDistribution(dist = "gamma",paramDf,bootstraps = 10000)
      #param = dfit$printDistributionSummary() %>% select(param,mean,sd,lower,upper)
      psip = ParametricSerialIntervalProvider$new(providerController, dist="gamma",paramDf = paramDf,offset = 0, bootstraps=10000, epiestimMode = epiestimMode)
      return(psip)

      # TODO: 
      # exportSimpleParameterisedConfig function to replicate old behaviour
      # exportQuickParameterisedConfig function to 
      # add in functions to serial interval provider to modify parameters over time and statistic - including priors
      # add in functions to DistribuitionFit to create discretised distributions for bootstrapped distributions
      # could do this as a distributionfit object on time delays between symptom and statistic.
      # EpiEstim configuration builder that is aware of offsets, priors, discretised dists and statistics
      
      # fit DistributionFit plot & work out how to do it for censored.
      # as an aside - could we use greta to infer a generational interval given an incubation period
      
      

    #})
  }
  

SerialIntervalProvider$fromFF100 = function(dataProviderController) {
  
  ff100 = dataProviderController$spim$getFF100()
  tmp = ff100 %>% inner_join(ff100, by=c("ContactOf_FF100_ID"="FF100_ID"),suffix=c(".infectee",".infector"))
  tmp = tmp %>% select(FF100_ID,ContactOf_FF100_ID,contains("date"))
  dfit = DistributionFit$new(distributions = "gamma")
  dfit$fromUncensoredData(tmp,valueExpr = as.integer(date_onset.infectee - date_onset.infector),truncate = TRUE)
  return(FittedSerialIntervalProvider$new(providerController = dataProviderController,offset = 0,dfit = dfit))
  
}

SerialIntervalProvider$default = function(dataProviderController,...) {
  out = dataProviderController$getSaved("SERIAL-INTERVAL-DEFAULT",...,orElse = function() {
    SerialIntervalProvider$resampledSerialInterval(dataProviderController)
  })
  out$controller = dataProviderController
  return(out)
}

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




#browser()
# Here is the big assumption:
# Serial interval can only be positive and can be fitted using a gamma.
# This is proven to be wrong but the question is can we do any better?

#         samples = samples[samples > 0 & !is.na(samples) & samples<21]
#           
#           
#           fit.gamma <- fitdistrplus::fitdist(samples, distr = "gamma", method = "mle")
#           #summary(fit.gamma)
#           fit.shape = fit.gamma$estimate[[1]]
#           fit.rate = fit.gamma$estimate[[2]]
#           fit.mean = fit.shape/fit.rate
#           fit.sd = sqrt(fit.shape/(fit.rate)^2)
#           fir.aic = fit.gamma$aic
#           boot.samples = boot.samples %>% bind_rows(tibble(shape = fit.shape,rate = fit.rate,mean = fit.mean,sd=fit.sd))
#         }))
#       }
#       out$estimates = boot.samples
#       return(out)
#     })
#     
#     sd_lowfit = min(bootstrap$estimates$sd)
#     sd_highfit = max(bootstrap$estimates$sd)
#     fit_data = bootstrap$estimates$sd
#     
#     #find the mode
#     tmp_max = which.max(density(bootstrap$estimates$sd)$y)
#     sd_mode = density(bootstrap$estimates$sd)$x[tmp_max]
#     
#     fit.std_as_norm = suppressWarnings(fitdistrplus::fitdist(fit_data, distr = "tnorm", fix.arg=
#       list(
#         lower=sd_lowfit,
#         upper=sd_highfit,
#         mean=sd_mode
#       ),
#       start = list(
#         sd = sd(bootstrap$estimates$sd)
#       ), lower=c(0), method="mle"))
#     #summary(fit.gamma)
#     fit.std_as_norm.sd = fit.std_as_norm$estimate[[1]]
#     
#     cfg = EpiEstim::make_config(list(
#       mean_si = mean(bootstrap$estimates$mean), 
#       std_mean_si = sd(bootstrap$estimates$mean), 
#       min_mean_si = quantile(x = bootstrap$estimates$mean,confint[[1]])[[1]],
#       max_mean_si = quantile(x = bootstrap$estimates$mean,confint[[2]])[[1]],
#       std_si = sd_mode, #mean(bootstrap$estimates$sd), or could in theory use the mode
#       std_std_si = fit.std_as_norm.sd, #sd(bootstrap$estimates$sd), 
#       # The problem here is the skew of the bootstrap distribution of SD. If we use this then because epiestim resamples using truncated normal it will tend to over estimate 
#       # this is now fixed by fitting normal distribution to bootstrapped sds, so these will be assymetrically distributed around fitted mean which is what we want.
#       min_std_si = quantile(x = bootstrap$estimates$sd,confint[[1]])[[1]],
#       max_std_si = quantile(x = bootstrap$estimates$sd,confint[[2]])[[1]],
#       # min_std_si = mean(bootstrap$estimates$sd)+qnorm(confint[[1]])*sd(bootstrap$estimates$sd),
#       # max_std_si = mean(bootstrap$estimates$sd)+qnorm(confint[[2]])*sd(bootstrap$estimates$sd),
#       mean_prior = 1,
#       std_prior = 0.5,
#       n1 = 100), method="uncertain_si")
#     
#     bootstrap$config = cfg
#     bootstrap$method = "uncertain_si"
#     bootstrap$window = 7
#     # Calculate the mean serial intervals
#     return(bootstrap)
# 



# cfg = EpiEstim::make_config(list(
#   mean_si = wtSIs$mean_si, 
#   std_mean_si = wtSIs$std_mean_si,
#   min_mean_si = wtSIs$min_mean_si, 
#   max_mean_si = wtSIs$max_mean_si,
#   std_si = wtSIs$std_si, 
#   std_std_si = std_std_si, # from chisquared fit of SD - #wtSIs$std_std_si - from normal fit of SD,
#   min_std_si = wtSIs$min_std_si, 
#   max_std_si = wtSIs$max_std_si,
#   mean_prior = 1,
#   std_prior = 0.5,
#   n1 = 100), method="uncertain_si")
# 
# bootstrap = list()
# bootstrap$sources = serialIntervals
# bootstrap$config = cfg
# bootstrap$method = "uncertain_si"
# bootstrap$window = 7
# return(bootstrap)
