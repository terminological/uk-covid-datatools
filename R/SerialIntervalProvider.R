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
  #getQuantiles = function(q = c(0.025,0.25,0.5,0.75,0.975)) {stop("abstract definition")},
  isUncertain = function() {stop("abstract definition")},
  
  printSerialInterval = function(confint = c(0.025,0.975)) {
    ci = floor((confint[2]-confint[1])*100)
    sum = self$getSummary(confint)
    if(self$isUncertain()) {
        sprintf("%s distribution, with a mean plus %1d%% credible interval of %1.2f days (%1.2f; %1.2f), and a standard deviation of %1.2f days (%1.2f; %1.2f)",
                sum$distName, ci, 
                sum$meanOfMean, sum$minOfMean, sum$maxOfMean,
                sum$meanOfSd, sum$minOfSd, sum$maxOfSd)
        # sprintf(" distribution, with a mean plus %1d%% credible interval of %1.2f \U00B1 %1.2f (%1.2f; %1.2f), ",ci, 
        #         sum$meanOfMean, sum$sdOfMean, sum$minOfMean, sum$maxOfMean),
        # sprintf("and a standard deviation of %1.2f \U00B1 %1.2f (%1.2f; %1.2f)",
        #         sum$meanOfSd, sum$sdOfSd, sum$minOfSd, sum$maxOfSd)
        # ))
    } else {
      return(sprintf("%s distribution, with a mean of the serial interval of %1.2f days, and the standard deviation of %1.2f days", sum$distName, sum$meanOfMean, sum$meanOfSd))
    }
  }

))

## Fitted SI provider ----

FittedSerialIntervalProvider = R6::R6Class("FittedSerialIntervalProvider", inherit=SerialIntervalProvider, public = list(
  
  dfit = NULL,
  summary = NULL,
  confint = NULL,
  
  initialize = function(providerController, offset=0, dfit, ...) {
    super$initialize(providerController, offset, ...)
    if(dfit$shifted !=0) stop("Cannot handle shifted distributions")
    self$dfit = dfit
  },
  
  getSummary = function(confint = c(0.025,0.975),distName = "gamma") {
    if (identical(self$summary,NULL) | any(self$confint != confint)) {
      self$summary = self$dfit$printDistributionSummary(confint = confint) %>% filter(dist == distName)
      self$confint = confint
    }
    tmp = self$summary
    return(tibble_row(
      distName = distName,
      meanOfMean = tmp %>% filter(param=="mean") %>% pull(mean) %>% mean(), 
      sdOfMean = tmp %>% filter(param=="mean") %>% pull(sd) %>% mean(),
      minOfMean = tmp %>% filter(param=="mean") %>% pull(lower) %>% mean(),
      maxOfMean = tmp %>% filter(param=="mean") %>% pull(upper) %>% mean(),
      meanOfSd = tmp %>% filter(param=="sd") %>% pull(mean) %>% mean(), 
      sdOfSd = tmp %>% filter(param=="sd") %>% pull(sd) %>% mean(), 
      minOfSd = tmp %>% filter(param=="sd") %>% pull(lower) %>% mean(), 
      maxOfSd = tmp %>% filter(param=="sd") %>% pull(upper) %>% mean()
    ))
  },
  
  # getQuantiles = function(q = c(0.025,0.25,0.5,0.75,0.975)) {
  #   self$dfit$calculateQuantiles(q) %>% filter(dist=="gamma") %>% select(probability,Mean.quantile)
  # },
  
  getBasicConfig = function(priorR0=1, priorR0Sd=2, quick=TRUE,...) {
    tmp = self$getSummary(...)
    if(!self$isUncertain() | quick) {
      cfg = EpiEstim::make_config(list(
        mean_si = tmp$meanOfMean, 
        std_si = tmp$meanOfSd, 
        mean_prior = priorR0,
        std_prior = priorR0Sd, seed=101, mcmc_control = EpiEstim::make_mcmc_control(seed=101)), method= "parametric_si")
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
        n1 = 100, seed=101, mcmc_control = EpiEstim::make_mcmc_control(seed=101)),method = "uncertain_si")
      method = "uncertain_si"
    }
    return(list(cfg = cfg, method = method))
  },
  
  getCustomConfigs = function(period=40, priorR0=1, priorR0Sd=2, quick=TRUE, ...) {
    
    if(!self$isUncertain() | quick) {
      
      tmp = self$dfit$discreteProbabilities(q = 0:(period-1), summarise = TRUE)
      tmp = tmp %>% bind_rows(
        tmp %>% group_by(!!!self$dfit$grps) %>% summarise(
          value = max(value)+1,
          Mean.discreteProbability=1-sum(Mean.discreteProbability)
        )
      )
      
      out = tmp %>% group_by(!!!self$dfit$grps) %>% summarise(config = list(list(
        cfg = EpiEstim::make_config(list(
          si_distr = Mean.discreteProbability,
          mean_prior = priorR0,
          std_prior = priorR0Sd, seed=101, mcmc_control = EpiEstim::make_mcmc_control(seed=101)), method= "non_parametric_si"),
        method= "non_parametric_si",
        si_sample = NULL 
      )))
    
    } else {
      
      tmp = self$dfit$discreteProbabilities(q = 0:(period-1), summarise = FALSE)
      tmp = tmp %>% bind_rows(
        tmp %>% group_by(!!!self$dfit$grps, dist, bootstrapNumber) %>% summarise(
          value = max(value)+1,
          discreteProbability=1-sum(discreteProbability)
        )
      )
      # browser()
      out = tmp %>% group_by(!!!self$dfit$grps) %>% group_modify(function(d,g,...) {
        tmp2 = d %>% arrange(value) %>% pull(discreteProbability) %>% matrix(ncol = max(tmp$bootstrapNumber), byrow=TRUE)
        tmp3 = list(
          cfg = EpiEstim::make_config(list(
            mean_prior = priorR0,
            std_prior = priorR0Sd,
            n1 = max(tmp$bootstrapNumber), seed=101, mcmc_control = EpiEstim::make_mcmc_control(seed=101)),method = "si_from_sample"),
          method = "si_from_sample",
          si_sample = tmp2
        )
        return(tibble(config = list(tmp3)))
      })
      
    }
    return(out)
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
  
  initialize = function(providerController, dist, paramDf, offset=0, epiestimMode = TRUE, allowNonZeroAtZero = FALSE, ...) {
    fitter = DistributionFit$new()
    paramDf = ukcovidtools::parameterDefinition(paramDf)
    fitter$withSingleDistribution(dist = dist, paramDf = paramDf, epiestimMode = epiestimMode, ...)
    shapeFilter = fitter$bootstraps %>% filter(dist=="gamma" & param=="shape" & value > 1) %>% pull(bootstrapNumber)
    if (!allowNonZeroAtZero) {
      fitter$bootstraps = fitter$bootstraps %>% filter(bootstrapNumber %in% shapeFilter)
      if (nrow(fitter$bootstraps) == 0) stop("No valid shaped distributions for serial interval defined. N.b. gamma shape parameters must be greater than 1 (or mean > sd)")
    }
    self$paramDf = paramDf
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
      upper = first(na.omit(upper)), 
      .groups="drop"
    )
    
    return(tibble_row(
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
    
    if(!is.data.frame(samples)) samples = tibble(value=samples)
    if(!"bootstrapNumber" %in% colnames(samples)) samples = samples %>% mutate(bootstrapNumber = 1)
    if(!"value" %in% colnames(samples)) stop("must have a value column")
    if(any(is.na(samples$value))) stop("NAs in samples")
    #browser()
    # even though this is non parametric we will fit a set of estimates to the data.
    dfit = DistributionFit$new(distributions = c("norm","gamma","nbinom"), shifted = 0) #offset)
    dfit$models$gamma$start$shape = 1.1
    dfit$models$gamma$lower$shape = 1
    dfit$models$gamma$support = c(.Machine$double.xmin,Inf)
    dfit$models$nbinom$support = c(.Machine$double.xmin,Inf)
    dfit$fromBootstrappedData(samples %>% ungroup() %>% select(bootstrapNumber, value))
    super$initialize(providerController,offset,dfit,...)
    self$bootstrapSamples = samples
    self$uncertain = length(unique(samples$bootstrapNumber))>1
  },
  
  isUncertain = function() {self$uncertain},
  
  getSummary = function(confint = c(0.025,0.975)) {
    self$bootstrapSamples %>% ungroup() %>% #TODO: allow differently grouped estimates
      group_by(bootstrapNumber) %>% 
      filter(value > 0) %>%
      summarise(mean1 = mean(value), sd1=sd(value), .groups="drop") %>%
      summarise(
        meanOfMean = mean(mean1), 
        sdOfMean = sd(mean1),
        minOfMean = quantile(mean1,confint[1]),
        maxOfMean = quantile(mean1,confint[2]),
        meanOfSd = mean(sd1), 
        sdOfSd = sd(sd1),
        minOfSd = quantile(sd1,confint[1]),
        maxOfSd = quantile(sd1,confint[2]), 
        .groups="drop"
      ) %>%
      mutate(distName = "truncated empirical")
  },
  
  getBasicConfig = function(priorR0=1, priorR0Sd=2, quick=TRUE,...) {
    tmp = self$getCustomConfigs(period = floor(quantile(self$bootstrapSamples$value,0.95)),priorR0, priorR0Sd, quick,....)
    return(tmp$config[[1]])
  },
  
  getCustomConfigs = function(
      period=floor(quantile(self$bootstrapSamples$value,0.95)), 
      priorR0=1, priorR0Sd=2, quick=TRUE, ...) {
    
    boots = 1:max(self$bootstrapSamples$bootstrapNumber)
    bins = 0:period
    
    if(!self$isUncertain() | quick) {
      
      tmp = self$bootstrapSamples %>% ungroup() %>%
        filter(value > 0 & value < period) %>%
        #group_by(bootstrapNumber) %>%
        mutate(bin = ceiling(value)) %>%
        group_by(bin) %>%
        summarise(discreteCount = n(), .groups="drop") %>%
        mutate(discreteProbability = discreteCount/sum(discreteCount)) %>%
        tidyr::complete(bin = bins,fill=list(discreteProbability=0))
        
      si_distr = tmp %>% pull(discreteProbability)
      
      tmp3 = list(list(
        cfg = EpiEstim::make_config(list(
          si_distr = si_distr,
          mean_prior = priorR0,
          std_prior = priorR0Sd, seed=101, mcmc_control = EpiEstim::make_mcmc_control(seed=101)), method= "non_parametric_si"),
        method= "non_parametric_si",
        si_sample = NULL 
      ))
      
    } else {
      
      tmp = self$bootstrapSamples %>% ungroup() %>%
        filter(value > 0 & value < period) %>%
        mutate(bin = ceiling(value)) %>%
        group_by(bootstrapNumber,bin) %>%
        summarise(discreteCount = n(), .groups="drop") %>%
        group_by(bootstrapNumber) %>%
        mutate(discreteProbability = discreteCount/sum(discreteCount))
      
      si_distr = tibble(bootstrapNumber = boots) %>% 
        left_join(
          tibble(bin = bins), by=character()
        ) %>% left_join(
          tmp %>% select(-discreteCount), by = c("bootstrapNumber","bin")
        ) %>% mutate(
          discreteProbability = ifelse(is.na(discreteProbability),0,discreteProbability)
        )
  
      tmp2 = si_distr %>% arrange(bin, bootstrapNumber) %>% pull(discreteProbability) %>% matrix(ncol = max(tmp$bootstrapNumber), byrow=TRUE)
      
      #TODO: groupings???
      tmp3 = list(list(
        cfg = EpiEstim::make_config(list(
          mean_prior = priorR0,
          std_prior = priorR0Sd,
          n1 = max(tmp$bootstrapNumber), seed=101, mcmc_control = EpiEstim::make_mcmc_control(seed=101)),method = "si_from_sample"),
        method = "si_from_sample",
        si_sample = tmp2
      ))
      
      
    }
    return(tibble(
      statistic = "case",
      config = tmp3
    ))
  }
  
))

#### Static defintions ----
  
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
  

SerialIntervalProvider$resampledSerialInterval = function(providerController, resamples = ukcovidtools::serialIntervalResampling, ...) {
  npsip = NonParametricSerialIntervalProvider$new(providerController = providerController,samples = force(resamples)) 
  return(npsip)
}
          
SerialIntervalProvider$metaAnalysis = function(providerController, metaDf = ukcovidtools::serialIntervalMetaAnalysis, epiestimMode = FALSE, ...) {
  psip = ParametricSerialIntervalProvider$new(providerController, dist="gamma",paramDf = force(metaDf), offset = 0, bootstraps=10000, epiestimMode = epiestimMode)
  return(psip)
}


SerialIntervalProvider$generationInterval = function(providerController, bootstrapsDf = ukcovidtools::generationIntervalSimulation, epiestimMode = FALSE, ...) {
  genIntFit = DistributionFit$new()
  genIntFit$fromBootstrappedDistributions(fittedDistributions = bootstrapsDf)
  psip = FittedSerialIntervalProvider$new(providerController, dfit = genIntFit)
  return(psip)
}
  
# SerialIntervalProvider$midmarketSerialInterval = function(providerController, epiestimMode = FALSE, ...) {
#     #out = providerController$getSaved("SERIAL-INTERVAL-MIDMARKET",...,orElse = function() {
#       serialIntervals = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRdVV2wm6CcqqLAGymOLGrb8JXSe5muEOotE7Emq9GHUXJ1Fu2Euku9d2LhIIK5ZvrnGsinH11ejnUt/pub?gid=0&single=true&output=csv")
#       
#       unk=function(x) ifelse(is.na(x),"unk",sprintf("%1.2f",x))
#       conf=function(x,xmin,xmax) return(paste0(unk(x),"\n(",unk(xmin),"-",unk(xmax),")"))
#       
#       # Estimate the mean serial intervals,
#       wtSIs = serialIntervals %>% filter(assumed_distribution == "gamma" & estimate_type %>% stringr::str_starts("serial")) %>% summarise(
#         mean_si = weighted.mean(mean_si_estimate,sample_size,na.rm = TRUE),
#         min_mean_si = weighted.mean(mean_si_estimate_low_ci,sample_size,na.rm = TRUE),
#         max_mean_si = weighted.mean(mean_si_estimate_high_ci,sample_size,na.rm = TRUE),
#         std_si  = weighted.mean(ifelse(is.na(std_si_estimate_low_ci),NA,1)*std_si_estimate,sample_size,na.rm = TRUE),
#         min_std_si  = weighted.mean(std_si_estimate_low_ci,sample_size,na.rm = TRUE),
#         max_std_si  = weighted.mean(std_si_estimate_high_ci,sample_size,na.rm = TRUE)
#       )
#       
#       # SD should be distributed as chsqd which is a gamma with scale=2
#       # fit.sd.gamma = suppressWarnings(nls(y ~ qgamma(x, shape=shape, scale=2), data = tibble( x=c(0.025,0.5,0.975), y=c(wtSIs$min_std_si, wtSIs$std_si, wtSIs$max_std_si)) ))
#       # sd_shape = summary(fit.sd.gamma)$parameters[["shape",1]]
#       # std_std_si = sqrt(sd_shape*4) # shape*scale^2
#       # # ultimately this is going to be modelled by epiestim as a normal.
#       
#       std_mean_si = (wtSIs$max_mean_si-wtSIs$min_mean_si)/3.92 #confidence intervals all 95%
#       
#       paramDf = tribble(
#         ~param, ~mean, ~sd, ~lower, ~upper,
#         "mean", wtSIs$mean_si, NA, wtSIs$min_mean_si, wtSIs$max_mean_si,
#         "sd", wtSIs$std_si, NA, wtSIs$min_std_si, wtSIs$max_std_si
#       )
#       
#       #paramDf2 = DistributionFit$convertParameters(paramDf = paramDf)
#       
#       #dfit = DistributionFit$new(distributions = "gamma")
#       #dfit$models$gamma$lower$shape = 1
#       #dfit$withSingleDistribution(dist = "gamma",paramDf,bootstraps = 10000)
#       #param = dfit$printDistributionSummary() %>% select(param,mean,sd,lower,upper)
#       psip = ParametricSerialIntervalProvider$new(providerController, dist="gamma",paramDf = paramDf,offset = 0, bootstraps=10000, epiestimMode = epiestimMode)
#       return(psip)
# 
#       # TODO: 
#       # exportSimpleParameterisedConfig function to replicate old behaviour
#       # exportQuickParameterisedConfig function to 
#       # add in functions to serial interval provider to modify parameters over time and statistic - including priors
#       # add in functions to DistribuitionFit to create discretised distributions for bootstrapped distributions
#       # could do this as a distributionfit object on time delays between symptom and statistic.
#       # EpiEstim configuration builder that is aware of offsets, priors, discretised dists and statistics
#       
#       # fit DistributionFit plot & work out how to do it for censored.
#       # as an aside - could we use greta to infer a generational interval given an incubation period
#       
#       
# 
#     #})
#   }

SerialIntervalProvider$truncatedNormals = function(dataProviderController, observationIntervals = force(ukcovidtools::ukCovidObservationIntervals)) {
  truncNorm = DistributionFit$new("tnorm")
  observationIntervals %>% inner_join(
    tibble(
      between = c("test","onset","onset","death","admission","admission"),
      statistic = c("case","symptom","triage","death","icu admission","hospital admission"),
    ),
    by="between"
  ) %>% 
  group_by(statistic) %>%
  summarise(
    tibble(
      param = c("mean","sd"),
      mean = c(meanOfMean, meanOfSd),
      sd = c(sdOfMean, sdOfSd),
      lower = c(lowerOfMean, lowerOfSd),
      upper = c(upperOfMean, upperOfSd),
    )
  ) %>% 
  group_walk(function(d,g,...)
    truncNorm$withSingleDistribution(dist = "tnorm", paramDf = d, bootstraps = 100, statistic = g$statistic)
  )
  return(FittedSerialIntervalProvider$new(providerController = dataProviderController,offset = 0,dfit = truncNorm))
}
  

SerialIntervalProvider$fromFF100 = function(dataProviderController) {
  
  ff100 = dataProviderController$spim$getFF100()
  tmp = ff100 %>% inner_join(ff100, by=c("ContactOf_FF100_ID"="FF100_ID"),suffix=c(".infectee",".infector"))
  tmp = tmp %>% select(FF100_ID,ContactOf_FF100_ID,contains("date"))
  dfit = DistributionFit$new(distributions = c("gamma","norm","nbinom"))
  dfit$fromUncensoredData(tmp,valueExpr = as.integer(date_onset.infectee - date_onset.infector),truncate = TRUE)
  return(FittedSerialIntervalProvider$new(providerController = dataProviderController,offset = 0,dfit = dfit))
  
}

SerialIntervalProvider$default = function(dataProviderController,...) {
  out = dataProviderController$getSaved("SERIAL-INTERVAL-DEFAULT",...,orElse = function() {
     SerialIntervalProvider$resampledSerialInterval(dataProviderController)
  })
  # out = SerialIntervalProvider$metaAnalysis(dataProviderController)
  out$controller = dataProviderController
  return(out)
}

