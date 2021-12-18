#' General timeseries processing
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
  
  getBasicConfig = function(priorR0=1, priorR0Sd=2, quick=TRUE,...) {stop("abstract definition")},
  getCustomConfigs = function(period=20,priorR0=1, priorR0Sd=2, quick=TRUE,...) {stop("abstract definition")},
  
  getSummary = function(confint = c(0.025,0.975)) {stop("abstract definition")},
  #getQuantiles = function(q = c(0.025,0.25,0.5,0.75,0.975)) {stop("abstract definition")},
  isUncertain = function() {stop("abstract definition")},
  
  printSerialInterval = function(confint = c(0.025,0.975)) {
    ci = floor((confint[2]-confint[1])*100)
    sum = self$getSummary(confint)
    if(self$isUncertain()) {
        sum = sum %>% mutate(
          ci = ci,
          output = sprintf("%s distribution, with a mean plus %1d%% confidence interval of %1.2f days (%1.2f; %1.2f), and a standard deviation of %1.2f days (%1.2f; %1.2f)",
                distName, ci, meanOfMean, minOfMean, maxOfMean, meanOfSd, minOfSd, maxOfSd)
        )
    } else {
      sum = sum %>% mutate(
        ci = ci,
        output = sprintf("%s distribution, with a mean of the serial interval of %1.2f days, and the standard deviation of %1.2f days",
                         distName, meanOfMean, meanOfSd)
      )
    }
    if (!sum %>% is_grouped_df()) return(sum %>% pull(output))
    return(sum %>% ungroup() %>% select(-distName,-minOfMean,-minOfSd,-maxOfMean,-maxOfSd,-meanOfMean,-meanOfSd,-sdOfMean,-sdOfSd, -ci))
  },
  
  getInfectivityProfile = function(period = 20, quick = FALSE) {
    # will always start with zero. index of numbers is zero based, values represent P(case|time<index)
    # using nomenclature from Wallinga (2008), y is infectivity profile, a is time points
    a = as.numeric(0:(period-1))
    tmp = self$getCustomConfigs(period,quick = quick)
    if(quick) {
      tmp = tmp %>% mutate(y = map(config, ~matrix(.x$cfg$si_distr),ncol=1), a=list(a)) %>% select(-config)
    } else {
      tmp = tmp %>% mutate(y = map(config, ~.x$si_sample), a=list(a)) %>% select(-config)
    }
    return(tmp)
  }

))

## Fitted SI provider ----

FittedSerialIntervalProvider = R6::R6Class("FittedSerialIntervalProvider", inherit=SerialIntervalProvider, public = list(
  
  dfit = NULL,
  summary = NULL,
  confint = NULL,
  
  initialize = function(providerController, offset=0, dfit=NULL, ...) {
    super$initialize(providerController, offset, ...)
    if(!identical(dfit,NULL) & dfit$shifted !=0) stop("Cannot handle shifted distributions")
    self$dfit = dfit
  },
  
  getSummary = function(confint = c(0.025,0.975),distName = "gamma") {
    if (identical(self$summary,NULL) | any(self$confint != confint)) {
      self$summary = self$dfit$printDistributionSummary(confint = confint) %>% filter(dist == distName)
      self$confint = confint
    }
    tmp = self$summary %>% select(-shift,-`Mean (95% CI)`,-Distribution) %>% pivot_wider(names_from=param,names_glue = "{.value}_of_{param}",values_from=c(lower,upper,mean,sd))
    return(tmp %>% rename(
      distName = dist,
      meanOfMean = mean_of_mean, 
      sdOfMean = sd_of_mean,
      minOfMean = lower_of_mean,
      maxOfMean = upper_of_mean,
      meanOfSd = mean_of_sd, 
      sdOfSd = sd_of_sd, 
      minOfSd = lower_of_sd, 
      maxOfSd = upper_of_sd
    ) %>% group_by(!!!self$dfit$grps))
  },
  
  # getQuantiles = function(q = c(0.025,0.25,0.5,0.75,0.975)) {
  #   self$dfit$calculateQuantiles(q) %>% filter(dist=="gamma") %>% select(probability,Mean.quantile)
  # },
  
  getBasicConfig = function(priorR0=1, priorR0Sd=2, quick=TRUE,...) {
    tmp2 = self$dfit$printDistributionSummary() %>% select(-shift,-`Mean (95% CI)`,-Distribution) %>% pivot_wider(names_from=param,names_glue = "{.value}_of_{param}",values_from=c(lower,upper,mean,sd))
    tmp3 = tmp2 %>% group_modify(function(d,g,...) {
      if(!self$isUncertain() | quick) {
        cfg = EpiEstim::make_config(list(
          mean_si = d$mean_of_mean %>% unname(), 
          std_si = d$mean_of_sd %>% unname(), 
          mean_prior = priorR0,
          std_prior = priorR0Sd, seed=101, mcmc_control = EpiEstim::make_mcmc_control(seed=101)), method= "parametric_si")
        method= "parametric_si"
      } else {
        cfg = EpiEstim::make_config(list(
          mean_si = d$mean_of_mean %>% unname(), 
          std_mean_si = d$sd_of_mean %>% unname(), 
          min_mean_si = d$lower_of_mean %>% unname(), 
          max_mean_si = d$upper_of_mean %>% unname(), 
          std_si = d$mean_of_sd %>% unname(), 
          std_std_si = d$sd_of_sd %>% unname(), 
          min_std_si = d$lower_of_sd %>% unname(), 
          max_std_si = d$upper_of_sd %>% unname(), 
          mean_prior = priorR0,
          std_prior = priorR0Sd,
          n1 = 100, seed=101, mcmc_control = EpiEstim::make_mcmc_control(seed=101)),method = "uncertain_si")
        method = "uncertain_si"
      }
      return(tibble(config=list(list(cfg = cfg, method = method))))
    })
    return(tmp3 %>% group_by(!!!self$dfit$grps))
  },
  
  getCustomConfigs = function(period=20, priorR0=1, priorR0Sd=2, quick=TRUE, ...) {
    
    if(!self$isUncertain() | quick) {
      
      tmp = self$dfit$discreteProbabilities(q = 0:(period-1), summarise = TRUE, truncate=TRUE)
      
      out = tmp %>% group_by(!!!self$dfit$grps) %>% summarise(config = list(list(
        cfg = EpiEstim::make_config(list(
          si_distr = Mean.discreteProbability,
          mean_prior = priorR0,
          std_prior = priorR0Sd, seed=101, mcmc_control = EpiEstim::make_mcmc_control(seed=101)), method= "non_parametric_si"),
        method= "non_parametric_si",
        si_sample = NULL 
      )))
    
    } else {
      
      tmp = self$dfit$discreteProbabilities(q = 0:(period-1), summarise = FALSE, truncate=TRUE)
      
      # browser()
      out = tmp %>% group_by(!!!self$dfit$grps) %>% 
        group_modify(function(d,g,...) {
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
    return(out %>% group_by(!!!self$dfit$grps))
  },
  
  isUncertain = function() TRUE
  
))

#### Parametric SI provider ----

#' General timeseries processing
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
#' @export
NonParametricSerialIntervalProvider = R6::R6Class("NonParametricSerialIntervalProvider", inherit= SerialIntervalProvider, public = list(
  
  bootstrapSamples = NULL,
  uncertain = NULL,
  grps = NULL,
  
  initialize = function(providerController, samples, offset=0,...) {
    
    if(!is.data.frame(samples)) samples = tibble(value=samples)
    if(!"bootstrapNumber" %in% colnames(samples)) samples = samples %>% mutate(bootstrapNumber = 1)
    if(!"value" %in% colnames(samples)) stop("must have a value column")
    if(any(is.na(samples$value))) stop("NAs in samples")
    samples = samples %>% group_by(across(c(-bootstrapNumber,-value)))
    self$grps = samples %>% groups()
    super$initialize(providerController,offset,...)
    self$bootstrapSamples = samples
    self$uncertain = length(unique(samples$bootstrapNumber))>1
  },
  
  isUncertain = function() {self$uncertain},
  
  getSummary = function(confint = c(0.025,0.975)) {
    self$bootstrapSamples %>% # ungroup() %>% #TODO: allow differently grouped estimates
      group_by(bootstrapNumber, .add=TRUE) %>% 
      filter(value > 0) %>%
      summarise(mean1 = mean(value), sd1=sd(value), .groups="drop_last") %>%
      summarise(
        meanOfMean = mean(mean1), 
        sdOfMean = sd(mean1),
        minOfMean = quantile(mean1,confint[1]),
        maxOfMean = quantile(mean1,confint[2]),
        meanOfSd = mean(sd1), 
        sdOfSd = sd(sd1),
        minOfSd = quantile(sd1,confint[1]),
        maxOfSd = quantile(sd1,confint[2]), 
        .groups="drop_last"
      ) %>%
      mutate(distName = "truncated empirical") %>% group_by(!!!self$grps)
  },
  
  getBasicConfig = function(priorR0=1, priorR0Sd=2, quick=TRUE,...) {
    tmp = self$getCustomConfigs(period = floor(quantile(self$bootstrapSamples$value,0.95)),priorR0, priorR0Sd, quick,....)
    return(tmp)
  },
  
  getCustomConfigs = function(
      period=20, 
      priorR0=1, priorR0Sd=2, quick=TRUE, ...) {
    
    boots = 1:max(self$bootstrapSamples$bootstrapNumber)
    bins = 0:period
    
    if(!self$isUncertain() | quick) {
      
      tmp = self$bootstrapSamples %>% #ungroup() %>%
        filter(value > 0 & value < period) %>%
        #group_by(bootstrapNumber) %>%
        mutate(bin = ceiling(value)) %>%
        group_by(bin, .add=TRUE) %>%
        summarise(discreteCount = n(), .groups="drop_last") %>%
        mutate(discreteProbability = discreteCount/sum(discreteCount)) %>%
        tidyr::complete(bin = bins,fill=list(discreteProbability=0))
        
      out = tmp %>% group_by(!!!self$grps) %>% group_modify(function(d,g,...) {
        si_distr = d %>% arrange(bin) %>% pull(discreteProbability)
        tmp3 = list(list(
          cfg = EpiEstim::make_config(list(
            si_distr = si_distr,
            mean_prior = priorR0,
            std_prior = priorR0Sd, seed=101, mcmc_control = EpiEstim::make_mcmc_control(seed=101)), method= "non_parametric_si"),
          method= "non_parametric_si",
          si_sample = NULL 
        ))
        return(tibble(config=tmp3))
      })
      
    } else {
      
      tmp = self$bootstrapSamples %>% #ungroup() %>%
        filter(value > 0 & value < period) %>%
        mutate(bin = ceiling(value)) %>%
        group_by(bootstrapNumber,bin, .add=TRUE) %>%
        summarise(discreteCount = n(), .groups="drop_last") %>%
        group_by(bootstrapNumber, .add=TRUE) %>%
        mutate(discreteProbability = discreteCount/sum(discreteCount)) %>%
        tidyr::complete(bin = bins,fill=list(discreteProbability=0))
      
      out = tmp %>% group_by(!!!self$grps) %>% group_modify(function(d,f,...) {
        
        si_distr = d %>% arrange(bin, bootstrapNumber) %>% pull(discreteProbability) 
        tmp2 = si_distr %>% matrix(ncol = max(d$bootstrapNumber), byrow=TRUE)
        # tmp2 is the matrix of probabilities that is needed by jepidemic
        
        tmp3 = list(list(
          cfg = EpiEstim::make_config(list(
            mean_prior = priorR0,
            std_prior = priorR0Sd,
            n1 = max(tmp$bootstrapNumber), seed=101, mcmc_control = EpiEstim::make_mcmc_control(seed=101)),method = "si_from_sample"),
          method = "si_from_sample",
          si_sample = tmp2
        ))
        return(tibble(config=tmp3))
      })
      
    }
    return(out %>% group_by(!!!self$grps))
  }
  
))

#### Static defintions ----
  
SerialIntervalProvider$printSerialIntervalSources = function(serialIntervals = ukcovidtools::serialIntervalEstimates) {
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
  

SerialIntervalProvider$resampledSerialInterval = function(providerController, resamples = ukcovidtools::serialIntervalResampling %>% select(-source,-N), ...) {
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

SerialIntervalProvider$generationIntervalCombined = function(providerController, bootstrapsDf = ukcovidtools::generationIntervalsCombined, epiestimMode = FALSE, collapseGroups = FALSE, ...) {
  if (collapseGroups) {
    bootstrapsDf = bootstrapsDf %>% group_by(across(c(-param,-value,-dist))) %>% mutate(newBoot = cur_group_id()) %>% 
      select(bootstrapNumber = newBoot, param, value, dist)
  }
  genIntFit = DistributionFit$new()
  genIntFit$fromBootstrappedDistributions(fittedDistributions = bootstrapsDf)
  psip = FittedSerialIntervalProvider$new(providerController, dfit = genIntFit)
  return(psip)
}

SerialIntervalProvider$generationIntervalHart2020Empirical = function(providerController, resamples = ukcovidtools::generationIntervalsHart2021Empirical, ...) {
  npsip = NonParametricSerialIntervalProvider$new(providerController = providerController,samples = force(resamples)) 
  return(npsip)
}





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
    suppressWarnings(
      truncNorm$withSingleDistribution(dist = "tnorm", paramDf = d, bootstraps = 100, statistic = g$statistic)
    )
  )
  return(FittedSerialIntervalProvider$new(providerController = dataProviderController,offset = 0,dfit = truncNorm))
}
  
SerialIntervalProvider$fixedGamma = function(dataProviderController, mean = 5, sd = 4) {
  gamma = DistributionFit$new("gamma")
  suppressWarnings(
    gamma$withSingleDistribution(dist = "gamma",paramDf = tibble(param=c("mean","sd"), mean=c(mean,sd), sd=c(NA,NA), upper=c(NA,NA), lower=c(NA,NA)), bootstraps = 1)
  )
  return(FittedSerialIntervalProvider$new(providerController = dataProviderController,offset = 0,dfit = gamma))
}

SerialIntervalProvider$uncertainGamma = function(dataProviderController, meanOfMean = 5, sdOfMean = 0.5, meanOfsd = 4, sdOfSd = 0.2) {
  gamma = DistributionFit$new("gamma")
  suppressWarnings(
    gamma$withSingleDistribution(dist = "gamma",paramDf = tibble(param=c("mean","sd"), mean=c(meanOfMean,meanOfSd), sd=c(sdOfMean,sdOfSd), upper=c(NA,NA), lower=c(NA,NA)), bootstraps = 100)
  )
  return(FittedSerialIntervalProvider$new(providerController = dataProviderController,offset = 0,dfit = gamma))
}

SerialIntervalProvider$default = function(dataProviderController,...) {
  out = dataProviderController$getSaved("SERIAL-INTERVAL-DEFAULT",...,orElse = function() {
     SerialIntervalProvider$resampledSerialInterval(dataProviderController)
  })
  out$controller = dataProviderController
  return(out)
}

