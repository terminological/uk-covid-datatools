#' NHS datasets
#' @export
SyntheticDatasetProvider = R6::R6Class("SyntheticDatasetProvider", inherit=CovidTimeseriesProvider, public = list(
  
  initialize = function(providerController, ...) {
    super$initialize(providerController, ...)
  },
  
  alternatingRandom = function(n=6,meanHi=0.005,meanLo=-meanHi,sd=0.005,fn = rnorm) {
    as.vector(t(matrix(c(fn(n/2,mean=meanHi,sd=sd),fn(n/2,mean=meanLo,sd=sd)),nrow = n/2)))
  },
  
  periodicGrowthRate = function(name, dateAtTime0 = "2020-01-01", length=365, period=91, baseline=0, smooth=TRUE, amplitude=0.1, Gt.mean=5, Gt.sd=4) {
    Gt.alpha = Gt.mean^2/Gt.sd^2
    Gt.beta = Gt.mean/Gt.sd^2
    if(smooth) {
      x = 0:(length-1)
      y = sin(x/period*(2*pi))*amplitude+baseline
      tmp = tibble(
        time = x,
        date = as.Date(dateAtTime0)+time,
        Growth.actual = y
      )
    } else {
      x = seq(0,length,by = period/4)
      y = rep(c(0,1,0,-1),length.out=length(x))
      int = approx(x=x, y=y, n = length, method="linear", rule=2)
      tmp = tibble(
        time = int$x,
        date = as.Date(dateAtTime0)+time,
        #Growth.actual = rates[cut(time,breaks = c(breaks,Inf), include.lowest = TRUE)]
        Growth.actual = int$y*amplitude+baseline
      )
    }
    tmp = tmp %>% mutate(
      Rt.actual = (1+Growth.actual/Gt.beta)^Gt.alpha,
      Gt.mean = Gt.mean,
      Gt.sd = Gt.sd
    )
    events = tibble(
      breaks = seq(0,length,by = period/4),
      rates = rep(c(0,1,0,-1),length.out=length(breaks))*amplitude+baseline,
      `Start date` = as.Date(dateAtTime0)+breaks,
      `End date` = NA, 
      Label = paste0("Growth: ",sprintf("%1.3f", rates))
    )
    serial = SerialIntervalProvider$fixedGamma(self$controller,mean = Gt.mean, sd=Gt.sd)
    return(list(
      name = name,
      ts = tmp %>% mutate(source=name), 
      events = events %>% mutate(source=name),
      serial = serial,
      infectivityProfile = serial$getCustomConfigs()$config[[1]]$cfg$si_distr
    ))
  },
  
  generateGrowthRate = function(name, dateAtTime0 = "2020-01-01", length=365, breaks = sort(c(0,sample(0:364,4))), knots=breaks, rates = self$alternatingRandom(), smooth=TRUE, sawtooth=FALSE, Gt.mean=5, Gt.sd=4, ...) {
    Gt.alpha = Gt.mean^2/Gt.sd^2
    Gt.beta = Gt.mean/Gt.sd^2
    if(length(breaks) != length(rates)-1) stop("rates must be one longer than breaks")
    if(smooth) {
      sp = spline(x=c(knots,length), y = c(rates), xout=1:length)
      tmp = tibble(
        time = sp$x,
        date = as.Date(dateAtTime0)+time,
        Growth.actual = sp$y
      )
    } else {
      int = approx(x=c(breaks,length), y=c(rates), xout = 1:length, method=(if(sawtooth) "linear" else "constant"),f=1, rule=2)
      tmp = tibble(
        time = int$x,
        date = as.Date(dateAtTime0)+time,
        #Growth.actual = rates[cut(time,breaks = c(breaks,Inf), include.lowest = TRUE)]
        Growth.actual = int$y
      )
    }
    tmp = tmp %>% mutate(
      Rt.actual = (1+Growth.actual/Gt.beta)^Gt.alpha,
      Gt.mean = Gt.mean,
      Gt.sd = Gt.sd
    )
    events = tibble(
      `Start date` = as.Date(dateAtTime0)+c(breaks,length),
      `End date` = NA, 
      Label = paste0("Growth: ",sprintf("%1.3f", rates)), 
      rates=rates, 
      breaks=c(breaks,length)
      ) %>% 
      arrange(breaks)
    serial = SerialIntervalProvider$fixedGamma(self$controller,mean = Gt.mean, sd=Gt.sd)
    
    return(list(
      name = name,
      ts = tmp %>% mutate(source=name), 
      events = events %>% mutate(source=name),
      serial = serial,
      infectivityProfile = serial$getCustomConfigs()$config[[1]]$cfg$si_distr
    ))
  },
  
  generateRt = function(name, dateAtTime0 = "2020-01-01", length=365, breaks = sort(c(0,sample(0:364,5))), knots=breaks, rt = self$alternatingRandom(fn = rlnorm), smooth=TRUE, Gt.mean=5, Gt.sd=4, ...) {
    Gt.alpha = Gt.mean^2/Gt.sd^2
    Gt.beta = Gt.mean/Gt.sd^2
    if(length(breaks) != length(rt)) stop("rt must be same length as breaks")
    if(smooth) {
      sp = spline(x=c(knots,(length-1)) , y = log(c(1,rt)),n = length)
      tmp = tibble(
        time = sp$x,
        date = as.Date(dateAtTime0)+time,
        Rt.actual = exp(sp$y)
      )
    } else {
      tmp = tibble(
        time = 0:(length-1),
        date = as.Date(dateAtTime0)+time,
        Rt.actual = rates[cut(time,breaks = c(breaks,Inf),include.lowest = TRUE)]
      )
    }
    tmp = tmp %>% mutate(
      Growth.actual = (Rt.actual^(-Gt.alpha)-1)*Gt.beta,
      Gt.mean = Gt.mean,
      Gt.sd = Gt.sd
    )
    events = tibble(`Start date` = as.Date(dateAtTime0)+breaks,`End date` = NA, Label = paste0("Rt: ",sprintf("%1.3f", rt)), rt=rt, breaks=breaks) %>% arrange(breaks)
    serial = SerialIntervalProvider$fixedGamma(self$controller,mean = Gt.mean, sd=Gt.sd)
    
    return(list(
      ts = tmp %>% mutate(source=name), 
      events = events,
      serial = serial,
      infectivityProfile = serial$getCustomConfigs()$config[[1]]$cfg$si_distr
    ))
  },
  
  addImportations = function(growthRateTs, importDf = tibble(time = c(1)), rate=100) {
    if (!("import" %in% colnames(importDf))) importDf = importDf %>% mutate(import = rpois(nrow(importDf),rate))
    growthRateTs$ts = growthRateTs$ts %>% left_join(importDf, by="time") %>% mutate(import = ifelse(is.na(import),0,import))
    return(growthRateTs)
  },
  
  addPoissonRate = function(growthRateTs) {
    x = growthRateTs$ts$import[[1]]
    for(i in 2:nrow(growthRateTs$ts)) {
      last = x[length(x)]
      x = c(x, last*exp(growthRateTs$ts$Growth.actual[[i-1]])+growthRateTs$ts$import[[i]])
    }
    growthRateTs$ts = growthRateTs$ts %>% mutate(Est.actual = x)
    return(growthRateTs)
  },
  
  addBinomialRate = function(growthRateTs,baseline) {
    x = growthRateTs$ts$import[[1]]/baseline
    for(i in 2:nrow(growthRateTs$ts)) {
      last_p_t = x[length(x)]
      r_t = growthRateTs$ts$Growth.actual[[i-1]]
      x = c(x, 
            # p_{t+1} &= \frac{1}{1 + \frac{(1-p_t)}{p_t}e^{-r_t}}
            1/(1+(1-p_t)/p_t*exp(-r_t))+
              growthRateTs$ts$import[[i]]/baseline)
    }
    growthRateTs$ts = growthRateTs$ts %>% mutate(Proportion.actual = x)
    return(growthRateTs)
  },
  
  addObservedRate = function(growthRateTs, observedFraction = 1, weekendEffect = 0.1) {
    weekendProb = case_when(
      format(growthRateTs$ts$date,"%A") == "Saturday" ~ -1,
      format(growthRateTs$ts$date,"%A") == "Sunday" ~ -1,
      format(growthRateTs$ts$date,"%A") == "Monday" ~ 2,
      TRUE ~ 0
    )*weekendEffect+1
    growthRateTs$ts = growthRateTs$ts %>% mutate(Est.observed = Est.actual*weekendProb*observedFraction)
    return(growthRateTs)
  },
  
  addBootrappedObservations = function(growthRateTs, bootstraps = 100, delayHalfLife = 0, lastObservation = nrow(growthRateTs$ts)) {
    delayProb = rev(1-0.8*exp(-(0:(lastObservation-1))*log(2)/delayHalfLife))
    obsDate = growthRateTs$ts$date[[lastObservation]]
    growthRateTs$ts = growthRateTs$ts %>% 
      filter(time <= lastObservation) %>%
      mutate(
        observationDate = obsDate,
        Est.delayed = Est.observed*delayProb
      ) %>% 
      group_by_all() %>%
      summarise(
        tibble(
          subgroup = 1:bootstraps, 
          value=rpois(bootstraps,Est.delayed)
        )) %>%
      ungroup() %>%
      mutate(statistic = "case",type = "incidence", code="XYZ", name="Test",codeType = "TEST",gender=NA_character_,ageCat=NA_character_)
    return(growthRateTs)
  },
  
  getGrowthRateBasedDataset = function(
    growthRateTs = NULL,
    bootstraps = 100, 
    seed=100, 
    weekendEffect = 0.1, 
    Gt.mean=5, Gt.sd=4, 
    periodic=FALSE, 
    name = "synthetic",
    ...) {
    
    if (is.null(growthRateTs)) {
      if(periodic) {
        growthRateTs = self$periodicGrowthRate(..., name = name, Gt.mean = Gt.mean, Gt.sd = Gt.sd) 
      } else {
        growthRateTs = self$generateGrowthRate(..., name = name, Gt.mean = Gt.mean, Gt.sd = Gt.sd)
      }
    }
    
    out = growthRateTs %>%
      self$addImportations(importDf = tibble(time = 1, import = seed)) %>%
      self$addPoissonRate() %>%
      self$addObservedRate(weekendEffect = weekendEffect) %>%
      self$addBootstrappedObservations(delayHalfLife = 0)
    
    return(out)
  },
  
  getTwoAlternativesDataset = function(
    scenario1,
    scenario2,
    delayHalfLife1, 
    delayHalfLife2,
    timepoints = nrow(scenario1$ts), #c(35,45,55,100)
    ...
  ) {
    
    scenario1Ts = bind_rows(lapply(timepoints, 
        function(time) (scenario1 %>%
          self$addBootrappedObservations(delayHalfLife = delayHalfLife1, lastObservation = time))$ts
    ))
    
    scenario2Ts = bind_rows(lapply(timepoints, 
        function(time) (scenario2 %>%
           self$addBootrappedObservations(delayHalfLife = delayHalfLife2, lastObservation = time))$ts
    ))
    
    combinedTs = scenario1Ts %>% inner_join(scenario2Ts, by=c("time","date","observationDate","subgroup","statistic","type","code","name","codeType","gender","ageCat"), suffix=c(".s1",".s2")) 
    combinedTs = combinedTs %>%
      mutate(
        Growth.relative = Growth.actual.s1 - Growth.actual.s2,
        Rt.advantage = Rt.actual.s1/Rt.actual.s2,
        Proportion.actual = Est.actual.s1 / (Est.actual.s1+Est.actual.s2),
        Total.actual = Est.actual.s1+Est.actual.s2,
        proportion = value.s1/(value.s1+value.s2),
        total = value.s1+value.s2
      )
      
    combinedEvents = bind_rows(scenario1$events,scenario2$events)
    serials = list()
    serials[[scenario1$name]]=scenario1$serial
    serials[[scenario2$name]]=scenario2$serial
    ip = list()
    ip[[scenario1$name]]=scenario1$infectivityProfile
    ip[[scenario2$name]]=scenario2$infectivityProfile
    
    return(list(
      name = paste0(scenario1$name," vs ",scenario2$name),
      ts = combinedTs,
      events = combinedEvents,
      serial = serials,
      infectivityProfile = ip
    ))
    
  },
  
  sNegOmicronScenario = function(topLevel,weekendEffect) {
    gr = self$generateGrowthRate(name="sNeg cases", length = 100, breaks = c(30,40,50,70,85),rates = c(0,topLevel,topLevel,topLevel/2,-0.05,0),sawtooth = TRUE,smooth = FALSE,dateAtTime0 = as.Date("2021-10-15"))
    gr = gr %>%
      self$addImportations(tibble(time=1,import=25)) %>% 
      self$addPoissonRate() %>% 
      self$addObservedRate(weekendEffect = weekendEffect)
  },
  
  sPosOmicronScenario = function(weekendEffect) {
    gr = self$generateGrowthRate(name="sPos cases", length = 100, breaks = c(30,40),rates = c(0,0,-0.02),sawtooth = TRUE,smooth = FALSE,dateAtTime0 = as.Date("2021-10-15"))
    gr = gr %>%
      self$addImportations(tibble(time=1,import=50000)) %>% 
      self$addPoissonRate() %>% 
      self$addObservedRate(weekendEffect = weekendEffect)
  }
  #### Projections package R_t based ----
  #install.packages("projections")
  # 
  # Rt = tibble::tibble(
  #   date = as.Date(as.Date("2020-01-01"):(as.Date("2020-01-01")+99),"1970-01-01"),
  #   time_change = 1:100
  # ) %>% mutate(
  #   R = case_when(
  #     time_change <= 20 ~ 1,
  #     time_change <= 40 ~ 1.1,
  #     time_change <= 60 ~ 0.6,
  #     time_change <= 80 ~ 1,
  #     TRUE ~ 1.5,
  #   )
  # )
  # 
  # 
  # 
  # seed = incidence::incidence(dates = as.Date(unlist(lapply(as.Date("2019-12-15"):as.Date("2019-12-31"),function(x) rep(x,runif(1,95,105)))),"1970-01-01"))
  # proj_3 <- projections::project(x = seed, R = c(Rt$R,1), n_sim=1,si = Flu2009$si_distr, n_days = 200, time_change = Rt$time_change)
  # plot(proj_3)
  # tmp = as.data.frame(seed) %>% rename(date = dates, incidence=counts) %>% bind_rows(as.data.frame(proj_3,long=TRUE) %>% group_by(date) %>% summarise(incidence = mean(incidence)))
  # Rt = Rt %>% inner_join(tmp, by="date")
  # 
  # 
  # i <- incidence::incidence(dates = "2020-01-01")
  # 
  # # Rt_vec = c(
  # #   rep(4,20),
  # #   rep(2.1,20),
  # #   rep(0.5,20),
  # #   rep(1.2,20))
  # 
  # seed = incidence::incidence(dates = as.Date(unlist(lapply(as.Date("2019-12-15"):as.Date("2019-12-31"),function(x) rep(x,runif(1,95,105)))),"1970-01-01"))
  # 
  # # Rt_vec = spline(c(0,20,40,60,80,100), c(2,1,1.1,0.6,1,1.5), n=101)$y
  # 
  # Rt_vec = c(
  #   spline(c(0,20,40,50), c(1.25,1,0.75,1.5), n=51)$y,
  #   spline(c(51,60,80,100), c(3,2,0.6,1), n=50)$y
  # )
  # 
  # plot(Rt_vec)
  # 
  # proj_4 <- projections::project(seed,
  #                                R = Rt_vec,
  #                                si = Flu2009$si_distr,
  #                                n_days = length(Rt_vec),
  #                                time_change = 1:(length(Rt_vec)-1),
  #                                n_sim = 100)
  # plot(proj_4)
  # 
  # Rt = proj_4 %>% as.data.frame(row.names = TRUE) %>%
  #   mutate(R = Rt_vec) %>%
  #   pivot_longer(cols = c(-dates,-R),values_to="incidence",names_to="sim",names_pattern="([0-9]+)") %>%
  #   mutate(sim = as.integer(sim), incidence=as.numeric(incidence)) %>% 
  #   rename(date = dates)
  
 
  
))