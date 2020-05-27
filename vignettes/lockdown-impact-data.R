
if (file.exists(paste0("~/Dropbox/covid19/current-rt/rt-timeseries-",Sys.Date(),".Rdata"))) {
  
  load(file=paste0("~/Dropbox/covid19/current-rt/rt-timeseries-",Sys.Date(),".Rdata"))

} else {

  if (!exists("cfg",inherit=FALSE)) source("./covid-serial-interval.R")
  
  ts = getUKCovidTimeseries()
  
  
  ts$date = Sys.Date()
  
  message("calculating rt for UK")
  
  #ts$r0UK = ts$tidyUK %>% normaliseAndCleanse(adjustUnknowns = FALSE) %>% tidyEstimateRt(cfg, window = 7)# %>% filter(!is.na(`Median(R)`))
  ts$r0UK = ts$tidyUK %>% normaliseAndCleanse(adjustUnknowns = FALSE,smoothWeekly = TRUE) %>% tidyEstimateRt(cfg, window = 7)
  message("calculating rt for UK (using deaths)")
  ts$r0UKDeaths = ts$tidyUK %>% normaliseAndCleanse(cumulativeCasesVar = cumulative_deaths, adjustUnknowns = FALSE, smoothWeekly = TRUE) %>% tidyEstimateRt(cfg, window = 7)
  message("calculating rt for UK (using tracker data)")
  #ts$r0UKTracker = ts$tidyUK %>% normaliseAndCleanse(cumulativeCasesVar = cumulative_cases_from_tracker, adjustUnknowns = FALSE) %>% tidyEstimateRt(cfg, window = 7)
  ts$r0UKTracker = ts$tidyUK %>% normaliseAndCleanse(cumulativeCasesVar = cumulative_cases_from_tracker, adjustUnknowns = FALSE, smoothWeekly = TRUE) %>% tidyEstimateRt(cfg, window = 7) 
  
  
  message("calculating rt for UK regions")
  #ts$r0UKRegional = ts$tidyUKRegional %>% #filter(date < as.Date("2020-04-11")) %>% 
  #  group_by(uk_region) %>% normaliseAndCleanse(adjustUnknowns = FALSE) %>% tidyEstimateRt(cfg, window = 7)# %>% filter(!is.na(`Median(R)`))
  ts$r0UKRegional = ts$tidyUKRegional %>% #filter(date < as.Date("2020-04-11")) %>% 
    group_by(uk_region) %>% normaliseAndCleanse(adjustUnknowns = FALSE, smoothWeekly = TRUE) %>% tidyEstimateRt(cfg, window = 7)# %>% filter(!is.na(`Median(R)`))
  
  
  
  message("calculating rt for UK regions (using deaths)")
  ts$r0UKRegionalDeaths = ts$tidyUKRegional %>% #filter(date < as.Date("2020-04-11")) %>% 
    group_by(uk_region) %>% normaliseAndCleanse(cumulativeCasesVar = cumulative_deaths, adjustUnknowns = FALSE, smoothWeekly = TRUE) %>% tidyEstimateRt(cfg, window = 7) 
  
  
  message("calculating rt for England NHS")
  ts$r0EnglandNHS = ts$tidyEnglandNHS %>% #filter(date < as.Date("2020-04-11")) %>% 
    group_by(england_nhs_region) %>% normaliseAndCleanse(adjustUnknowns = FALSE, smoothWeekly = TRUE) %>% tidyEstimateRt(cfg, window = 7)# %>% filter(!is.na(`Median(R)`))
  message("calculating rt for England PHE")
  ts$r0EnglandPHE = ts$tidyEnglandPHE %>% #filter(date < as.Date("2020-04-11")) %>% 
    group_by(england_phe_region) %>% normaliseAndCleanse(adjustUnknowns = FALSE, smoothWeekly = TRUE) %>% tidyEstimateRt(cfg, window = 7)# %>% filter(!is.na(`Median(R)`))
  
  # message("calculating rt for UK at UTLA level")
  # ts$r0CombinedUK = ts$tidyCombinedUK %>% #filter(date < as.Date("2020-04-11")) %>% 
  #   #filter(!stringr::str_starts(code,"N")) %>% 
  #   group_by(code, name) %>% normaliseAndCleanse(adjustUnknowns = FALSE) %>% tidyEstimateRt(cfg, window=7) #, totalExpr = daily_total, unknownExpr = daily_total-sum(cumulative_cases) ) %>% tidyEstimateRt(cfg, window=7)
  # # TODO: Dates missing
  # 
  message("calculating rt for UK at LTLA level")
  ts$r0CombinedUK_LTLA = ts$tidyCombinedUK_LTLA %>% #filter(date < as.Date("2020-04-11")) %>% 
    #filter(!stringr::str_starts(code,"N")) %>% 
    group_by(code, name) %>% normaliseAndCleanse(adjustUnknowns = FALSE, smoothWeekly = TRUE) %>% tidyEstimateRt(cfg, window=7) #, totalExpr = daily_total, unknownExpr = daily_total-sum(cumulative_cases) ) %>% tidyEstimateRt(cfg, window=7)
  
  
  
  #### Add in rate of change R(t) estimates
  
  
  ## Unitary authority trajectories
  
  # * Basically select the last 5 days
  # * fit a linear model
  # * get the gradient
  
  # all UK unitary authorities and health boards
  message("calculating rates of change UK UTLA & LTLA")
  
  # ts$r0CombinedUK = ts$r0CombinedUK %>% deltaR0timeseries()
  # write_csv(ts$r0CombinedUK, "~/Git/uk-covid-datatools/vignettes/Supplementary_Rt_Timeseries_by_Unitary_Authority.csv")
  
  ts$r0CombinedUK_LTLA = ts$r0CombinedUK_LTLA %>% deltaR0timeseries()
  write_csv(ts$r0CombinedUK_LTLA, "~/Git/uk-covid-datatools/vignettes/Supplementary_Rt_Timeseries_by_Lower_Tier_Local_Authority.csv")
  
  # all UK regions
  message("calculating rates of change UK")
  ts$r0UK = ts$r0UK %>% deltaR0timeseries()
  ts$r0UKDeaths = ts$r0UKDeaths %>% deltaR0timeseries()
  ts$r0UKTracker = ts$r0UKTracker %>% deltaR0timeseries()
  
  message("calculating rates of change UK")
  ts$r0UKRegional = ts$r0UKRegional %>% deltaR0timeseries()
  ts$r0UKRegionalDeaths = ts$r0UKRegionalDeaths %>% deltaR0timeseries()
  
  # england NHS and PHE regions only
  ts$r0EnglandNHS = ts$r0EnglandNHS %>% deltaR0timeseries()
  ts$r0EnglandPHE = ts$r0EnglandPHE %>% deltaR0timeseries()
  
  ts$r0CombinedUK = ts$r0CombinedUK_LTLA %>% ungroup()
  ts$r0EnglandNHS = ts$r0EnglandNHS %>% ungroup()
  ts$r0EnglandPHE = ts$r0EnglandPHE %>% ungroup()
  ts$r0UKRegional = ts$r0UKRegional %>% ungroup()

  save(ts, file=paste0("~/Dropbox/covid19/current-rt/rt-timeseries-",Sys.Date(),".Rdata"))

}
