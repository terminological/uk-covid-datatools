
if (!exists("ts",inherits=FALSE) || is.null(ts$date) || ts$date < Sys.Date()) {
  
  source("./covid-serial-interval.R")
  
  ts = getUKCovidTimeseries()
  
  ts$date = Sys.Date()
  ts$r0UKRegional = ts$tidyUKRegional %>% #filter(date < as.Date("2020-04-11")) %>% 
    group_by(uk_region) %>% normaliseAndCleanse(adjustUnknowns = FALSE) %>% tidyEstimateRt(cfg, window = 7)# %>% filter(!is.na(`Median(R)`))
  ts$r0EnglandNHS = ts$tidyEnglandNHS %>% #filter(date < as.Date("2020-04-11")) %>% 
    group_by(england_nhs_region) %>% normaliseAndCleanse(adjustUnknowns = FALSE) %>% tidyEstimateRt(cfg, window = 7)# %>% filter(!is.na(`Median(R)`))
  ts$r0CombinedUK = ts$tidyCombinedUK %>% #filter(date < as.Date("2020-04-11")) %>% 
    #filter(!stringr::str_starts(code,"N")) %>% 
    group_by(code, name) %>% normaliseAndCleanse(adjustUnknowns = FALSE) %>% tidyEstimateRt(cfg, window=7) #, totalExpr = daily_total, unknownExpr = daily_total-sum(cumulative_cases) ) %>% tidyEstimateRt(cfg, window=7)
  # TODO: Dates missing
  
  ts$r0UKRegionalDeaths = ts$tidyUKRegional %>% #filter(date < as.Date("2020-04-11")) %>% 
    group_by(uk_region) %>% normaliseAndCleanse(cumulativeCasesVar = cumulative_deaths, adjustUnknowns = FALSE) %>% tidyEstimateRt(cfg, window = 7) 
  
  #### Add in rate of change R(t) estimates
  
  
  ## Unitary authority trajectories
  
  # * Basically select the last 5 days
  # * fit a linear model
  # * get the gradient
  
  # all UK unitary authorities and health boards
  ts$r0CombinedUK = ts$r0CombinedUK %>% deltaR0timeseries()
  
  write_csv(ts$r0CombinedUK, "~/Git/uk-covid-datatools/vignettes/Supplementary_Rt_Timeseries_by_Unitary_Authority.csv")
  
  # england NHS regions only
  ts$r0EnglandNHS = ts$r0EnglandNHS %>% deltaR0timeseries()
  
  # all UK regions
  ts$r0UKRegional = ts$r0UKRegional %>% deltaR0timeseries()
  ts$r0UKRegionalDeaths = ts$r0UKRegionalDeaths %>% deltaR0timeseries()
  
  ts$r0CombinedUK = ts$r0CombinedUK %>% ungroup()
  ts$r0EnglandNHS = ts$r0EnglandNHS %>% ungroup()
  ts$r0UKRegional = ts$r0UKRegional %>% ungroup()

}
