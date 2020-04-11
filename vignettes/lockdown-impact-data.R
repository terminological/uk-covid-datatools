#### Serial intervals from the literature ----

serialIntervals = tibble(
  mean_si_estimate = c(3.96, 6.3, 4.22, 4.56, 3.95, 5.21, 4.7, 7.5,6.6),
  mean_si_estimate_low_ci = c(3.53, 5.2, 3.43, 2.69,-4.47, -3.35, 3.7, 5.3, 0.7),
  mean_si_estimate_high_ci = c(4.39, 7.6, 5.01, 6.42, 12.51,13.94, 6.0, 19.0, 19.0),
  std_si_estimate = c(4.75,4.2, 0.4, 0.95, 4.24, 4.32, 2.3, 3.4, NA),
  std_si_estimate_low_ci = c(4.46, 3.1, NA, NA, 4.03, 4.06, 1.6, NA, NA),
  std_si_estimate_high_ci = c(5.07, 5.3, NA, NA, 4.95, 5.58, 3.5, NA, NA),
  sample_size = c(468,48,135,93,45,54,28,16,90),
  population = c("China", "Shenzhen","Taijin","Singapore","Taijin","Singapore", "SE Asia", "Wuhan","Italy"),
  source = c(
    "Zhanwei Du et al. Serial Interval of COVID-19 among Publicly Reported Confirmed Cases. Emerging Infectious Disease journal 26, (2020)",
    "Bi, Q. et al. Epidemiology and Transmission of COVID-19 in Shenzhen China: Analysis of 391 cases and 1,286 of their close contacts. Infectious Diseases (except HIV/AIDS) (2020) doi:10.1101/2020.03.03.20028423",
    "Tindale, L. et al. Transmission interval estimates suggest pre-symptomatic spread of COVID-19. Epidemiology (2020) doi:10.1101/2020.03.03.20029983",
    "Tindale, L. et al. Transmission interval estimates suggest pre-symptomatic spread of COVID-19. Epidemiology (2020) doi:10.1101/2020.03.03.20029983",
    "Ganyani, T. et al. Estimating the generation interval for COVID-19 based on symptom onset data. Infectious Diseases (except HIV/AIDS) (2020) doi:10.1101/2020.03.05.20031815",
    "Ganyani, T. et al. Estimating the generation interval for COVID-19 based on symptom onset data. Infectious Diseases (except HIV/AIDS) (2020) doi:10.1101/2020.03.05.20031815",
    "Nishiura, H., Linton, N. M. & Akhmetzhanov, A. R. Serial interval of novel coronavirus (COVID-19) infections. Int. J. Infect. Dis. (2020) doi:10.1016/j.ijid.2020.02.060",
    "Li, Q. et al. Early Transmission Dynamics in Wuhan, China, of Novel Coronavirus-Infected Pneumonia. N. Engl. J. Med. (2020) doi:10.1056/NEJMoa2001316",
    "Cereda, D. et al. The early phase of the COVID-19 outbreak in Lombardy, Italy. arXiv [q-bio.PE] (2020)")
)

unk=function(x) ifelse(is.na(x),"unk",x)

SItable1 = serialIntervals %>% mutate(
  `Mean SI\n(95% CrI) days`=paste0(mean_si_estimate,"\n(",unk(mean_si_estimate_low_ci),"-",
                                   unk(mean_si_estimate_high_ci),")"),
  `Std SI\n(95% CrI) days`=paste0(unk(std_si_estimate),"\n(",unk(std_si_estimate_low_ci),"-",unk(std_si_estimate_high_ci),")")
) %>% select(-contains("estimate")) %>% select(
  `Reference`=source,
  `Mean SI\n(95% CrI) days`,
  `Std SI\n(95% CrI) days`,
  `N`=sample_size,
  `Population`=population
)

#### Calculate the mean serial intervals ----

wtSIs = serialIntervals %>% summarise(
  mean_si = weighted.mean(mean_si_estimate,sample_size,na.rm = TRUE),
  min_mean_si = weighted.mean(mean_si_estimate_low_ci,sample_size,na.rm = TRUE),
  max_mean_si = weighted.mean(mean_si_estimate_high_ci,sample_size,na.rm = TRUE),
  std_si  = weighted.mean(ifelse(is.na(std_si_estimate_low_ci),NA,1)*std_si_estimate,sample_size,na.rm = TRUE),
  min_std_si  = weighted.mean(std_si_estimate_low_ci,sample_size,na.rm = TRUE),
  max_std_si  = weighted.mean(std_si_estimate_high_ci,sample_size,na.rm = TRUE)
  #total = sum(sample_size)
) %>% mutate(
  std_mean_si = (max_mean_si - min_mean_si) / 3.92, # TODO: fit gamma
  std_std_si = (max_std_si - min_std_si) / 3.92
)

#### Construct a R_t timeseries for Regional breakdowns ----

ts = ukcovidtools::getUKCovidTimeseries()

cfg = EpiEstim::make_config(list(
  mean_si = wtSIs$mean_si, 
  std_mean_si = wtSIs$std_mean_si,
  min_mean_si = wtSIs$min_mean_si, 
  max_mean_si = wtSIs$max_mean_si,
  std_si = wtSIs$std_si, 
  std_std_si = wtSIs$std_si,
  min_std_si = wtSIs$min_std_si, 
  max_std_si = wtSIs$max_std_si), method="uncertain_si")

ts$r0UKRegional = ts$tidyUKRegional %>% group_by(uk_region) %>% normaliseAndCleanse() %>% tidyEstimateRt(cfg, window = 5) %>% filter(!is.na(`Median(R)`))
ts$r0EnglandNHS = ts$tidyEnglandNHS %>% group_by(england_nhs_region) %>% normaliseAndCleanse() %>% tidyEstimateRt(cfg, window = 5) %>% filter(!is.na(`Median(R)`))
ts$r0CombinedUK = ts$tidyCombinedUK %>% group_by(code, name) %>% normaliseAndCleanse() %>% tidyEstimateRt(cfg, window=5)

#### Add in rate of change R(t) estimates

deltaR0timeseriesFn = function(toFit) {
  tmp = toFit %>% group_modify(function(d,g,...) {
    if ((min(d$date)+10)>max(d$date)) return(tibble(date=as.Date(NA),slope=as.double(NA),slopeLowerCi=as.double(NA),slopeUpperCi=as.double(NA),r_squared=as.double(NA)))
    endDate = seq((min(d$date)+10),max(d$date),1)
    r0s = sapply(endDate, function(x) d$r[d$date > x-10 & d$date <=x]) # hopefully a vector of vectors
    dates = sapply(endDate, function(x) d$date[d$date > x-10 & d$date <=x])
    out = NULL
    
    for (i in 1:ncol(r0s)) {
      # cant be arsed trying to vectorise this.
      date=as.Date(dates[,i],origin=as.Date("1970-01-01"))
      r=r0s[,i]
      lmResult = lm(r~date,data=tibble(r=r,date=date))
      out = out %>% bind_rows(tibble(
        date = max(date),
        slope = summary(lmResult)$coefficients[[2]],
        slopeLowerCi = as.double(confint(lmResult, "date", level=0.95)[[1]]),
        slopeUpperCi = as.double(confint(lmResult, "date", level=0.95)[[2]]),
        r_squared = summary(lmResult)$r.squared
      ))
    }
    return(out)
  })
  return(tmp)
}

## Unitary authority trajectories

# * Basically select the last 5 days
# * fit a linear model
# * get the gradient

# all UK unitary authorities and health boards
toFit = ts$r0CombinedUK %>% select(code, name, date, r=`Median(R)`) %>% filter(!is.na(r)) %>% group_by(code,name)
ts$r0CombinedUK = ts$r0CombinedUK %>% left_join(deltaR0timeseriesFn(toFit) , by=c("code","name","date"))

write_csv(ts$r0CombinedUK, "~/Git/uk-covid-datatools/vignettes/Supplementary_Rt_Timeseries_by_Unitary_Authority.csv")

# england NHS regions only
toFit = ts$r0EnglandNHS%>% select(england_nhs_region, date, r=`Median(R)`) %>% filter(!is.na(r)) %>% group_by(england_nhs_region)
ts$r0EnglandNHS = ts$r0EnglandNHS %>% left_join(deltaR0timeseriesFn(toFit) , by=c("england_nhs_region","date"))

# all UK regions
toFit = ts$r0UKRegional %>% select(uk_region, date, r=`Median(R)`) %>% filter(!is.na(r)) %>% group_by(uk_region)
ts$r0UKRegional = ts$r0UKRegional %>% left_join(deltaR0timeseriesFn(toFit) , by=c("uk_region","date"))

ts$UKregional = ts$UKregional %>% ungroup()
ts$r0EnglandNHS = ts$r0EnglandNHS %>% ungroup()
ts$r0UKRegional = ts$r0UKRegional %>% ungroup()

keyDates = tibble(
  date = c(as.Date(c(
    #"2020-03-13",
    "2020-03-16",
    #"2020-03-19",
    "2020-03-23","2020-03-27")), max(ts$r0UKRegional$date, na.rm=TRUE)),
  event = c(#"Inpatient only testing",
    "Social isolation of vulnerable",
    #"Travel ban / school closure",
    "Social distancing","One SI post lockdown",
    "Latest")
) %>% mutate(label = paste0(date,": \n",event))

# markup the timeseries data with key dates:

ts$r0CombinedUK = ts$r0CombinedUK %>% left_join(keyDates, by="date")
ts$r0EnglandNHS = ts$r0EnglandNHS %>% left_join(keyDates, by="date")
ts$r0UKRegional = ts$r0UKRegional %>% left_join(keyDates, by="date")

rm(toFit)