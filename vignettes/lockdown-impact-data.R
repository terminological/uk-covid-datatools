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

#ts = ukcovidtools::getUKCovidTimeseries()
ts = getUKCovidTimeseries()

cfg = EpiEstim::make_config(list(
  mean_si = wtSIs$mean_si, 
  std_mean_si = wtSIs$std_mean_si,
  min_mean_si = wtSIs$min_mean_si, 
  max_mean_si = wtSIs$max_mean_si,
  std_si = wtSIs$std_si, 
  std_std_si = wtSIs$std_si,
  min_std_si = wtSIs$min_std_si, 
  max_std_si = wtSIs$max_std_si), method="uncertain_si")

ts$r0UKRegional = ts$tidyUKRegional %>% #filter(date < as.Date("2020-04-11")) %>% 
  group_by(uk_region) %>% normaliseAndCleanse(adjustUnknowns = FALSE) %>% tidyEstimateRt(cfg, window = 7)# %>% filter(!is.na(`Median(R)`))
ts$r0EnglandNHS = ts$tidyEnglandNHS %>% #filter(date < as.Date("2020-04-11")) %>% 
  group_by(england_nhs_region) %>% normaliseAndCleanse() %>% tidyEstimateRt(cfg, window = 7)# %>% filter(!is.na(`Median(R)`))
ts$r0CombinedUK = ts$tidyCombinedUK %>% #filter(date < as.Date("2020-04-11")) %>% 
  #filter(!stringr::str_starts(code,"N")) %>% 
  group_by(code, name) %>% normaliseAndCleanse() %>% tidyEstimateRt(cfg, window=7)
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
toFit = ts$r0UKRegional %>% select(uk_region, date, r=`Median(R)`) %>% filter(!is.na(r)) %>% group_by(uk_region)
ts$r0UKRegional = ts$r0UKRegional %>% left_join(deltaR0timeseriesFn(toFit) , by=c("uk_region","date"))

ts$r0CombinedUK = ts$r0CombinedUK %>% ungroup()
ts$r0EnglandNHS = ts$r0EnglandNHS %>% ungroup()
ts$r0UKRegional = ts$r0UKRegional %>% ungroup()



rm(toFit)