#### Serial intervals from the literature ----

serialIntervals = read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRdVV2wm6CcqqLAGymOLGrb8JXSe5muEOotE7Emq9GHUXJ1Fu2Euku9d2LhIIK5ZvrnGsinH11ejnUt/pub?gid=0&single=true&output=csv")

unk=function(x) ifelse(is.na(x),"unk",sprintf("%1.2f",x))
conf=function(x,xmin,xmax) return(paste0(unk(x),"\n(",unk(xmin),"-",unk(xmax),")"))

#TODO: remove this from here
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

#### Calculate the mean serial intervals ----

wtSIs = serialIntervals %>% filter(assumed_distribution == "gamma") %>% summarise(
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

cfg = EpiEstim::make_config(list(
  mean_si = wtSIs$mean_si, 
  std_mean_si = wtSIs$std_mean_si,
  min_mean_si = wtSIs$min_mean_si, 
  max_mean_si = wtSIs$max_mean_si,
  std_si = wtSIs$std_si, 
  std_std_si = wtSIs$std_si,
  min_std_si = wtSIs$min_std_si, 
  max_std_si = wtSIs$max_std_si), method="uncertain_si")


## How to you reconstruct a distribution from a cfg object?
# EpiEstim::discr_si
# sds = qgamma(seq(5,195,5)/200,cfg$$std_si, cfg$std_std_si)
# means = qgamma(seq(5,195,5)/200,cfg$mean_si, cfg$std_mean_si)
# out = rep(0,21)
# pgamma()


