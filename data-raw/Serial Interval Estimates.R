
library(readxl)
here::i_am("data-raw/Serial Interval Estimates.R")

serialIntervalEstimates <- read_excel(here::here("data-raw/Serial Interval Estimates.xlsx"))
usethis::use_data(serialIntervalEstimates, overwrite = TRUE)

# Format e.g. Challen et al. 2020
# ukcovidtools::generationIntervalSimulation
# bootstrapNumber param  value dist 
# <int> <chr>  <dbl> <chr>
# 1 shape 11.0   gamma
# 1 rate   2.21  gamma

generationIntervalsSimulation = readr::read_csv(here::here("data-raw/generation-intervals/generation-intervals.csv"))
usethis::use_data(generationIntervalsSimulation, overwrite = TRUE)
generationIntervalsChallen2020 = generationIntervalsSimulation %>% mutate(
  variantSpecificGt = "pre-Alpha",
  sourceOfGt = "Challen (2020)"
)
usethis::use_data(generationIntervalsChallen2020, overwrite = TRUE)

set.seed(101)
generationIntervalsHart2020Independent = readxl::read_xlsx(here::here("data-raw/generation-intervals/GT_posterior.xlsx")) %>% sample_n(100,replace = FALSE) %>% mutate(
   shape = mean^2/sd^2,
   rate = mean/sd^2,
   bootstrapNumber = row_number()) %>% select(bootstrapNumber, shape,rate) %>% pivot_longer(
     cols = c(shape,rate),
     names_to = "param",
     values_to = "value"
   ) %>% mutate(
     dist = "gamma",
     variantSpecificGt = "pre-Alpha",
    sourceOfGt = "Hart (2021) independent"
  ) 
usethis::use_data(generationIntervalsHart2020Independent, overwrite = TRUE)

set.seed(101)
generationIntervalsHart2020Mechanistic = readxl::read_xlsx(here::here("data-raw/generation-intervals/GT_posterior_mech.xlsx")) %>% sample_n(100,replace = FALSE) %>% mutate(
  shape = mean^2/sd^2,
  rate = mean/sd^2,
  bootstrapNumber = row_number()) %>% 
  select(bootstrapNumber, shape,rate) %>% pivot_longer(
    cols = c(shape,rate),
    names_to = "param",
    values_to = "value"
  ) %>% mutate(
    dist = "gamma",
    variantSpecificGt = "pre-Alpha",
    sourceOfGt = "Hart (2021) mechanistic"
  ) 
usethis::use_data(generationIntervalsHart2020Mechanistic, overwrite = TRUE)

set.seed(101)
generationIntervalsHart2021Intrinsic = readxl::read_xlsx(here::here("data-raw/generation-intervals/Hart-personal-communication/intrinsic.xlsx")) %>% sample_n(100,replace = FALSE)
generationIntervalsHart2021Intrinsic = bind_rows(
  generationIntervalsHart2021Intrinsic %>%
    select(mean = mean_Alpha, sd = sd_Alpha) %>%
    mutate(
      bootstrapNumber = row_number(),
      shape = mean^2/sd^2,
      rate = mean/sd^2,
      dist = "gamma",
      variantSpecificGt = "Alpha",
      sourceOfGt = "Hart (2021) intrinsic"
    ),
  generationIntervalsHart2021Intrinsic %>%
    select(mean = mean_Delta, sd = sd_Delta) %>%
    mutate(
      bootstrapNumber = row_number(),
      shape = mean^2/sd^2,
      rate = mean/sd^2,
      dist = "gamma",
      variantSpecificGt = "Delta",
      sourceOfGt = "Hart (2021) intrinsic"
    )
  ) %>% select(-mean,-sd) %>% pivot_longer(
    cols = c(shape,rate),
    names_to = "param",
    values_to = "value"
  )
usethis::use_data(generationIntervalsHart2021Intrinsic, overwrite = TRUE)


set.seed(101)
generationIntervalsHart2021Household = readxl::read_xlsx(here::here("data-raw/generation-intervals/Hart-personal-communication/household.xlsx")) %>% sample_n(100,replace = FALSE)
generationIntervalsHart2021Household = bind_rows(
  generationIntervalsHart2021Household %>%
    select(mean = mean_Alpha, sd = sd_Alpha) %>%
    mutate(
      bootstrapNumber = row_number(),
      shape = mean^2/sd^2,
      rate = mean/sd^2,
      dist = "gamma",
      variantSpecificGt = "Alpha",
      sourceOfGt = "Hart (2021) household"
    ),
  generationIntervalsHart2021Household %>%
    select(mean = mean_Delta, sd = sd_Delta) %>%
    mutate(
      bootstrapNumber = row_number(),
      shape = mean^2/sd^2,
      rate = mean/sd^2,
      dist = "gamma",
      variantSpecificGt = "Delta",
      sourceOfGt = "Hart (2021) household"
    )
) %>% select(-mean,-sd) %>% pivot_longer(
  cols = c(shape,rate),
  names_to = "param",
  values_to = "value"
)
usethis::use_data(generationIntervalsHart2021Household, overwrite = TRUE)

set.seed(101)
generationIntervalsGanyani2020 = readRDS(here::here("data-raw/generation-intervals/EpiNow2_gi.rds")) %>% sample_n(100,replace = FALSE) %>% mutate(
  bootstrapNumber = row_number(),
  shape = mean^2/sd^2,
  rate = mean/sd^2,
  dist = "gamma",
  variantSpecificGt = "pre-Alpha",
  sourceOfGt = "Ganyani (2020)"
) %>% select(-mean,-sd) %>% pivot_longer(
  cols = c(shape,rate),
  names_to = "param",
  values_to = "value"
)
usethis::use_data(generationIntervalsGanyani2020, overwrite = TRUE)

# Combination
generationIntervalsCombined = bind_rows(
  generationIntervalsChallen2020, 
  generationIntervalsGanyani2020,
  generationIntervalsHart2020Independent,
  generationIntervalsHart2020Mechanistic,
  generationIntervalsHart2021Household,
  generationIntervalsHart2021Intrinsic
)
usethis::use_data(generationIntervalsCombined, overwrite = TRUE)

# 
# # Parameterised gtAssumptions
# gtAssumptions = gtAssumptions %>% mutate(
#   a = list(c(seq(0.5,19.5),25)),
#   y = purrr::pmap(list(alpha,beta,a), function(.shape,.rate,.p) { 
#     tmp = pgamma(.p,.shape,.rate)-lag(pgamma(.p,.shape,.rate),default=0) 
#     tmp = tmp/sum(tmp)
#   }),
# )
# 

## Empirical gt ----
# e.g. ukcovidtools::serialIntervalResampling
# A tibble: 124,742 x 4
# bootstrapNumber value     N sourceOfGt
# <int> <dbl> <dbl>  <dbl>
# 1  2.67    21     10
# 1  4.65    21     10
# 1  2.23    21     10
# 1  4.37    21     10

generationIntervalsHart2021Empirical = readxl::read_xlsx(here::here("data-raw/generation-intervals/Hart-personal-communication/household_GT_samples.xlsx"))
generationIntervalsHart2021Empirical = generationIntervalsHart2021Empirical %>% mutate(
    bootstrapNumber = (row_number()-1) %/% 100 + 1
  ) %>%
  pivot_longer(cols = c(Alpha,Delta), names_to = "variantSpecificGt", values_to="value") %>%
  mutate(
    sourceOfGt = "Hart (2021) empirical"
  )
usethis::use_data(generationIntervalsHart2021Empirical, overwrite = TRUE)

# 
# set.seed(101)
# 
# bootstraps = bind_rows(lapply(1:100, function(x) {
#   fn = function(gt, variantName, sgene) {
#     # browser()
#     s = tibble(x=sample(gt,size = 500,replace = TRUE))
#     breaks = c(-Inf,seq(0.5,19.5),25)
#     tab_s = s %>% 
#       mutate(x_cut = as.integer(cut(x,breaks))) %>% 
#       group_by(x_cut) %>% 
#       filter(!is.na(x_cut)) %>% # truncate
#       count() %>% 
#       ungroup() %>% 
#       complete(x_cut=1:(length(breaks)-1),fill=list(n=0)) %>%
#       mutate(p = n/sum(n)) %>% 
#       arrange(x_cut)
#     mean_s = mean(s$x)
#     sd_s = sd(s$x)
#     tibble(
#       mean = mean_s,
#       sd = sd_s,
#       alpha = NA,
#       beta = NA,
#       variant = variantName,
#       subgroup = sgene,
#       a=list(c(seq(0.5,19.5),25)),
#       y=list(tab_s$p)
#     )
#   }
#   # browser()
#   return(bind_rows(
#     fn(gtHouseholdEmpiricalVariant$Alpha,"Alpha","negative"),
#     fn(gtHouseholdEmpiricalVariant$Delta,"Delta","positive")
#   ))
# }))
# 
# gtAssumptions2 = bind_rows(gtAssumptions,bootstraps %>% mutate(sourceOfGt = "Hart (pers comm) empirical"))
# 
# p = ggplot(gtAssumptions2, aes(x=mean,y=sd,colour = variant))+geom_point(size=0.5)+facet_wrap(vars(sourceOfGt),ncol=2)+
#   xlab("Generation time mean (days)")+
#   ylab("Generation time standard deviation (days)")
# 
# standardPrintOutput::saveHalfPageFigure(p, paste0(here::here("output/SuppFig-GenerationTimeSamples-"),Sys.Date()))