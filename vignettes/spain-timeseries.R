spainTimeseries <- read_csv(
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vTMvdD_oSWf_JK3kbcJsr4U6RO8KUQVJT067V8DXFhmbZQTTjtlbJOVHJT-UQ7vWxFXzj-E6RqU6r4l/pub?gid=634361737&single=true&output=csv",
  col_types = cols(date = col_date(format = "%Y-%m-%d")))


spainTimeseries = spainTimeseries %>% mutate(
  P_hospital_admission_given_confirmed = hospitalised/confirmed,
  P_ITU_admission_given_confirmed = ITU/confirmed,
)

ggplot(spainTimeseries, aes(x=date,y=P_hospital_admission_given_confirmed, colour=age))+geom_line()
ggplot(spainTimeseries, aes(x=date,y=P_ITU_admission_given_confirmed, colour=age))+geom_line()

approxAgeDist = spainTimeseries %>% group_by(age) %>% summarise(
  P_hospital_admission_given_confirmed = mean(P_hospital_admission_given_confirmed),
  P_ITU_admission_given_confirmed = mean(P_ITU_admission_given_confirmed)
)

p_hosp_5to85 = approxAgeDist$P_hospital_admission_given_confirmed
p_hosp = spline(seq(5,85,10), p_hosp_5to85, xout=seq(2.5,82.5,5))
labels = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+')
p_hosp_by_age = tibble(
  ageCategory = labels,
  probability = p_hosp$y
)
View(p_hosp_by_age)
write.csv(p_hosp_by_age, "~/Dropbox/covid19/ventilator-demand/P_admission_given_confirmed_by_age.csv")

p_itu_5to85 = approxAgeDist$P_ITU_admission_given_confirmed
p_itu_5to85[9] = p_itu_5to85[8]
#p_itu_5to85[10] = p_itu_5to85[8]
#p_itu_5to85[11] = p_itu_5to85[8]
p_itu = spline(seq(5,85,10), p_itu_5to85, xout=seq(2.5,82.5,5))
labels = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+')
p_itu_by_age = tibble(
  ageCategory = labels,
  probability = p_itu$y
)
View(p_itu_by_age)
write.csv(p_itu_by_age, "~/Dropbox/covid19/ventilator-demand/P_itu_given_confirmed_by_age.csv")



# spainTimeseries = spainTimeseries %>% mutate(
#   logConfirmed = log(confirmed+1),
#   logHospitalised = log(hospitalised+1),
#   logITU = log(ITU+1),
#   logDied = log(died+1),
#   confirmedCohortDate = as.integer(date-max(date),"days"),
#   hospitalisedCohortDate = confirmedCohortDate-3,
#   ituCohortDate = confirmedCohortDate-4,
#   diedCohortDate = confirmedCohortDate-5,
# )
# 
# spainIncidence = spainTimeseries %>% group_by(age) %>% arrange(date) %>% mutate(
#   confirmedIncidence = confirmed,#-lag(confirmed),
#   hospIncidence = hospitalised,-lag(hospitalised),
#   ituIncidence = ITU,#-lag(ITU),
#   diedIncidence = died,#-lag(died),
# )
# 
# confirmed = spainIncidence %>% select(age, date=confirmedCohortDate, incidence=confirmedIncidence) %>% filter(!is.na(incidence))
# hospitalised = spainIncidence %>% select(age, date=hospitalisedCohortDate, admitted=hospIncidence) %>% filter(!is.na(admitted))
# itu = spainIncidence %>% select(age, date=ituCohortDate, admittedITU=ituIncidence) %>% filter(!is.na(admittedITU))
# died = spainIncidence %>% select(age, date=diedCohortDate, died=diedIncidence) %>% filter(!is.na(died))
# 
# spainCohort = confirmed %>% 
#   full_join(hospitalised,by=c("age","date")) %>%
#   full_join(itu,by=c("age","date")) %>%
#   full_join(died,by=c("age","date"))
# 
# 
# spainTimeseries %>% group_by(age) %>% group_modify(function(d,g,...) {
#   
#   lmConf = lm(logConfirmed~confirmedCohortDate, data=d)
#   lmHospitalised = lm(logHospitalised~confirmedCohortDate, data=d)
#   lmITU = lm(logITU~confirmedCohortDate, data=d)
#   lmDied = lm(logDied~confirmedCohortDate, data=d)
#   
#   return(tibble(
#     confLogSlope = lmConf$coefficients[[2]],
#     hospitalisedLogSlope = lmHospitalised$coefficients[[2]],
#     ITULogSlope = lmITU$coefficients[[2]],
#     diedLogSlope = lmDied$coefficients[[2]]
#   ))
# })