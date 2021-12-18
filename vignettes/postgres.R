library(dbplyr)
library(tidyverse)

stop("Moved to episode-analysis")

here::i_am("vignettes/postgres.Rmd")

# con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "spim_test")

# con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "~/Data/covid/test_spim.sqlite3", cache_size = -4*1024*1024, extended_types = TRUE, synchronous="off")
# 
# db_copy_to(con, "mtcars", mtcars,temporary=FALSE)
# DBI::dbListTables(con)
# 
# dbmtcars = tbl(con,"mtcars")

devtools::load_all()
ukcovidtools::reload()
options("ukcovid.reproduce.at"=as.Date("2021-11-04"))
options("ukcovid.cache.debug"=TRUE)

deaths = dpc$spim$getDeathsLineList()
genomics = dpc$spim$getVAMLineList()
ctas = dpc$spim$getCTASLineList()
immunisations = dpc$spim$getVaccinationLineList()
admissions = dpc$spim$getAdmissionLineList()
cases = dpc$spim$getLineList()
sgene = dpc$spim$getSGeneLineList()

linked_genomics = dpc$spim$getLinkedGenomics()
linked_ctas = dpc$spim$getLinkedCtas()
linked_vaccinations = dpc$spim$getLinkedVaccinations()
linked_deaths = dpc$spim$getLinkedDeaths()

mpi = dpc$spim$getMasterPersonIndex(nocache = TRUE)

# TODO: linkage analysis and effect on episodes
# TODO: admit_episodes and infection episodes done differently

admit_episodes = dpc$spim$getAdmissionEpisodes()
admit_episodes_link = dpc$spim$getAdmissionEpisodesLink()
outcome_events = dpc$spim$getOutcomeEvents()
diagnosis_events = dpc$spim$getDiagnosisEvents()
vaccination_events = dpc$spim$getVaccinationEvents()

combined_genomics = dpc$spim$getCombinedGenomics()

event_timeline = dpc$spim$getEventTimeline()
augmented_event_timeline = dpc$spim$getAugmentedEventTimeline()

# TODO: lli = dpc$spim$getLineListIncidence(filterExpr = asymptomatic_indicator != "Y")

# admissions %>% filter(FINALID==-5626663) %>% glimpse()
# admit_episodes %>% filter(FINALID==-5626663) %>% glimpse()
# admit_episodes_link %>% filter(FINALID==-5626663) %>% glimpse()

# # Patient died then have a suqsequent admission
# admit_events %>% group_by(FINALID) %>% window_order(date) %>% filter(event=="death" & lead(FINALID,default = 0) == FINALID) %>% ungroup() %>% count()
# examples = admit_events %>% group_by(FINALID) %>% window_order(date) %>% filter(event=="death" & lead(FINALID,default = 0) == FINALID) %>% select(FINALID) %>% distinct()
# admissions %>% inner_join(examples, by="FINALID") %>% collect() 
# examples %>% collect() %>% readr::write_csv("~/Dropbox/covid19/exports/finalid-with-inconsistent-admissions.csv")

con = dpc$spim$getSQLLite()
deleteTempTables(con)


## MPI / Linkage analysis ----
# DESCRIBE finalid linking
linkage = readr::read_csv(paste0(dpc$spim$todayWd,"/linkage-counts.csv"))
linkage %>% group_by(cases,admissions) %>% summarise(n=sum(n))
linkage %>% group_by(cases,deaths) %>% summarise(n=sum(n))
linkage %>% group_by(cases,genomics) %>% summarise(n=sum(n))
# DEBUG mismatches
# re-read in mismatches (n.b. todayWd might not be correct directory)
mismatches = readRDS(paste0(dpc$spim$todayWd,"/mismatches.Rdata"))
mismatches$gender %>% group_by(FINALID) %>% summarise(lhs = min(tablecode), rhs=max(tablecode)) %>% group_by(lhs,rhs) %>% count() %>% View()
# quite a number of the LTLA mismatches are due to difference between LAD19 and LAD20:
# vaccinations 
mismatches$LTLA_code %>% filter(tablecode==64) %>% group_by(LTLA_code) %>% count() %>% dpc$codes$findNamesByCode(codeVar = LTLA_code) %>% arrange(desc(n)) %>% ungroup() %>% mutate(frac = n/sum(n)) %>% View()
# the ethnicity issues are less clear cut. Quite a lot of disagreement between source 4 and others also.
with(mismatches$ethnic_cat, table(tablecode,ethnic_cat))
tmp = mismatches$ethnic_cat %>% group_by(FINALID) %>% arrange(tablecode) %>% summarise(tablecode = paste0(tablecode,collapse = "|"),ethnic_cat = paste0(ethnic_cat,collapse = "|"))
tmp %>% group_by(tablecode,ethnic_cat) %>% count() %>% arrange(desc(n)) %>% View()


# testing line list incidence

devtools::load_all("~/Git/uk-covid-datatools/")
ukcovidtools::reload()

con = dpc$spim$getSQLLite()
deleteTempTables(con,"incidence_tmp")

deaths = dpc$spim$getDeathsLineList()
cases = dpc$spim$getLineList()

deaths2 = dpc$spim$getDeathsLineListIncidence(
  ageBreaks = c(1,5,15,25,45,55,65,75,85),
  deathOrReport = "death",
  subgroup = NULL
)

symptomaticCases2 = dpc$spim$getLineListIncidence(
  ageBreaks = c(1,5,15,25,45,55,65,75,85),
  filterExpr = asymptomatic_indicator != "Y",
  subgroup = NULL
)

test = deaths %>% dpc$spim$getIncidenceFromLineList(
  dateVar = dod,
  codeExpr = "E92000001", #nhser_code,
  codeType = "CTRY", #NHSER
  statistic = "death",
  ageBreaks = NULL,
  genderExpr = NA_character_,
  subgroupExpr = NA_character_,
  noteExpr = NA_character_,
  excludeExpr = (is.na(death_type28) & is.na(death_type60cod) & is.na(covidcod))
)

test %>% collect() %>% covidTimeseriesFormat() %>% View()

test2 = cases %>% dpc$spim$getIncidenceFromLineList(
  dateVar = specimen_date,
  codeExpr = NHSER_code,
  codeType = "NHSER",
  statistic = "case",
  ageBreaks = NULL,
  genderExpr = NA_character_,
  subgroupExpr = pillar,
  noteExpr = NA_character_,
  excludeExpr = asymptomatic_indicator == "Y"
)

test2 %>% collect() %>% covidTimeseriesFormat() %>% View()

tsp$plotIncidenceQuantiles(test2 %>% collect(), colour = subgroup)+facet_wrap(vars(name))+scale_y_continuous(trans="log1p")


# TODO: infection_episodes

## Event timeline testing ----
# Test plot 
event_timeline %>% group_by(FINALID) %>% summarise(events = n()) %>% group_by(events) %>% count()
# deleteTempTables(con)

multipleTests = diagnosis_events %>% filter(event=="positive test") %>% group_by(FINALID) %>% summarise(events = n()) %>% filter(events > 4 )
multipleAdmissions = outcome_events %>% filter(event=="admission") %>% group_by(FINALID) %>% summarise(events = n()) %>% filter(events > 4 )
largest = event_timeline %>% group_by(FINALID) %>% summarise(events = n()) %>% filter(events > 12)

tmp = event_timeline %>% semi_join(multipleTests, by="FINALID")  %>% semi_join(multipleAdmissions, by="FINALID")  %>% semi_join(largest, by="FINALID") %>% compute()
ggplot(tmp %>% collect(),aes(x=date,y=as.factor(FINALID),colour=event))+geom_point()

## Timeline relative date analysis ----
# getAugmentedTimeline = function(event_timeline, table = "test") {
#    
#   NONE_AFTER = julianday.Date(as.Date("2100-01-01"))
#   NONE_BEFORE = julianday.Date(as.Date("1970-01-01"))
#   GAP_BETWEEN_INFECTIONS = 28
#   
#   tmp1 = event_timeline %>%
#     group_by(FINALID) %>%
#     window_order(desc(date),desc(event_type_order)) %>%
#     mutate(
#       next_positive_date = cummin(ifelse(event %in% c("positive test","sequencing"),date,NONE_AFTER)),
#       next_admission_date = cummin(ifelse(event %in% c("admission"),date,NONE_AFTER)),
#       next_discharge_date = cummin(ifelse(event %in% c("discharge"),date,NONE_AFTER)),
#       next_AE_visit_date = cummin(ifelse(event %in% c("a&e visit"),date,NONE_AFTER)),
#       next_death_date = cummin(ifelse(event %in% c("death"),date,NONE_AFTER)),
#       next_symptom_onset_date = cummin(ifelse(event %in% c("symptom onset"),date,NONE_AFTER)),
#       # nextEra = cummin(era)
#     ) %>%
#     mutate(
#       infection_episode_end = ifelse(
#         event %in% c("positive test","sequencing") &
#           date < lag(next_positive_date,default=NONE_AFTER)-GAP_BETWEEN_INFECTIONS # WINDOW FOR NEW
#         # so this is lagged as we are in reverse date order
#         # it is effectively testing whether a given test result is >28 days before the next positive test result, or if there is no next test result.
#         ,TRUE,FALSE)
#     ) %>%
#     mutate(
#       next_infection_episode_end_date =  cummin(ifelse(infection_episode_end,date,NONE_AFTER))
#     ) %>%
#     # needed to reset the ordering:
#     compute(indexes=list("FINALID","date","event_type_order")) 
#   
#   tmp2 = tmp1 %>%
#     group_by(FINALID) %>%
#     window_order(date,event_type_order) %>%
#     mutate(
#       prev_positive_date = cummax(ifelse(event %in% c("positive test","sequencing"),date,NONE_BEFORE)),
#       prev_admission_date = cummax(ifelse(event %in% c("admission"),date,NONE_BEFORE)),
#       prev_discharge_date = cummax(ifelse(event %in% c("discharge"),date,NONE_BEFORE)),
#       prev_symptom_onset_date = cummax(ifelse(event %in% c("symptom onset"),date,NONE_BEFORE)),
#       prev_AE_visit_date = cummax(ifelse(event %in% c("a&e visit"),date,NONE_BEFORE)),
#       prev_full_vaccination_date = cummax(ifelse(event %in% c("vaccination") & subgroup=="second",date,NONE_BEFORE)),
#       prev_vaccination_date = cummax(ifelse(event %in% c("vaccination"),date,NONE_BEFORE))
#     ) %>%
#     mutate(
#       # fully vaccinated if full vaccination date >14 days before now.
#       is_fully_vaccinated = prev_full_vaccination_date!=NONE_BEFORE & prev_full_vaccination_date+14 <= date
#     ) %>% 
#     # needed to reset the ordering:
#     compute(indexes=list("FINALID","date","event_type_order")) 
#   
#   tmp3 = tmp2 %>%
#     mutate(covid_related = case_when(
#       # an A&E visit more than 14 days before or more than 28 days after any positive test is not covid related
#       event=="a&e visit" & date < next_positive_date-14 & date > prev_positive_date+28 ~ FALSE,
#       # an admission with no discharge following it when admission more than 28 days after positive test is not covid related
#       event=="admission" & next_discharge_date==NONE_AFTER & date > prev_positive_date+28 ~ FALSE,
#       # an admission with subsequent discharged more than 14 days before positive test & admission date more than 28 days after positive test are not covid related
#       event=="admission" & next_discharge_date < next_positive_date-14 & date > prev_positive_date+28 ~ FALSE,
#       # an discharge more than 14 days before positive test & associated admission date more than 28 days after positive test are not covid related
#       event=="discharge" & date < next_positive_date-14 & prev_admission_date > prev_positive_date+28 ~ FALSE,
#       # symptoms falling outside of 28 days before, or 28 days after a positive test
#       event=="symptom onset" & date < next_positive_date-28 & date > prev_positive_date+28 ~ FALSE,
#       # deaths falling outside of 90 days after a positive test, however all deaths here are thought to be covid related
#       # all other items are covid related at this point athough this will be changed later:
#       # i.e. all positive tests, all vaccinations, all sequencing results, all sgene results, all deaths (will be adjusted later)
#       TRUE ~ TRUE
#     )) %>%
#     group_by(FINALID) %>%
#     window_order(date,event_type_order) %>%
#     mutate(
#       infection_episode_start = ifelse(
#         event %in% c("positive test","sequencing") &
#           date > lag(prev_positive_date,default=NONE_BEFORE)+GAP_BETWEEN_INFECTIONS # WINDOW FOR NEW
#         ,TRUE,FALSE),
#       infection_episode = cumsum(infection_episode_start),
#       #TODO: could add in admission episodes here
#       prev_covid_admission_date = cummax(ifelse(covid_related == TRUE & event %in% c("admission"),date,NONE_BEFORE)),
#       prev_covid_discharge_date = cummax(ifelse(covid_related == TRUE & event %in% c("discharge"),date,NONE_BEFORE)),
#       prev_covid_AE_visit_date = cummax(ifelse(covid_related == TRUE & event %in% c("a&e visit"),date,NONE_BEFORE)),
#       prev_infection_episode_start_date = cummax(ifelse(infection_episode_start,date,NONE_BEFORE))
#     ) %>%
#     mutate(
#       in_hospital_with_covid = case_when(
#         # need to be between and admission and a discharge...
#         # this does not include the admission and the discharge itself (and poss randomly )
#         # disc -> adm -> event -> disc -> adm
#         prev_discharge_date < prev_covid_admission_date & next_discharge_date == NONE_AFTER  ~ TRUE,
#         prev_discharge_date < prev_covid_admission_date & next_discharge_date < next_admission_date  ~ TRUE,
#         TRUE ~ FALSE
#       )
#     ) %>%
#     mutate(
#       # covid related deaths
#       covid_related = case_when(
#         # deaths from death line list are by definition covid related (according to PHE)
#         # this is enforced above
#         # deaths from in hospital records may be not covid related.
#         # TODO: this should also include something to do with proximity to covid related admission?
#         # if death occurs in someone that has not yet tested positive
#         event=="death" & source=="admissions" & prev_positive_date == NONE_BEFORE ~ FALSE,
#         # if the death is less than 28 days of since positive test or during a a covid-related hospital stay or within 28 days of a covid related hospital discharge then is covid related
#         # expressed as complement.
#         # i.e. if death is more that 28 days since positive test AND not in a covid hospital stay AND more that 28 days since a covid related hospital discharge it is not covid related.
#         event=="death" & source=="admissions" & date > prev_positive_date+28 & !in_hospital_with_covid & date > prev_covid_discharge_date+28 ~ FALSE,
#         TRUE ~ covid_related
#       )
#     ) %>%
#     compute(indexes=list("FINALID","date","event_type_order","infection_episode","covid_related"), temporary=FALSE, name=table)
# 
#   return(tmp3)
# }


# ```
# 
# 
# ```{r}
# testTimelines = tibble::tribble(
#   ~FINALID, ~event, ~date, ~source, ~subgroup, ~test,
#   1, "positive test", 101, "any", NA_character_,"era start",
#   1, "positive test", 102, "any", NA_character_,NA,
#   1, "positive test", 103, "any", NA_character_,"era end",
#   1, "positive test", 150, "any", NA_character_,"era start",
#   1, "positive test", 151, "any", NA_character_,NA,
#   1, "positive test", 152, "any", NA_character_,"era end",
# 
#   2, "admission", 101, "any", NA_character_,"not covid admission",
#   2, "discharge", 102, "any", NA_character_,"not covid admission",
#   2, "positive test", 150, "any", NA_character_,"not covid admission",
#   2, "admission", 151, "any", NA_character_,"covid admission",
#   2, "positive test", 152, "any", NA_character_,"covid admission",
# 
# ) %>% mutate(
#   record_id = row_number(),
#   date = julianday.Date(date,"1970-01-01"),
#   from_source = NA_character_, from_record_id = NA_character_,
#   event_type_order = case_when(
#     event=="symptom onset" ~1L,
#     event=="positive test" ~ 2L,
#     event=="sequencing" ~ 3L,
#     event=="a&e visit" ~ 4L,
#     event=="admission" ~ 5L,
#     event=="discharge" ~ 6L,
#     event=="vaccination" ~ 7L,
#     event=="death" ~ 8L
#   ))
# 
# testTimelines = dbplyr::tbl_memdb(testTimelines,name="test2")
# 
# tests = list(
#   "era start" = expr(infection_episode_start == 1),
#   "era end" = expr(infection_episode_end == 1),
#   "not covid admission" = expr(in_hospital_with_covid == 0),
#   "covid admission" = expr(in_hospital_with_covid == 1)
# )
# 
# 
# testData = getAugmentedTimeline(event_timeline = testTimelines) %>% collect() %>% glimpse()
# testData %>% filter(!is.na(test)) %>% group_by(FINALID,test) %>% group_modify(function(d,g,...) {
#   tst = tests[[g$test]]
#   tmp = d %>% mutate(result=!!tst)
#   #browser()
#   tmp %>% summarise(success=all(result))
# })
# 
# 
# 
# ```
# 
# 
# ```{r eval=FALSE}
# 
# multipleTests = timelines %>% filter(event_type_order==2) %>% group_by(FINALID) %>% summarise(events = n()) %>% filter(events > 4 )
# multipleAdmissions = timelines %>% filter(event_type_order %in% c(5,4,6)) %>%  group_by(FINALID) %>% summarise(events = n()) %>% filter(events > 4 )
# largest = timelines %>% group_by(FINALID) %>% summarise(events = n()) %>% filter(events > 12)
# 
# timelines2 = timelines %>% augmentTimeline() %>% compute(indexes=list("record_id","FINALID","date","event_type_order"), name="timelines2", temporary=FALSE)
# 
tmp2 = timelines2 %>% semi_join(multipleTests, by="FINALID")  %>% semi_join(multipleAdmissions, by="FINALID")  %>% semi_join(largest, by="FINALID") %>%
  collect() %>% arrange(FINALID,date)
# 
# ggplot(tmp2 %>% filter(covid_related==1),aes(x=as.Date(date,"1970-01-01"),y=as.factor(FINALID),colour=event))+geom_point()
# 
# tmp3 = timelines2 %>% ungroup() %>% filter(event_type_order==2 & era_start==1 & lastSymptomOnset != 0) %>% mutate(symptomToFirstTest = date-lastSymptomOnset) %>% select(symptomToFirstTest) %>% collect()
# 
# ggplot(tmp3,aes(x=symptomToFirstTest))+geom_histogram(binwidth = 1)+coord_cartesian(xlim=c(-10,40))
# 
# 
# tmp4 = timelines2 %>% ungroup() %>% filter(event_type_order==4 & covid_related==1 & lastSymptomOnset != 0) %>% mutate(symptomToAEVisit = date-lastSymptomOnset) %>% select(symptomToAEVisit) %>% collect()
# ggplot(tmp4,aes(x=symptomToAEVisit))+geom_histogram(binwidth = 1)+coord_cartesian(xlim=c(-10,40))
# 
# ref = timelines2 %>% ungroup() %>% select(event,event_type_order,subgroup) %>% distinct() %>% collect()
# 
# tmp5 = timelines2 %>% ungroup() %>% filter(event_type_order==5 & covid_related==1 & lastEraStart != 0) %>% mutate(testPosToAdmit = date-lastEraStart) %>% select(symptomToAdmit,subgroup) %>% collect()
# ggplot(tmp5,aes(x=symptomToAdmit,colour=subgroup))+geom_line(stat="count")+coord_cartesian(xlim=c(-10,40))+scale_y_continuous(trans="log1p")
# 
# 
# tmp5 = timelines2 %>% ungroup() %>% filter(event_type_order==8 & covid_related==1 & lastEraStart != 0) %>% mutate(delay = date-lastEraStart) %>% select(delay,subgroup) %>% collect()
# ggplot(tmp5,aes(x=delay,colour=subgroup))+geom_line(stat="count")+coord_cartesian(xlim=c(-1,200))+scale_y_continuous(trans="log1p")
# 
# 
# tmp5 = timelines2 %>% ungroup() %>% filter(event_type_order==6 & covid_related==1 & lastCovidAdmission != 0) %>% mutate(delay = date-lastCovidAdmission) %>% select(delay,subgroup) %>% collect()
# ggplot(tmp5,aes(x=delay,colour=subgroup))+geom_line(stat="count")+coord_cartesian(xlim=c(-1,40))
# 
# tmp5 = timelines2 %>% ungroup() %>% filter(event_type_order==5 & covid_related==1 & nextDischarge != NONE_AFTER) %>% mutate(delay = nextDischarge-date) %>% select(delay,subgroup) %>% collect()
# ggplot(tmp5,aes(x=delay,colour=subgroup))+geom_line(stat="count")+coord_cartesian(xlim=c(-1,40))
# 
# 
# # single day admission
# # incomplete hospital admission
# View(tmp2 %>% filter(FINALID == -3295130))
# # admission with record id 3146196 shoudl not be covid related
# 
# #Possible covid readmissions
# View(tmp2 %>% filter(FINALID == -2803137))
# 
# ```
# 
# 
#   
# 

# 
# 
# 
# 
# ```{r}
# # admissions %>% glimpse()
# # admissions %>% group_by(hospital_event_rank) %>% count()
# # admissions %>% group_by(linkset, hoslink) %>% count()
# # 
# # # majority of admissions "Unlinked"
# # admissions %>% group_by(onset_category) %>% count()
# # 
# # admissions %>% filter(specimen_date > hospital_in-28 & specimen_date < hospital_in+7) %>% filter(onset_category=="Unlinked")
# # admissions %>% filter(specimen_date > hospital_in-28 & specimen_date < hospital_in+7) %>% group_by(onset_category,covidICD) %>% count()
# # 
# # admissions %>% 
# #   filter(specimen_date > hospital_in-28 & specimen_date < hospital_in+7) %>% 
# #   group_by(onset_category,ecds_discharge) %>% 
# #   count()
# # 
# # # hospital event rank is reverse of what I woudl expect. Its 1 for most recent episode. however multiple per spell
# # # SUS data has HRG per spell & multiple e.g. FINALID: -5626863
# # # N.B. this patient looks like a vaccine failure also
# # 
# # # length of stay is hocus pocus e.g. -5625932 had an allergy and been defined as a hospital onset.
# # 
# # admTs = admissions %>% 
# #   filter(linkset != "CoV") %>%
# #   filter(specimen_date >= hospital_in-14 & specimen_date < hospital_in+28) %>% 
# # #  filter(hospital_event_rank == 1) %>%
# # #  filter(ecds_discharge != "Discharged") %>% 
# #   #group_by(onset_category) %>% 
# #   collect()
# #   #summarise(los = mean(length_of_stay,na.rm=TRUE))
# #   #glimpse()
# ```
# 
# 
# ```{r}
# # 
# # nat = dpc$datasets$getPHEApiNations()
# # 
# # ggplot(admTs %>% filter(hospital_in > "2020-03-01"), aes(x=hospital_in)) + geom_histogram(binwidth = 1)
# # 
# # engCount = bind_rows(
# #   admTs %>% filter(ecds_discharge != "Discharged") %>% group_by(hospital_in) %>% summarise(count=n()) %>% mutate(source = "ll"),
# #   nat %>% filter(statistic == "hospital admission" & type=="incidence" & name=="England") %>% select(hospital_in = date, count=value) %>% mutate(source = "api")
# # )
# # 
# # ggplot(engCount %>% filter(hospital_in > "2020-03-01"), aes(x=hospital_in,y=count,colour=source)) + geom_line()
# 
# ```
# 
