
library(dbplyr)
library(tidyverse)

here::i_am("vignettes/postgres.Rmd")

# con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "spim_test")

# con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "~/Data/covid/test_spim.sqlite3", cache_size = -4*1024*1024, extended_types = TRUE, synchronous="off")
# 
# db_copy_to(con, "mtcars", mtcars,temporary=FALSE)
# DBI::dbListTables(con)
# 
# dbmtcars = tbl(con,"mtcars")
devtools::load_all()

sources = dpc$spim$getAllLatest()

id = sources$date %>% max(na.rm = TRUE)
dbname = paste0("~/Data/covid/spim-",id,".sqlite3")
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = paste0("~/Data/covid/spim-",id,".sqlite3"), cache_size = -4*1024*1024, extended_types = TRUE, synchronous="off")
tables = DBI::dbListTables(con)

if (!("sources" %in% tables)) {
  db_copy_to(con, table="sources", values=sources, temporary=FALSE)
}
if (!("immunisations" %in% tables)) {
  tmp2 = dpc$spim$getImmunizationLineList(dbfile = dbname) %>% mutate(record_id=row_number())
  db_copy_to(con, table="immunisations", values=tmp2, temporary=FALSE, unique_indexes=list("record_id"))
  DBI::dbSendStatement(con,"CREATE INDEX IF NOT EXISTS X_immunisations_finalid ON immunisations (FINALID)") %>% DBI::dbClearResult()
  DBI::dbSendStatement(con,"CREATE INDEX IF NOT EXISTS X_immunisations_finalid2 ON immunisations (finalid2)") %>% DBI::dbClearResult()
  rm(tmp2)
  dpc$spim$unloadCache()
}
tables = DBI::dbListTables(con)
immunisations = tbl(con,"immunisations")
if (!("admissions" %in% tables)) {
  tmp = dpc$spim$getAdmissionLineList() %>% mutate(record_id=row_number())
  db_copy_to(con, table="admissions", values=tmp,temporary=FALSE, unique_indexes=list("record_id"))
  DBI::dbSendStatement(con,"CREATE INDEX IF NOT EXISTS X_admissions_finalid ON admissions (FINALID)") %>% DBI::dbClearResult()
  rm(tmp)
  dpc$spim$unloadCache()
}
tables = DBI::dbListTables(con)
admissions = tbl(con,"admissions")
# admissions %>% glimpse()
# DBI::dbDisconnect(con)
# TODO: https://digital.nhs.uk/services/national-casemix-office/downloads-groupers-and-tools/hrg4-2020-21-national-costs-grouper
# HRG grouper
if (!("cases" %in% tables)) {
  tmp3 = dpc$spim$getLineList() %>% mutate(record_id=row_number())
  db_copy_to(con, table="cases", values=tmp3 %>% mutate(FINALID = as.integer(FINALID)),temporary=FALSE, unique_indexes=list("record_id"))
  DBI::dbSendStatement(con,"CREATE INDEX IF NOT EXISTS X_cases_finalid ON cases (FINALID)") %>% DBI::dbClearResult()
  rm(tmp3)
  dpc$spim$unloadCache()
}
tables = DBI::dbListTables(con)
cases = tbl(con,"cases")
if (!("sgene" %in% tables)) {
  tmp4 = dpc$spim$getSGeneLineList() %>% mutate(record_id=row_number())
  db_copy_to(con, table="sgene", values=tmp4 %>% mutate(FINALID = as.integer(FINALID)),temporary=FALSE, unique_indexes=list("record_id"))
  DBI::dbSendStatement(con,"CREATE INDEX IF NOT EXISTS X_sgene_finalid ON sgene (FINALID)") %>% DBI::dbClearResult()
  rm(tmp4)
  dpc$spim$unloadCache()
}
tables = DBI::dbListTables(con)
sgene = tbl(con,"sgene")
if (!("deaths" %in% tables)) {
  tmp5 = dpc$spim$getDeathsLineList() %>% mutate(record_id=row_number())
  db_copy_to(con, table="deaths", values=tmp5 %>% select(-finalid) %>% mutate(FINALID = as.integer(FINALID)),temporary=FALSE, unique_indexes=list("record_id"))
  DBI::dbSendStatement(con,"CREATE INDEX IF NOT EXISTS X_deaths_finalid ON deaths (FINALID)") %>% DBI::dbClearResult()
  rm(tmp5)
  dpc$spim$unloadCache()
}
tables = DBI::dbListTables(con)
deaths = tbl(con,"deaths")
if (!("genomics" %in% tables)) {
  tmp6 = dpc$spim$getVAMLineList() %>% mutate(record_id=row_number())
  db_copy_to(con, table="genomics", values=tmp6 %>% rename(FINALID = finalid),temporary=FALSE, unique_indexes=list("record_id"))
  DBI::dbSendStatement(con,"CREATE INDEX IF NOT EXISTS X_genomics_finalid ON genomics (FINALID)") %>% DBI::dbClearResult()
  rm(tmp6)
  dpc$spim$unloadCache()
}
tables = DBI::dbListTables(con)
genomics = tbl(con,"genomics")
if (!("ctas" %in% tables)) {
  tmp7 = dpc$spim$getCTASLineList() %>% mutate(record_id=row_number())
  db_copy_to(con, table="ctas", values=tmp7 %>% mutate(FINALID = ifelse(is.na(sgtf_finalid),genomic_finalid,sgtf_finalid)),temporary=FALSE, unique_indexes=list("record_id"))
  DBI::dbSendStatement(con,"CREATE INDEX IF NOT EXISTS X_ctas_finalid ON ctas (FINALID)") %>% DBI::dbClearResult()
  rm(tmp7)
  dpc$spim$unloadCache()
}
tables = DBI::dbListTables(con)
ctas = tbl(con,"ctas")
# event analysis
compute <- function(remote_df, ...) {
  existing_groups <- groups(remote_df)
  remote_df <-
    remote_df %>%
    dplyr::compute(...)
  remote_df <-
    tbl(remote_df$src$con, sql_render(remote_df))
  remote_df <-
    group_by(remote_df, !!!existing_groups)
  return(remote_df)
}

remote_name = function(tbl) {tbl %>% dbplyr::sql_render() %>% stringr::str_extract("`(.*)`") %>% stringr::str_remove_all("`")}


cleanup = function(con,...) {
  x = DBI::dbListTables(con)
  x = x[x %>% stringr::str_starts("dbplyr")]
  sapply(x, function(y) DBI::dbRemoveTable(con,y))
}

rm(adm,aeAttend,aeAdmit,disch,died,hospDied,aeDied,addHospDied,addECDSDied,xfer,addHospDied,addECDSDied,xfer,addECDSAdm)
#object 'adm' not foundobject 'aeAttend' not foundobject 'aeAdmit' not foundobject 'disch' not foundobject 'died' not foundobject 'hospDied' not foundobject 'aeDied' not foundobject 'addHospDied' not foundobject 'addECDSDied' not foundobject 'xfer' not foundobject 'addHospDied' not foundobject 'addECDSDied' not foundobject 'xfer' not found
rm(onsets,addCtasSymptoms,ctasSymptoms)
#object 'onsets' not found

# Immunisation event
imms1 = immunisations %>% 
  filter(!is.na(vaccination_date) & !is.na(FINALID)) %>% 
  mutate(event = "vaccination", subgroup=tolower(dose_number), source="immunisations", from_source=NA_character_, from_record_id=NA_character_) %>%
  select(FINALID, event, date=vaccination_date, source, record_id, from_source, from_record_id,subgroup) %>%
  compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))

imms2 = immunisations %>% 
  filter(!is.na(vaccination_date) & !is.na(finalid2)) %>% 
  mutate(event = "vaccination", subgroup=tolower(dose_number), source="immunisations", from_source=NA_character_, from_record_id=NA_character_) %>%
  select(FINALID = finalid2, event, date=vaccination_date, source, record_id, from_source, from_record_id,subgroup) %>%
  compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))

immunisationEvents = imms1 %>% 
  union(imms2 %>% anti_join(imms1,by="FINALID")) %>% 
  compute(indexes=list("record_id","FINALID","date"))


# Unified events
timelines = testEvents %>% 
  union_all(immunisationEvents) %>%
  union_all(admissionEvents) %>%
  union_all(symptomEvents) %>%
  mutate(event_type_order = case_when(
    event=="symptom onset" ~1,
    event=="positive test" ~ 2,
    event=="sequencing" ~ 3,
    event=="a&e visit" ~ 4,
    event=="admission" ~ 5,
    event=="discharge" ~ 6,
    event=="vaccination" ~ 7,
    event=="death" ~ 8
  )) %>%
  compute(indexes=list("record_id","FINALID","date","event_type_order"), name="timelines", temporary=FALSE)
timeline

#timelines = tbl(con,"timelines")
timelines %>% group_by(FINALID) %>% summarise(events = n()) %>% group_by(events) %>% count()
# events
# <int>
#   n
# <int>
#   1	452111
# 2	530138
# 3	1291117
# 4	1080289
# 5	1303134
# 6	639108
# 7	346300
# 8	216785
# 9	147878
# 10	98854
# ...
# 1-10 of 593 rows

multipleTests = testEvents %>% group_by(FINALID) %>% summarise(events = n()) %>% filter(events > 4 )
multipleAdmissions = admissionEvents %>% group_by(FINALID) %>% summarise(events = n()) %>% filter(events > 4 )
largest = timelines %>% group_by(FINALID) %>% summarise(events = n()) %>% filter(events > 12)

tmp = timelines %>% semi_join(multipleTests, by="FINALID")  %>% semi_join(multipleAdmissions, by="FINALID")  %>% semi_join(largest, by="FINALID") %>% compute()
ggplot(tmp,aes(x=as.Date(date,"1970-01-01"),y=as.factor(FINALID),colour=event))+geom_point()



# tmp2 = tmp %>% 
augmentTimeline = function(timelines) {
  timelines %>%
    group_by(FINALID) %>%
    window_order(desc(date),desc(event_type_order)) %>% 
    mutate(
      nextPositive = cummin(ifelse(event %in% c("positive test","sequencing"),date,999999)),
      nextAdmission = cummin(ifelse(event %in% c("admission"),date,999999)),
      nextDischarge = cummin(ifelse(event %in% c("discharge"),date,999999)),
      nextAEVisit = cummin(ifelse(event %in% c("a&e visit"),date,999999)),
      nextDeath = cummin(ifelse(event %in% c("death"),date,999999)),
      nextSymptomOnset = cummin(ifelse(event %in% c("symptom onset"),date,999999)),
      # nextEra = cummin(era)
    ) %>%
    mutate(
      era_end = ifelse(
        event %in% c("positive test","sequencing") & 
          date < lag(nextPositive,default=999999)-28 # WINDOW FOR NEW
        # so this is lagged as we are in reverse date order
        # it is effectively testing whether a given test result is >28 days before the next positive test result, or if there is no next test result.
        ,1,0)
    ) %>%
    mutate(
      nextEraEnd =  cummin(ifelse(era_end,date,999999))
    ) %>%
    # needed to reset the ordering:
    compute(indexes=list("FINALID","date","event_type_order")) %>%
    group_by(FINALID) %>%
    window_order(date,event_type_order) %>% 
    # TODO: this is not deterministic as does not order events by type.
    # need to add in a sequencing for events such that symptom onset < test < sequencing < admission < discharge < vaccination < death
    mutate(
      lastPositive = cummax(ifelse(event %in% c("positive test","sequencing"),date,0)),
      lastAdmission = cummax(ifelse(event %in% c("admission"),date,0)),
      lastDischarge = cummax(ifelse(event %in% c("discharge"),date,0)),
      lastSymptomOnset = cummax(ifelse(event %in% c("symptom onset"),date,0)),
      lastAEVisit = cummax(ifelse(event %in% c("a&e visit"),date,0))
    ) %>%
    # needed to reset the ordering:
    compute(indexes=list("FINALID","date","event_type_order")) %>%
    
    mutate(covid_related = case_when(
      event=="a&e visit" & date < nextPositive-14 & date > lastPositive+28 ~ FALSE,
      # no discharge following admission
      event=="admission" & nextDischarge==999999 & date > lastPositive+28 ~ FALSE,
      # discharged more than 14 days before positive test & admission date greater than 28 days of positive test are not covid related
      event=="admission" & nextDischarge < nextPositive-14 & date > lastPositive+28 ~ FALSE,
      event=="discharge" & date < nextPositive-14 & lastAdmission > lastPositive+28 ~ FALSE,
      # symptoms falling outside of 28 days before, or 28 days after a positive test
      event=="symptom onset" & date < nextPositive-28 & date > lastPositive+28 ~ FALSE,
      # deaths falling outside of 90 days after a positive test
      TRUE ~ TRUE
    )) %>%
    window_order(date,event_type_order) %>% 
    mutate(
      era_start = ifelse(
        event %in% c("positive test","sequencing") & 
          date > lag(lastPositive,default=0)+28 # WINDOW FOR NEW
        ,1,0),
      era = cumsum(era_start)
    ) %>% 
    mutate(
      lastCovidAdmission = cummax(ifelse(covid_related == TRUE & event %in% c("admission"),date,0)),
      lastCovidDischarge = cummax(ifelse(covid_related == TRUE & event %in% c("discharge"),date,0)),
      lastCovidAEVisit = cummax(ifelse(covid_related == TRUE & event %in% c("a&e visit"),date,0)),
      lastEraStart = cummax(ifelse(era_start,date,0)),
    ) %>%
    mutate(
      in_hospital_with_covid = case_when(
        # need to be between and admission and a discharge...
        # this does not include the admission and the discharge itself (and poss randomly )
        # disc -> adm -> event -> disc -> adm
        lastDischarge < lastCovidAdmission & nextDischarge == 999999  ~ TRUE,
        lastDischarge < lastCovidAdmission & nextDischarge < nextAdmission  ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>% 
    mutate(
      # covid related deaths
      covid_related = case_when(
        # deaths from death line list are by definition covid related (according to PHE)
        # deaths from in hospital records may be also covid related.
        # TODO: this should also include something to do with proximity to covid related admission? 
        # if death occurs in someone that has not yet tested positive 
        event=="death" & source=="admissions" & lastPositive == 0 ~ FALSE,
        # if the death is less than 28 days of since positive test or during a a covid-related hospital stay or within 28 days of a covid related hospital discharge then is covid related
        # expressed as complement.
        event=="death" & source=="admissions" & date > lastPositive+28 & !in_hospital_with_covid & date>lastCovidDischarge+28 ~ FALSE,
        TRUE ~ covid_related
      )
    ) %>%
    compute(indexes=list("FINALID","date","event_type_order"))
}
testTimelines = tibble::tribble(
  ~FINALID, ~event, ~date, ~source, ~subgroup, ~test, 
  1, "positive test", 101, "any", NA_character_,"era start", 
  1, "positive test", 102, "any", NA_character_,NA,
  1, "positive test", 103, "any", NA_character_,"era end", 
  1, "positive test", 150, "any", NA_character_,"era start", 
  1, "positive test", 151, "any", NA_character_,NA,
  1, "positive test", 152, "any", NA_character_,"era end", 
  
  2, "admission", 101, "any", NA_character_,"not covid admission", 
  2, "discharge", 102, "any", NA_character_,"not covid admission", 
  2, "positive test", 150, "any", NA_character_,"not covid admission", 
  2, "admission", 151, "any", NA_character_,"covid admission", 
  2, "positive test", 152, "any", NA_character_,"covid admission", 
  
) %>% mutate(
  record_id = row_number(), 
  from_source = NA_character_, from_record_id = NA_character_,
  event_type_order = case_when(
    event=="symptom onset" ~1,
    event=="positive test" ~ 2,
    event=="sequencing" ~ 3,
    event=="a&e visit" ~ 4,
    event=="admission" ~ 5,
    event=="discharge" ~ 6,
    event=="vaccination" ~ 7,
    event=="death" ~ 8
  ))

testTimelines = dbplyr::tbl_memdb(testTimelines, indexes=list("FINALID","date","event_type_order"), name="test_timelines", overwrite = TRUE)
# Error in dbplyr::tbl_memdb(testTimelines, indexes = list("FINALID", "date",  : 
#                                                            unused arguments (indexes = list("FINALID", "date", "event_type_order"), overwrite = TRUE)
#                                                          NA
 multipleTests = timelines %>% filter(event_type_order==2) %>% group_by(FINALID) %>% summarise(events = n()) %>% filter(events > 4 )
 multipleAdmissions = timelines %>% filter(event_type_order %in% c(5,4,6)) %>%  group_by(FINALID) %>% summarise(events = n()) %>% filter(events > 4 )
 largest = timelines %>% group_by(FINALID) %>% summarise(events = n()) %>% filter(events > 12)
 
 timelines2 = timelines %>% augmentTimeline() %>% compute(indexes=list("record_id","FINALID","date","event_type_order"), name="timelines2", temporary=FALSE)
 
 tmp2 = timelines2 %>% semi_join(multipleTests, by="FINALID")  %>% semi_join(multipleAdmissions, by="FINALID")  %>% semi_join(largest, by="FINALID") %>% 
   collect() %>% arrange(FINALID,date)
 
 ggplot(tmp2 %>% filter(covid_related==1),aes(x=as.Date(date,"1970-01-01"),y=as.factor(FINALID),colour=event))+geom_point()
 
 tmp3 = timelines2 %>% ungroup() %>% filter(event_type_order==2 & era_start==1 & lastSymptomOnset != 0) %>% mutate(symptomToFirstTest = date-lastSymptomOnset) %>% select(symptomToFirstTest) %>% collect()
 
 ggplot(tmp3,aes(x=symptomToFirstTest))+geom_histogram(binwidth = 1)+coord_cartesian(xlim=c(-10,40))
 
 
 tmp4 = timelines2 %>% ungroup() %>% filter(event_type_order==4 & covid_related==1 & lastSymptomOnset != 0) %>% mutate(symptomToAEVisit = date-lastSymptomOnset) %>% select(symptomToAEVisit) %>% collect()
 ggplot(tmp4,aes(x=symptomToAEVisit))+geom_histogram(binwidth = 1)+coord_cartesian(xlim=c(-10,40))
 
 ref = timelines2 %>% ungroup() %>% select(event,event_type_order,subgroup) %>% distinct() %>% collect()
 
 tmp5 = timelines2 %>% ungroup() %>% filter(event_type_order==5 & covid_related==1 & lastEraStart != 0) %>% mutate(testPosToAdmit = date-lastEraStart) %>% select(symptomToAdmit,subgroup) %>% collect()
 ggplot(tmp5,aes(x=symptomToAdmit,colour=subgroup))+geom_line(stat="count")+coord_cartesian(xlim=c(-10,40))+scale_y_continuous(trans="log1p")
 
 
 tmp5 = timelines2 %>% ungroup() %>% filter(event_type_order==8 & covid_related==1 & lastEraStart != 0) %>% mutate(delay = date-lastEraStart) %>% select(delay,subgroup) %>% collect()
 ggplot(tmp5,aes(x=delay,colour=subgroup))+geom_line(stat="count")+coord_cartesian(xlim=c(-1,200))+scale_y_continuous(trans="log1p")
 
 
 tmp5 = timelines2 %>% ungroup() %>% filter(event_type_order==6 & covid_related==1 & lastCovidAdmission != 0) %>% mutate(delay = date-lastCovidAdmission) %>% select(delay,subgroup) %>% collect()
 ggplot(tmp5,aes(x=delay,colour=subgroup))+geom_line(stat="count")+coord_cartesian(xlim=c(-1,40))
 
 tmp5 = timelines2 %>% ungroup() %>% filter(event_type_order==5 & covid_related==1 & nextDischarge != 999999) %>% mutate(delay = nextDischarge-date) %>% select(delay,subgroup) %>% collect()
 ggplot(tmp5,aes(x=delay,colour=subgroup))+geom_line(stat="count")+coord_cartesian(xlim=c(-1,40))
 
 
 # single day admission
 # incomplete hospital admission
 View(tmp2 %>% filter(FINALID == -3295130))
 # admission with record id 3146196 shoudl not be covid related
 
 #Possible covid readmissions
 View(tmp2 %>% filter(FINALID == -2803137))
 # Linkage analysis
 # admissions %>% filter(!is.na(FINALID)) %>% semi_join(immunisations, by="FINALID") %>% select(FINALID) %>% distinct() %>% count()
 # admissions %>% filter(!is.na(FINALID)) %>% anti_join(immunisations, by="FINALID") %>% select(FINALID) %>% distinct() %>% count()
 # immunisations %>% filter(!is.na(FINALID)) %>% select(FINALID) %>% distinct() %>% count()
 # immunisations %>% filter(is.na(FINALID)) %>% count()
 # admissions %>% filter(!is.na(FINALID)) %>% select(FINALID) %>% distinct() %>% count()
 # 
 # 
 # cases %>% group_by(vaccine_linked) %>% count()
 # 
 # cases %>% filter(!is.na(FINALID)) %>% semi_join(immunisations, by="FINALID") %>% select(FINALID) %>% distinct() %>% count()
 # cases %>% filter(!is.na(FINALID)) %>% anti_join(immunisations, by="FINALID") %>% select(FINALID) %>% distinct() %>% count()
 # 
 # genomics %>% glimpse()
 # 
 # #cases %>% group_by(cat,residential_category) %>% count() %>% collect() %>% View()
 # 
 # DBI::dbListTables(con)
 # admissions %>% glimpse()
 # admissions %>% group_by(hospital_event_rank) %>% count()
 # admissions %>% group_by(linkset, hoslink) %>% count()
 # 
 # # majority of admissions "Unlinked"
 # admissions %>% group_by(onset_category) %>% count()
 # 
 # admissions %>% filter(specimen_date > hospital_in-28 & specimen_date < hospital_in+7) %>% filter(onset_category=="Unlinked")
 # admissions %>% filter(specimen_date > hospital_in-28 & specimen_date < hospital_in+7) %>% group_by(onset_category,covidICD) %>% count()
 # 
 # admissions %>% 
 #   filter(specimen_date > hospital_in-28 & specimen_date < hospital_in+7) %>% 
 #   group_by(onset_category,ecds_discharge) %>% 
 #   count()
 # 
 # # hospital event rank is reverse of what I woudl expect. Its 1 for most recent episode. however multiple per spell
 # # SUS data has HRG per spell & multiple e.g. FINALID: -5626863
 # # N.B. this patient looks like a vaccine failure also
 # 
 # # length of stay is hocus pocus e.g. -5625932 had an allergy and been defined as a hospital onset.
 # 
 # admTs = admissions %>% 
 #   filter(linkset != "CoV") %>%
 #   filter(specimen_date >= hospital_in-14 & specimen_date < hospital_in+28) %>% 
 # #  filter(hospital_event_rank == 1) %>%
 # #  filter(ecds_discharge != "Discharged") %>% 
 #   #group_by(onset_category) %>% 
 #   collect()
 #   #summarise(los = mean(length_of_stay,na.rm=TRUE))
 #   #glimpse()
 # 
 # nat = dpc$datasets$getPHEApiNations()
 # 
 # ggplot(admTs %>% filter(hospital_in > "2020-03-01"), aes(x=hospital_in)) + geom_histogram(binwidth = 1)
 # 
 # engCount = bind_rows(
 #   admTs %>% filter(ecds_discharge != "Discharged") %>% group_by(hospital_in) %>% summarise(count=n()) %>% mutate(source = "ll"),
 #   nat %>% filter(statistic == "hospital admission" & type=="incidence" & name=="England") %>% select(hospital_in = date, count=value) %>% mutate(source = "api")
 # )
 # 
 # ggplot(engCount %>% filter(hospital_in > "2020-03-01"), aes(x=hospital_in,y=count,colour=source)) + geom_line()