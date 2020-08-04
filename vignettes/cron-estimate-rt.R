#!/usr/bin/Rscript

library(tidyverse)

setwd("~/Git/uk-covid-datatools/vignettes/")

devtools::load_all("~/Git/uk-covid-datatools/")
dpc = DataProviderController$setup("~/Data/maps")
dpc$loadSpimSources("~/S3/encrypted/")
tsp = dpc$timeseriesProcessor()

#### Current Rt for UK, CTRY, NHSER ----

currentRt = tsp$getDaily(id = "CURRENT-RT", orElse=function() {
  
  description = NULL
  rationale = function(codeType, statistic, sources, processing) {
    description <<- description %>% bind_rows(tibble(
      codeType = codeType,
      statistic = statistic,
      sources = sources,
      processing = processing,
    ))
  }
  
  ### Triage calls - CTRY ----
  triageCTRY = dpc$spim$getOneOneOne() %>% 
    filter(statistic == "triage" & codeType == "CTRY" & name=="England" & source %in% c("111","999")) %>% 
    tsp$aggregateSource(fn=sum) %>% #, na.rm=TRUE) %>% 
    filter(date >= "2020-03-15") %>%
    dpc$demog$findDemographics()
  finalTriageCTRY = triageCTRY %>% filter(subgroup %in% c("urgent clinical review","emergency ambulance")) %>% tsp$aggregateSubgroup() %>% dpc$demog$findDemographics()
  rationale(codeType = "CTRY","triage","SPI-M 111 & 999 feed (only available for England)","Selected only calls with outcomes with 1 or 2 hour urgent clinical review, or ambulance dispatch. Daily 111 & 999 case counts added together.")
  
  ### Cases - CTRY ----
  casesCTRY = dpc$datasets$getTomWhiteIndicators() %>% filter(statistic == "case" & codeType == "CTRY") %>% tsp$incidenceFromCumulative() %>% dpc$demog$findDemographics()
  finalCasesCTRY = casesCTRY
  rationale(codeType = "CTRY","case","4 nations case counts from 4 national PH sites, retrieved as timeseries from Tom White's github site","Cumulative case counts are converted to incidence figures.")
  
  ### Admissions - CTRY ----
  admissionsCTRY = dpc$spim$getSPIMextract() %>% 
    filter(statistic == "hospital admission" & type %in% c("incidence","cumulative") & codeType == "CTRY" & is.na(ageCat)) %>% 
    dpc$demog$findDemographics()
  finalAdmissionsCTRY = admissionsCTRY %>% 
    filter(!(name=="Wales" & source %in% c("hospital_inc_new"))) %>%
    tsp$aggregateSource(namedListOfSources = list(
      hosp_inc_sum = c("hospital_inc", "hospital_inc_new"),
      hosp_inc_acuteone_sum = c("hospital_inc_acuteone", "hospital_inc_new_acuteone")
    ), fn=sum) %>% 
    tsp$aggregateSource(fn=mean) %>% 
    dpc$demog$findDemographics()
  rationale(codeType = "CTRY","hospital admission",
      paste0("Various SPI-M data feeds: ", paste0(unique(admissionsCTRY$source), collapse = ", ")),
      "Hospital_inc_new data set excluded for Wales. Hospital_inc_new and hospital_inc fields combined by summation, result and other sources combined by average")
  
  ### ICU Admissions - CTRY ----
  #TODO: not followed up on at present
  
  ### Deaths - CTRY ----
  deathsCTRY = dpc$spim$getSPIMextract() %>% 
    filter(statistic=="death" & source %in% c("chess_death_inc_line","sitrep_death_inc_line","dashboard_daily_death_inc_dash","nisra_death_inc_line","ons_death_inc_line","phw_death_inc_line") & is.na(ageCat) & codeType=="CTRY") %>% 
    dpc$demog$findDemographics()
  deathsCTRYSources = c("chess_death_inc_line","sitrep_death_inc_line","phw_death_inc_line")
  finalDeathsCTRY = deathsCTRY %>%  
    filter(source %in% deathsCTRYSources) %>% 
    tsp$aggregateSource(fn=mean) %>%
    dpc$demog$findDemographics()
  rationale(codeType = "CTRY","death",
            paste0("SPI-M data feeds considered: ", paste0(unique(deathsCTRY$source), collapse = ", ")),
            paste0("Feeds selected: ", paste0(unique(deathsCTRYSources), collapse = ", "),"; combined by average if more than one per country")
  )
  
  ### Cases - UK ----
  casesUK = bind_rows(
      dpc$datasets$getTomWhiteIndicators() %>% filter(statistic == "case" & codeType == "UK") %>% tsp$incidenceFromCumulative(),
      finalCasesCTRY %>% tsp$aggregateGeography(targetCodeTypes = "UK", completeness = "both",keepOriginal = FALSE)
    ) %>% 
    dpc$demog$findDemographics()
  finalCasesUK = finalCasesCTRY %>% 
    tsp$aggregateGeography(targetCodeTypes = "UK", completeness = "both",keepOriginal = FALSE) %>%
    dpc$demog$findDemographics()
  rationale(codeType = "UK","case","UK headline from PHE and 4 nations case counts from 4 national PH sites, retrieved as timeseries from Tom White's github site",
            "Ultimately sum of 4 nations case counts previously selected for CTRY cases used, as headline numbers maybe include Pillar 2. Cumulative case counts are converted to incidence figures.")
  
  ### Admissions - UK ----
  admissionsUK = finalAdmissionsCTRY %>% 
    tsp$aggregateGeography(targetCodeTypes = "UK", completeness = "both",keepOriginal = FALSE) %>% 
    dpc$demog$findDemographics()
  finalAdmissionsUK = admissionsUK
  rationale(codeType = "UK","hospital admission","Sum of 4 nations figures previously selected for CTRY hospital admissions","none")
  
  ### Deaths - UK ----
  deathsUK = bind_rows(
    dpc$datasets$getTomWhiteIndicators() %>% filter(statistic == "death" & codeType == "UK") %>% tsp$incidenceFromCumulative(),
    finalDeathsCTRY %>% tsp$aggregateGeography(targetCodeTypes = "UK", completeness = "both",keepOriginal = FALSE)
  ) %>% dpc$demog$findDemographics()
  finalDeathsUK = finalDeathsCTRY %>% 
    tsp$aggregateGeography(targetCodeTypes = "UK", completeness = "both",keepOriginal = FALSE) %>% 
    dpc$demog$findDemographics()
  rationale(codeType = "UK","death","Sum of 4 nations figures previously selected for CTRY deaths","none")
  
  ### Triage calls - NHSER ----
  triageNHSER = dpc$spim$getOneOneOne() %>% 
    filter(statistic == "triage" & codeType == "NHSER" & source %in% c("111","999")) %>% 
    tsp$aggregateSource(fn=sum) %>% #, na.rm=TRUE) %>% 
    filter(date >= "2020-03-15") %>%
    dpc$demog$findDemographics()
  finalTriageNHSER = triageNHSER %>% filter(subgroup %in% c("urgent clinical review","emergency ambulance")) %>% 
    tsp$aggregateSubgroup() %>% 
    dpc$demog$findDemographics()
  rationale(codeType = "NHSER","triage","SPI-M 111 & 999 feed","Selected only calls with outcomes with 1 or 2 hour urgent clinical review, or ambulance dispatch. Daily 111 & 999 case counts added together.")
  
  ### Cases - NHSER ----
  casesNHSER = dpc$spim$getLineListIncidence(specimenOrReport = "specimen") %>% 
    filter(codeType == "NHSER") %>% 
    tsp$aggregateAge() %>% 
    tsp$aggregateGender() %>% #tsp$aggregateSubgroup()
    dpc$demog$findDemographics()
  finalCasesNHSER = casesNHSER %>% filter(subgroup == "Pillar 1")
  rationale(codeType = "NHSER","case","SPI-M line list","Line list aggregated by age and gender. Pillar 1 tests only as testing volumes variable in Pillar 2.")
  
  ### Admissions - NHSER ----
  admissionsNHSER = dpc$spim$getSPIMextract() %>% 
    filter(source %in% c("hospital_inc","hospital_inc_new") & is.na(ageCat) & codeType=="NHSER") %>% 
    dpc$demog$findDemographics()
  finalAdmissionsNHSER = admissionsNHSER %>% 
    tsp$aggregateSource(list(admissions = c("hospital_inc","hospital_inc_new")), fn=sum)  %>% 
    dpc$demog$findDemographics()
  rationale(codeType = "NHSER","hospital admission","SPI-M hospital_inc and hospital_inc_new fields","Souces combined by summation")
  
  ### ICU Admissions - NHSER ----
  icuAdmissionsNHSER = dpc$spim$getSPIMextract() %>% 
    filter(source %in% c("chess_icu_admissions") & is.na(ageCat) & codeType=="NHSER") %>% 
    dpc$demog$findDemographics()
  finalIcuAdmissionsNHSER = icuAdmissionsNHSER
  rationale(codeType = "NHSER","icu admission","SPI-M chess_icu_admissions field","none")
  
  ### Deaths - NHSER ----
  deathsNHSER = dpc$spim$getDeathsLineListIncidence(deathOrReport = "death") %>% 
    filter(codeType == "NHSER") %>% 
    tsp$aggregateAge() %>% 
    tsp$aggregateGender() %>% 
    dpc$demog$findDemographics()
  finalDeathsNHSER = deathsNHSER %>% tsp$aggregateSubgroup(fn=sum) %>% dpc$demog$findDemographics() %>% filter(name != "Unknown (England)")
  rationale(codeType = "NHSER","death","SPI-M deaths line list","Various care settings aggregated by summation. Result is all deaths in all locations")
  
  ## Combined data set ----
  
  sourceDatasets = list(
    casesUK = casesUK,
    casesCTRY = casesCTRY,
    casesNHSER = casesNHSER,
    triageCTRY = triageCTRY,
    triageNHSER = triageNHSER,
    admissionsUK = admissionsUK,
    admissionsCTRY = admissionsCTRY,
    admissionsNHSER = admissionsNHSER,
    icuAdmissionsNHSER = icuAdmissionsNHSER,
    deathsUK = deathsUK,
    deathsCTRY = deathsCTRY,
    deathsNHSER = deathsNHSER,
    finalCasesUK = finalCasesUK,
    finalCasesCTRY = finalCasesCTRY,
    finalCasesNHSER = finalCasesNHSER,
    finalTriageCTRY = finalTriageCTRY,
    finalTriageNHSER = finalTriageNHSER,
    finalAdmissionsUK = finalAdmissionsUK,
    finalAdmissionsCTRY = finalAdmissionsCTRY,
    finalAdmissionsNHSER = finalAdmissionsNHSER,
    finalIcuAdmissionsNHSER = finalIcuAdmissionsNHSER,
    finalDeathsUK = finalDeathsUK,
    finalDeathsCTRY = finalDeathsCTRY,
    finalDeathsNHSER = finalDeathsNHSER
  )
  
  finalDataset = dplyr::bind_rows(
    finalCasesUK %>% mutate(source = "4NationsCases"),
    finalCasesCTRY %>% mutate(source = "4NationsCases"),
    finalCasesNHSER %>% mutate(source = "4NationsCases"),
    finalTriageCTRY %>% mutate(source = "Triage"),
    finalTriageNHSER %>% mutate(source = "Triage"),
    finalAdmissionsUK %>% mutate(source = "Admissions"),
    finalAdmissionsCTRY %>% mutate(source = "Admissions"),
    finalAdmissionsNHSER %>% mutate(source = "Admissions"),
    finalIcuAdmissionsNHSER %>% mutate(source = "IcuAdmissions"),
    finalDeathsUK %>% mutate(source = "Deaths"),
    finalDeathsCTRY %>% mutate(source = "Deaths"),
    finalDeathsNHSER %>% mutate(source = "Deaths")
  ) %>% dplyr::select(source, statistic, type, subgroup, gender, ageCat, date, code, codeType, name, value) %>% dpc$demog$findDemographics()
  
  
  
  set.seed(100)
  finalRtDataset = finalDataset %>%
    tsp$estimateRt() %>%
    tsp$logIncidenceStats() %>%
    tsp$estimateVolatilty(valueVar = `Mean(R)`)
  
  final28DayRtDataset = finalDataset %>%
    tsp$estimateRt(window = 28) %>%
    tsp$logIncidenceStats(growthRateWindow = 28) %>%
    tsp$estimateVolatilty(valueVar = `Mean(R)`)
  

  # devtools::load_all("~/Git/uk-covid-datatools/")
  # dpc = DataProviderController$setup("~/Data/maps")
  # dpc$loadSpimSources("~/S3/encrypted/")
  # tsp = dpc$timeseriesProcessor()
  
  finalRtAssumed = finalDataset %>%
    tsp$estimateRtWithAssumptions(quick = TRUE) %>%
    tsp$logIncidenceStats() %>%
    tsp$estimateVolatilty(valueVar = `Mean(R)`)
  
  
  return(list(
    source = sourceDatasets,
    rt = finalRtDataset,
    rt28 = final28DayRtDataset,
    rtAssumed = finalRtAssumed,
    rationale = description
  ))
  
})

#### Current Rt for LHB, HB, LTLA ----


