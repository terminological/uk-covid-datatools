#!/usr/bin/Rscript

library(tidyverse)

setwd("~/Git/uk-covid-datatools/vignettes/")

devtools::load_all("~/Git/uk-covid-datatools/")
ukcovidtools::setup()
# dpc = DataProviderController$setup("~/Data/maps")
# s3 = fileProvider("~/Dropbox/coviddata.yaml","s3")
# dpc$loadSpimSources(s3)
# tsp = dpc$timeseriesProcessor()

# triageNHSER = dpc$spim$getOneOneOne() %>% 
#   filter(statistic == "triage" & codeType == "NHSER" & source %in% c("111","999")) %>% 
#   tsp$aggregateSource(fn=sum, na.rm=TRUE) %>% 
#   filter(date >= "2020-03-15") %>%
#   dpc$demog$findDemographics()
# finalTriageNHSER = triageNHSER %>% filter(subgroup %in% c("urgent clinical review","emergency ambulance")) %>% 
#   tsp$aggregateSubgroup() %>% 
#   dpc$demog$findDemographics()
# tsp$plotIncidenceQuantiles(triageNHSER, denominatorExpr = population/1000000, events = events, colour=subgroup)+
#    facet_wrap(vars(name))+
#    scale_y_continuous(trans="log1p", breaks = c(0,5,15,50,150))+ylab("per 1M per day")


# TODO: Investigate backcalculation with:
# https://epiforecasts.io/EpiNow2/dev/reference/estimate_infections.html

#### Current Rt for UK, CTRY, NHSER ----

currentDataset = tsp$getDaily(id = "CURRENT-DATASET", orElse=function() {
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
  triageCTRY = dpc$spim$getOneOneOneIncidence(dateFrom = as.integer(Sys.Date()-26*7)) %>% filter(!ageCat %in% c("<1","1-4","5-14","15-24")) %>% 
    tsp$aggregateAge() %>%
    tsp$aggregateGeography(targetCodeTypes = "CTRY") %>%
    filter(statistic == "triage" & codeType == "CTRY") %>% 
    #tsp$aggregateSource(fn=sum) %>% #, na.rm=TRUE) %>% 
    filter(date >= "2020-03-15") %>%
    dpc$demog$findDemographics()
  # triageCTRY = dpc$spim$getOneOneOne() %>% 
  #   filter(statistic == "triage" & codeType == "CTRY" & name=="England" & source %in% c("111","999")) %>% 
  #   tsp$aggregateSource(fn=sum, na.rm=TRUE) %>% 
  #   filter(date >= "2020-03-15") %>%
  #   dpc$demog$findDemographics()
  finalTriageCTRY = triageCTRY %>% filter(subgroup %in% c("urgent clinical review","emergency ambulance")) %>% tsp$aggregateSubgroup() %>% dpc$demog$findDemographics()
  rationale(codeType = "CTRY","triage","SPI-M 111 breakdown (only available for England)","Selected only calls with outcomes with 1 or 2 hour urgent clinical review, or ambulance dispatch, in ages > 24.")
  
  tmp4Nations = dpc$datasets$getPHEApiNations()
  
  ### Cases - CTRY ----
  casesCTRY = bind_rows(
    tmp4Nations %>% filter(statistic == "case" & codeType == "CTRY") %>% dpc$demog$findDemographics(),
    dpc$spim$getLineListIncidence(specimenOrReport = "specimen",subgroup = asymptomatic_indicator) %>% 
      filter(codeType == "CTRY" & subgroup!="Y") %>% 
      tsp$aggregateAge() %>% 
      tsp$aggregateGender() %>% tsp$aggregateSubgroup() %>%
      dpc$demog$findDemographics()
  )
  
  finalCasesCTRY = casesCTRY %>% filter(!(name=="England" & source=="phe api")) %>% mutate(source = "aggregate PHE and linelist", subgroup=NA)
  rationale(codeType = "CTRY","case",
            "4 nations case counts from 4 national PH sites, retrieved as timeseries from PHE API, England pillar 1 & 2 tests from linelist where not asymptomatic",
            "Line list ")
  
  ### Admissions - CTRY ----
  # admissionsCTRY = dpc$spim$getSPIMextract() %>% 
  #   filter(statistic == "hospital admission" & type %in% c("incidence","cumulative") & codeType == "CTRY" & is.na(ageCat)) %>% 
  #   dpc$demog$findDemographics()
  # finalAdmissionsCTRY = admissionsCTRY %>% 
  #   filter(!(name=="Wales" & source %in% c("hospital_inc_new","hospital_inc_new_acuteone"))) %>%
  #   filter(!(name=="Scotland" & source %in% c("hospital_inc_new","hospital_inc_new_acuteone"))) %>%
  #   tsp$aggregateSource(namedListOfSources = list(
  #     hosp_inc_sum = c("hospital_inc", "hospital_inc_new"),
  #     hosp_inc_acuteone_sum = c("hospital_inc_acuteone", "hospital_inc_new_acuteone")
  #   ), fn=sum) %>% 
  #   tsp$aggregateSource(fn=mean, na.rm=TRUE) %>% 
  #   dpc$demog$findDemographics()
  # rationale(codeType = "CTRY","hospital admission",
  #     paste0("Various SPI-M data feeds: ", paste0(unique(admissionsCTRY$source), collapse = ", ")),
  #     "Hospital_inc_new & Hospital_inc_new_acuteone data set excluded for Wales. Hospital_inc_new and hospital_inc fields combined by summation, result and other sources combined by average, excluding missing values")
  
  admissionsCTRY = bind_rows(
    tmp4Nations %>% filter(statistic == "hospital admission"),
    dpc$spim$getSPIMextract() %>% filter(name == "Wales" & type == "incidence" & statistic =="hospital admission" & source=="spim_hosp_inc_new_authorisation_date")
  ) %>% dpc$demog$findDemographics()
  
  finalAdmissionsCTRY = admissionsCTRY %>% filter(name != "Wales" | source != "phe api") %>% mutate(source="mixed api and spim")
  rationale(codeType = "CTRY","hospital admission",
            "PHE api for everythig except Wales, spim_hosp_inc_new_authorisation_date field for Wales",
            "None")
  
  ### ICU Admissions - CTRY ----
  #TODO: not followed up on at present
  
  ### Deaths - CTRY ----
  deathsCTRY = tmp4Nations %>% 
    filter(statistic=="death") %>% 
    dpc$demog$findDemographics()
  finalDeathsCTRY = deathsCTRY 
  rationale(codeType = "CTRY","death",
            "PHE API",
            "none")
  
  
  # deathsCTRY = dpc$spim$getSPIMextract() %>% 
  #   filter(statistic=="death" & source %in% c("chess_death_inc_line","sitrep_death_inc_line","dashboard_daily_death_inc_dash","nisra_death_inc_line","ons_death_inc_line","phw_death_inc_line") & is.na(ageCat) & codeType=="CTRY") %>% 
  #   dpc$demog$findDemographics()
  # deathsCTRYSources = c("chess_death_inc_line","sitrep_death_inc_line","phw_death_inc_line")
  # finalDeathsCTRY = deathsCTRY %>%  
  #   filter(source %in% deathsCTRYSources) %>% 
  #   tsp$aggregateSource(fn=mean) %>%
  #   dpc$demog$findDemographics()
  # rationale(codeType = "CTRY","death",
  #           paste0("SPI-M data feeds considered: ", paste0(unique(deathsCTRY$source), collapse = ", ")),
  #           paste0("Feeds selected: ", paste0(unique(deathsCTRYSources), collapse = ", "),"; combined by average if more than one per country")
  # )
  # 
  ### Cases - UK ----
  maxDateCases = finalCasesCTRY %>% group_by(name) %>% summarise(date = max(date)) %>% pull(date) %>% min()
  
  casesUK = #bind_rows(
      #dpc$datasets$getTomWhiteIndicators() %>% filter(statistic == "case" & codeType == "UK") %>% tsp$incidenceFromCumulative(),
      finalCasesCTRY %>% filter(date<=maxDateCases) %>% 
        tsp$aggregateGeography(targetCodeTypes = "UK", completeness = "both",keepOriginal = FALSE) %>% 
    dpc$demog$findDemographics()
  finalCasesUK = casesUK
    #finalCasesCTRY %>% filter(date<=maxDateCases) %>%
    #tsp$aggregateGeography(targetCodeTypes = "UK", completeness = "source",keepOriginal = FALSE) %>%
    #dpc$demog$findDemographics()
  rationale(codeType = "UK","case","UK headline from PHE and 4 nations case counts from 4 national PH sites, retrieved as timeseries from PHE API. Pillar 1 tests from line list for England.",
            "Ultimately sum of 4 nations case counts previously selected for CTRY cases used as headline numbers variably include Pillar 2 tests.")
  
  ### Admissions - UK ----
  # maxDateAdmissions = finalAdmissionsCTRY %>% group_by(name) %>% summarise(date = max(date)) %>% pull(date) %>% min()
  admissionsUK = finalAdmissionsCTRY %>% #filter(date<=maxDateAdmissions) %>%
    tsp$aggregateGeography(targetCodeTypes = "UK", completeness = "both",keepOriginal = FALSE) %>% 
    dpc$demog$findDemographics()
  finalAdmissionsUK = admissionsUK
  rationale(codeType = "UK","hospital admission","Sum of 4 nations figures previously selected for CTRY hospital admissions","none")
  
  ### Deaths - UK ----
  #maxDateDeaths = finalDeathsCTRY %>% filter(name %in% c("England","Wales")) %>% group_by(name) %>% summarise(date = max(date)) %>% pull(date) %>% min()
  deathsUK = #bind_rows(
    #dpc$datasets$getTomWhiteIndicators() %>% filter(statistic == "death" & codeType == "UK") %>% tsp$incidenceFromCumulative(),
    finalDeathsCTRY %>% # filter(date<=maxDateDeaths) %>% 
      tsp$aggregateGeography(targetCodeTypes = "UK", completeness = "both",keepOriginal = FALSE) %>% 
    dpc$demog$findDemographics()
  finalDeathsUK = deathsUK
    #finalDeathsCTRY %>% filter(date<=maxDateDeaths) %>% 
    #tsp$aggregateGeography(targetCodeTypes = "UK", completeness = "both",keepOriginal = FALSE) %>% 
    #dpc$demog$findDemographics()
  rationale(codeType = "UK","death","Sum of 4 nations figures previously selected for CTRY deaths from PHE API","none")
  
  ### Triage calls - NHSER ----
  triageNHSER = dpc$spim$getOneOneOneIncidence(dateFrom = as.integer(Sys.Date()-26*7)) %>% 
    filter(!ageCat %in% c("<1","1-4","5-14","15-24")) %>% 
    tsp$aggregateAge(na.rm=TRUE) %>%
    tsp$aggregateGeography(targetCodeTypes = "NHSER",na.rm=TRUE) %>%
    filter(statistic == "triage" & codeType == "NHSER") %>% 
    #tsp$aggregateSource(fn=sum) %>% #, na.rm=TRUE) %>% 
    filter(date >= "2020-03-15") %>%
    dpc$demog$findDemographics()
  finalTriageNHSER = triageNHSER %>% filter(subgroup %in% c("urgent clinical review","emergency ambulance")) %>% 
    tsp$aggregateSubgroup() #%>% 
    #dpc$demog$findDemographics()
  rationale(codeType = "NHSER","triage","SPI-M 111 feed","Selected only calls with outcomes with 1 or 2 hour urgent clinical review, or ambulance dispatch, in ages > 24.")
  
  ### Cases - NHSER ----
  casesNHSER = 
    dpc$spim$getLineListIncidence(specimenOrReport = "specimen",subgroup = asymptomatic_indicator) %>% 
    filter(codeType == "NHSER") %>% 
    filter(code != "E99999999") %>%
    tsp$aggregateAge() %>% 
    tsp$aggregateGender() %>% 
    dpc$demog$findDemographics()
  finalCasesNHSER = casesNHSER %>% 
    filter(subgroup!="Y") %>%
    tsp$aggregateSubgroup() #%>%
    #dpc$demog$findDemographics()
  rationale(codeType = "NHSER","case","SPI-M line list","Line list aggregated by age and gender. unknown regions removed. Pillar 1 and 2 combined - symptomatic and unknown only.")
  
  nhserApi = tsp$datasets$getPHEApiNHSRegions()
  
  ### Admissions - NHSER ----
  
  sariAdmissions = tsp$spim$getSARISummary()
  tmp = sariAdmissions %>% tsp$aggregateAge()
  tmp2 = tmp %>% tsp$imputeAndWeeklyAverage()
  tmp3 = tmp2 %>% select(-value) %>% rename(value = Imputed.value) %>% tsp$aggregateGeography(targetCodeTypes = "NHSER")
  sariAdmissionsNHSER = tmp3 %>% filter(type=="incidence" & statistic=="hospital admission" & date > "2020-03-15")
  
  admissionsNHSER = bind_rows(
      nhserApi %>% filter(statistic =="hospital admission"), 
      dpc$spim$getSPIMextract() %>% filter(source %in% c("hospital_inc","hospital_inc_new") & is.na(ageCat) & codeType=="NHSER"),
      sariAdmissionsNHSER
    ) %>% 
    dpc$demog$findDemographics()
  finalAdmissionsNHSER = admissionsNHSER %>% 
    filter(source=="phe api")
    #tsp$aggregateSource(list(admissions = c("hospital_inc","hospital_inc_new")), fn=sum)  #%>% 
    #dpc$demog$findDemographics()
  rationale(codeType = "NHSER","hospital admission","PHE api","none") #"SPI-M hospital_inc and hospital_inc_new fields","Souces combined by summation")
  
  ### ICU Admissions - NHSER ----
  icuAdmissionsNHSER = dpc$spim$getSPIMextract() %>% 
    filter(source %in% c("chess_icu_admissions") & is.na(ageCat) & codeType=="NHSER") %>% 
    dpc$demog$findDemographics()
  finalIcuAdmissionsNHSER = icuAdmissionsNHSER
  rationale(codeType = "NHSER","icu admission","SPI-M chess_icu_admissions field","none")
  
  ### Deaths - NHSER ----
  deathsNHSER = dpc$spim$getDeathsLineListIncidence(deathOrReport = "death") %>% 
    filter(codeType == "NHSER") %>% 
    filter(code != "E99999999") %>%
    tsp$aggregateAge() %>% 
    tsp$aggregateGender() %>% 
    dpc$demog$findDemographics()
  finalDeathsNHSER = deathsNHSER %>% tsp$aggregateSubgroup(fn=sum) %>% #dpc$demog$findDemographics() %>% 
    filter(name != "Unknown (England)")
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
  
  return(list(
    source = sourceDatasets,
    finalDataset = finalDataset,
    rationale = description
  ))
  
})

#### Current Rt for LHB, HB, LTLA ----

# currentRtQuick = tsp$getDaily(id = "CURRENT-RT-QUICK", orElse=function() {
#   set.seed(100)
#   with(currentDataset, {
#     # browser()
#     finalRtAssumed = finalDataset %>%
#       tsp$estimateRtWithAssumptions(quick = TRUE) %>%
#       tsp$logIncidenceStats() %>%
#       tsp$adjustRtDates() %>%
#       tsp$estimateVolatilty(valueVar = `Mean(R)`)
#     
#     return(list(
#       rtAssumedQuick = finalRtAssumed
#     ))
#   })
# })

# currentRt = c(
#   currentDataset,
#   currentRtQuick
# )

currentRtSlow = tsp$getDaily(id = "CURRENT-RT-SLOW", orElse=function() {
  set.seed(100)
  with(currentDataset, {
    
    # finalRtDataset = finalDataset %>%
    #   tsp$estimateRt() %>%
    #   tsp$logIncidenceStats() %>%
    #   tsp$adjustRtDates() %>%
    #   tsp$estimateVolatilty(valueVar = `Mean(R)`)
    
    final14DayRtDataset = finalDataset %>%
      tsp$estimateRt(window = 14) %>%
      tsp$logIncidenceStats(growthRateWindow = 14) %>%
      tsp$adjustRtDates() %>%
      tsp$estimateVolatilty(valueVar = `Mean(R)`)
    
    final7DayRtDataset = finalDataset %>%
      tsp$logIncidenceStats(growthRateWindow = 7) %>% 
      tsp$estimateRt(window = 7) %>%
      tsp$adjustRtDates() %>%
      tsp$estimateVolatilty(valueVar = `Mean(R)`)
    
    # final28DayRtDataset = finalDataset %>%
    #   tsp$estimateRt(window = 28) %>%
    #   tsp$logIncidenceStats(growthRateWindow = 28) %>%
    #   tsp$adjustRtDates() %>%
    #   tsp$estimateVolatilty(valueVar = `Mean(R)`)
    # 
    # finalRtCorrected = finalDataset %>%
    #   tsp$estimateRtWithAssumptions(quick = FALSE) %>%
    #   tsp$logIncidenceStats() %>%
    #   #tsp$adjustRtDates() %>%
    #   #tsp$adjustRtCorrFac() %>%
    #   tsp$estimateVolatilty(valueVar = `Mean(R)`)
    
    return(list(
      # rt = finalRtDataset,
      rt14 = final14DayRtDataset,
      rt7 = final7DayRtDataset#,
      # rt28 = final28DayRtDataset#,
      #rtAssumed = finalRtCorrected
    ))
  })
})

currentRt = c(
  currentDataset,
  #currentRtQuick,
  currentRtSlow
)


#try(detach("package:jepidemic", unload = TRUE),silent = TRUE)
#remove.packages("jepidemic")

if (!require(jepidemic)) {
  devtools::install("~/Git/jepidemic/r-library/", upgrade = "never")
}

currentRtJEpidemic = tsp$getDaily(id = "CURRENT-RT-JEPIDEMIC", orElse=function() {
  set.seed(100)
  with(currentDataset, {

    unsmoothed7DayRtDataset = finalDataset %>%
      tsp$logIncidenceStats(growthRateWindow = 7) %>% mutate(
        Imputed.value = ifelse(is.na(value), Est.value, value)
      )  %>% 
      filter(statistic %in% c("case","hospital admission","death")) %>% 
      covidStandardGrouping() 

    
    
    J = jepidemic::JavaApi$new()
    estim = J$CoriEstimator$new(r0Mean = 5,r0SD = 5,maxWindow = 21)
    estim$withInfectivityProfileMatrix(dpc$serial$getBasicConfig(quick = FALSE)$si_sample)
    estim$withAdaptivePrior(factor = 1.25)
    estim$selectAdaptiveWindow(incidenceSum = 200,minWindow = 7)
    #estim$selectMinimumUncertainty(timeVsRt = 1,minWindow = 6)
    estim$collectMixtureQuantiles()
    estim$inMiddleOfTimeseries()
    estim$legacySupport(TRUE)
    jepidem = estim$estimateRt(unsmoothed7DayRtDataset,dateColName = "date", incidenceColName = "Imputed.value")
    
    unsmoothed7DayRtDataset = unsmoothed7DayRtDataset %>% mutate(Anomaly.R = Anomaly) %>% 
      inner_join(jepidem, by = covidStandardJoins()) %>%
      tsp$adjustRtDates() %>%
      tsp$estimateVolatilty(valueVar = `Mean(R)`)
    
    return(list(jepi7 = unsmoothed7DayRtDataset))
  })
})

currentRt = c(
  currentDataset,
  currentRtSlow,
  currentRtJEpidemic
)
