devtools::document()

devtools::load_all("~/Git/standard-print-output/")
standardPrintOutput::setDefaults()
#test_that("Dataset provider does something sane", {
  
  # dpc = DataProviderController$setup("~/Data/maps")
  # dpc$loadSpimSources("~/S3/encrypted/")
  # deaths = dpc$datasets$getNHSDeaths() #nocache=TRUE)
  # tsp = dpc$timeseriesProcessor()
  # deaths2 = deaths %>% tsp$imputeAndWeeklyAverage() #nocache=TRUE)
  # deaths %>% tsp$plotIncidence(group=code)
  
  
#})
devtools::load_all()
  dpc = DataProviderController$setup("~/Data/maps")
  dpc$loadSpimSources("~/S3/encrypted/")
  tsp = dpc$timeseriesProcessor()
  #tsp$midmarketSerialInterval()#nocache=TRUE)
  
  #deaths = dpc$spim$getDeathsLineListIncidence(deathOrReport = "death") %>% filter(codeType == "NHSER") %>% tsp$aggregateAge() %>% tsp$aggregateGender() %>% tsp$aggregateSubgroup()
  
  # cases = dpc$spim$getLineListIncidence(specimenOrReport = "specimen") %>% filter(codeType == "NHSER") %>% tsp$aggregateAge() %>% tsp$aggregateGender() #%>% tsp$aggregateSubgroup()
  # deaths = dpc$spim$getDeathsLineListIncidence(deathOrReport = "death") %>% filter(codeType == "NHSER") %>% tsp$aggregateAge() %>% tsp$aggregateGender() #%>% tsp$aggregateSubgroup()
  # tsp$plotIncidenceRollmean(cases, colour=name)+facet_wrap(vars(subgroup))+ scale_y_continuous(trans="log1p")
  # tsp$plotIncidenceQuantiles(cases, colour=name)+facet_wrap(vars(subgroup))+scale_y_continuous(trans="log1p")
  #deathReport %>% tsp$plotIncidenceQuantiles(colour=name)
  
  # deathsNoSetting = dpc$spim$getDeathsLineListIncidence(deathOrReport = "death") %>% filter(codeType == "NHSER") %>% tsp$aggregateAge() %>% tsp$aggregateGender() %>% tsp$aggregateSubgroup()
  
  
  # cases = dpc$demog$findDemographics(cases)
  # deaths = dpc$demog$findDemographics(deaths)
  # deathsNoSetting = dpc$demog$findDemographics(deathsNoSetting)
  # tsp$plotIncidenceQuantiles(deaths, denominatorExpr = population/1000000, events = events, colour=subgroup)+facet_wrap(vars(name))+scale_y_continuous(trans="log1p")+ylab("per 1M per day")
  # tsp$plotIncidenceQuantiles(deathsNoSetting, denominatorExpr = population/1000000, events = events)+facet_wrap(vars(name))+scale_y_continuous(trans="log1p")+ylab("per 1M per day")
  # tsp$plotIncidenceQuantiles(cases, denominatorExpr = population/1000000, events = events, colour=subgroup)+facet_wrap(vars(name))+scale_y_continuous(trans="log1p")+ylab("per 1M per day")
  events = dpc$datasets$getSignificantDates() %>% filter(Label %in% c("Lockdown","VE day","Anti-racism demos","Cummingsgate"))
  
  #tmp = cases %>% tsp$imputeAndWeeklyAverage() %>% tsp$estimateRtQuick()
  
  #p1 = tsp$plotRt(cases,colour=name, events = events)+facet_grid(cols = vars(subgroup), rows=vars(name))
  #p1
  #p2 = tsp$plotGrowthRate(cases,colour=name, events = events)+facet_grid(cols = vars(subgroup), rows=vars(name))
  #p2

  oneoneone = dpc$spim$getOneOneOne() %>% filter(statistic == "triage" & codeType == "NHSER" & source %in% c("111","999")) %>% tsp$aggregateSource()
  oneoneone = dpc$demog$findDemographics(oneoneone)
  tsp$plotIncidenceQuantiles(oneoneone, denominatorExpr = population/1000000, events = events, colour=subgroup)+facet_wrap(vars(name))+scale_y_continuous(trans="log1p")+ylab("per 1M per day")
  
  urgentOneOneOne = oneoneone %>% filter(subgroup %in% c("urgent clinical review","emergency ambulance")) %>% tsp$aggregateSubgroup()
  urgentOneOneOne = dpc$demog$findDemographics(urgentOneOneOne)
  tsp$plotIncidenceQuantiles(urgentOneOneOne, denominatorExpr = population/1000000, events = events)+facet_wrap(vars(name))+scale_y_continuous(trans="log1p")+ylab("per 1M per day")
  tsp$plotGrowthRate(urgentOneOneOne, events = events)+facet_wrap(vars(name))
  
  
  
  all = dpc$spim$getSPIMextract()
  admissions = all %>% filter(source %in% c("hospital_inc","hospital_inc_new","chess_icu_admissions") & !is.na(ageCat) & codeType=="NHSER") %>% 
    tsp$aggregateSource(list(admissions = c("hospital_inc","hospital_inc_new")))
  admissions = dpc$demog$findDemographics(admissions)
  tsp$plotIncidenceQuantiles(admissions, denominatorExpr = population/1000000, colour=ageCat, events = events)+facet_grid(cols=vars(source), rows=vars(name))+scale_y_continuous(trans="log1p")+ylab("per 100K per day")
  tsp$plotGrowthRate(admissions %>% tsp$aggregateAge(), events = events)+facet_grid(cols=vars(source), rows=vars(name))
  tsp$plotRt(admissions %>% tsp$aggregateAge(), events = events)+facet_grid(cols=vars(source), rows=vars(name))
  
  
  ctryHospAdm = all %>% filter(statistic == "hospital admission" & type %in% c("incidence","cumulative") & codeType == "CTRY" & is.na(ageCat))
  ctryHospAdm = dpc$demog$findDemographics(ctryHospAdm)
  tsp$plotIncidenceRollmean(ctryHospAdm, denominatorExpr = population/1000000, colour=source, events = events) +facet_wrap(vars(name))+scale_y_continuous(trans="log1p")
  
  #ctryHospAdm2 = ctryHospAdm %>% filter(!(name=="Wales" & source %in% c("hospital_inc_new","spim_hosp_inc_new")))
  ctryHospAdm2 = ctryHospAdm %>% filter(!(name=="Wales" & source %in% c("hospital_inc_new")))
  #ctryHospAdm2 = ctryHospAdm
  tsp$plotIncidenceRollmean(ctryHospAdm2, denominatorExpr = population/1000000, colour=source, events = events) +facet_wrap(vars(name))+scale_y_continuous(trans="log1p")
  
  ctryHospAdm2 = ctryHospAdm2 %>% tsp$aggregateSource(fn=mean) %>% dpc$demog$findDemographics()
  tmp = ctryHospAdm2 %>% tsp$imputeAndWeeklyAverage()
  tsp$plotIncidenceRollmean(ctryHospAdm2, denominatorExpr = population/1000000, events = events) +facet_wrap(vars(name))+scale_y_continuous(trans="log1p")
  
  
  
  all %>% tsp$describe() %>% View()
  ctryDeath = all %>% 
    filter(statistic=="death" & source %in% c("chess_death_inc_line","sitrep_death_inc_line","dashboard_daily_death_inc_dash","nisra_death_inc_line","ons_death_inc_line","phw_death_inc_line") & is.na(ageCat) & codeType=="CTRY") %>% 
    dpc$demog$findDemographics()
  tsp$plotIncidenceRollmean(ctryDeath, denominatorExpr = population/1000000, events = events, colour=source) +facet_wrap(vars(name))+scale_y_continuous(trans="log1p")
  
  ctryDeath2 = all %>% 
    filter(statistic=="death" & source %in% c("chess_death_inc_line","sitrep_death_inc_line","phw_death_inc_line") & is.na(ageCat) & codeType=="CTRY") %>% 
    tsp$aggregateSource(fn=mean) %>%
    dpc$demog$findDemographics()
  tsp$plotIncidenceRollmean(ctryDeath2, denominatorExpr = population/1000000, events = events) +facet_wrap(vars(name))+scale_y_continuous(trans="log1p")
  
  
  
# # library(tidyverse)
# # library(sf)
# ugp = UKGeographyProvider$new("~/Data/maps")
# upp = UKPostcodeProvider$new("~/Data/maps")
# ump = UKCodeMappingProvider$new(upp)
# udp = UKDemographicsProvider$new(ugp,ump)
# test = ncp$getNHSTrustIcuCatchment()
# tmp3 = udp$getSingleDigitDemographics(test$crossMapping, code, trustId, weightExpr = 1, combineGenders = TRUE)
# # # udp$debug = TRUE
# # tmp = udp$getDemographicsForCodeTypes(codeTypes=c("HB","LHB","CCG"))
# 
# tmp = udp$getDemographicsForCodeTypes(codeTypes=c("PHEC")) %>% ump$findNamesByCode()
# # ugp$loadAllMaps()
# # # dm = ugp$getPHEDashboardMap()
# # # ugp$saveShapefile("DASH_LTLA", dm)
# # # tmp = udp$getDemographicsForShape("DASH_LTLA", dm %>% dplyr::group_by(code,name), combineGenders = FALSE)
# # 
# # # sf = udp$getDemographicsMap()
# # # d = udp$getDetailedDemographics()
# # # x = udp$getDemographicsForShape("CTRY19")
# # # sf = udp$getDemographicsForShape("WD11")
# # # # sf = udp$getDemographicsForShape("LSOA11")
# # # # sf = udp$getDemographicsForShape("SGDZ11")
# # # 
# sf = udp$getDemographicsForShape("HB19")
# # # udp$preview("LHB19")
# # # sf = usp$getDemographicsForShape("CTYUA19")
# sf = udp$getDemographicsForShape("LAD19")
# # # sf = usp$getDemographicsForShape("CCG20")
# # # sf = usp$getDemographicsForShape("NHSER20")
# # # sf = usp$getDemographicsForShape("PHEC16")
# # # sf = usp$getDemographicsForShape("LGD12")
# # # plot(sf)
# # 
# # tmp = usp$getHospitals(icuBeds>0 & sector=="NHS Sector")
# # catch = usp$createCatchment(
# #   supplyId = "ICUBEDS", supplyShape = tmp %>% dplyr::rename(hospId = code, hospName = name) %>% dplyr::group_by(hospId,hospName), supplyIdVar = hospId, supplyVar = icuBeds,
# #   demandId = "DEMOG", demandShape = usp$getDemographicsMap(), demandVar = count, demandIdVar = code
# # )
# # 
# # usp$preview(shape=catch$map %>% dplyr::mutate(per100K = 100000*icuBeds/count),nameVar = hospName, codeVar = per100K, poi=catch$suppliers, poiNameVar = hospName, poiCodeVar = icuBeds)


# # source("./R/PassthroughFilesystemCache.R")
# # source("./R/UKPostcodeProvider.R")
# # upp = UKPostcodeProvider$new("~/Data/maps")
# ump = UKCodeMappingProvider$new(upp)
# odsCodes = ump$getODSCodes()
# onsCodes = ump$getONSRegister()
# manualCodes = ump$getManualCodes()
# onsMaps = ump$getONSMappings()
# odsMaps = ump$getODSMappings()
# manualMaps = ump$getManualMappings()
# descriptions = ump$getDescriptions()
# codes = ump$getCodes()
# maps = ump$getMappings()
# tc = ump$getTransitiveClosure()
# # 
# # out = tibble(code=c("S08000011",
# # "S08000012",
# # "S08000013"
# # )) %>% ump$findNamesByCode()
# # 
# # # ump$debug = TRUE
# # #ump$getODSMappings()
# # #tmp = ump$getODSCodes()
# # #tmp = ump$getMappings()
# ump$findCodesByName(tibble(x="Musgrove Park Hospital"),nameVar = x)
# 
# # # View(tc %>% group_by(fromCodeType, toCodeType) %>% count())
# # # View(tc %>% filter(fromCodeType == "NHS site" & toCodeType=="CCG"))
# # # View(tc %>% filter(fromCodeType == "NHS site" & toCodeType=="PHEC"))


# ugp = UKGeographyProvider$new("~/Data/maps")
# upp  = UKPostcodeProvider$new("~/Data/maps")
# ump = UKCodeMappingProvider$new(postcodeProvider = upp)
# udp= UKDemographicsProvider$new(geographyProvider=ugp, codeMappingProvider = ump)
# ncp = NHSCapacityProvider$new(demographicsProvider = udp)
# 
# tmp = ncp$getNHSTrustIcuCatchment() #nocache=TRUE)
# tmp = ncp$getNHSSiteAcuteCatchment() # nocache=TRUE)
# tmp = ncp$getNHSTrustAcuteCatchment() #nocache=TRUE)
# tmp = ncp$getNHSSiteIcuCatchment() #nocache=TRUE)
# # 
# # 
# # tmp2 = udp$getDemographicsFromWeightedMapping(mappingDf = tmp$crossMapping, fromCodeVar = code,toCodeVar = hospId)
# # library(tidyverse)
# # library(sf)
# # usp = UKStatisticsProvider$new("~/Data/maps")
# # usp$loadAllMaps()
# # # dm = usp$getPHEDashboardMap()
# # # #usp$saveShapefile("DASH_LTLA", dm)
# # # tmp = usp$getDemographics("DASH_LTLA", dm %>% group_by(code,name), combineGenders = FALSE)
# # 
# # # sf = usp$getDemographicsMap()
# # # d = usp$getDetailedDemographics()
# # # x = usp$getDemographics("CTRY19")
# # # sf = usp$getDemographics("WD11")
# # # # sf = usp$getDemographics("LSOA11")
# # # # sf = usp$getDemographics("SGDZ11")
# # # sf = usp$getDemographics("SHB19")
# # # usp$preview("LHB19")
# # # sf = usp$getDemographics("CTYUA19")
# # # sf = usp$getDemographics("LAD19")
# # # sf = usp$getDemographics("CCG20")
# # # sf = usp$getDemographics("NHSER20")
# # # sf = usp$getDemographics("PHEC16")
# # # sf = usp$getDemographics("LGD12")
# # # plot(sf)
# # 
# # tmp = usp$getHospitals(icuBeds>0 & sector=="NHS Sector")
# # catch = usp$createCatchment(
# #   supplyId = "ICUBEDS", supplyShape = tmp %>% rename(hospId = code, hospName = name) %>% group_by(hospId,hospName), supplyIdVar = hospId, supplyVar = icuBeds,
# #   demandId = "DEMOG", demandShape = usp$getDemographicsMap(), demandVar = count, demandIdVar = code
# # )
# # 
# # usp$preview(shape=catch$map %>% mutate(per100K = 100000*icuBeds/count),nameVar = hospName, codeVar = per100K, poi=catch$suppliers, poiNameVar = hospName, poiCodeVar = icuBeds)


# # smoke tests
# ugp = UKGeographyProvider$new("~/Data/maps")
# upp = UKPostcodeProvider$new("~/Data/maps")
# ump = UKCodeMappingProvider$new(upp)
# udp= UKDemographicsProvider$new(geographyProvider=ugp, codeMappingProvider = ump)
# ncp = NHSCapacityProvider$new(demographicsProvider=udp)
# ndp = NHSDatasetProvider$new("~/S3/encrypted/",ncp)
# ndp$findDatafiles()
# ndp$debug=TRUE
# tmp = ndp$getDstlSource()

# # library(tidyverse)
# # library(sf)
# ugp = UKGeographyProvider$new("~/Data/maps")
# # # ugp$nocache = TRUE
# # tmp = ugp$getMap("HB19")
# # ugp$loadAllMaps()
# 
# # 
# 
# # 
# # ugp$preview(shape=catch$map %>% dplyr::mutate(per100K = 100000*icuBeds/count),nameVar = hospName, codeVar = per100K, poi=catch$suppliers, poiNameVar = hospName, poiCodeVar = icuBeds)

