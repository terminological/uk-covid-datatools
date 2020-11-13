setwd("~/Git/uk-covid-datatools/vignettes/")
devtools::load_all("~/Git/standard-print-output/")
#devtools::load_all("~/Git/uk-covid-datatools/")
source("cron-estimate-rt.R")
#readRDS("~/Data/maps/2020-09-14/CURRENT-DATASET-2020-09-14.rda")
#readRDS("~/Data/maps/2020-09-14/CURRENT-RT-QUICK-2020-09-14.rda")
#readRDS("~/Data/maps/2020-09-14/CURRENT-RT-SLOW-2020-09-14.rda")

rtConfig = tsp$serial

# export7dayRt = currentRt$rt %>% 
#   filter(source %in% c("4NationsCases","Triage","Deaths")) %>% 
#   filter(
#     source != "Deaths" | name %in% c("United Kingdom","England")
#   ) %>% 
#   mutate(
#     modelType = case_when(source == "4NationsCases" ~ "Cases", source == "Deaths" ~ "Deaths", source == "Triage" ~ "Emergency", TRUE ~ NA_character_)
#   ) %>% ungroup()
# 
# export7dayGrowth = currentRt$rt %>% 
#   filter(source %in% c("4NationsCases","Triage","Deaths")) %>% 
#   filter(
#     source != "Deaths" | name %in% c("United Kingdom","England")
#   ) %>% 
#   mutate(
#     modelType = case_when(source == "4NationsCases" ~ "Cases", source == "Deaths" ~ "Deaths", source == "Triage" ~ "Emergency", TRUE ~ NA_character_)
#   ) %>% ungroup()

export14dayRt = currentRt$rt14 %>% 
  tsp$adjustRtConfidence(sdMultiplier = 4, predicate = statistic=="case" & date > "2020-09-01" & date < "2020-09-21") %>%
  tsp$adjustRtConfidence(sdMultiplier = 2, predicate = statistic=="case" & date > "2020-09-21") %>%
  filter(source %in% c("4NationsCases","Admissions","Deaths")) %>% 
  filter(
    source != "Deaths" | name %in% c("United Kingdom","England")
  ) %>%
  filter(
    !(source == "Admissions" & name %in% c("Wales"))
    #!(name %in% c("Wales"))
  ) %>%
  mutate(
    modelType = case_when(source == "4NationsCases" ~ "Cases", source == "Deaths" ~ "Deaths", source == "Triage" ~ "Emergency", source == "Admissions" ~ "Cases", TRUE ~ NA_character_)
  ) %>% ungroup()

export14dayGrowth = currentRt$rt14 %>% 
  filter(source %in% c("4NationsCases","Admissions","Deaths")) %>% 
  filter(
    source != "Deaths" | name %in% c("United Kingdom","England")
  ) %>%
  filter(
    !(source == "Admissions" & name %in% c("Wales"))
    #!(name %in% c("Wales"))
  ) %>% 
  mutate(
    modelType = case_when(source == "4NationsCases" ~ "Cases", source == "Deaths" ~ "Deaths", source == "Triage" ~ "Emergency", source == "Admissions" ~ "Cases", TRUE ~ NA_character_)
  ) %>% ungroup()



# export28dayRt = currentRt$rt28 %>% 
#   filter(source %in% c("4NationsCases","Triage","Deaths")) %>% 
#   filter(
#     source != "Deaths" | name %in% c("United Kingdom","England")
#   ) %>% 
#   mutate(
#     modelType = case_when(source == "4NationsCases" ~ "Cases", source == "Deaths" ~ "Deaths", source == "Triage" ~ "Emergency", TRUE ~ NA_character_)
#   ) %>% ungroup()
# 
# export28dayGrowth = currentRt$rt28 %>% 
#   filter(source %in% c("4NationsCases","Triage","Deaths")) %>% 
#   filter(
#     source != "Deaths" | name %in% c("United Kingdom","England")
#   ) %>% 
#   filter(source %in% c("4NationsCases","Deaths","Triage")) %>% mutate(
#     modelType = case_when(source == "4NationsCases" ~ "Cases", source == "Deaths" ~ "Deaths", source == "Triage" ~ "Emergency", TRUE ~ NA_character_)
#   ) %>% ungroup()


  ## create SPI-M export ----
  # exportSPIM7day = bind_rows(
  #   export7dayRt %>%
  #     filter(!is.na(`Median(R)`))  %>% 
  #     convertRtToSPIM(name,modelExpr = paste0("EpiEstim/",source), modelTypeExpr = modelType, version="0.01"),
  #   
  #   rtConfig %>% convertSerialIntervalToSPIM(modelName = "EpiEstim/4NationsCases", version="0.01"),
  #   rtConfig %>% convertSerialIntervalToSPIM(modelName = "EpiEstim/Deaths", version="0.01"),
  #   rtConfig %>% convertSerialIntervalToSPIM(modelName = "EpiEstim/Triage", version="0.01"),
  #   
  #   export7dayGrowth %>%
  #     filter(!is.na(`Growth.windowed.value`)) %>% 
  #     convertGrowthRateToSPIM(name,modelExpr = paste0("EpiEstim/",source),modelTypeExpr = modelType, version="0.01"),
  #   export7dayGrowth %>%
  #     filter(!is.na(`Growth.windowed.value`) & is.finite(`Growth.windowed.value`)) %>% 
  #     convertDoublingTimeToSPIM(name,modelExpr = paste0("EpiEstim/",source),modelTypeExpr = modelType, version="0.01")
  # )
  # 
  # excelOutput <- openxlsx::createWorkbook()
  # openxlsx::addWorksheet(excelOutput, "Template")
  # openxlsx::writeData(excelOutput, sheet = "Template", x = exportSPIM7day)
  # openxlsx::saveWorkbook(excelOutput, paste0("~/Dropbox/covid19/current-rt/SPIMestimates-",Sys.Date(),"-7Day.xlsx"), overwrite = TRUE)
  
  exportSPIM14day = bind_rows(
    export14dayRt %>%
      filter(!is.na(`Median(R)`))  %>% 
      convertRtToSPIM(name,modelExpr = paste0("EpiEstim/",source), modelTypeExpr = modelType, version="0.03"),
    
      rtConfig %>% convertSerialIntervalToSPIM(modelName = "EpiEstim/4NationsCases", version="0.03"),
      rtConfig %>% convertSerialIntervalToSPIM(modelName = "EpiEstim/Deaths", version="0.03"),
      rtConfig %>% convertSerialIntervalToSPIM(modelName = "EpiEstim/Admissions", version="0.03"),
      #rtConfig %>% convertSerialIntervalToSPIM(modelName = "EpiEstim/Triage", version="0.03"),
      
      export14dayGrowth %>%
        filter(!is.na(`Growth.windowed.value`)) %>% 
        convertGrowthRateToSPIM(name,modelExpr = paste0("EpiEstim/",source),modelTypeExpr = modelType, version="0.03"),
      export14dayGrowth %>%
        filter(!is.na(`Growth.windowed.value`) & is.finite(`Growth.windowed.value`)) %>% 
        convertDoublingTimeToSPIM(name,modelExpr = paste0("EpiEstim/",source),modelTypeExpr = modelType, version="0.03")
    )
  
    excelOutput <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(excelOutput, "Template")
    openxlsx::writeData(excelOutput, sheet = "Template", x = exportSPIM14day)
    openxlsx::saveWorkbook(excelOutput, paste0("~/Dropbox/covid19/current-rt/SPIMestimates-",Sys.Date(),"-14Day.xlsx"), overwrite = TRUE)
    
    exportSPIM14day %>% mutate(
      date = as.Date(paste0(`Year of Value`,"-",`Month of Value`,"-",`Day of Value`),"%Y-%m-%d"),
      estimate = sprintf("%1.2f (%1.2f; %1.2f)",`Quantile 0.5`,`Quantile 0.05`,`Quantile 0.95`)
    ) %>%
      group_by(ValueType,Geography,Model) %>% 
      filter(date == max(date)) %>%
      select(date,estimate) %>%
      filter(ValueType %in% c("doubling_time","R","growth_rate")) %>%
      pivot_wider(names_from = ValueType, values_from = estimate) %>%
      group_by(Geography,Model) %>% 
      standardPrintOutput::saveTable(paste0("~/Dropbox/covid19/current-rt/SPIMestimates-summary-",Sys.Date()),defaultFontSize = 6)
    

    # ## create SPI-M export ----
    # exportSPIM28Day = bind_rows(
    #   export28dayRt %>%
    #     filter(!is.na(`Median(R)`)) %>% 
    #     convertRtToSPIM(name,modelExpr = paste0("EpiEstim/",source),modelTypeExpr = modelType, version="0.02"),
    #   
    #   rtConfig %>% convertSerialIntervalToSPIM(modelName = "EpiEstim/4NationsCases", version="0.02"),
    #   rtConfig %>% convertSerialIntervalToSPIM(modelName = "EpiEstim/Deaths", version="0.02"),
    #   rtConfig %>% convertSerialIntervalToSPIM(modelName = "EpiEstim/Triage", version="0.02"),
    #   
    #   export28dayGrowth %>%
    #     filter(!is.na(`Growth.windowed.value`)) %>% 
    #     convertGrowthRateToSPIM(name,modelExpr = paste0("EpiEstim/",source),modelTypeExpr = modelType, version="0.02"),
    #   export28dayGrowth %>%
    #     filter(!is.na(`Growth.windowed.value`) & is.finite(`Growth.windowed.value`)) %>% 
    #     convertDoublingTimeToSPIM(name,modelExpr = paste0("EpiEstim/",source),modelTypeExpr = modelType, version="0.02")
    #   
    # )
    # 
    # excelOutput <- openxlsx::createWorkbook()
    # openxlsx::addWorksheet(excelOutput, "Template")
    # openxlsx::writeData(excelOutput, sheet = "Template", x = exportSPIM28Day)
    # openxlsx::saveWorkbook(excelOutput, paste0("~/Dropbox/covid19/current-rt/SPIMestimates-",Sys.Date(),"-28Day.xlsx"), overwrite = TRUE)
