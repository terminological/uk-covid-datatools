setwd("~/Git/uk-covid-datatools/vignettes/")
#devtools::load_all("~/Git/uk-covid-datatools/")
source("cron-estimate-rt.R")
#readRDS("~/Data/maps/2020-09-14/CURRENT-DATASET-2020-09-14.rda")
#readRDS("~/Data/maps/2020-09-14/CURRENT-RT-QUICK-2020-09-14.rda")
#readRDS("~/Data/maps/2020-09-14/CURRENT-RT-SLOW-2020-09-14.rda")

rtConfig = tsp$serial

export7dayRt = currentRt$rt %>% 
  filter(!(name %in% c("Northern Ireland","Scotland","Wales"))) %>% 
  filter(source %in% c("4NationsCases","Triage")) %>% 
  mutate(
    modelType = case_when(source == "4NationsCases" ~ "Cases", source == "Deaths" ~ "Deaths", source == "Triage" ~ "Emergency", TRUE ~ NA_character_)
  ) %>% ungroup()
export7dayGrowth = currentRt$rt %>% 
  #filter(!(name %in% c("Northern Ireland","Scotland","Wales"))) %>% 
  filter(source %in% c("4NationsCases","Triage")) %>% mutate(
    modelType = case_when(source == "4NationsCases" ~ "Cases", source == "Deaths" ~ "Deaths", source == "Triage" ~ "Emergency", TRUE ~ NA_character_)
  ) %>% ungroup()

export28dayRt = currentRt$rt28 %>% 
  filter(!(name %in% c("Northern Ireland","Scotland","Wales"))) %>% 
  #filter(!(name == "Wales" & statistic == "case")) %>% 
  filter(source %in% c("4NationsCases","Deaths","Triage")) %>% mutate(
    modelType = case_when(source == "4NationsCases" ~ "Cases", source == "Deaths" ~ "Deaths", source == "Triage" ~ "Emergency", TRUE ~ NA_character_)
  ) %>% ungroup()
export28dayGrowth = currentRt$rt28 %>% 
  #filter(!(name %in% c("Northern Ireland","Scotland"))) %>% 
  #filter(!(name == "Wales" & statistic == "case")) %>% 
  filter(source %in% c("4NationsCases","Deaths","Triage")) %>% mutate(
    modelType = case_when(source == "4NationsCases" ~ "Cases", source == "Deaths" ~ "Deaths", source == "Triage" ~ "Emergency", TRUE ~ NA_character_)
  ) %>% ungroup()


  ## create SPI-M export ----
  exportSPIM7day = bind_rows(
    export7dayRt %>%
      filter(!is.na(`Median(R)`))  %>% 
      convertRtToSPIM(name,modelExpr = paste0("EpiEstim/",source), modelTypeExpr = modelType, version="0.01"),
    
    rtConfig %>% convertSerialIntervalToSPIM(modelName = "EpiEstim/4NationsCases", version="0.01"),
    rtConfig %>% convertSerialIntervalToSPIM(modelName = "EpiEstim/Deaths", version="0.01"),
    rtConfig %>% convertSerialIntervalToSPIM(modelName = "EpiEstim/Triage", version="0.01"),
    
    export7dayGrowth %>%
      filter(!is.na(`Growth.windowed.value`)) %>% 
      convertGrowthRateToSPIM(name,modelExpr = paste0("EpiEstim/",source),modelTypeExpr = modelType, version="0.01"),
    export7dayGrowth %>%
      filter(!is.na(`Growth.windowed.value`) & is.finite(`Growth.windowed.value`)) %>% 
      convertDoublingTimeToSPIM(name,modelExpr = paste0("EpiEstim/",source),modelTypeExpr = modelType, version="0.01")
  )
  
  excelOutput <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(excelOutput, "Template")
  openxlsx::writeData(excelOutput, sheet = "Template", x = exportSPIM7day)
  openxlsx::saveWorkbook(excelOutput, paste0("~/Dropbox/covid19/current-rt/SPIMestimates-",Sys.Date(),"-7Day.xlsx"), overwrite = TRUE)
  
  ## create SPI-M export ----
  exportSPIM28Day = bind_rows(
    export28dayRt %>%
      filter(!is.na(`Median(R)`)) %>% 
      convertRtToSPIM(name,modelExpr = paste0("EpiEstim/",source),modelTypeExpr = modelType, version="0.02"),
    
    rtConfig %>% convertSerialIntervalToSPIM(modelName = "EpiEstim/4NationsCases", version="0.02"),
    rtConfig %>% convertSerialIntervalToSPIM(modelName = "EpiEstim/Deaths", version="0.02"),
    rtConfig %>% convertSerialIntervalToSPIM(modelName = "EpiEstim/Triage", version="0.02"),
    
    export28dayGrowth %>%
      filter(!is.na(`Growth.windowed.value`)) %>% 
      convertGrowthRateToSPIM(name,modelExpr = paste0("EpiEstim/",source),modelTypeExpr = modelType, version="0.02"),
    export28dayGrowth %>%
      filter(!is.na(`Growth.windowed.value`) & is.finite(`Growth.windowed.value`)) %>% 
      convertDoublingTimeToSPIM(name,modelExpr = paste0("EpiEstim/",source),modelTypeExpr = modelType, version="0.02")
    
  )
  
  excelOutput <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(excelOutput, "Template")
  openxlsx::writeData(excelOutput, sheet = "Template", x = exportSPIM28Day)
  openxlsx::saveWorkbook(excelOutput, paste0("~/Dropbox/covid19/current-rt/SPIMestimates-",Sys.Date(),"-28Day.xlsx"), overwrite = TRUE)
