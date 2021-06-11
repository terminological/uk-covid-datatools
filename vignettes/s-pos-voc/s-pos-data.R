#!/usr/bin/Rscript
setwd("~/Git/uk-covid-datatools/")

devtools::load_all("~/Git/standard-print-output/")
devtools::load_all("~/Git/uk-covid-datatools/")


library(patchwork)
library(rgdal)
library(ggplot2)
library(ggspatial)
library(rgeos)
library(maptools)
library(patchwork)
library(sp)
library(sf)

library(data.table)
library(dtplyr)
library(tidyverse, warn.conflicts = FALSE)

ggplot2::theme_set(standardPrintOutput::defaultFigureLayout())
standardPrintOutput::setDefaults()
ukcovidtools::setup()



# Utility function ----

mapVOCtoLineage = function(df, vocVar = "vam", lineageVar = "type", lineages=c("B.1.1.7","B.1.525","AV.1","B.1.617.1","B.1.617.2","B.1.351","P.1 & P.2")) {
  vocVar = ensym(vocVar)
  lineageVar = ensym(lineageVar)
  return(df %>% mutate(
    !!lineageVar := case_when(
      "B.1.1.7" %in% lineages & !!vocVar %>% stringr::str_detect("20DEC-01") ~ "B.1.1.7",
      "B.1.351" %in% lineages & !!vocVar %>% stringr::str_detect("20DEC-02") ~ "B.1.351",
      "B.1.525" %in% lineages & !!vocVar %>% stringr::str_detect("21FEB-03") ~ "B.1.525",
      "P.1 & P.2" %in% lineages & !!vocVar %>% stringr::str_detect("21JAN-02") ~ "P.1 & P.2",
      "P.1 & P.2" %in% lineages & !!vocVar %>% stringr::str_detect("21JAN-01") ~ "P.1 & P.2",
      "B.1.1.318" %in% lineages & !!vocVar %>% stringr::str_detect("21FEB-04") ~ "B.1.1.318",
      "B.1.617.1" %in% lineages & !!vocVar %>% stringr::str_detect("21APR-01") ~ "B.1.617.1",
      "B.1.617.2" %in% lineages & !!vocVar %>% stringr::str_detect("21APR-02") ~ "B.1.617.2",
      "B.1.617.3" %in% lineages & !!vocVar %>% stringr::str_detect("21APR-03") ~ "B.1.617.3",
      "AV.1" %in% lineages & !!vocVar %>% stringr::str_detect("21MAY-01") ~ "AV.1",
      "C.36.3" %in% lineages & !!vocVar %>% stringr::str_detect("21MAY-02") ~ "C.36.3",
      !!vocVar %>% stringr::str_starts("VOC|VUI") ~ "other VOC/VUI",
      !!vocVar %>% stringr::str_detect("E484") ~ "other VOC/VUI",
      TRUE ~ "non VOC/VUI"
    ) %>% ordered(c("non VOC/VUI",lineages,"other VOC/VUI"))
  ))
}

## Initialise data ----

# options("sposvoc.data"="~/Data/s-pos-voc/")

# dataPath = getOption("sposvoc.data",default=NA_character_)
# if(!is.na(dataPath)) {
#   loader = SPIMDatasetProvider$new(dpc, LocalFileProvider$new(root = dataPath))
# } else {
  loader = dpc$spim
# }

# Load from sources:

# dpc$spim$getLatestRawFile(dpc$spim$filter$vamLineList,to = "~/Data/s-pos-voc/")
# dpc$spim$getLatestRawFile(dpc$spim$filter$ctasLineList,to = "~/Data/s-pos-voc/")
# dpc$spim$getLatestRawFile(dpc$spim$filter$lineList,to = "~/Data/s-pos-voc/")
# dpc$spim$getLatestRawFile(dpc$spim$filter$sgene,to = "~/Data/s-pos-voc/")

ctasPath = dpc$spim$getLatest(dpc$spim$filter$ctasLineList)
message("Using: ",ctasPath)

ctasGenomics = dpc$getSaved("CTAS-GENOMICS",params = ctasPath, orElse = function(...) {
  ctas = loader$getCTASLineList()
  out = ctas %>% filter(!is.na(genomic_variant)) %>% select(c(starts_with("genomic"))) %>% rename_with(.fn=function(x) stringr::str_remove(x,"genomic_"))  %>% distinct() #,sgtf_finalid, sgtf_under30ct, p2ch3cq, p2ch1cq, p2ch2cq, p2ch4cq))
  rm(ctas)
  return(out)
})

vam = loader$getVAMLineList()
genomics = bind_rows(
  vam %>% select(finalid, variant, exposure_type, specimen_date=specimen_date_sk),
  ctasGenomics %>% anti_join(vam, by="finalid") %>% select(finalid, variant, exposure_type=exp_type,specimen_date)
)
ll = loader$getLineList()

# Split line list out into normalised parts.
llDemog = ll %>% select(FINALID, NHSER_code, NHSER_name, PHEC_code, PHEC_name, UTLA_code, UTLA_name, LTLA_code, LTLA_name, sex, age, ethnicity_final, imd_decile, imd_rank, residential_category, cat) %>% distinct()
llEpisode = ll %>% select(FINALID, specimen_date, asymptomatic_indicator, pillar, lab_report_date, pillar_2_testingkit, testcentreid, case_category) %>% mutate(episode_type="first positive",episode=1) %>% distinct()
llTest = ll %>% select(FINALID, specimen_date, case_category,asymptomatic_indicator) %>% mutate(linelist = TRUE)

# Split s gene line list out into normalised parts.
sgll = loader$getSGeneLineList()
sgllTest = sgll %>% 
  select(FINALID, specimen_date, CDR_Specimen_Request_SK, sgtf_under30CT) %>% 
  mutate(sglinelist = TRUE)

# combine tests from ll (first only) with tests from S-gene files
# N.B. this will be a bit of a funny mixture - multiple tests but only for cases that went through taqpath assay.
tests = llTest %>% 
  full_join(
    sgllTest,
    by = c("FINALID","specimen_date")  
  )  %>%
  mutate(
    linelist = ifelse(is.na(linelist),FALSE,linelist),
    sglinelist = ifelse(is.na(sglinelist),FALSE,sglinelist),
    case_category = ifelse(sglinelist,"TaqPath",case_category)
  ) %>% mutate(
    sGene = case_when(
      is.na(sgtf_under30CT) & sglinelist ~ "equivocal",
      is.na(sgtf_under30CT) & linelist ~ "unknown",
      sgtf_under30CT == 1 ~ "negative",
      sgtf_under30CT == 0 ~ "positive"
    )
  )

# this defines how long between tests before two tests are regarded as a new episode.
# if the tests are sgtf equivocal double this is allowed.
cutoff = 28

## calculate the individual episodes of covid resulting from runs of sequential positive tests <28 days apart.
allEpisodes = dpc$getSaved(id = "EPISODES",params = list(cutoff, tests), nocache = TRUE, orElse = function(...) {
  
  # use data.table to speed it all up.
  tests2 = dtplyr::lazy_dt(tests)
  
  # look for 
  tests3 = tests2 %>% arrange(FINALID,specimen_date) %>% 
    mutate(
      delay = ifelse(FINALID == lag(FINALID), as.numeric(specimen_date - lag(specimen_date)), NA_integer_)
    ) %>%
    mutate(era = case_when(
    is.na(delay) ~ "new",
    sGene == "equivocal" & delay < cutoff*2 ~ "same", # prolonged recovery
    delay < cutoff ~ "same",
    TRUE ~ "new"
  )) %>%
  # assign an eraIndex - essentially the count of novel infection episodes
    group_by(FINALID) %>% arrange(specimen_date) %>% mutate(eraIndex = cumsum(ifelse(era=="new",1,0)))
  # summarise sGene data into a single value for each era
  
  # TODO: each era may have multiple positive tests there is an opportunity to look at the CT values over time and 
  # fit some sort of model here
  tests4 = tests3 %>% group_by(FINALID,eraIndex) %>% 
    summarise(
      earliest_specimen_date = min(specimen_date,na.rm=TRUE), 
      latest_specimen_date = max(specimen_date,na.rm=TRUE), 
      tests=n(),
      anyPosSGene = any(sGene == "positive"),
      anyNegSGene = any(sGene == "negative"),
      anyEquivSGene = any(sGene == "equivocal"),
      anyUnknSGene = any(sGene == "unknown"),
      asymptomatic_indicator = first(na.omit(asymptomatic_indicator),default="U"),
      lft_only = all(na.omit(case_category=="LFT_Only"))
    ) 
  tests5 = tests4 %>% mutate(
    sGene=case_when(
      anyPosSGene & !anyNegSGene ~ "positive",
      anyNegSGene & !anyPosSGene ~ "negative",
      anyEquivSGene ~ "equivocal",
      TRUE ~ "unknown"
  )) %>% as_tibble()
  
  return(tests5 %>% select(-anyPosSGene, -anyNegSGene, -anyEquivSGene, -anyUnknSGene))
})

## Link genomics to episodes ----

# this will lose a few sequenced cases which are not able to be linked to the episodes
allVoc = genomics %>% 
  left_join(allEpisodes %>% select(FINALID, earliest_specimen_date,latest_specimen_date,eraIndex,sGene), by=c("finalid"="FINALID")) %>%
  mutate(
    # determine smallest time to each era
    offset = pmin(abs(as.numeric(specimen_date-earliest_specimen_date)),abs(as.numeric(specimen_date-latest_specimen_date)))
  ) %>%
  # for every era get sequence with smallest distance to it.
  group_by(finalid,eraIndex) %>% arrange(offset) %>% filter(row_number() == 1) %>% ungroup() %>%
  # make sure the sequence is within 28 days of the era
  filter(is.na(specimen_date) | (specimen_date>=earliest_specimen_date-28 & specimen_date<=latest_specimen_date+28)) %>%
  group_by(finalid) %>%
  filter(!is.na(specimen_date) | eraIndex == max(eraIndex)) %>%
  rename(episode_date = earliest_specimen_date) %>%
  select(-latest_specimen_date,-offset) %>%
  ungroup()

# S-gene positives for classifer ----

# filter to remove S gene positives before start of sequencing data
## Classify S+ ives ----

createClassifierInput = function(from = "2021-02-01", genomicFilterExpr = NULL, sgeneFilterExpr = NULL, lineages = c("AV.1","B.1.617.2","B.1.351","B.1.1.7"), ...) {
  
  genomicFilterExpr = enexpr(genomicFilterExpr)
  sgeneFilterExpr = enexpr(sgeneFilterExpr)
  if (identical(genomicFilterExpr,NULL)) genomicFilterExpr = TRUE
  if (identical(sgeneFilterExpr,NULL)) sgeneFilterExpr = TRUE
  
  dpc$getSaved(id = "VOC-CLASSIFIER",params = list(from,genomicFilterExpr,sgeneFilterExpr,lineages,allEpisodes,allVoc,mapVOCtoLineage), ..., orElse = function(...) {
    
    #browser()
    
    sgPosUniq = allEpisodes %>% filter(!!sgeneFilterExpr) %>% 
      select(FINALID, eraIndex, sGene, earliest_specimen_date, latest_specimen_date)
  
    tmp = allVoc %>% 
        filter(!!genomicFilterExpr) %>% 
        mapVOCtoLineage(vocVar = variant,lineages = lineages) %>%
        select(finalid = finalid, eraIndex, sGene, type, exposure_type, sequence_date = specimen_date) %>% 
        mutate(
          sequenced=TRUE, 
          exposure_type = case_when(
            exposure_type == "missing_data" ~ NA_character_,
            TRUE ~ stringr::str_remove(exposure_type,", Other")
          )
        )
    
    notTmp = sgPosUniq %>% 
      select(finalid = FINALID, eraIndex, sGene) %>% 
      mutate(type = NA_character_, sequenced = FALSE, exposure_type=NA_character_) %>% 
      anti_join(tmp, by=c("finalid","eraIndex"))
    
    tmp = tmp %>% bind_rows(notTmp) 
    tmp = tmp %>% filter(!is.na(finalid) & !is.na(eraIndex))
  
    tmp2 = tmp %>% 
      left_join(allEpisodes %>% select(-tests,-sGene), by=c("finalid"="FINALID","eraIndex")) %>%
      filter(earliest_specimen_date >= from)
      
    combinedSpositives = tmp2 %>% as_tibble() %>% 
      inner_join(llDemog, by=c("finalid"="FINALID"))  
    return(combinedSpositives)
  
  }) %>% return()

}

combinedSpositives = createClassifierInput(from="2021-02-01", genomicFilterExpr = !(variant %in% c("VOC-20DEC-01","VUI-21FEB-03")), sgeneFilterExpr = sGene == "positive", lineages = c("AV.1","B.1.617.2","B.1.351"))
# combinedSnotNegatives = createClassifierInput(from="2021-02-01", genomicFilterExpr = !(variant %in% c("VOC-20DEC-01","VUI-21FEB-03")), sgeneFilterExpr = sGene != "negative", lineages = c("AV.1","B.1.617.2","B.1.351"))
combinedCases = createClassifierInput(from="2021-02-01",lineages = c("AV.1","B.1.617.2","B.1.351","B.1.1.7"))

# Define areas ----

tmp = sapply(c("North West","Midlands","London"), function(regionName) {
  dpc$codes$getTransitiveClosure() %>% 
    filter(fromCodeType=="LAD",toCodeType=="NHSER") %>% 
    dpc$codes$findNamesByCode(codeVar = toCode) %>% 
    filter(name == regionName) %>% 
    pull(fromCode)
})

# manually defined subregions
areasOfConcern = list(
  
  `Bolton` = c("E08000001"), 
  `M65 Corridor` = c("E06000008","E07000120","E07000117"), # Blackburn, Hyndburn, Burnley
  `Manchester` = c("E08000003", "E08000006", "E08000008", "E08000007", "E08000005", "E08000002", "E07000037"), # Machester, Salford, Tameside, Stockport, Rochdale, Bury, High peak
  
  `Nottingham` = c("E06000018","E07000036","E07000173", "E07000176"), # Nottingham, Erewash, Gedling, Rushcliffe
  `Leicester` = c("E06000016","E07000129","E07000135"), # Leicester, Blaby, Oadby & Wigton
  `Birmingham` = c("E08000025", "E08000031", "E08000028", "E08000027","E08000029","E08000030") #Birmingham, Wolverhamptom, Sandwell, Dudley, Solihull, Walsall
  
  
  # `M6 Corridor` = c("E07000123","E08000010", "E07000118"),  # Preston, Wigan, Chorley, Could include St. Helens, Warringtom
  # `Sefton & Lpl` = c("E08000014","E08000012","E07000127", "E08000011","E08000015"), #Sefton, Liverpool, West lancashire, Knowlesley, Wirral
  # `Bedford etc` = c("E06000055"), # Bedford
  
  # `East London` = c("E09000025","E09000002","E09000031","E09000030","E09000012"),
  # `West London` = c("E09000018","E09000015","E09000005","E09000017")
  # `Southampton, Portsmouth & IoW` = c("E06000046","E06000045","E07000091","E06000044","E07000086")
) %>% c(tmp)

clusters2 = list(
  "NHS Regions" = c("North West","Midlands","London"),
  "NW Cluster" = c("Bolton","M65 Corridor","Manchester"), #"M6 Corridor","Sefton & Lpl"
  "Midlands" = c("Nottingham","Leicester","Birmingham") #"Bedford etc",
)

clusters = c(
  "Reference" = c("England"),
  clusters2
)


aocDf = lapply(names(areasOfConcern),function(name) tibble(area = rep(name,length(areasOfConcern[[name]])), code = areasOfConcern[[name]])) %>% bind_rows() %>% mutate(area = area %>% ordered(names(areasOfConcern)))
aocDf = aocDf %>% dpc$codes$findNamesByCode(outputNameVar = ltlaName) %>% mutate(ageCat=NA,gender=NA) %>% dpc$demog$findDemographics(codeVar = code)
lastDate = max(combinedSpositives$earliest_specimen_date,na.rm = TRUE)
earliestDate = lastDate-28

ladMap = dpc$geog$getMap("LAD19")

aocMap = ladMap %>% 
  filter(code %>% stringr::str_starts("E")) %>%
  left_join(
    aocDf %>% 
      filter(!area %in% c("Bedford etc","M6 Corridor","Sefton & Lpl")) %>%
      select(areaName = area,code), by="code") %>%
  mutate(areaName = ifelse(is.na(areaName),"other",as.character(areaName))) %>%
  group_by(areaName) %>%
  summarise()



saveAndZipClassifier = function(combined, filename, directory="~/Dropbox/covid19/sa-variant/") {
  
  filename = filename %>% stringr::str_replace("\\.csv","")
  
  lsoaMap = dpc$geog$getMap("LSOA11") %>% ungroup() %>% filter(code %>% stringr::str_starts("E"))
  lsoaCentroid = lsoaMap %>% sf::st_centroid()
  lsoaXY =  lsoaCentroid %>% mutate(
    x=unname(sf::st_coordinates(.)[,"X"]),
    y=unname(sf::st_coordinates(.)[,"Y"])) %>% 
    as_tibble() %>% 
    select(code,x,y)
  
  lsoa2msoa = dpc$codes$getTransitiveClosure() %>% 
    filter(fromCodeType == "LSOA" & toCodeType == "MSOA") %>% 
    dpc$codes$findNamesByCode(codeVar = toCode) %>% 
    select(LSOA_code = fromCode, MSOA_code = toCode, MSOA_name = name)
  
  if (any(duplicated(lsoa2msoa$LSOA_code))) stop("Duplicates in LSOA to MSOA mapping")
  
  tmp = combined %>% 
    dpc$spim$augmentLineListWithLSOA() %>%
    left_join(lsoa2msoa, by="LSOA_code") %>%
    inner_join(lsoaXY, by=c("LSOA_code"="code")) %>% 
    distinct() %>% 
    # mutate(
    #   is_617_1 = type == "B.1.617.1",
    #   is_617_2 = type == "B.1.617.2",
    #   is_351 = type == "B.1.351",
    #   is_other_voc = type == "other VOC/VUI",
    #   is_not_voc = type == "non VOC/VUI",
    #   is_variant = type != "non VOC/VUI"
    # ) %>% 
    select(
      # is_variant, 
      # is_617_1, is_617_2, is_351, is_other_voc, is_not_voc,
      type, finalid, 
      date = earliest_specimen_date, 
      sequenced, sGene, 
      #exposure_type, asymptomatic_indicator, 
      #ethnicity_final, age_group=ageCat,
      LTLA_code, LTLA_name,
      MSOA_code, MSOA_name,
      LSOA_code, LSOA_name, x, y
    )
  
  tmp %>% readr::write_csv(paste0(directory,filename,"-",Sys.Date(),".csv")) 
  
  tmpdir = getwd()
  setwd(directory)
  zip(paste0(directory,filename,"-",Sys.Date(),".zip"), path.expand(paste0(filename,"-",Sys.Date(),".csv")))
  setwd(tmpdir)
  
  return(tmp)
}

# generate classifier input ----

# combinedSpositives = createClassifierInput(from="2021-02-01", genomicFilterExpr = !(variant %in% c("VOC-20DEC-01","VUI-21FEB-03")), sgeneFilterExpr = sGene == "positive", lineages = c("B.1.617.1","B.1.617.2","B.1.351"))
# combinedSnotNegatives = createClassifierInput(from="2021-02-01", genomicFilterExpr = !(variant %in% c("VOC-20DEC-01","VUI-21FEB-03")), sgeneFilterExpr = sGene != "negative", lineages = c("B.1.617.1","B.1.617.2","B.1.351"))
# combinedCases = createClassifierInput(from="2021-02-01",lineages = c("B.1.617.1","B.1.617.2","B.1.351","B.1.1.7"))


# combinedSpositives = createClassifierInput(from="2020-12-31", filterExpr = sGene == "positive", lineages = c("B.1.617.1","B.1.617.2","B.1.351"))
# combinedCases = createClassifierInput(from="2021-02-01",lineages = c("B.1.617.1","B.1.617.2","B.1.351","B.1.1.7"))

# dpc$unloadCaches()
combinedSpositivesLSOA = combinedSpositives %>% saveAndZipClassifier(filename = "s-positives")
combinedCasesLSOA = combinedCases %>% saveAndZipClassifier(filename = "all-cases")



with(combinedCases, table(sGene,type,useNA = "ifany"))

## TODO----
## OLD CODE ----

# sgllEra = loader$getSGeneEras()


# take the latest values of the S pos for each person
# this could generate dups
# sgPosUniq = sgll %>% filter(sgtf_under30CT == "0") %>% 
#   group_by(FINALID) %>% 
#   filter(specimen_date==min(specimen_date,na.rm=TRUE))

# sgPosUniq = sgll %>% filter(sgtf_under30CT == "0") %>% 
#   group_by(FINALID) %>% 
#   arrange(desc(specimen_date)) %>%
#   filter(row_number()==1) %>% select(FINALID, specimen_date)


# isolationDelay = 
#   bind_rows(
#     ctas %>% filter(category=="case") %>%
#       select(finalid = genomic_finalid, date = genomic_specimen_date, isolation_date = start_of_isolation_date),
#     ctas %>% filter(category=="case") %>%
#       select(finalid = sgtf_finalid, date = sgtf_specimen_date, isolation_date = start_of_isolation_date)
#   ) %>% 
#   filter(!is.na(finalid) & !is.na(date) & !is.na(isolation_date)) %>%
#   distinct() %>% mutate(isolation_delay = as.integer(isolation_date - date)) %>%
#   filter(isolation_delay > -14 & isolation_delay < 21) %>%
#   mutate(
#     isolation_cat = case_when(
#       isolation_delay <= -7 ~ "very low risk",
#       isolation_delay <= -2 ~ "low risk",
#       isolation_delay <= 0 ~ "asymptomatic spread",
#       isolation_delay <= 4 ~ "high risk",
#       TRUE ~ "ineffective",
#     ) %>% ordered(c("very low risk","low risk","asymptomatic spread","high risk","ineffective"))
#   )

# TODO: number of traced

# potentialReinfectn = sgll %>%
#   inner_join(ll2 %>% select(FINALID,first_specimen_date = specimen_date), by="FINALID") %>%
#   mutate(reinfect56 = specimen_date > first_specimen_date+56) %>%
#   select(finalid=FINALID, date=specimen_date, reinfect56)

# tmp2 = tmp %>% inner_join(ll2 %>% select(-asymptomatic_indicator),by=c("finalid"="FINALID"),suffix=c("",".ll")) %>% 
#   left_join(sgPosUniq, by=c("finalid"="FINALID"),suffix=c("",".sgene")) %>% # creates duplicates
#   inner_join(lsoaXY, by = c("LSOA_code"="code"), suffix = c("",".cent")) %>%
#   left_join(traced, by="finalid") %>%
#   mutate(ctas_status = ifelse(is.na(ctas_status),"unknown",ctas_status)) %>%
#   left_join(asymptomatic, by=c("finalid","date")) %>%
#   left_join(potentialReinfectn, by=c("finalid","date")) %>%
#   left_join(isolationDelay, by=c("finalid","date"))

# mapSGTFtoSgene = function(df) {
#   if ("pillar" %in% names(df)) {
#     df %>% mutate(
#       sGene = case_when(
#         pillar != "Pillar 2" ~ "unknown",
#         is.na(sgtf_under30CT) ~ "equivocal",
#         sgtf_under30CT == 1 ~ "negative",
#         sgtf_under30CT == 0 ~ "positive"
#       ) %>% ordered(c("positive","negative","equivocal","unknown"))
#     )
#   } else {
#     df %>% mutate(
#       sGene = case_when(
#         is.na(sgtf_under30CT) ~ "equivocal",
#         sgtf_under30CT == 1 ~ "negative",
#         sgtf_under30CT == 0 ~ "positive"
#       ) %>% ordered(c("positive","negative","equivocal"))
#     )
#   }
# }

