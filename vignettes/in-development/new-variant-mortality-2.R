
library(tidyverse)
library(patchwork)
library(survival)

## set up some formatting functions ----
summaryTable = function(tmp,bootstraps=1) {
  groupPercent = function(df) df %>% summarise(N=round(n()/bootstraps)) %>% mutate(`%age`=sprintf("%1.1f%%",N/sum(N)*100))
  groupMean = function(df,col) df %>% filter(is.finite({{col}})) %>% summarise(N=round(n()/bootstraps),  `mean (SD)`=sprintf("%1.1f (\u00B1%1.1f)",mean({{col}},na.rm=TRUE),sd({{col}},na.rm=TRUE)))
   bind_rows(
    tmp %>% group_by(value = NHSER_name) %>% groupPercent() %>% mutate(category="Region"),
    tmp %>% group_by(value = ethnicity_final) %>% groupPercent() %>% mutate(category="Ethnicity"),
    tmp %>% group_by(value = sex) %>% groupPercent() %>% mutate(category="Gender"),
    tmp %>% group_by(value = sGeneEra) %>% groupPercent() %>% mutate(category="S gene"),
    tmp %>% group_by(value = deathStatus) %>% groupPercent() %>% mutate(category="Status"),
    tmp %>% group_by(value = ageCat) %>% groupPercent() %>% mutate(category="Age by category"),
    tmp %>% groupMean(age) %>% mutate(category="Age"),
    tmp %>% groupMean(CT_N) %>% mutate(category="N gene CT"),
    tmp %>% group_by(value=imd_decile_2) %>% groupPercent() %>% mutate(category="IMD")
  ) %>% ungroup()
}

tidyCoxmodel = function(survModel) {
  broom::tidy(survModel) %>% bind_cols(as.data.frame(confint(survModel))) %>% 
    mutate(
      HR = exp(estimate), 
      HR.lower=exp(`2.5 %`), 
      HR.upper=exp(`97.5 %`), 
      HR.pValue = p.value)
}

## set up data loading function ----
getDeathsLineList = function(path, ...) {
  tmp = readxl::read_excel(path, col_types = "text")
  datecols = c(colnames(tmp) %>% stringr::str_subset("date"),"dod")
    
  for(datecol in datecols) {
    tmp[[datecol]] = suppressWarnings(as.Date(as.numeric(tmp[[datecol]]),"1899-12-30"))
  }
  tmp = tmp %>% 
      dplyr::mutate(
        age = as.integer(age),
        gender = normaliseGender(ifelse(is.na(gender),"unknown",gender))
      )
  return(tmp %>% mutate(FINALID=as.numeric(finalid)) %>% dplyr::ungroup())
}

#' @description Load line list
#' 
#' @return raw line list data set
getSGeneLineList = function(path,...) {
  tmp = readr::read_csv(path)
  return(tmp %>% dplyr::ungroup())
}

normaliseGender = function(gender) {
  case_when(
    is.na(gender) ~ NA_character_,
    gender %>% stringr::str_detect("f|F") ~ "female",
    gender %>% stringr::str_detect("m|M") ~ "male",
    gender %>% stringr::str_detect("u|U") ~ "unknown",
    TRUE ~ "unknown")
}

# function to help interpret S gene N gene and ORF gene in 
interpretSGene = function(sGeneLineList, S_CT = 40, ORF1ab_CT = 30, N_CT = 30, Control_CT = Inf) {
  sGeneLineList %>% mutate(
    ORF1ab_CT_threshold = ORF1ab_CT,
    N_CT_threshold = N_CT,
    S_CT_threshold = S_CT,
    S_pos = P2CH3CQ > 0 & P2CH3CQ <= S_CT,
    S_undetect = P2CH3CQ == 0,
    N_pos = P2CH2CQ > 0 & P2CH2CQ <= N_CT,
    ORF1ab_pos = P2CH1CQ > 0 & P2CH1CQ <= ORF1ab_CT,
    Control_pos = P2CH4CQ > 0 & P2CH4CQ <= Control_CT,
    sGene = case_when(
      S_pos & N_pos & ORF1ab_pos & Control_pos ~ "Positive",
      S_undetect & N_pos & ORF1ab_pos & Control_pos ~ "Negative",
      TRUE ~ "Equivocal"
    ),
    CT_N = ifelse(P2CH2CQ > 0, P2CH2CQ, 40)
  ) %>% mutate(
    result = ifelse(!Control_pos,"No control",paste0(ifelse(S_pos,"S+","S-"),ifelse(N_pos,"N+","N-"),ifelse(ORF1ab_pos,"ORF+","ORF-")))
  )
}

#' @description Load line list
#' 
#' @return raw line list data set
getLineList = function(path,...) {
    tmp = readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()))
    tmp = tmp %>% 
      dplyr::mutate(
        Onsetdate = as.Date(Onsetdate,"%d/%m/%Y"),
        specimen_date = as.Date(specimen_date,"%d/%m/%Y"),
        lab_report_date = as.Date(lab_report_date,"%d/%m/%Y")
      ) 
    if(any(is.na(tmp$specimen_date))) stop("Problem parsing dates")
    return(tmp %>% mutate(
      pillar_2_testingkit = tolower(pillar_2_testingkit),
      age = suppressWarnings(as.numeric(age)),
      FINALID = as.numeric(FINALID),
      ethnicity_final = case_when(
        ethnicity_final %in% c("African (Black or Black British)","Any other Black background","Caribbean (Black or Black British)") ~ "Afro-carribbean",
        ethnicity_final %in% c("Any other Asian background","Bangladeshi (Asian or Asian British)","Indian (Asian or Asian British)","Pakistani (Asian or Asian British)") ~ "Asian",
        ethnicity_final %in% c("Any other White background","British (White)","Irish (White)") ~ "White",
        ethnicity_final %in% c("Any other Mixed background","Any other ethnic group","White and Black Caribbean (Mixed)","White and Black African (Mixed)","Chinese (other ethnic group)") ~ "Other",
        TRUE ~ "Unknown")
    ) %>% dplyr::ungroup())
}

## Load the data
# check if already loaded
# we discard S gene data for patients who have had more than one S gene test result. 
# this will put these patients into the "Unknown / P1" category, and will be excluded from the case control part of study but included in the main analysis.

loadData = function(dir="~/Data/new-variant",date="20210118", censorLength = 28, B117Date = as.Date("2020-10-01") ,latestDate = NULL) {
  ll = getLineList(path=paste0(dir,"/Anonymised Combined Line List ",date,".csv"))
  dll = getDeathsLineList(path = paste0(dir,"/",date," COVID19 Deaths.xlsx"))
  sgll = getSGeneLineList(path = paste0(dir,"/SGTF_linelist_",date,".csv"))
  sgllUniq = sgll %>% filter(!is.na(FINALID)) %>% group_by(FINALID) %>% mutate(sGeneResultCount = n()) %>% filter(sGeneResultCount == 1) %>% ungroup()
  #sgllUniq = sgll %>% filter(!is.na(FINALID)) %>% group_by(FINALID) %>% arrange(desc(specimen_date)) %>% mutate(sGeneResultCount = n()) %>% filter(row_number() == 1) %>% ungroup()
  if (identical(latestDate,NULL)) latestDate = max(dll$dod,na.rm=TRUE)
  
  # Is this a potential source of bias?
  excludedSgll = sgll %>% anti_join(sgllUniq, by = "FINALID")
  if(nrow(excludedSgll) > 0) {
    warning("we excluded ",excludedSgll %>% pull(FINALID) %>% unique() %>% length()," patients with multiple S Gene test results in pillar 2")
    excludedDeaths = dll %>% inner_join(excludedSgll, by="FINALID", suffix=c("",".sgene")) %>% interpretSGene() #%>% filter(specimen_date > "2020-10-01" & age>30)
    warning("of these ",excludedDeaths %>% filter(specimen_date.sgene > B117Date) %>% pull(FINALID) %>% unique() %>% length()," died during the study period")
    warning("of these ",excludedDeaths %>% filter(specimen_date.sgene > B117Date) %>% filter(sGene=="Positive") %>% pull(FINALID) %>% unique() %>% length()," would have been S gene positive")
    warning("of these ",excludedDeaths %>% filter(specimen_date.sgene > B117Date) %>% filter(sGene=="Negative") %>% pull(FINALID) %>% unique() %>% length()," would have been S gene negative")
  }
  
  # get the S gene line list and remove all patients who have more than one S Gene result. Otherwise we don;t have a single source in time for when to start the survival clock.
  # interpretSGene assigns sGene "Positive" for S+N+ORF+ with cutOff for S at 40 and N & ORF at 30; Negative is S-N+ORF+; equivocal is anything else (e.g. S-N-ORF+)
  # We exclude anything which does not match the line list.
  
  # Join the line list, deaths line list and uniquified sGene line list by FINALID
  coxData = ll %>% 
    left_join(sgllUniq, by="FINALID", suffix=c("",".sgene")) %>% 
    left_join(dll %>% mutate(FINALID=as.numeric(finalid)), by=c("FINALID"),suffix=c("",".death")) %>% 
    filter(is.na(specimen_date.sgene) | specimen_date.sgene <= latestDate) # make sure we are not including tests for which we could not have death info.
  
  coxData2 = coxData %>% 
    select(
      FINALID, 
      specimen_date, 
      lab_report_date, 
      specimen_date, 
      specimen_date.sgene, 
      dateadmission_NHSE, 
      dod,
      death_type28, 
      age,sex,imd_decile,
      P2CH3CQ,P2CH1CQ,P2CH2CQ,P2CH4CQ, # gene copy numbers
      sgtf, sgtf_under30CT, # gene copy number interpretation
      NHSER_name,NHSER_code,
      ethnicity_final, 
      LTLA_name) %>% 
    mutate(
      died = !is.na(dod),
      #diedWithin28days = !is.na(death_type28), # This is the line list definition of death within 28 days
      diedWithin28days = died & as.numeric(dod-specimen_date) <= censorLength, # This is a definition of death within 28 days not relying on line list definition
      dodAt28days = ifelse(diedWithin28days, dod, NA), # this is the date of death (or NA if it is >28 days after sgene specimen date or there is no specimen)
      censoredDate = pmin(latestDate, specimen_date+censorLength,na.rm = TRUE), # earliest of last recorded death or specimen date
    ) %>% 
    mutate(
      reportingDelay = as.numeric(lab_report_date-specimen_date),
      sGeneDelay = as.numeric(specimen_date.sgene-specimen_date),
      admissionDelay = as.numeric(dateadmission_NHSE-specimen_date),
      time = ifelse(diedWithin28days, dodAt28days-as.numeric(specimen_date), censoredDate-as.numeric(specimen_date)), 
      status = ifelse(!diedWithin28days,0,1)
    ) %>%
    mutate(
      ageCat = cut(age, breaks = c(-Inf,30,60,70,80,Inf), labels = c("<30","30-59","60-69","70-79","80+"), right=FALSE)
    )
  
  coxData2 = coxData2 %>%
    filter(!is.na(imd_decile)) %>%
    filter(!is.na(sex) & sex != "Unknown") %>% mutate() %>%
    filter(!is.na(ageCat) & !is.na(age)) %>%
    filter(!is.na(ethnicity_final) & !ethnicity_final %in% c("Unknown")) %>%
    filter(is.na(admissionDelay) | admissionDelay >= 0) %>%
    filter(is.na(reportingDelay) | (reportingDelay >= 0 & reportingDelay <20) ) %>%
    filter(is.na(sGeneDelay) | (sGeneDelay >= 0 & sGeneDelay <20) ) %>%
    filter(time>0) %>% # quite a few specimens take post mortem?
    mutate(
      ethnicity_final = ethnicity_final %>% forcats::as_factor() %>% forcats::fct_relevel("White"),
      sex = sex %>% forcats::as_factor(),
      imd_decile = forcats::fct_relevel(forcats::as_factor(as.numeric(imd_decile)),"5"),
      imd_decile_2 = forcats::as_factor(as.numeric(imd_decile)),
      deathStatus = ifelse(diedWithin28days,"Dead <28 days","Other")
    ) 
  
  # coxData3 %>% ,
  # sGeneEra = case_when(
  #   is.na(sGene) ~ "Unknown (and P1)",
  #   sGene == "Negative" & specimen_date.sgene >= B117Date ~ "Neg Post B.1.1.7",
  #   sGene == "Negative" & specimen_date.sgene < B117Date ~ "Neg Pre B.1.1.7",
  #   TRUE ~ as.character(sGene))
  # sGene = sGene %>% forcats::fct_relevel("Positive"),
  # sGeneEra = sGeneEra %>% forcats::as_factor() %>% forcats::fct_relevel("Positive"),
  # relativeCopyNumber = 2^(median(CT_N,na.rm=TRUE)-CT_N),
  
  ## quality control ----
  if (any(is.na(coxData2$LTLA_name))) stop("unknowns for LTLA")
  # if (any(is.na(coxData3$LTLA_name))) stop("unknowns for LTLA")
  
  ## case matching ----
  # match cases by "sex","imd_decile","LTLA_name","ethnicity_final","ageCat"
  # exclude people < 30 as mortality so low
  # then ensure age difference < 2 years & specimen date < 4 days
  # select S negatives and match to S positives
  # randomly select a single match out of possible matches
  return(coxData2)
}


runAnalysis = function(coxData, sGeneCtThreshold = ctThreshold, ctThreshold=30, bootstraps=10, ageTolerance = 3, specimenDateTolerance = 5,B117Date = as.Date("2020-10-01"),max = 450000*450000, includeRaw=FALSE, includeMatched=FALSE) {
  result = list(params=tibble(
    sGeneCtThreshold = sGeneCtThreshold,
    ctThreshold = ctThreshold,
    bootstraps=bootstraps,
    ageTolerance = ageTolerance,
    specimenDateTolerance = specimenDateTolerance
    ))
  
  message("Interpreting data with CT threshold of S<",sGeneCtThreshold,":N<",ctThreshold,":ORF<",ctThreshold)
  
  coxData3 = coxData %>% 
    interpretSGene(S_CT = sGeneCtThreshold,N_CT = ctThreshold,ORF1ab_CT = ctThreshold) %>% 
    mutate(
      sGeneEra = case_when(
        is.na(sGene) ~ "Unknown (and P1)",
        sGene == "Negative" & specimen_date.sgene >= B117Date ~ "Neg Post B.1.1.7",
        sGene == "Negative" & specimen_date.sgene < B117Date ~ "Neg Pre B.1.1.7",
        TRUE ~ as.character(sGene)
      ),
      sGene = sGene %>% forcats::fct_relevel("Positive"),
      sGeneEra = sGeneEra %>% forcats::as_factor() %>% forcats::fct_relevel("Positive"),
      relativeCopyNumber = 2^(median(CT_N,na.rm=TRUE)-CT_N),
      LTLA_name =  as.factor(LTLA_name)
    )
  
  coxData3 = coxData3 %>% filter(specimen_date > B117Date & age >= 30)
  
  if (includeRaw) result$rawData = coxData3
  
  posSgene = coxData3 %>% filter(sGene=="Positive")
  negSgene = coxData3 %>% filter(sGene=="Negative")
  
  
  message("S gene positives: ",posSgene %>% nrow())
  message("S gene negatives: ",negSgene %>% nrow())
  
  if(as.numeric(nrow(negSgene)) * as.numeric(nrow(posSgene)) > max) stop("this would try and create a very large cross join of ",nrow(posSgene),"x",nrow(negSgene))
  
  # pairwise matching
  matches = 
    negSgene %>% 
    select(FINALID,sex,imd_decile,LTLA_name,ethnicity_final,ageCat,age,specimen_date) %>% 
    inner_join(
      posSgene %>% select(FINALID,sex,imd_decile,LTLA_name,ethnicity_final,ageCat,age,specimen_date), 
      by=c("sex","imd_decile","LTLA_name","ethnicity_final","ageCat"),suffix=c("",".match")) %>% 
    filter(
      abs(age-age.match) <= ageTolerance & 
      abs(as.numeric(specimen_date-specimen_date.match)) <= specimenDateTolerance
    )
  
  rm(coxData3)
  
  # randomly select a single match out of possible matches
  set.seed(101)
  matchesSelected = NULL
  message("bootstrapping",appendLF = FALSE)
  for(i in 1:bootstraps) {
    message("..",i,appendLF = FALSE)
    tmp = matches %>% 
      mutate(rnd = runif(nrow(matches))) %>%
      arrange(rnd) %>% 
      group_by(FINALID) %>% filter(row_number()==1) %>% # filter out a single matching at random from pos to neg
      group_by(FINALID.match) %>% filter(row_number()==1) %>%  # filter out a single matching at random from neg to pos
      ungroup() %>% mutate(boot = i)
    matchesSelected = bind_rows(matchesSelected,tmp)
  }
  message("..finished")
  
  negMatched = matchesSelected %>% select(FINALID, boot) %>% inner_join(negSgene, by="FINALID")
  posMatched = matchesSelected %>% select(FINALID = FINALID.match, boot) %>% inner_join(posSgene, by="FINALID")
  
  # check for some systematic bias as a result of matching process
  # This is biased by censoring - TODO visualise and explain.
  # negExcluded = negSgene %>% left_join(negMatched %>% select("FINALID") %>% mutate(matched=1), by="FINALID")
  # posExcluded = posSgene %>% left_join(posMatched %>% select("FINALID") %>% mutate(matched=1), by="FINALID")
  # inclusions = bind_rows(negExcluded,posExcluded) %>% mutate(matched = ifelse(is.na(matched), 0, matched))
  # inclusions %>% group_by(sGene, died, matched) %>% summarise(count = n()) %>% clipr::write_clip() #%>% mutate(percentOfSGene = count/sum(count)) %>% group_by(died) %>% mutate(percentByDiedStatus = count/sum(count)) %>% arrange(sGene,died)
  # p_died_given_sGene = inclusions %>% group_by(sGene,died) %>% summarise(count = n()) %>% mutate(percent = count/sum(count))
  # p_died_given_sGene_and_matched = inclusions %>% filter(matched) %>% group_by(sGene,died) %>% summarise(count = n()) %>% mutate(percent = count/sum(count))
  # p_died_given_sGene %>% clipr::write_clip()
  # p_died_given_sGene_and_matched %>% clipr::write_clip()
  
  coxFinal = bind_rows(posMatched,negMatched) %>% 
    mutate(sGene = sGene %>% forcats::fct_drop()) # equivocal category is dropped
  
  if (includeMatched) result$rawMatchedData = coxFinal
  
  combinedSummary1 = coxFinal %>% group_by(sGene,boot) %>% summarise(pairs = n()) %>% left_join(
    coxFinal %>% filter(diedWithin28days) %>% group_by(sGene,boot) %>% summarise(deaths = n()),
    by = c("sGene","boot")
  )
  result$table1Data = combinedSummary1

  ## Construct table 1 summary of pairwise matching ----
  message("Summarising results")
  combinedSummary2 = summaryTable(posMatched, bootstraps) %>% rename_with(~paste0("S pos ",.x),.cols=c(N,`%age`,`mean (SD)`)) %>% #,by=c("category","value")) %>%
    full_join(summaryTable(negMatched, bootstraps) %>% rename_with(~paste0("S neg ",.x),.cols=c(N,`%age`,`mean (SD)`)),by=c("category","value")) %>% 
    full_join(summaryTable(coxFinal %>% filter(diedWithin28days), bootstraps) %>% rename_with(~paste0("Died ",.x),.cols=c(N,`%age`,`mean (SD)`)),by=c("category","value"))
  result$table2Data = combinedSummary2 %>% select(category,everything()) %>% arrange(category)


  deltas = matchesSelected %>% group_by(boot) %>% summarise(
    ageDifference = mean(age-age.match),
    specimenDateDifference = mean(as.numeric(specimen_date-specimen_date.match))
  )
  
  result$table3Data = deltas
  
  ## Survival models ----
  # Basic model
  # Adjusted for N gene CT values

  message("Calculating unadjusted survival models")
  bootSurvModels = coxFinal %>% group_by(boot) %>% group_modify(function(d,g,...) {
    survModel = survival::coxph(Surv(time,status) ~ sGene, d)
    out = tidyCoxmodel(survModel)
    rm(survModel)
    return(out)
  })

  result$table4Data = bootSurvModels
  
  message("Calculating survival models adjusted for S status and CT of N Gene")
  bootSurvModels2 = coxFinal %>% group_by(boot) %>% group_modify(function(d,g,...) {
    survModel2 = survival::coxph(Surv(time,status) ~ sGene+CT_N, d)
    out = tidyCoxmodel(survModel2)
    rm(survModel2)
    return(out)
  })

  result$table5Data = bootSurvModels2
  
  message("Calculating survival models adjusted for S status and age")
  bootSurvModels3 = coxFinal %>% group_by(boot) %>% group_modify(function(d,g,...) {
    survModel3 = survival::coxph(Surv(time,status) ~ sGene+age, d)
    out = tidyCoxmodel(survModel3)
    rm(survModel3)
    return(out)
  })
  
  result$table6Data = bootSurvModels3
  
  return(result)
}

summariseAnalysisRun = function(run) {
  tmp = run$table1Data %>% pivot_wider(names_from = sGene, values_from = deaths, names_prefix="deaths.")
  tmp = tmp %>% summarise(mean.matchedPairs = mean(pairs), mean.matchedDeaths.Positive = mean(deaths.Positive), mean.matchedDeaths.Negative = mean(deaths.Negative))
  tmp = tmp %>% bind_cols(
    run$table3Data %>% summarise(mean.ageDifference = mean(ageDifference), mean.specimenDateDifference = mean(specimenDateDifference)),
  )
  tmp2 = bind_rows(
    run$table4Data %>% group_by(term) %>% summarise(across(.cols = c(estimate,std.error,HR,HR.lower,HR.upper,HR.pValue), .fns = c(mean=mean,sd=sd), .names="{.fn}.{.col}")) %>% mutate(model = "S gene only"),
    run$table5Data %>% group_by(term) %>% summarise(across(.cols = c(estimate,std.error,HR,HR.lower,HR.upper,HR.pValue), .fns = c(mean=mean,sd=sd), .names="{.fn}.{.col}")) %>% mutate(model = "S gene + N gene CT"),
    run$table6Data %>% group_by(term) %>% summarise(across(.cols = c(estimate,std.error,HR,HR.lower,HR.upper,HR.pValue), .fns = c(mean=mean,sd=sd), .names="{.fn}.{.col}")) %>% mutate(model = "S gene + age")
  )
  tmp2 = tmp2 %>% left_join(tmp, by=character()) %>% left_join(run$params, by=character())
  return(tmp2 %>% select(model,everything()))
}

prettyPrintSummary = function(summaryDf) {
  out = summaryDf %>% group_by(model,term) %>% summarise(
    `Hazard rate (95% CI)`=sprintf("%1.1f (%1.1f \u2013 %1.1f)",mean.HR,mean.HR.lower,mean.HR.upper),
    `p value`=scales::pvalue(mean.HR.pValue,0.001)) %>%
    mutate(
      Value = case_when(term == "sGeneNegative"~"Negative",TRUE~NA_character_),
      Predictor = case_when(
        term == "sGeneNegative"~"S gene status",
        term == "CT_N"~"N gene CT value",
        term == "age"~"Age",
        TRUE~NA_character_
      ),
      Model = model) %>%
    ungroup() %>%
    select(-model,-term)
  out = out %>% bind_rows(
    out %>% select(Model) %>% distinct() %>% mutate(Value = "Positive (ref)",Predictor="S gene status",
                                                    `Hazard rate (95% CI)`="\u2014",`p value`="\u2014")
  ) %>%
    mutate(
      Predictor = Predictor %>% forcats::as_factor() %>% forcats::fct_relevel("S gene status"),
      Value = Value %>% forcats::as_factor() %>% forcats::fct_relevel("Positive (ref)"),
      Model = Model %>% forcats::as_factor() %>% forcats::fct_relevel("S gene only")
    )
  return(out %>% select(Model,Predictor,Value,`Hazard rate (95% CI)`,`p value`) %>% group_by(Model,Predictor) %>% arrange(Model,Predictor,Value))
}

