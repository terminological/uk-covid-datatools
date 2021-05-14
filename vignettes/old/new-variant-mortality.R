
library(tidyverse)
library(patchwork)
library(survival)

## set up some formatting functions ----
summaryTable = function(tmp) {
  groupPercent = function(df) df %>% summarise(N=n()) %>% mutate(`%age`=sprintf("%1.1f%%",N/sum(N)*100))
  groupMean = function(df,col) df %>% filter(is.finite({{col}})) %>% summarise(N=n(),  `mean (SD)`=sprintf("%1.1f (\u00B1%1.1f)",mean({{col}},na.rm=TRUE),sd({{col}},na.rm=TRUE)))
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
  return(tmp %>% dplyr::ungroup())
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
interpretSGene = function(sGeneLineList, S_CT = 40, ORF1ab_CT = 30, N_CT = 30) {
  sGeneLineList %>% mutate(
    ORF1ab_L=ifelse(P2CH1CQ==0,40,P2CH1CQ),
    N_L=ifelse(P2CH2CQ==0,40,P2CH2CQ),
    S_L=ifelse(P2CH3CQ==0,40,P2CH3CQ),
    ORF1ab_R=ifelse(P2CH1CQ==0,NA,P2CH1CQ),
    N_R=ifelse(P2CH2CQ==0,NA,P2CH2CQ),
    S_R=ifelse(P2CH3CQ==0,NA,P2CH3CQ)
  ) %>% mutate(
    ORF1ab_CT_threshold = ORF1ab_CT,
    N_CT_threshold = N_CT,
    S_CT_threshold = S_CT,
    S_pos = S_L < S_CT,
    N_pos = N_L < N_CT,
    ORF1ab_pos = ORF1ab_L < ORF1ab_CT,
    sGene = case_when(
      S_pos & N_pos & ORF1ab_pos ~ "Positive",
      !S_pos & N_pos & ORF1ab_pos ~ "Negative",
      TRUE ~ "Equivocal"
    )
  ) %>% mutate(
    result = paste0(ifelse(S_pos,"S+","S-"),ifelse(N_pos,"N+","N-"),ifelse(ORF1ab_pos,"ORF+","ORF-"))
  )
}

#' @description Load line list
#' 
#' @return raw line list data set
getLineList = function(path,...) {
    tmp = readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()))
    tmp = tmp %>% 
      dplyr::mutate(
        Onsetdate = as.Date(Onsetdate,"%d/%m/%y"),
        specimen_date = as.Date(specimen_date,"%d/%m/%y"),
        lab_report_date = as.Date(lab_report_date,"%d/%m/%y")
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


ll = getLineList(path="~/Data/new-variant/Anonymised Combined Line List 20210118.csv")
dll = getDeathsLineList(path = "~/Data/new-variant/20210118 COVID19 Deaths.xlsx")
sgll = getSGeneLineList(path = "~/Data/new-variant/SGTF_linelist_20210118.csv")

# ll = getLineList(path="~/Data/new-variant/Anonymised Combined Line List 20210115.csv")
# dll = getDeathsLineList(path = "~/Data/new-variant/20210115 COVID19 Deaths.xlsx")
# sgll = getSGeneLineList(path = "~/Data/new-variant/SGTF_linelist_20210115.csv")

censorLength = 28
B117Date = as.Date("2020-10-01")
latestDate = max(dll$dod,na.rm=TRUE)

# get the S gene line list and remove all patients who have more than one S Gene result. Otherwise we don;t have a single source in time for when to start the survival clock.
# interpretSGene assigns sGene "Positive" for S+N+ORF+ with cutOff for S at 40 and N & ORF at 30; Negative is S-N+ORF+; equivocal is anything else (e.g. S-N-ORF+)
# We exclude anything which does not match the line list.

sgll3 = sgll %>% interpretSGene(S_CT = 40,ORF1ab_CT = 30,N_CT = 30) %>% filter(!is.na(FINALID)) %>% group_by(FINALID) %>% filter(n() == 1) %>% ungroup()

# Join the line list, deaths line list and uniquified sGene line list by FINALID
coxData = ll %>% 
  left_join(sgll3, by="FINALID", suffix=c("",".sgene")) %>% 
  left_join(dll %>% mutate(FINALID=as.numeric(finalid)), by=c("FINALID"),suffix=c("",".death"))

coxData = coxData %>% 
  filter(is.na(specimen_date.sgene) | specimen_date.sgene <= latestDate) # make sure we are not including tests for which we could not have death info.

coxData2 = coxData %>% select(
  FINALID, specimen_date, 
  lab_report_date, `specimen_date.sgene`, dateadmission_NHSE, dod,death_type28,age,sex,imd_decile,sGene,NHSER_name,NHSER_code,CT_N = N_L, ethnicity_final, LTLA_name) %>% 
  mutate(
    died = !is.na(dod),
    diedWithin28days = !is.na(death_type28), # This is the line list definition of death within 28 days
    dodAt28days = ifelse(
      is.na(specimen_date.sgene) | 
        dod > specimen_date.sgene+censorLength | 
        dod < specimen_date.sgene, NA, dod), # this is the date of death (or NA if it is >28 days after sgene specimen date.)
    censoredAt28days = ifelse(is.na(dodAt28days),0,1),
    censoredDate = pmin(latestDate, specimen_date.sgene+censorLength,na.rm = TRUE), # earliest of last recorded death or specimen date
  ) %>% 
  mutate(
    reportingDelay = as.numeric(lab_report_date-specimen_date),
    time = ifelse(censoredAt28days==0, censoredDate-as.numeric(specimen_date.sgene), dodAt28days-as.numeric(specimen_date.sgene)), 
    admissionDelay = as.numeric(dateadmission_NHSE-specimen_date),
    status = censoredAt28days
  ) %>%
  mutate(
    ageCat = cut(age, breaks = c(-Inf,30,60,70,80,Inf), labels = c("<30","30-59","60-69","70-79","80+"), right=FALSE),
    sGeneEra = case_when(
      is.na(sGene) ~ "Unknown (and P1)",
      sGene == "Negative" & specimen_date.sgene >= B117Date ~ "Neg Post B.1.1.7",
      sGene == "Negative" & specimen_date.sgene < B117Date ~ "Neg Pre B.1.1.7",
      TRUE ~ as.character(sGene))
  )

coxData3 = coxData2 %>%
  filter(!is.na(imd_decile)) %>%
  filter(!is.na(sex) & sex != "Unknown") %>% mutate() %>%
  filter(!is.na(ageCat) & !is.na(age)) %>%
  filter(!is.na(ethnicity_final) & !ethnicity_final %in% c("Unknown")) %>%
  filter(is.na(admissionDelay) | admissionDelay >= 0) %>%
  filter(is.na(reportingDelay) | (reportingDelay >= 0 & reportingDelay <10) ) %>%
  mutate(
    ethnicity_final = ethnicity_final %>% forcats::as_factor() %>% forcats::fct_relevel("White"),
    sex = sex %>% forcats::as_factor(),
    imd_decile = forcats::fct_relevel(forcats::as_factor(as.numeric(imd_decile)),"5"),
    imd_decile_2 = forcats::as_factor(as.numeric(imd_decile)),
    sGene = sGene %>% forcats::fct_relevel("Positive"),
    sGeneEra = sGeneEra %>% forcats::as_factor() %>% forcats::fct_relevel("Positive"),
    relativeCopyNumber = 2^(median(CT_N,na.rm=TRUE)-CT_N),
    deathStatus = ifelse(diedWithin28days,"Dead <28 days","Other")
  )

## quality control ----
if (any(is.na(coxData3$LTLA_name))) stop("unknowns for LTLA")
# if (any(is.na(coxData3$LTLA_name))) stop("unknowns for LTLA")

## case matching ----
# match cases by "sex","imd_decile","LTLA_name","ethnicity_final","ageCat"
# exclude people < 30 as mortality so low
# then ensure age difference < 2 years & specimen date < 4 days
# select S negatives and match to S positives
# randomly select a single match out of possible matches

posSgene = coxData3 %>% filter(sGene=="Positive" & specimen_date.sgene > B117Date & age >= 30)
negSgene = coxData3 %>% filter(sGene=="Negative" & specimen_date.sgene > B117Date & age >= 30)

# pairwise matching
matches = negSgene %>% inner_join(posSgene, by=c("sex","imd_decile","LTLA_name","ethnicity_final","ageCat"),suffix=c("",".match")) %>% filter(
  abs(age-age.match) < 2 & 
  abs(as.numeric(specimen_date.sgene-specimen_date.sgene.match)) < 4
)

# randomly select a single match out of possible matches
set.seed(102)
matchesSelected = matches %>% 
  group_by(FINALID) %>% sample_n(1) %>% # filter out a single matching at random from pos -> neg
  group_by(FINALID.match) %>% sample_n(1) %>%  # filter out a single matching at random from neg -> pos
  ungroup() 

negMatched = matchesSelected %>% select(-ends_with(".match"))
posMatched = matchesSelected %>% select(FINALID = FINALID.match) %>% inner_join(posSgene, by="FINALID")

coxFinal = bind_rows(posMatched,negMatched) %>% 
  mutate(sGene = sGene %>% forcats::fct_drop()) # equivocal category is dropped

## Construct table 1 summary of pairwise matching ----
combinedSummary2 = summaryTable(posMatched) %>% rename_with(~paste0("S pos ",.x),.cols=c(N,`%age`,`mean (SD)`)) %>% #,by=c("category","value")) %>%
    full_join(summaryTable(negMatched) %>% rename_with(~paste0("S neg ",.x),.cols=c(N,`%age`,`mean (SD)`)),by=c("category","value")) %>% 
    full_join(summaryTable(coxFinal %>% filter(diedWithin28days)) %>% rename_with(~paste0("Died ",.x),.cols=c(N,`%age`,`mean (SD)`)),by=c("category","value"))

table1Data = combinedSummary2 %>% select(category,everything()) %>% arrange(category)

## Check for residual biases in case matched data set ----
# small mean age difference and specimen date difference
# no obvious pattern in admission delays
# CT values lower in sGene negative and in those who died. This is ? feature of sGene neg infection
# specimen dates reasonably distributed in time given that the sGene negative numbers increasing. Could constrain analysis to post Dec only.

deltas = matchesSelected %>% summarise(
  ageDifference = mean(age-age.match),
  specimenDateDifference = mean(as.numeric(specimen_date.sgene-specimen_date.sgene.match))
)
# There is a mean age difference of `r sprintf("%1.1f",deltas$ageDifference)` years.
# There is a mean specimen date difference of `r sprintf("%1.1f",deltas$specimenDateDifference)` years.

p1 = ggplot(coxFinal,aes(fill=sGene,x=admissionDelay))+
  geom_bar(aes(y = ..prop..), stat="count",position=position_dodge(width=0.7), width = 0.6)+
  ylab("probability")+
  coord_cartesian(xlim=c(0,15))+facet_wrap(~sGene, scales = "free_y", ncol=1)

p2 = ggplot(coxFinal,aes(x=sGene,colour=diedWithin28days,y=CT_N))+geom_boxplot()+ylab("N Gene CT value")

p3 = ggplot(coxFinal,aes(fill=sGene,x=specimen_date.sgene))+
  geom_bar(stat="count",position=position_dodge(width=0.7), width = 0.6)+
  ylab("count")+
  facet_wrap(~sGene, scales = "free_y", ncol=1)

fig1 = p1+p2+p3+patchwork::plot_annotation(tag_levels = "A")+patchwork::plot_layout(ncol=3,guides="collect")

## Survival models ----
# Basic model
# Adjusted for N gene CT values

survModel = survival::coxph(Surv(time,status) ~ sGene, coxFinal)


survModel2 = survival::coxph(Surv(time,status) ~ sGene+CT_N, coxFinal)

# Supplementary materials
## Summary of full unmatched dataset
combinedSummary = summaryTable(coxData3) %>% rename_with(~paste0("All ",.x),.cols=c(N,`%age`,`mean (SD)`)) %>%
  left_join(summaryTable(coxData3 %>% filter(!is.na(sGene))) %>% rename_with(~paste0("Tested ",.x),.cols=c(N,`%age`,`mean (SD)`)),by=c("category","value")) %>%
  left_join(summaryTable(coxData3 %>% filter(diedWithin28days)) %>% rename_with(~paste0("Died ",.x),.cols=c(N,`%age`,`mean (SD)`)),by=c("category","value")) %>%
  left_join(summaryTable(coxData3 %>% filter(diedWithin28days & !is.na(sGene))) %>% rename_with(~paste0("Died + Tested ",.x),.cols=c(N,`%age`,`mean (SD)`)),by=c("category","value"))


## Outputs ----
# View(table1Data)
# fig1
# summary(survModel)
# summary(survModel2)

