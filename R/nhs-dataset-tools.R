# load CHESS data set

#' create a neighbourhood network from a shapefile
#' 
#' @param path - a path to the chess csv file
#' @import dplyr
#' @return raw CHESS data set
#' @export
getCHESS = function(path) {
  out = readr::read_csv(path, 
                  col_types = cols(
                    caseid = col_integer(),
                    trustcode = col_character(),
                    trustname = col_character(),
                    dateupdated = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
                    weekno = col_double(),
                    weekofadmission = col_double(),
                    yearofadmission = col_double(),
                    ageyear = col_double(),
                    agemonth = col_double(),
                    ethnicity = col_character(),
                    postcode = col_character(),
                    sex = col_character(),
                    estimateddate = col_date(format = "%Y-%m-%d"),
                    notknownonset = col_character(),
                    infectionswabdate = col_date(format = "%Y-%m-%d"),
                    labtestdate = col_date(format = "%Y-%m-%d"),
                    typeofspecimen = col_character(),
                    otherspecimentype = col_character(),
                    covid19 = col_character(),
                    influenzaah1n1pdm2009 = col_logical(),
                    influenzaah3n2 = col_character(),
                    influenzab = col_character(),
                    influenzaanonsubtyped = col_logical(),
                    influenzaaunsubtypable = col_logical(),
                    rsv = col_character(),
                    otherresult = col_character(),
                    otherdetails = col_character(),
                    admittedfrom = col_character(),
                    dateadmittedicu = col_date(format = "%Y-%m-%d"),
                    hoursadmittedicu = col_double(),
                    minutesadmittedicu = col_double(),
                    dateleavingicu = col_date(format = "%Y-%m-%d"),
                    othercomplication = col_character(),
                    sbother = col_character(),
                    admittedhospital = col_character(),
                    sbdate = col_date(format = "%Y-%m-%d"),
                    ventilatedwhilstadmitted = col_logical(),
                    admissionflu = col_character(),
                    otherhospital = col_character(),
                    admissioncovid19 = col_character(),
                    ispneumoniacomplication = col_character(),
                    isardscomplication = col_character(),
                    isunknowncomplication = col_character(),
                    isothercoinfectionscomplication = col_character(),
                    isothercomplication = col_character(),
                    issecondarybacterialpneumoniacom = col_character(),
                    ventilatedwhilstadmitteddays = col_double(),
                    patientecmo = col_logical(),
                    wasthepatientadmittedtoicu = col_character(),
                    organismname = col_character(),
                    daysecmo = col_logical(),
                    hospitaladmissiondate = col_date(format = "%Y-%m-%d"),
                    hospitaladmissionhours = col_double(),
                    hospitaladmissionminutes = col_double(),
                    admissionrsv = col_character(),
                    respiratorysupportnone = col_character(),
                    oxygenviacannulaeormask = col_character(),
                    highflownasaloxygen = col_character(),
                    noninvasiveventilation = col_character(),
                    invasivemechanicalventilation = col_character(),
                    respiratorysupportecmo = col_character(),
                    anticovid19treatment = col_character(),
                    chonicrepiratory = col_character(),
                    chonicrepiratorycondition = col_character(),
                    asthmarequiring = col_character(),
                    asthmarequiringcondition = col_character(),
                    chronicheart = col_character(),
                    chronicheartcondition = col_character(),
                    chronicrenal = col_character(),
                    chronicrenalcondition = col_character(),
                    chronicliver = col_character(),
                    chroniclivercondition = col_character(),
                    chronicneurological = col_character(),
                    chronicneurologicalcondition = col_character(),
                    isdiabetes = col_character(),
                    diabetestype = col_character(),
                    immunosuppressiontreatment = col_character(),
                    immunosuppressiontreatmentcondit = col_character(),
                    immunosuppressiondisease = col_character(),
                    immunosuppressiondiseaseconditio = col_character(),
                    other = col_character(),
                    othercondition = col_character(),
                    obesityclinical = col_character(),
                    obesitybmi = col_character(),
                    pregnancy = col_character(),
                    gestationweek = col_double(),
                    travel = col_logical(),
                    travelto = col_logical(),
                    traveldateofreturn = col_logical(),
                    prematurity = col_character(),
                    hypertension = col_character(),
                    hypertensioncondition = col_character(),
                    travelin14days = col_character(),
                    travelin14dayscondition = col_character(),
                    worksashealthcareworker = col_character(),
                    contactwithconfirmedcovid19case = col_character(),
                    contactwithconfirmedcovid19casec = col_character(),
                    finaloutcome = col_character(),
                    finaloutcomedate = col_date(format = "%Y-%m-%d"),
                    transferdestination = col_character(),
                    outcomeother = col_character(),
                    causeofdeath = col_character()
                  ))
  return(out)
}


#' Load line list
#' 
#' @param path - path to the line list file
#' @import dplyr
#' @return raw CHESS data set
#' @export
getLineList = function(path) {
  readxl::read_excel(path.expand(path), 
         col_types = c("numeric", "text", "text", 
                       "text", "text", "text", "text", "text", 
                       "text", "text", "numeric", "date", 
                       "date", "date"))
}

#' Load ff100 file
#' 
#' @param path - path to the ff100 file
#' @import dplyr
#' @return raw FF100 data set
#' @export
getFF100 = function(path) {
    readr::read_csv(path,
                 col_types = cols(
                   FF100_ID = col_integer(),
                   ContactOf_FF100_ID = col_integer(),
                   date_reported = col_date(format = "%Y-%m-%d"),
                   date_labtest = col_date(format = "%Y-%m-%d"),
                   date_onset = col_date(format = "%Y-%m-%d"),
                   date_hosp_adm = col_date(format = "%Y-%m-%d"),
                   date_hosp_dis = col_date(format = "%Y-%m-%d"),
                   hosp_adm = col_logical(),
                   date_NHSdirect = col_date(format = "%Y-%m-%d"),
                   NHSdirect = col_logical(),
                   date_GP_first = col_date(format = "%Y-%m-%d"),
                   GP = col_logical(),
                   date_AEhosp_first = col_date(format = "%Y-%m-%d"),
                   AEhosp = col_logical(),
                   age = col_double(),
                   gender = col_character(),
                   local_authority = col_character(),
                   travel_anywhere = col_logical(),
                   heart_ds = col_logical(),
                   diabetes = col_logical(),
                   immunodeficiency = col_logical(),
                   kidney_ds = col_logical(),
                   liver_ds = col_logical(),
                   resp_ds = col_logical(),
                   asthma = col_logical(),
                   malignancy = col_logical(),
                   organ_recipient = col_logical(),
                   neuro_ds = col_logical(),
                   pregnant = col_logical(),
                   fever = col_logical(),
                   runny_nose = col_logical(),
                   sneezing = col_logical(),
                   cough = col_logical(),
                   short_breath = col_logical(),
                   sore_throat = col_logical(),
                   diarrhoea = col_logical(),
                   nausea = col_logical(),
                   vomit = col_logical(),
                   fatigue = col_logical(),
                   muscle_ache = col_logical(),
                   joint_ache = col_logical(),
                   appetite_loss = col_logical(),
                   headache = col_logical(),
                   seizure = col_logical(),
                   alter_consious = col_logical(),
                   nose_bleed = col_logical(),
                   rash = col_logical(),
                   smell_loss = col_logical(),
                   symptom_other = col_logical(),
                   any_symptom = col_logical(),
                   status = col_character(),
                   case_classification = col_character(),
                   HCW_exposure = col_logical(),
                   ARDS = col_logical(),
                   mech_ventl = col_logical(),
                   ICU_adm = col_logical(),
                   date_ICU_adm = col_date(format = "%Y-%m-%d"),
                   date_recovery = col_date(format = "%Y-%m-%d"),
                   date_death = col_date(format = "%Y-%m-%d"),
                   date_exposure_first = col_date(format = "%Y-%m-%d"),
                   date_exposure_last = col_date(format = "%Y-%m-%d"),
                   exposure_setting_final = col_character()
                 ))
}

#' Load Chess summary file
#' 
#' @param path - path to the ff100 file
#' @import dplyr
#' @return raw FF100 data set
#' @export
getCHESSSummary = function(path) {
  read_csv(paths$chessSummary, col_types = cols(
    DateRange = col_date("%d-%m-%Y"),
    DateOfAdmission = col_date("%d-%m-%Y"),
    YearofAdmission = col_integer(),
    TrustName = col_character(),
    Code = col_character(),
    .default = col_integer()))
  chessSummary = chessSummary %>% select(-X67) %>% pivot_longer(cols = c(everything(),-all_of(c("DateRange","DateOfAdmission","YearofAdmission","TrustName","Code","Total"))), names_to = "variable", values_to = "count")
  chessSummary = chessSummary %>% filter(Code != "Total")
  tmp = chessSummary %>% mutate(
    toAge = str_replace(variable,"^.*_([^_]+)$","\\1"),
    fromAge = str_replace(variable,"^.*_([^_]+)_[^_]+$","\\1"),
    variable = str_replace(variable,"^(.*)_[^_]+_[^_]+$","\\1")
  )
  tmp = tmp %>% mutate(fromAge = ifelse(fromAge=="GreaterThanEqual", toAge, fromAge))
  tmp = tmp %>% mutate(fromAge = ifelse(fromAge=="LessThan", 0, fromAge))
  chessSummary = tmp %>% mutate(toAge = ifelse(fromAge==toAge, 120, toAge))
 return(chessSummary)
}

## Chess data

#' Cleanses CHESS data set and adds a few fields
#' 
#' * Detects trusts that have not been updating records
#' * establishes a censoring date on a trust by trust basis
#' * checks all outcomes have an outcomedate
#' * checks outcome dates are after admission date
#' * excludes patients whose tests are >10 days after admission e.g. hospital acquired
#' * exludes paediatric patients, and those with no known gender
#' * adds censoring status (as status)
#' * adds time from admission to outcome (as timeToOutcome)
#' 
#' @param path - path to the ff100 file
#' @import dplyr
#' @return cleansed CHESS data set
#' @export
cleanseCHESSData = function(CHESSdf, date) {
  CHESS_date = as.Date(date)

  hospAcc = CHESSdf %>% group_by(trustcode, trustname) %>% summarise(
    updated = sum(if_else(is.na(dateupdated),0,1)),
    mostRecentUpdate = as.Date(suppressWarnings(max(dateupdated,na.rm=TRUE))),
    outcome = sum(if_else(is.na(finaloutcomedate),0,1)),
    mostRecentOutcome = suppressWarnings(max(finaloutcomedate,na.rm=TRUE)),
    outcomeWithoutDates = sum(if_else(!is.na(finaloutcome) & is.na(finaloutcomedate),1,0))
  )
  
  incHosp = hospAcc %>% filter(
    mostRecentOutcome >= CHESS_date-3 & mostRecentUpdate >= CHESS_date-3 & outcomeWithoutDates < 5
  ) %>% mutate(
    censorDate = if_else(mostRecentOutcome < mostRecentUpdate, mostRecentUpdate, mostRecentOutcome)
  ) %>% select(
    trustcode, censorDate
  )
  
  CHESSClean = CHESSdf %>% inner_join(incHosp, by="trustcode") %>% 
    filter(!(is.na(finaloutcomedate) & !is.na(finaloutcome) )) %>% 
    mutate(
      censored = is.na(finaloutcomedate),
      outcomedate = as.Date(if_else(is.na(finaloutcomedate),censorDate,finaloutcomedate)),
      timeToTest = as.numeric(labtestdate - hospitaladmissiondate),
      age = ageyear+agemonth/12,
      timeToOutcome = as.numeric(outcomedate - hospitaladmissiondate)
    ) %>% filter(
      !is.na(time) & timeToTest < 10 & timeToOutcome > 0 & sex != "Unknown"
    ) %>% mutate(
      sex = as.factor(sex),
      finaloutcome = as.factor(finaloutcome)
    ) %>% mutate(
      sex = relevel(sex, ref="Male")
    )
  
  return(CHESSClean)
}


#' generrate survival data and right censored data from 
#' 
#' @param path - path to the ff100 file
#' @import dplyr
#' @return data set in survival format including status, time, left, right, ageCat columns 
#' @export
generateSurvivalData = function(df, startDateVar, outcomeDateVar, outcomeExpr, ageVar = "age",
                                ageBreaks = c(-Inf,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,Inf),
                                ageLabels = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+'),
                                ageReferenceCat = NA
) {
  startDateVar = ensym(startDateVar) 
  outcomeDateVar = ensym(outcomeDateVar)
  outcomeExpr = enexpr(outcomeExpr)
  ageVar = ensym(ageVar)
  
  out = df %>% 
    filter(!is.na(!!startDateVar)) %>% 
    mutate(
      status = if_else(!!outcomeExpr,1,0),
      time = as.numeric(!!outcomeDateVar - !!startDateVar),
      ageCat = cut(!!ageVar,breaks = ageBreaks, labels = ageLabels, ordered_result = TRUE, include.lowest = TRUE)
    ) %>% mutate(
      left=time,
      right=ifelse(status==0, NA, time)
    ) %>% as.data.frame() 
  
  if (!is.na(ageReferenceCat)) {
    out = out %>% mutate(
      ageCat = relevel(factor(as.character(ageCat),ordered=FALSE), ref = ageReferenceCat)
    )
  }
  
  return(out)
}
