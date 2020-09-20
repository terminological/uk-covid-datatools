#' SPIM private data
#' @export
SPIMDatasetProvider = R6::R6Class("SPIMDatasetProvider", inherit=CovidTimeseriesProvider, public = list(
  
  
    directory=NULL,
    
    initialize = function(providerController, path, ...) {
      self$directory = path.expand(path)
      super$initialize(providerController, ...)
    },
    
    ### filters ----
    
    filter=list(
      chess="CHESS COVID19", #~/S3/encrypted/5Apr/NHS/CHESS COVID19 CaseReport 20200405.csv",
      lineList="Anonymised .*Line List",
      rcgp="RCGP",
      deathsLineList="COVID19 Deaths",
      ff100="FF100",
      chessSummary="CHESS Aggregate Report",
      oneOneOne = "SPIM-111-999",
      onsWeekly = "SPIM_ONS",
      aeSitrep = "AESitrep",
      trust = "SPIM_trust",
      seroprevalence = "seroprev",
      negPillar1 = "Negatives pillar1",
      negPillar2 = "Negatives pillar2",
      oneOneOneLineList = "111telephony_CLEANSED",
      fourNationsCases = "Casedata_AllNations"
    ),
    
    #### Get raw file paths ----
    
    #' @description Search a file path for the 
    #' 
    #' @param path - path to the line list file
    
    #' @return raw line list data set
    
    getPaths = function(...) {
      path = self$directory
      return(self$getDaily("DATAFILES", ..., orElse = function (...) {
        tmp = list.files(path=path,recursive = TRUE)
        return(tmp)
        
        
        # paths=list(
        #   chess=fn("CHESS COVID19"), #~/S3/encrypted/5Apr/NHS/CHESS COVID19 CaseReport 20200405.csv",
        #   lineList=fn("Anonymised .*Line List"),
        #   rcgp=fn("RCGP"),
        #   deathsLineList=fn("COVID19 Deaths"),
        #   ff100=fn("FF100"),
        #   chessSummary=fn("CHESS Aggregate Report"),
        #   oneOneOne = fn("SPIM-111-999"),
        #   onsWeekly = fn("SPIM_ONS"),
        #   aeSitrep = fn("AESitrep"),
        #   trust = fn("SPIM_trust"),
        #   seroprevalence = fn("seroprev"),
        #   negPillar1 = fn("Negatives pillar1"),
        #   negPillar2 = fn("Negatives pillar2"),
        #   fullList = tmp
        #   # sitrep="~/S3/encrypted/8Apr/Covid sitrep report incl CIC 20200408 FINAL.xlsx"
        # )
        # if(any(is.na(paths))) stop("Missing files")
        # return(paths)
      }))
    },
    
    getLatest = function(search) {
      tmp2 = self$getPaths() %>% stringr::str_subset(search)
      tmp2Date = tmp2 %>% stringr::str_extract("20[1-2][0-9]-?[0-1][0-9]-?[0-3][0-9]") %>% stringr::str_remove_all("-")
      tmp3 = tmp2[tmp2Date == max(tmp2Date)]
      if(length(tmp3)==0) {
        warning("Missing file: ",search)
        return(NA)
      }
      return(paste0(self$directory,"/",tmp3))
    },
    
    getNewerThan = function(search, date = as.Date("2020-01-01")) {
      return(self$getSpecificDates(search,(date+1):Sys.Date()))
    },
    
    getSpecificDates = function(search, dates) {
      tmp2 = self$getPaths() %>% stringr::str_subset(search)
      tmp2Date = tmp2 %>% stringr::str_extract("/([^/]+)\\.") %>% stringr::str_extract("20[1-2][0-9]-?[0-1][0-9]-?[0-3][0-9]") %>% stringr::str_remove_all("-") %>% as.Date("%Y%m%d")
      tmp3 = tmp2[tmp2Date %in% dates]
      if(length(tmp3)==0) {
        warning("Missing file: ",search)
        return(NA)
      }
      return(paste0(self$directory,"/",tmp3))
    },
    
    #### Get raw SPIM files ----
    
    
    getOneOneOneLineList = function(dateFrom=Sys.Date()-28, ...) {
      self$getDaily(id = "SPIM-111-LINE-LIST",params=list(dateFrom),...,orElse= function(...) {
        paths = self$getNewerThan(search = self$filter$oneOneOneLineList, date = dateFrom)
        tmp = lapply(paths, function(x) readr::read_csv(x, col_types = readr::cols(.default = readr::col_character())))
        tmp2 = bind_rows(tmp)
        tmp3 = tmp2 %>% mutate(
            date = as.Date(DateVal),
            name = case_when(
              CCGName == "NHS Herefordshire and Worcestershire CCG" ~ "NHS Herefordshire CCG", # this name is not in ONS
              TRUE ~ CCGName
            ),
            ageCat = stringr::str_remove(AgeGroup,"age"),
            source = "111",
            subgroup = case_when(
              MetricName %like% "%Clinical Assessment service 1 hour%" ~ "urgent clinical review",
              MetricName %like% "PC Speak to 1h/CAS" ~ "urgent clinical review",
              MetricName %like% "%Clinical Assessment service 2 hour%" ~ "urgent clinical review",
              MetricName %like% "PC Speak to/Contact 2h" ~ "urgent clinical review",
              MetricName %like% "%Clinical Assessment%" ~ "clinical review",
              MetricName %like% "%Contact%" ~ "clinical review",
              MetricName %like% "%Ambulance%" ~ "emergency ambulance",
              (MetricName %like% "%Self Care%" | MetricName %like% "%isolate%") ~ "self care",
              TRUE ~ "other" #as.character(NA)
            ),
            gender = NA_character_,
            note = MetricName
          ) %>% 
          select(-MetricCode,-DateVal,-AgeGroup,-CCGName,-MetricName) %>%
          dplyr::mutate(subgroup = factor(subgroup,levels=c("self care", "clinical review", "urgent clinical review", "emergency ambulance", "other"), ordered = TRUE)) %>%
          self$codes$findCodesByName(codeTypes = c("CCG","CCG20"))
        return(tmp3 %>% select(-name.original))
      })
      #ccgs = self$codes$getCodes() %>% filter(codeType == "CCG" & status == "live")
    },
    
    #' @description Load line list
    #' 
    #' @param path - path to the line list file
    #' @return raw line list data set
    getDeathsLineList = function(...) {
      path = self$getLatest(self$filter$deathsLineList)
      self$getDaily("DEATHS-LINE-LIST", ..., orElse = function (...) {
        tmp = readxl::read_excel(path.expand(path), 
                                 col_types = "text")
        datecols = c(colnames(tmp) %>% stringr::str_subset("date"),"dod")
        
        for(datecol in datecols) {
          tmp[[datecol]] = suppressWarnings(as.Date(as.numeric(tmp[[datecol]]),"1899-12-30"))
        }
        tmp = tmp %>% 
          dplyr::mutate(
            age = as.integer(age),
            gender = self$normaliseGender(ifelse(is.na(gender),"unknown",gender))
          )
        return(tmp %>% dplyr::ungroup())
      })
    },
    
    #' @description Load line list
    #' 
    
    #' @return raw line list data set
    
    getLineList = function(...) {
      path = self$getLatest(self$filter$lineList)
      self$getDaily("LINE-LIST", ..., orElse = function (...) {
        if (stringr::str_detect(path,"csv")) {
          tmp = readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()))
          tmp = tmp %>% 
            dplyr::mutate(
              Onsetdate = suppressWarnings(as.Date(Onsetdate,"%m/%d/%Y")),
              specimen_date = suppressWarnings(as.Date(specimen_date,"%m/%d/%Y")),
              lab_report_date = suppressWarnings(as.Date(lab_report_date,"%m/%d/%Y")),
              age = suppressWarnings(as.numeric(age)),
            ) 
          if(any(is.na(tmp$specimen_date))) browser()
        } else {
          tmp = readxl::read_excel(path.expand(path), 
                   col_types = "text") #c("numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "date", "date", "date"))
          tmp = tmp %>% 
            dplyr::mutate(
              Onsetdate = suppressWarnings(as.Date(as.numeric(Onsetdate),"1899-12-30")),
              specimen_date = suppressWarnings(as.Date(as.numeric(specimen_date),"1899-12-30")),
              lab_report_date = suppressWarnings(as.Date(as.numeric(lab_report_date),"1899-12-30")),
              age = suppressWarnings(as.numeric(age)),
            )
        }
        
        return(tmp %>% dplyr::ungroup())
      })
      
      # TODO: https://github.com/sarahhbellum/NobBS
    },
    
    #' @description Load the seroprevalance file
    #' 
    
    #' @return raw FF100 data set
    
    getSeroprevalence = function(...) {
      path = self$getLatest(self$filter$seroprevalence)
      self$getDaily("SEROPREV", ..., orElse = function (...) {
        # ID	Barcode	surv	age	age_m	Sex	Region	Location	sample_region	SampleDate	isoweek_sample	EuroImm_outcome	EuroImm_Units	RBD_outcome	RBD_Units
        
        for (sheet in readxl::excel_sheets(path.expand(path))) {
          a1 = readxl::read_excel(path.expand(path), sheet = sheet, range = "A1",col_names = FALSE)
          if (a1[[1]]=="Barcode") break
        }
        
        data = readxl::read_excel(path.expand(path), sheet = sheet, col_types = "text") %>% dplyr::mutate(
          SampleDate = suppressWarnings(as.Date(as.numeric(SampleDate),"1899-12-30")),
        ) %>% dplyr::mutate(
          SampleDate = if_else(is.na(SampleDate) & surv == "NHSBT_Wales_wk17", as.Date("2020-04-20"), SampleDate),
          age = suppressWarnings((as.numeric(age)))
        )
        if ("Abbott_units" %in% colnames(data)) data = data %>% dplyr::mutate(Abbott_units = suppressWarnings(as.numeric(Abbott_units)))
        if ("EuroImmun_units" %in% colnames(data)) data = data %>% dplyr::mutate(EuroImmun_units = suppressWarnings(as.numeric(EuroImmun_units)))
        if ("RBD_units" %in% colnames(data)) data = data %>% dplyr::mutate(RBD_units = suppressWarnings(as.numeric(RBD_units)))
        
        
        data2 = data %>% 
          self$postcodes$lookupWeightedFeatureByOutcode(outcodeVar = Location, onspdVar = ccg) %>% 
          dplyr::mutate(weight = ifelse(is.na(weight),0,weight)) %>%
          dplyr::group_by(Barcode) %>% 
          dplyr::arrange(desc(weight)) %>%
          dplyr::filter(row_number()==1) %>%
          dplyr::select(-weight) %>%
          self$postcodes$lookupWeightedFeatureByOutcode(outcodeVar = Location, onspdVar = nhser) %>% 
          dplyr::group_by(Barcode) %>% 
          dplyr::mutate(weight = ifelse(is.na(weight),0,weight)) %>%
          dplyr::arrange(desc(weight)) %>%
          dplyr::filter(row_number()==1) %>%
          dplyr::select(-weight)
        
        return(data2 %>% dplyr::filter(!is.na(SampleDate)) %>% dplyr::ungroup())
      })
    },
    
    #' @description Load ff100 file
    #' 
    
    #' @return raw FF100 data set
    
    getFF100 = function() {
      path = self$getLatest(self$filter$ff100)
      readr::read_csv(path,
                      col_types = readr::cols(
                        FF100_ID = readr::col_integer(),
                        ContactOf_FF100_ID = readr::col_integer(),
                        date_reported = readr::col_date(format = "%Y-%m-%d"),
                        date_labtest = readr::col_date(format = "%Y-%m-%d"),
                        date_onset = readr::col_date(format = "%Y-%m-%d"),
                        date_hosp_adm = readr::col_date(format = "%Y-%m-%d"),
                        date_hosp_dis = readr::col_date(format = "%Y-%m-%d"),
                        hosp_adm = readr::col_logical(),
                        date_NHSdirect = readr::col_date(format = "%Y-%m-%d"),
                        NHSdirect = readr::col_logical(),
                        date_GP_first = readr::col_date(format = "%Y-%m-%d"),
                        GP = readr::col_logical(),
                        date_AEhosp_first = readr::col_date(format = "%Y-%m-%d"),
                        AEhosp = readr::col_logical(),
                        age = readr::col_double(),
                        gender = readr::col_character(),
                        local_authority = readr::col_character(),
                        travel_anywhere = readr::col_logical(),
                        heart_ds = readr::col_logical(),
                        diabetes = readr::col_logical(),
                        immunodeficiency = readr::col_logical(),
                        kidney_ds = readr::col_logical(),
                        liver_ds = readr::col_logical(),
                        resp_ds = readr::col_logical(),
                        asthma = readr::col_logical(),
                        malignancy = readr::col_logical(),
                        organ_recipient = readr::col_logical(),
                        neuro_ds = readr::col_logical(),
                        pregnant = readr::col_logical(),
                        fever = readr::col_logical(),
                        runny_nose = readr::col_logical(),
                        sneezing = readr::col_logical(),
                        cough = readr::col_logical(),
                        short_breath = readr::col_logical(),
                        sore_throat = readr::col_logical(),
                        diarrhoea = readr::col_logical(),
                        nausea = readr::col_logical(),
                        vomit = readr::col_logical(),
                        fatigue = readr::col_logical(),
                        muscle_ache = readr::col_logical(),
                        joint_ache = readr::col_logical(),
                        appetite_loss = readr::col_logical(),
                        headache = readr::col_logical(),
                        seizure = readr::col_logical(),
                        alter_consious = readr::col_logical(),
                        nose_bleed = readr::col_logical(),
                        rash = readr::col_logical(),
                        smell_loss = readr::col_logical(),
                        symptom_other = readr::col_logical(),
                        any_symptom = readr::col_logical(),
                        status = readr::col_character(),
                        case_classification = readr::col_character(),
                        HCW_exposure = readr::col_logical(),
                        ARDS = readr::col_logical(),
                        mech_ventl = readr::col_logical(),
                        ICU_adm = readr::col_logical(),
                        date_ICU_adm = readr::col_date(format = "%Y-%m-%d"),
                        date_recovery = readr::col_date(format = "%Y-%m-%d"),
                        date_death = readr::col_date(format = "%Y-%m-%d"),
                        date_exposure_first = readr::col_date(format = "%Y-%m-%d"),
                        date_exposure_last = readr::col_date(format = "%Y-%m-%d"),
                        exposure_setting_final = readr::col_character()
                      ))
    },
    
    #' @description Load Bristol data
    #' 
    #' @param path - path to the bristol data file
    
    #' @return raw Bristol data set
    
    getBristolData = function(path) {#
      data = readr::read_csv(path, 
                             col_types = readr::cols(
                               enrollment_date = readr::col_date(format = "%Y-%m-%d"),
                               admission_date = readr::col_date(format = "%Y-%m-%d"),
                               radiology_date = readr::col_date(format = "%Y-%m-%d"),
                               virology_date_of_assessmen = readr::col_date(format = "%Y-%m-%d"),
                               ppv23_date = readr::col_date(format = "%Y-%m-%d"),
                               flu_date = readr::col_date(format = "%Y-%m-%d"),
                               discharge_date = readr::col_date(format = "%Y-%m-%d"),
                               radiology_othertest = readr::col_character(),
                               egfr_result = readr::col_character(),
                               vitamin_d_during_admission = readr::col_character(),
                               d_dimer = readr::col_character()
                             ),
                             na = c("", "NA", "n/a", "N/A", "Not done")
      )
      
      data = data %>% dplyr::mutate(
        vitamin_d_during_admission = suppressWarnings(vitamin_d_during_admission %>% stringr::str_remove(" \\(readmission\\)") %>% as.numeric()),
        d_dimer = suppressWarnings(d_dimer %>% stringr::str_remove(" \\(readmission\\)") %>% as.numeric()),
        fio2 = suppressWarnings(fio2 %>% stringr::str_remove_all("%") %>% as.numeric())
      ) %>% dplyr::mutate(
        fio2 = if_else(fio2<21, NA_real_, fio2)
      )
      
      data = data %>% dplyr::filter((is.na(discharge_date) | (discharge_date < Sys.Date() & admission_date < discharge_date)))
      
      
      #Setting Factors(will create new variable for factors)
      data$redcap_repeat_instrument.factor = factor(data$redcap_repeat_instrument,levels=c("radiology_results","bacterialfungal_culture","virology_results"))
      data$covid19.factor = factor(data$covid19,levels=c("1","0"))
      data$acute_illness.factor = factor(data$acute_illness,levels=c("1","2"))
      data$evidence_1___1.factor = factor(data$evidence_1___1,levels=c("0","1"))
      data$evidence_1___2.factor = factor(data$evidence_1___2,levels=c("0","1"))
      data$evidence_1___3.factor = factor(data$evidence_1___3,levels=c("0","1"))
      data$lrti_symptoms___1.factor = factor(data$lrti_symptoms___1,levels=c("0","1"))
      data$lrti_symptoms___2.factor = factor(data$lrti_symptoms___2,levels=c("0","1"))
      data$lrti_symptoms___3.factor = factor(data$lrti_symptoms___3,levels=c("0","1"))
      data$lrti_symptoms___4.factor = factor(data$lrti_symptoms___4,levels=c("0","1"))
      data$lrti_symptoms___5.factor = factor(data$lrti_symptoms___5,levels=c("0","1"))
      data$lrti_symptoms___6.factor = factor(data$lrti_symptoms___6,levels=c("0","1"))
      data$lrti_symptoms___7.factor = factor(data$lrti_symptoms___7,levels=c("0","1"))
      data$lrti_symptoms___8.factor = factor(data$lrti_symptoms___8,levels=c("0","1"))
      data$exclusion_criteria.factor = factor(data$exclusion_criteria,levels=c("1","0"))
      data$previous_enrolled_particip.factor = factor(data$previous_enrolled_particip,levels=c("1","0"))
      data$lrtd_diagnosis_excluded.factor = factor(data$lrtd_diagnosis_excluded,levels=c("1","0"))
      data$hosp.factor = factor(data$hosp,levels=c("1","2","3"))
      data$gender.factor = factor(data$gender,levels=c("1","2"))
      data$referral_source.factor = factor(data$referral_source,levels=c("1","0"))
      data$ethnicity.factor = factor(data$ethnicity,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19"))
      data$smoking.factor = factor(data$smoking,levels=c("1","2","3","4"))
      data$vaping.factor = factor(data$vaping,levels=c("1","2","3"))
      data$bris_cap_consent_obtained.factor = factor(data$bris_cap_consent_obtained,levels=c("1","2"))
      data$demographics_and_screening_complete.factor = factor(data$demographics_and_screening_complete,levels=c("0","1","2"))
      data$resp_disease2___1.factor = factor(data$resp_disease2___1,levels=c("0","1"))
      data$resp_disease2___2.factor = factor(data$resp_disease2___2,levels=c("0","1"))
      data$resp_disease2___3.factor = factor(data$resp_disease2___3,levels=c("0","1"))
      data$resp_disease2___4.factor = factor(data$resp_disease2___4,levels=c("0","1"))
      data$resp_disease2___5.factor = factor(data$resp_disease2___5,levels=c("0","1"))
      data$resp_disease2___6.factor = factor(data$resp_disease2___6,levels=c("0","1"))
      data$chd___1.factor = factor(data$chd___1,levels=c("0","1"))
      data$chd___2.factor = factor(data$chd___2,levels=c("0","1"))
      data$chd___3.factor = factor(data$chd___3,levels=c("0","1"))
      data$chd___4.factor = factor(data$chd___4,levels=c("0","1"))
      data$chd___5.factor = factor(data$chd___5,levels=c("0","1"))
      data$chd___6.factor = factor(data$chd___6,levels=c("0","1"))
      data$ckd.factor = factor(data$ckd,levels=c("1","2","3"))
      data$liver_disease.factor = factor(data$liver_disease,levels=c("1","2","3"))
      data$diabetes.factor = factor(data$diabetes,levels=c("1","2","3","4","5"))
      data$dementia___1.factor = factor(data$dementia___1,levels=c("0","1"))
      data$dementia___2.factor = factor(data$dementia___2,levels=c("0","1"))
      data$dementia___3.factor = factor(data$dementia___3,levels=c("0","1"))
      data$dementia___4.factor = factor(data$dementia___4,levels=c("0","1"))
      data$dementia___5.factor = factor(data$dementia___5,levels=c("0","1"))
      data$hemiplegia.factor = factor(data$hemiplegia,levels=c("1","0"))
      data$pvd.factor = factor(data$pvd,levels=c("1","0"))
      data$immsup.factor = factor(data$immsup,levels=c("1","0"))
      data$immunodeficiency.factor = factor(data$immunodeficiency,levels=c("1","0"))
      data$ctd.factor = factor(data$ctd,levels=c("1","0"))
      data$hiv___1.factor = factor(data$hiv___1,levels=c("0","1"))
      data$hiv___2.factor = factor(data$hiv___2,levels=c("0","1"))
      data$hiv___3.factor = factor(data$hiv___3,levels=c("0","1"))
      data$cancer.factor = factor(data$cancer,levels=c("1","2","3"))
      data$haem_malig___1.factor = factor(data$haem_malig___1,levels=c("0","1"))
      data$haem_malig___2.factor = factor(data$haem_malig___2,levels=c("0","1"))
      data$haem_malig___3.factor = factor(data$haem_malig___3,levels=c("0","1"))
      data$transplant.factor = factor(data$transplant,levels=c("1","0"))
      data$pregnancy.factor = factor(data$pregnancy,levels=c("1","2","3","4","5","6"))
      data$pud.factor = factor(data$pud,levels=c("1","0"))
      data$drugs___1.factor = factor(data$drugs___1,levels=c("0","1"))
      data$drugs___2.factor = factor(data$drugs___2,levels=c("0","1"))
      data$drugs___3.factor = factor(data$drugs___3,levels=c("0","1"))
      data$drugs___4.factor = factor(data$drugs___4,levels=c("0","1"))
      data$drugs___5.factor = factor(data$drugs___5,levels=c("0","1"))
      data$rockall_frailty_score1.factor = factor(data$rockall_frailty_score1,levels=c("1","2","3","4","5","6","7","8","9","10"))
      data$abx_14d_prior.factor = factor(data$abx_14d_prior,levels=c("1","2","3"))
      data$comorbidities_complete.factor = factor(data$comorbidities_complete,levels=c("0","1","2"))
      data$temperature.factor = factor(data$temperature,levels=c("1","2","3"))
      data$ox_on_admission.factor = factor(data$ox_on_admission,levels=c("1","0"))
      data$crb65_age.factor = factor(data$crb65_age,levels=c("1","0"))
      data$confusion.factor = factor(data$confusion,levels=c("1","0"))
      data$resp_rate.factor = factor(data$resp_rate,levels=c("1","0"))
      data$blood_pressure.factor = factor(data$blood_pressure,levels=c("1","0"))
      data$fever.factor = factor(data$fever,levels=c("1","2","3"))
      data$hypothermia.factor = factor(data$hypothermia,levels=c("1","2","3"))
      data$chills.factor = factor(data$chills,levels=c("1","2","3"))
      data$rigors.factor = factor(data$rigors,levels=c("1","2","3"))
      data$cough.factor = factor(data$cough,levels=c("1","2","3"))
      data$wheeze.factor = factor(data$wheeze,levels=c("1","2","3"))
      data$sputum.factor = factor(data$sputum,levels=c("1","2","3"))
      data$sob.factor = factor(data$sob,levels=c("1","2","3"))
      data$pleurisy.factor = factor(data$pleurisy,levels=c("1","2","3"))
      data$tachypnoea.factor = factor(data$tachypnoea,levels=c("1","2","3"))
      data$maliase.factor = factor(data$maliase,levels=c("1","2","3"))
      data$oe_cap.factor = factor(data$oe_cap,levels=c("1","2","3"))
      data$oe_lrtd.factor = factor(data$oe_lrtd,levels=c("1","2","3"))
      data$nhya.factor = factor(data$nhya,levels=c("1","2","3","4","5"))
      data$admission_data_complete.factor = factor(data$admission_data_complete,levels=c("0","1","2"))
      data$patient_blood_group.factor = factor(data$patient_blood_group,levels=c("1","2","3","4","5","6","7","8","9"))
      data$admission_blood_results_complete.factor = factor(data$admission_blood_results_complete,levels=c("0","1","2"))
      data$radio_test.factor = factor(data$radio_test,levels=c("1","2","3","4"))
      data$radiology_result.factor = factor(data$radiology_result,levels=c("1","2","3","4","5","6"))
      data$radiology_results_complete.factor = factor(data$radiology_results_complete,levels=c("0","1","2"))
      data$micro_test_done.factor = factor(data$micro_test_done,levels=c("1","0"))
      data$micro_test.factor = factor(data$micro_test,levels=c("1","2","3","4","5","6"))
      data$micro_isolates.factor = factor(data$micro_isolates,levels=c("1","2","3"))
      data$isolate_identified___1.factor = factor(data$isolate_identified___1,levels=c("0","1"))
      data$isolate_identified___2.factor = factor(data$isolate_identified___2,levels=c("0","1"))
      data$isolate_identified___3.factor = factor(data$isolate_identified___3,levels=c("0","1"))
      data$isolate_identified___4.factor = factor(data$isolate_identified___4,levels=c("0","1"))
      data$isolate_identified___5.factor = factor(data$isolate_identified___5,levels=c("0","1"))
      data$isolate_identified___6.factor = factor(data$isolate_identified___6,levels=c("0","1"))
      data$isolate_identified___7.factor = factor(data$isolate_identified___7,levels=c("0","1"))
      data$isolate_identified___8.factor = factor(data$isolate_identified___8,levels=c("0","1"))
      data$isolate_identified___9.factor = factor(data$isolate_identified___9,levels=c("0","1"))
      data$isolate_identified___10.factor = factor(data$isolate_identified___10,levels=c("0","1"))
      data$isolate_identified___11.factor = factor(data$isolate_identified___11,levels=c("0","1"))
      data$isolate_identified___12.factor = factor(data$isolate_identified___12,levels=c("0","1"))
      data$isolate_identified___13.factor = factor(data$isolate_identified___13,levels=c("0","1"))
      data$isolate_identified___14.factor = factor(data$isolate_identified___14,levels=c("0","1"))
      data$isolate_identified___15.factor = factor(data$isolate_identified___15,levels=c("0","1"))
      data$isolate_identified___16.factor = factor(data$isolate_identified___16,levels=c("0","1"))
      data$isolate_identified___17.factor = factor(data$isolate_identified___17,levels=c("0","1"))
      data$isolate_identified___18.factor = factor(data$isolate_identified___18,levels=c("0","1"))
      data$isolate_identified___19.factor = factor(data$isolate_identified___19,levels=c("0","1"))
      data$isolate_identified___20.factor = factor(data$isolate_identified___20,levels=c("0","1"))
      data$isolate_identified___21.factor = factor(data$isolate_identified___21,levels=c("0","1"))
      data$isolate_identified___22.factor = factor(data$isolate_identified___22,levels=c("0","1"))
      data$isolate_identified___23.factor = factor(data$isolate_identified___23,levels=c("0","1"))
      data$isolate_identified___24.factor = factor(data$isolate_identified___24,levels=c("0","1"))
      data$isolate_identified___25.factor = factor(data$isolate_identified___25,levels=c("0","1"))
      data$isolate_identified___26.factor = factor(data$isolate_identified___26,levels=c("0","1"))
      data$isolate_identified___27.factor = factor(data$isolate_identified___27,levels=c("0","1"))
      data$isolate_identified___28.factor = factor(data$isolate_identified___28,levels=c("0","1"))
      data$isolate_identified___29.factor = factor(data$isolate_identified___29,levels=c("0","1"))
      data$isolate_identified___30.factor = factor(data$isolate_identified___30,levels=c("0","1"))
      data$isolate_identified___31.factor = factor(data$isolate_identified___31,levels=c("0","1"))
      data$isolate_identified___32.factor = factor(data$isolate_identified___32,levels=c("0","1"))
      data$isolate_identified___33.factor = factor(data$isolate_identified___33,levels=c("0","1"))
      data$isolate_identified___34.factor = factor(data$isolate_identified___34,levels=c("0","1"))
      data$isolate_identified___35.factor = factor(data$isolate_identified___35,levels=c("0","1"))
      data$isolate_identified___36.factor = factor(data$isolate_identified___36,levels=c("0","1"))
      data$isolate_identified___37.factor = factor(data$isolate_identified___37,levels=c("0","1"))
      data$isolate_identified___38.factor = factor(data$isolate_identified___38,levels=c("0","1"))
      data$isolate_identified___39.factor = factor(data$isolate_identified___39,levels=c("0","1"))
      data$isolate_identified___40.factor = factor(data$isolate_identified___40,levels=c("0","1"))
      data$isolate_identified___41.factor = factor(data$isolate_identified___41,levels=c("0","1"))
      data$isolate_identified___42.factor = factor(data$isolate_identified___42,levels=c("0","1"))
      data$isolate_identified___43.factor = factor(data$isolate_identified___43,levels=c("0","1"))
      data$isolate_identified___44.factor = factor(data$isolate_identified___44,levels=c("0","1"))
      data$isolate_identified___45.factor = factor(data$isolate_identified___45,levels=c("0","1"))
      data$isolate_identified___46.factor = factor(data$isolate_identified___46,levels=c("0","1"))
      data$isolate_identified___47.factor = factor(data$isolate_identified___47,levels=c("0","1"))
      data$isolate_identified___48.factor = factor(data$isolate_identified___48,levels=c("0","1"))
      data$isolate_identified___49.factor = factor(data$isolate_identified___49,levels=c("0","1"))
      data$isolate_identified___50.factor = factor(data$isolate_identified___50,levels=c("0","1"))
      data$isolate_identified___51.factor = factor(data$isolate_identified___51,levels=c("0","1"))
      data$isolate_identified___52.factor = factor(data$isolate_identified___52,levels=c("0","1"))
      data$isolate_identified___53.factor = factor(data$isolate_identified___53,levels=c("0","1"))
      data$isolate_identified___54.factor = factor(data$isolate_identified___54,levels=c("0","1"))
      data$isolate_identified___55.factor = factor(data$isolate_identified___55,levels=c("0","1"))
      data$isolate_identified___56.factor = factor(data$isolate_identified___56,levels=c("0","1"))
      data$isolate_identified___57.factor = factor(data$isolate_identified___57,levels=c("0","1"))
      data$isolate_identified___58.factor = factor(data$isolate_identified___58,levels=c("0","1"))
      data$isolate_identified___59.factor = factor(data$isolate_identified___59,levels=c("0","1"))
      data$isolate_identified___60.factor = factor(data$isolate_identified___60,levels=c("0","1"))
      data$isolate_identified___61.factor = factor(data$isolate_identified___61,levels=c("0","1"))
      data$isolate_class.factor = factor(data$isolate_class,levels=c("1","2","3","4"))
      data$micro_lab.factor = factor(data$micro_lab,levels=c("1","2","3"))
      data$pn_result.factor = factor(data$pn_result,levels=c("1","2","3"))
      data$pen_susceptibility___1.factor = factor(data$pen_susceptibility___1,levels=c("0","1"))
      data$pen_susceptibility___2.factor = factor(data$pen_susceptibility___2,levels=c("0","1"))
      data$pen_susceptibility___3.factor = factor(data$pen_susceptibility___3,levels=c("0","1"))
      data$pen_susceptibility___4.factor = factor(data$pen_susceptibility___4,levels=c("0","1"))
      data$pen_susceptibility___5.factor = factor(data$pen_susceptibility___5,levels=c("0","1"))
      data$pen_susceptibility___6.factor = factor(data$pen_susceptibility___6,levels=c("0","1"))
      data$pen_susceptibility___7.factor = factor(data$pen_susceptibility___7,levels=c("0","1"))
      data$septrin_susceptibility___1.factor = factor(data$septrin_susceptibility___1,levels=c("0","1"))
      data$septrin_susceptibility___2.factor = factor(data$septrin_susceptibility___2,levels=c("0","1"))
      data$septrin_susceptibility___3.factor = factor(data$septrin_susceptibility___3,levels=c("0","1"))
      data$septrin_susceptibility___4.factor = factor(data$septrin_susceptibility___4,levels=c("0","1"))
      data$septrin_susceptibility___5.factor = factor(data$septrin_susceptibility___5,levels=c("0","1"))
      data$septrin_susceptibility___6.factor = factor(data$septrin_susceptibility___6,levels=c("0","1"))
      data$septrin_susceptibility___7.factor = factor(data$septrin_susceptibility___7,levels=c("0","1"))
      data$doxy_susceptibility___1.factor = factor(data$doxy_susceptibility___1,levels=c("0","1"))
      data$doxy_susceptibility___2.factor = factor(data$doxy_susceptibility___2,levels=c("0","1"))
      data$doxy_susceptibility___3.factor = factor(data$doxy_susceptibility___3,levels=c("0","1"))
      data$doxy_susceptibility___4.factor = factor(data$doxy_susceptibility___4,levels=c("0","1"))
      data$doxy_susceptibility___5.factor = factor(data$doxy_susceptibility___5,levels=c("0","1"))
      data$doxy_susceptibility___6.factor = factor(data$doxy_susceptibility___6,levels=c("0","1"))
      data$doxy_susceptibility___7.factor = factor(data$doxy_susceptibility___7,levels=c("0","1"))
      data$levoflox_suscept___1.factor = factor(data$levoflox_suscept___1,levels=c("0","1"))
      data$levoflox_suscept___2.factor = factor(data$levoflox_suscept___2,levels=c("0","1"))
      data$levoflox_suscept___3.factor = factor(data$levoflox_suscept___3,levels=c("0","1"))
      data$levoflox_suscept___4.factor = factor(data$levoflox_suscept___4,levels=c("0","1"))
      data$levoflox_suscept___5.factor = factor(data$levoflox_suscept___5,levels=c("0","1"))
      data$levoflox_suscept___6.factor = factor(data$levoflox_suscept___6,levels=c("0","1"))
      data$levoflox_suscept___7.factor = factor(data$levoflox_suscept___7,levels=c("0","1"))
      data$cef_susceptibility___1.factor = factor(data$cef_susceptibility___1,levels=c("0","1"))
      data$cef_susceptibility___2.factor = factor(data$cef_susceptibility___2,levels=c("0","1"))
      data$cef_susceptibility___3.factor = factor(data$cef_susceptibility___3,levels=c("0","1"))
      data$cef_susceptibility___4.factor = factor(data$cef_susceptibility___4,levels=c("0","1"))
      data$cef_susceptibility___5.factor = factor(data$cef_susceptibility___5,levels=c("0","1"))
      data$cef_susceptibility___6.factor = factor(data$cef_susceptibility___6,levels=c("0","1"))
      data$cef_susceptibility___7.factor = factor(data$cef_susceptibility___7,levels=c("0","1"))
      data$bacterialfungal_culture_complete.factor = factor(data$bacterialfungal_culture_complete,levels=c("0","1","2"))
      data$viral_testing_performed.factor = factor(data$viral_testing_performed,levels=c("1","0"))
      data$specimen_type.factor = factor(data$specimen_type,levels=c("1","2","3","4","5"))
      data$virus_isolated.factor = factor(data$virus_isolated,levels=c("1","0"))
      data$specimen_sample_location.factor = factor(data$specimen_sample_location,levels=c("1","2","3","4","5"))
      data$test_type.factor = factor(data$test_type,levels=c("1","2"))
      data$virus_pathogen___1.factor = factor(data$virus_pathogen___1,levels=c("0","1"))
      data$virus_pathogen___2.factor = factor(data$virus_pathogen___2,levels=c("0","1"))
      data$virus_pathogen___3.factor = factor(data$virus_pathogen___3,levels=c("0","1"))
      data$virus_pathogen___4.factor = factor(data$virus_pathogen___4,levels=c("0","1"))
      data$virus_pathogen___5.factor = factor(data$virus_pathogen___5,levels=c("0","1"))
      data$virus_pathogen___6.factor = factor(data$virus_pathogen___6,levels=c("0","1"))
      data$virus_pathogen___7.factor = factor(data$virus_pathogen___7,levels=c("0","1"))
      data$virus_pathogen___8.factor = factor(data$virus_pathogen___8,levels=c("0","1"))
      data$virus_pathogen___9.factor = factor(data$virus_pathogen___9,levels=c("0","1"))
      data$virus_pathogen___10.factor = factor(data$virus_pathogen___10,levels=c("0","1"))
      data$virus_pathogen___11.factor = factor(data$virus_pathogen___11,levels=c("0","1"))
      data$virus_pathogen___12.factor = factor(data$virus_pathogen___12,levels=c("0","1"))
      data$virology_results_complete.factor = factor(data$virology_results_complete,levels=c("0","1","2"))
      data$pneumovax_ppv23.factor = factor(data$pneumovax_ppv23,levels=c("1","2","3"))
      data$flu_vaccine.factor = factor(data$flu_vaccine,levels=c("1","2","3"))
      data$covid19_vax.factor = factor(data$covid19_vax,levels=c("1","2","3"))
      data$vaccination_status_complete.factor = factor(data$vaccination_status_complete,levels=c("0","1","2"))
      data$inpatient_admission.factor = factor(data$inpatient_admission,levels=c("1","0"))
      data$covid_19_diagnosis.factor = factor(data$covid_19_diagnosis,levels=c("1","2","3"))
      data$final_standard_of_care_lrt___1.factor = factor(data$final_standard_of_care_lrt___1,levels=c("0","1"))
      data$final_standard_of_care_lrt___2.factor = factor(data$final_standard_of_care_lrt___2,levels=c("0","1"))
      data$final_standard_of_care_lrt___3.factor = factor(data$final_standard_of_care_lrt___3,levels=c("0","1"))
      data$final_standard_of_care_lrt___4.factor = factor(data$final_standard_of_care_lrt___4,levels=c("0","1"))
      data$final_standard_of_care_lrt___5.factor = factor(data$final_standard_of_care_lrt___5,levels=c("0","1"))
      data$final_standard_of_care_lrt___6.factor = factor(data$final_standard_of_care_lrt___6,levels=c("0","1"))
      data$final_standard_of_care_lrt___7.factor = factor(data$final_standard_of_care_lrt___7,levels=c("0","1"))
      data$final_standard_of_care_lrt___8.factor = factor(data$final_standard_of_care_lrt___8,levels=c("0","1"))
      data$final_standard_of_care_lrt___9.factor = factor(data$final_standard_of_care_lrt___9,levels=c("0","1"))
      data$final_standard_of_care_lrt___10.factor = factor(data$final_standard_of_care_lrt___10,levels=c("0","1"))
      data$dlrtd_outcome_at_30_days.factor = factor(data$dlrtd_outcome_at_30_days,levels=c("1","2","3","4","5","6"))
      data$highest_level_care_require.factor = factor(data$highest_level_care_require,levels=c("1","2","3"))
      data$did_the_patient_receive_ec.factor = factor(data$did_the_patient_receive_ec,levels=c("1","0"))
      data$inotropic_support_required.factor = factor(data$inotropic_support_required,levels=c("1","2","3"))
      data$did_the_patient_have_respi.factor = factor(data$did_the_patient_have_respi,levels=c("1","0"))
      data$ventilatory_support.factor = factor(data$ventilatory_support,levels=c("1","2","3","4","5"))
      data$renal_replacement_therapy.factor = factor(data$renal_replacement_therapy,levels=c("1","0"))
      data$complications___1.factor = factor(data$complications___1,levels=c("0","1"))
      data$complications___2.factor = factor(data$complications___2,levels=c("0","1"))
      data$complications___3.factor = factor(data$complications___3,levels=c("0","1"))
      data$complications___4.factor = factor(data$complications___4,levels=c("0","1"))
      data$complications___5.factor = factor(data$complications___5,levels=c("0","1"))
      data$complications___6.factor = factor(data$complications___6,levels=c("0","1"))
      data$complications___7.factor = factor(data$complications___7,levels=c("0","1"))
      data$complications___8.factor = factor(data$complications___8,levels=c("0","1"))
      data$complications___9.factor = factor(data$complications___9,levels=c("0","1"))
      data$complications___10.factor = factor(data$complications___10,levels=c("0","1"))
      data$complications___11.factor = factor(data$complications___11,levels=c("0","1"))
      data$complications___12.factor = factor(data$complications___12,levels=c("0","1"))
      data$complications___13.factor = factor(data$complications___13,levels=c("0","1"))
      data$complications___14.factor = factor(data$complications___14,levels=c("0","1"))
      data$complications___15.factor = factor(data$complications___15,levels=c("0","1"))
      data$ip_death.factor = factor(data$ip_death,levels=c("1","0"))
      data$outcome_data_complete.factor = factor(data$outcome_data_complete,levels=c("0","1","2"))
      
      levels(data$redcap_repeat_instrument.factor)=c("Radiology Results","Bacterial/Fungal Culture","Virology Results")
      levels(data$covid19.factor)=c("Yes","No")
      levels(data$acute_illness.factor)=c("Yes","No")
      levels(data$evidence_1___1.factor)=c("Unchecked","Checked")
      levels(data$evidence_1___2.factor)=c("Unchecked","Checked")
      levels(data$evidence_1___3.factor)=c("Unchecked","Checked")
      levels(data$lrti_symptoms___1.factor)=c("Unchecked","Checked")
      levels(data$lrti_symptoms___2.factor)=c("Unchecked","Checked")
      levels(data$lrti_symptoms___3.factor)=c("Unchecked","Checked")
      levels(data$lrti_symptoms___4.factor)=c("Unchecked","Checked")
      levels(data$lrti_symptoms___5.factor)=c("Unchecked","Checked")
      levels(data$lrti_symptoms___6.factor)=c("Unchecked","Checked")
      levels(data$lrti_symptoms___7.factor)=c("Unchecked","Checked")
      levels(data$lrti_symptoms___8.factor)=c("Unchecked","Checked")
      levels(data$exclusion_criteria.factor)=c("Yes","No")
      levels(data$previous_enrolled_particip.factor)=c("Yes","No")
      levels(data$lrtd_diagnosis_excluded.factor)=c("Yes","No")
      levels(data$hosp.factor)=c("Southmead","BRI","RUH")
      levels(data$gender.factor)=c("Male","Female")
      levels(data$referral_source.factor)=c("Yes","No")
      levels(data$ethnicity.factor)=c("White British","White Irish","White - Gypsy or Irish Traveller","Any other white background","White and black Caribbean","White and black African","White and Asian","Any other mixed/multiple ethnic background","Indian","Pakistani","Bangladeshi","Chinese","Other Asian","African","Caribbean","Any other black/African/Caribbean background","Arab","Other ethnic group","Unknown ethnic group")
      levels(data$smoking.factor)=c("Current smoker","Ex-smoker","Non-smoker","Unknown")
      levels(data$vaping.factor)=c("Yes","No","Unknown")
      levels(data$bris_cap_consent_obtained.factor)=c("Yes","No")
      levels(data$demographics_and_screening_complete.factor)=c("Incomplete","Unverified","Complete")
      levels(data$resp_disease2___1.factor)=c("Unchecked","Checked")
      levels(data$resp_disease2___2.factor)=c("Unchecked","Checked")
      levels(data$resp_disease2___3.factor)=c("Unchecked","Checked")
      levels(data$resp_disease2___4.factor)=c("Unchecked","Checked")
      levels(data$resp_disease2___5.factor)=c("Unchecked","Checked")
      levels(data$resp_disease2___6.factor)=c("Unchecked","Checked")
      levels(data$chd___1.factor)=c("Unchecked","Checked")
      levels(data$chd___2.factor)=c("Unchecked","Checked")
      levels(data$chd___3.factor)=c("Unchecked","Checked")
      levels(data$chd___4.factor)=c("Unchecked","Checked")
      levels(data$chd___5.factor)=c("Unchecked","Checked")
      levels(data$chd___6.factor)=c("Unchecked","Checked")
      levels(data$ckd.factor)=c("No","Mild","Moderate or Severe CKD")
      levels(data$liver_disease.factor)=c("None","Mild","Moderate or Severe")
      levels(data$diabetes.factor)=c("No","Type 1 DM - no complications","Type 1 DM - complications","Type 2 DM - no complications","Type 2 DM - complications")
      levels(data$dementia___1.factor)=c("Unchecked","Checked")
      levels(data$dementia___2.factor)=c("Unchecked","Checked")
      levels(data$dementia___3.factor)=c("Unchecked","Checked")
      levels(data$dementia___4.factor)=c("Unchecked","Checked")
      levels(data$dementia___5.factor)=c("Unchecked","Checked")
      levels(data$hemiplegia.factor)=c("Yes","No")
      levels(data$pvd.factor)=c("Yes","No")
      levels(data$immsup.factor)=c("Yes","No")
      levels(data$immunodeficiency.factor)=c("Yes","No")
      levels(data$ctd.factor)=c("Yes","No")
      levels(data$hiv___1.factor)=c("Unchecked","Checked")
      levels(data$hiv___2.factor)=c("Unchecked","Checked")
      levels(data$hiv___3.factor)=c("Unchecked","Checked")
      levels(data$cancer.factor)=c("None","Solid Organ Cancer - no mets","Solid Organ Cancer - Metastatic Disease")
      levels(data$haem_malig___1.factor)=c("Unchecked","Checked")
      levels(data$haem_malig___2.factor)=c("Unchecked","Checked")
      levels(data$haem_malig___3.factor)=c("Unchecked","Checked")
      levels(data$transplant.factor)=c("Yes","No")
      levels(data$pregnancy.factor)=c("Not pregnant","First Trimester","Second Trimester","Third Trimester","Pregnant, unsure of trimester","Post-partum")
      levels(data$pud.factor)=c("Yes","No")
      levels(data$drugs___1.factor)=c("Unchecked","Checked")
      levels(data$drugs___2.factor)=c("Unchecked","Checked")
      levels(data$drugs___3.factor)=c("Unchecked","Checked")
      levels(data$drugs___4.factor)=c("Unchecked","Checked")
      levels(data$drugs___5.factor)=c("Unchecked","Checked")
      levels(data$rockall_frailty_score1.factor)=c(" 1 - Very Fit  People who are robust, active, energetic and motivated. These people commonly exercise regularly. They are among the fittest for their age."," 2 - Well  People who have no active disease symptoms but are less fit than category 1. Often, they exercise or are very active occasionally, e.g. seasonally."," 3 - Managing Well People whose medical problems are well controlled, but are not regularly active beyond routine walking.","4 - Vulnerable While not dependent on others for daily help, often symptoms limit activities. A common complaint is being slowed up, and/or being tired during the day.","5 - Mildly Frail  These people often have more evident slowing, and need help in high order IADLs (finances, transportation, heavy housework, medications). Typically, mild frailty progressively impairs shopping and walking outside alone, meal preparation and housework.","6 - Moderately Frail People need help with all outside activities and with keeping house. Inside, they often have problems with stairs and need help with bathing and might need minimal assistance (cuing, standby) with dressing.","7 - Severely Frail Completely dependent for personal care, from whatever cause (physical or cognitive). Even so, they seem stable and not at high risk of dying (within ~ 6 months).","8 - Very Severely Frail  Completely dependent, approaching the end of life. Typically, they could not recover even from a minor illness.","9 - Approaching the end of life This category applies to people with a life expectancy < 6 months, who are not otherwise evidently frail.","NOT RECORDED IN CLERKING/Unassessed")
      levels(data$abx_14d_prior.factor)=c("Yes","No","Unknown")
      levels(data$comorbidities_complete.factor)=c("Incomplete","Unverified","Complete")
      levels(data$temperature.factor)=c("Fever (T>38.5°C)","Hypothermia (T< 35.5°C)","Normal")
      levels(data$ox_on_admission.factor)=c("Yes","No")
      levels(data$crb65_age.factor)=c("Yes","No")
      levels(data$confusion.factor)=c("Yes","No")
      levels(data$resp_rate.factor)=c("Yes","No")
      levels(data$blood_pressure.factor)=c("Yes","No")
      levels(data$fever.factor)=c("Absent","Present","Unknown")
      levels(data$hypothermia.factor)=c("Absent","Present","Unknown")
      levels(data$chills.factor)=c("Absent","Present","Unknown")
      levels(data$rigors.factor)=c("Absent","Present","Unknown")
      levels(data$cough.factor)=c("Absent","Present","Unknown")
      levels(data$wheeze.factor)=c("Absent","Present","Unknown")
      levels(data$sputum.factor)=c("Absent","Present","Unknown")
      levels(data$sob.factor)=c("Absent","Present","Unknown")
      levels(data$pleurisy.factor)=c("Absent","Present","Unknown")
      levels(data$tachypnoea.factor)=c("Absent","Present","Unknown")
      levels(data$maliase.factor)=c("Absent","Present","Unknown")
      levels(data$oe_cap.factor)=c("Absent","Present","Unknown")
      levels(data$oe_lrtd.factor)=c("Absent","Present","Unknown")
      levels(data$nhya.factor)=c("Unable to determine/unknown","Class I No symptoms","Class II Mild symptoms (eg mild SOB)","Class III Marked limitation in activity due to symptoms (eg walking 20-100m). Comfortable only at rest","Class IV Severe limitations, symptoms at rest, bedbound")
      levels(data$admission_data_complete.factor)=c("Incomplete","Unverified","Complete")
      levels(data$patient_blood_group.factor)=c("A+","A-","B+","B-","AB+","AB-","O+","O-","Unknown")
      levels(data$admission_blood_results_complete.factor)=c("Incomplete","Unverified","Complete")
      levels(data$radio_test.factor)=c("CXR","CT scan","US thorax","Other")
      levels(data$radiology_result.factor)=c("Normal","Consistent with Pneumonia","Consistent with heart failure","Consistent with pleural effusion","Consistent with COVID-19","Other abnormal finding")
      levels(data$radiology_results_complete.factor)=c("Incomplete","Unverified","Complete")
      levels(data$micro_test_done.factor)=c("Yes","No")
      levels(data$micro_test.factor)=c("Blood culture","Sputum","Pleural fluid","Bronchoalveolar lavage","Tracheal Aspirate","Urinary antigen (pn/lg)")
      levels(data$micro_isolates.factor)=c("Yes","No","Unknown")
      levels(data$isolate_identified___1.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___2.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___3.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___4.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___5.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___6.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___7.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___8.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___9.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___10.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___11.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___12.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___13.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___14.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___15.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___16.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___17.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___18.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___19.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___20.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___21.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___22.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___23.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___24.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___25.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___26.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___27.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___28.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___29.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___30.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___31.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___32.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___33.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___34.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___35.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___36.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___37.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___38.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___39.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___40.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___41.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___42.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___43.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___44.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___45.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___46.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___47.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___48.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___49.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___50.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___51.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___52.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___53.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___54.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___55.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___56.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___57.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___58.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___59.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___60.factor)=c("Unchecked","Checked")
      levels(data$isolate_identified___61.factor)=c("Unchecked","Checked")
      levels(data$isolate_class.factor)=c("Pathogen","Contaminant","Normal Flora","Coloniser")
      levels(data$micro_lab.factor)=c("Yes","No","Unknown")
      levels(data$pn_result.factor)=c("Not tested/no result","Non-typable","Serotype obtained")
      levels(data$pen_susceptibility___1.factor)=c("Unchecked","Checked")
      levels(data$pen_susceptibility___2.factor)=c("Unchecked","Checked")
      levels(data$pen_susceptibility___3.factor)=c("Unchecked","Checked")
      levels(data$pen_susceptibility___4.factor)=c("Unchecked","Checked")
      levels(data$pen_susceptibility___5.factor)=c("Unchecked","Checked")
      levels(data$pen_susceptibility___6.factor)=c("Unchecked","Checked")
      levels(data$pen_susceptibility___7.factor)=c("Unchecked","Checked")
      levels(data$septrin_susceptibility___1.factor)=c("Unchecked","Checked")
      levels(data$septrin_susceptibility___2.factor)=c("Unchecked","Checked")
      levels(data$septrin_susceptibility___3.factor)=c("Unchecked","Checked")
      levels(data$septrin_susceptibility___4.factor)=c("Unchecked","Checked")
      levels(data$septrin_susceptibility___5.factor)=c("Unchecked","Checked")
      levels(data$septrin_susceptibility___6.factor)=c("Unchecked","Checked")
      levels(data$septrin_susceptibility___7.factor)=c("Unchecked","Checked")
      levels(data$doxy_susceptibility___1.factor)=c("Unchecked","Checked")
      levels(data$doxy_susceptibility___2.factor)=c("Unchecked","Checked")
      levels(data$doxy_susceptibility___3.factor)=c("Unchecked","Checked")
      levels(data$doxy_susceptibility___4.factor)=c("Unchecked","Checked")
      levels(data$doxy_susceptibility___5.factor)=c("Unchecked","Checked")
      levels(data$doxy_susceptibility___6.factor)=c("Unchecked","Checked")
      levels(data$doxy_susceptibility___7.factor)=c("Unchecked","Checked")
      levels(data$levoflox_suscept___1.factor)=c("Unchecked","Checked")
      levels(data$levoflox_suscept___2.factor)=c("Unchecked","Checked")
      levels(data$levoflox_suscept___3.factor)=c("Unchecked","Checked")
      levels(data$levoflox_suscept___4.factor)=c("Unchecked","Checked")
      levels(data$levoflox_suscept___5.factor)=c("Unchecked","Checked")
      levels(data$levoflox_suscept___6.factor)=c("Unchecked","Checked")
      levels(data$levoflox_suscept___7.factor)=c("Unchecked","Checked")
      levels(data$cef_susceptibility___1.factor)=c("Unchecked","Checked")
      levels(data$cef_susceptibility___2.factor)=c("Unchecked","Checked")
      levels(data$cef_susceptibility___3.factor)=c("Unchecked","Checked")
      levels(data$cef_susceptibility___4.factor)=c("Unchecked","Checked")
      levels(data$cef_susceptibility___5.factor)=c("Unchecked","Checked")
      levels(data$cef_susceptibility___6.factor)=c("Unchecked","Checked")
      levels(data$cef_susceptibility___7.factor)=c("Unchecked","Checked")
      levels(data$bacterialfungal_culture_complete.factor)=c("Incomplete","Unverified","Complete")
      levels(data$viral_testing_performed.factor)=c("Yes","No")
      levels(data$specimen_type.factor)=c("Sputum","Saliva","Mucus","Pleural Fluid","Swabbed material")
      levels(data$virus_isolated.factor)=c("Yes","No")
      levels(data$specimen_sample_location.factor)=c("Nasal cavity","Oropharynx","Nasopharynx","Other Upper Respiratory System","Lower Respiratory System")
      levels(data$test_type.factor)=c("PCR viral panel","Viral culture")
      levels(data$virus_pathogen___1.factor)=c("Unchecked","Checked")
      levels(data$virus_pathogen___2.factor)=c("Unchecked","Checked")
      levels(data$virus_pathogen___3.factor)=c("Unchecked","Checked")
      levels(data$virus_pathogen___4.factor)=c("Unchecked","Checked")
      levels(data$virus_pathogen___5.factor)=c("Unchecked","Checked")
      levels(data$virus_pathogen___6.factor)=c("Unchecked","Checked")
      levels(data$virus_pathogen___7.factor)=c("Unchecked","Checked")
      levels(data$virus_pathogen___8.factor)=c("Unchecked","Checked")
      levels(data$virus_pathogen___9.factor)=c("Unchecked","Checked")
      levels(data$virus_pathogen___10.factor)=c("Unchecked","Checked")
      levels(data$virus_pathogen___11.factor)=c("Unchecked","Checked")
      levels(data$virus_pathogen___12.factor)=c("Unchecked","Checked")
      levels(data$virology_results_complete.factor)=c("Incomplete","Unverified","Complete")
      levels(data$pneumovax_ppv23.factor)=c("Received PneumoVax","Not received","Unknown")
      levels(data$flu_vaccine.factor)=c("Received Seasonal Flu vaccine","Not received","Unknown")
      levels(data$covid19_vax.factor)=c("Received COVID19 vaccine","Not received","Unknown")
      levels(data$vaccination_status_complete.factor)=c("Incomplete","Unverified","Complete")
      levels(data$inpatient_admission.factor)=c("Yes","No")
      levels(data$covid_19_diagnosis.factor)=c("COVID-19 - laboratory confirmed","COVID-19 - clinical diagnosis","COVID-19 excluded")
      levels(data$final_standard_of_care_lrt___1.factor)=c("Unchecked","Checked")
      levels(data$final_standard_of_care_lrt___2.factor)=c("Unchecked","Checked")
      levels(data$final_standard_of_care_lrt___3.factor)=c("Unchecked","Checked")
      levels(data$final_standard_of_care_lrt___4.factor)=c("Unchecked","Checked")
      levels(data$final_standard_of_care_lrt___5.factor)=c("Unchecked","Checked")
      levels(data$final_standard_of_care_lrt___6.factor)=c("Unchecked","Checked")
      levels(data$final_standard_of_care_lrt___7.factor)=c("Unchecked","Checked")
      levels(data$final_standard_of_care_lrt___8.factor)=c("Unchecked","Checked")
      levels(data$final_standard_of_care_lrt___9.factor)=c("Unchecked","Checked")
      levels(data$final_standard_of_care_lrt___10.factor)=c("Unchecked","Checked")
      levels(data$dlrtd_outcome_at_30_days.factor)=c("Deceased","Recovered","Recovered, with sequelae","Ongoing recovery","Not recovered","Unknown")
      levels(data$highest_level_care_require.factor)=c("General Medical Ward","Intensive Care/HDU","CCU or high-care area")
      levels(data$did_the_patient_receive_ec.factor)=c("Yes","No")
      levels(data$inotropic_support_required.factor)=c("Yes","No","Unknown")
      levels(data$did_the_patient_have_respi.factor)=c("Yes","No")
      levels(data$ventilatory_support.factor)=c("Intubation","BiPAP","CPAP","High-Flow Nasal Cannulae","None")
      levels(data$renal_replacement_therapy.factor)=c("Yes","No")
      levels(data$complications___1.factor)=c("Unchecked","Checked")
      levels(data$complications___2.factor)=c("Unchecked","Checked")
      levels(data$complications___3.factor)=c("Unchecked","Checked")
      levels(data$complications___4.factor)=c("Unchecked","Checked")
      levels(data$complications___5.factor)=c("Unchecked","Checked")
      levels(data$complications___6.factor)=c("Unchecked","Checked")
      levels(data$complications___7.factor)=c("Unchecked","Checked")
      levels(data$complications___8.factor)=c("Unchecked","Checked")
      levels(data$complications___9.factor)=c("Unchecked","Checked")
      levels(data$complications___10.factor)=c("Unchecked","Checked")
      levels(data$complications___11.factor)=c("Unchecked","Checked")
      levels(data$complications___12.factor)=c("Unchecked","Checked")
      levels(data$complications___13.factor)=c("Unchecked","Checked")
      levels(data$complications___14.factor)=c("Unchecked","Checked")
      levels(data$complications___15.factor)=c("Unchecked","Checked")
      levels(data$ip_death.factor)=c("Yes","No")
      levels(data$outcome_data_complete.factor)=c("Incomplete","Unverified","Complete")
      
      # data = data %>% dplyr::rename(
      #   `Record ID Number`=record_number,
      #   `Repeat Instrument`=redcap_repeat_instrument,
      #   `Repeat Instance`=redcap_repeat_instance,
      #   `Enrollment Date`=enrollment_date,
      #   `Admission Date`=admission_date,
      #   `Date of Birth`=dob_1,
      #   `Age at admission`=age_at_admission,
      #   `Does this patient have suspected or proven COVID-19 infection? (current or previous infection)`=covid19,
      #   `Is this an acute illness?  (Ie, Under 21 days)`=acute_illness,
      #   `Evidence of acute respiratory illness or HF   (choice=Clinical or radiological diagnosis)`=evidence_1___1,
      #   `Evidence of acute respiratory illness or HF   (choice=New/worsening symptoms or findings)`=evidence_1___2,
      #   `Evidence of acute respiratory illness or HF   (choice=None)`=evidence_1___3,
      #   `Symptoms   Requires ≥ 2 to be included if no clinical/radiological diagnosis  (choice=Fever)`=lrti_symptoms___1,
      #   `Symptoms   Requires ≥ 2 to be included if no clinical/radiological diagnosis  (choice=Cough)`=lrti_symptoms___2,
      #   `Symptoms   Requires ≥ 2 to be included if no clinical/radiological diagnosis  (choice=Sputum)`=lrti_symptoms___3,
      #   `Symptoms   Requires ≥ 2 to be included if no clinical/radiological diagnosis  (choice=Dyspnea (SOB))`=lrti_symptoms___4,
      #   `Symptoms   Requires ≥ 2 to be included if no clinical/radiological diagnosis  (choice=Tachypnea (RR≥20/min))`=lrti_symptoms___5,
      #   `Symptoms   Requires ≥ 2 to be included if no clinical/radiological diagnosis  (choice=Pleurisy)`=lrti_symptoms___6,
      #   `Symptoms   Requires ≥ 2 to be included if no clinical/radiological diagnosis  (choice=Auscultatory findings (crackles, bronchial breathing, dullness on percussion))`=lrti_symptoms___7,
      #   `Symptoms   Requires ≥ 2 to be included if no clinical/radiological diagnosis  (choice=Radiological findings)`=lrti_symptoms___8,
      #   `Signs/Symptoms develop over 48 hours into admission`=exclusion_criteria,
      #   `Previous enrolled participants readmitted ≤ 7 days after discharge `=previous_enrolled_particip,
      #   `LRTD diagnosis excluded`=lrtd_diagnosis_excluded,
      #   `Hospital`=hosp,
      #   `Gender`=gender,
      #   `Care home resident`=referral_source,
      #   `Ethnicity`=ethnicity,
      #   `Smoking status`=smoking,
      #   `Vaping (in last 30 days)`=vaping,
      #   `Days of symptoms before admission`=days_of_symptoms_before_ad,
      #   `Written AVON-CAP Consent Obtained`=bris_cap_consent_obtained,
      #   `Date of BRISTOL-CAP Consent`=date_of_briscap_consent,
      #   `Complete?`=demographics_and_screening_complete,
      #   `Respiratory Disease (choice=None)`=resp_disease2___1,
      #   `Respiratory Disease (choice=COPD (Chronic Obstructive Pulmonary Disease/Emphysema))`=resp_disease2___2,
      #   `Respiratory Disease (choice=Asthma)`=resp_disease2___3,
      #   `Respiratory Disease (choice=Bronchiectasis)`=resp_disease2___4,
      #   `Respiratory Disease (choice=Pulmonary Fibrosis/Interstitial Lung Disease)`=resp_disease2___5,
      #   `Respiratory Disease (choice=Other)`=resp_disease2___6,
      #   `Other Respiratory Disease`=other_respiratory_disease,
      #   `Chronic Heart Disease (choice=None)`=chd___1,
      #   `Chronic Heart Disease (choice=Hypertension)`=chd___2,
      #   `Chronic Heart Disease (choice=Atrial Fibrillation)`=chd___3,
      #   `Chronic Heart Disease (choice=Ischaemic heart disease)`=chd___4,
      #   `Chronic Heart Disease (choice=Heart failure/CCF)`=chd___5,
      #   `Chronic Heart Disease (choice=Other)`=chd___6,
      #   `Other Chronic Heart Disease`=other_respiratory_disease_2,
      #   `Chronic Kidney Disease (CKD) Mod-Severe =eGFR< 30, Cr>265 umol/L, dialysis, transplantation, uremic syndrome`=ckd,
      #   `Liver Disease Mild = cirrhosis without portal HTN, chronic hepatitis  Mod-Severe = cirrhosis with portal HTN +/- variceal bleeding`=liver_disease,
      #   `Diabetes`=diabetes,
      #   `Cognitive Impairment/Dementia (choice=None)`=dementia___1,
      #   `Cognitive Impairment/Dementia (choice=Dementia)`=dementia___2,
      #   `Cognitive Impairment/Dementia (choice=Cognitive Impairment)`=dementia___3,
      #   `Cognitive Impairment/Dementia (choice=CVA (stroke))`=dementia___4,
      #   `Cognitive Impairment/Dementia (choice=TIA (mini-stroke))`=dementia___5,
      #   `Hemiplegiahemiplegia or paraplegia `=hemiplegia,
      #   `Peripheral Vascular Disease Intermittent claudication, periph. arterial bypass for insufficiency, gangrene, acute arterial insufficiency, untreated aneurysm (>=6cm) `=pvd,
      #   `Immunosuppressive Medication(includes oral steroids, biologics, chemotherapy)`=immsup,
      #   `Immunodeficiency(eg SCID, hypogammaglobulinaemia, splenectomy)`=immunodeficiency,
      #   `Connective Tissue Disease (SLE, polymyositis, mixed Connective Tissue Disease, polymyalgia rheumatica, moderate to severe Rheumatoid Arthritis)`=ctd,
      #   `HIV status (choice=Negative (no HIV), or not tested)`=hiv___1,
      #   `HIV status (choice=HIV)`=hiv___2,
      #   `HIV status (choice=AIDS)`=hiv___3,
      #   `Solid Organ Cancer/Malignancy Initially treated in the last 5 years exclude non-melanomatous skin cancers and in situ cervical carcinoma`=cancer,
      #   `Haematological Malignancy Leukaemia = CML, CLL, AML, ALL, Polycythaemia Vera Lymphoma = NHL, Hodgkins, Waldenström, multiple myeloma  (choice=None)`=haem_malig___1,
      #   `Haematological Malignancy Leukaemia = CML, CLL, AML, ALL, Polycythaemia Vera Lymphoma = NHL, Hodgkins, Waldenström, multiple myeloma  (choice=Leukaemia)`=haem_malig___2,
      #   `Haematological Malignancy Leukaemia = CML, CLL, AML, ALL, Polycythaemia Vera Lymphoma = NHL, Hodgkins, Waldenström, multiple myeloma  (choice=Lymphoma)`=haem_malig___3,
      #   `Organ Transplantation`=transplant,
      #   `Pregnancy/Post partum`=pregnancy,
      #   `Gastric/Duodenal Ulcer Disease Patients who have required treatment for PUD `=pud,
      #   `Drug Misuse (choice=None)`=drugs___1,
      #   `Drug Misuse (choice=Alcohol excess)`=drugs___2,
      #   `Drug Misuse (choice=IVDU (Intravenous Drug Usage))`=drugs___3,
      #   `Drug Misuse (choice=Marijuana)`=drugs___4,
      #   `Drug Misuse (choice=Other smoked drugs)`=drugs___5,
      #   `Rockwood Frailty Score`=rockall_frailty_score1,
      #   `Used Antibiotics in 14 days prior to Hospitalisation`=abx_14d_prior,
      #   `Antibiotic Used`=antibiotic_used,
      #   `Comorbidities complete?`=comorbidities_complete,
      #   `Date of tests`=date_of_tests,
      #   `Heart Rate`=hr,
      #   `Systolic BP (mmHg)`=systolic_bp,
      #   `Diastolic BP (mmHg)`=diastolic_bp,
      #   `Temperature`=temperature,
      #   `Respiratory Rate`=rr,
      #   `Oxygen Saturation`=pulse_ox,
      #   `FiO2 RA = 21%`=fio2,
      #   `Did the patient require oxygen supplementation < 4 hours of admission?`=ox_on_admission,
      #   `NEWS-2 Score`=news2,
      #   `Age for reference (≥65)`=age_over65,
      #   `Age ≥65`=crb65_age,
      #   `Confusion (AMTS ≤8)`=confusion,
      #   `Respiratory Rate (≥30)`=resp_rate,
      #   `BP (systolic < 90mmHg or diastolic ≤60 mmHg)`=blood_pressure,
      #   `CRB65 Score`=autocalc_65score,
      #   `Fever`=fever,
      #   `Hypothermia`=hypothermia,
      #   `Chills`=chills,
      #   `Rigors`=rigors,
      #   `New or Increased Cough`=cough,
      #   `New or Increased Wheeze`=wheeze,
      #   `New or Increased Sputum Production`=sputum,
      #   `New of Increased Shortness of Breath`=sob,
      #   `New or Increased Pleuritic chest pain`=pleurisy,
      #   `New or Increased Tachypnea`=tachypnoea,
      #   `New or Increased Malaise`=maliase,
      #   `Abnormal Auscultatory Findings Suggestive of Pneumonia`=oe_cap,
      #   `Abnormal Auscultatory Findings Suggestive of other LRTD`=oe_lrtd,
      #   `NYHA - Heart Failure  `=nhya,
      #   `Admission data complete?`=admission_data_complete,
      #   `Glucose`=glucose,
      #   `Albumin`=albumin_result,
      #   `White cell count `=wcc_result,
      #   `Haemoglobin`=hb,
      #   `Neutrophils`=pmn_result,
      #   `Lymphocytes`=lo_result,
      #   `CRP`=crp_result,
      #   `Sodium (Na)`=na_result,
      #   `Urea (Ur)`=ur_result,
      #   `eGFR`=egfr_result,
      #   `NT-proBNP`=nt_probnp,
      #   `Ferritin`=vitamin_d_during_admission,
      #   `D-dimer`=d_dimer,
      #   `Patient Blood Group`=patient_blood_group,
      #   `Admission blood results complete?`=admission_blood_results_complete,
      #   `Date of Radiological Investigation`=radiology_date,
      #   `Type of Radiological Test`=radio_test,
      #   `If other, please specify`=radiology_othertest,
      #   `Radiology Result`=radiology_result,
      #   `If other, please specify`=radiology_other_result,
      #   `Radiology results complete?`=radiology_results_complete,
      #   `Bacterial/Fungal Investigations performed`=micro_test_done,
      #   `Date of Microbiology Test`=micro_test_date,
      #   `Microbiology Test Type`=micro_test,
      #   `Isolates Identified?`=micro_isolates,
      #   `Isolate Name (choice=Aspergillus)`=isolate_identified___1,
      #   `Isolate Name (choice=Streptococcus Agalactiae)`=isolate_identified___2,
      #   `Isolate Name (choice=Candida)`=isolate_identified___3,
      #   `Isolate Name (choice=Achromobacter Xylosoxidans)`=isolate_identified___4,
      #   `Isolate Name (choice=Bacteroides)`=isolate_identified___5,
      #   `Isolate Name (choice=Bacteroides fragilis)`=isolate_identified___6,
      #   `Isolate Name (choice=Bacteroides ovatus)`=isolate_identified___7,
      #   `Isolate Name (choice=Bacteroides uniformis)`=isolate_identified___8,
      #   `Isolate Name (choice=Burholderia cepacia)`=isolate_identified___9,
      #   `Isolate Name (choice=Citrobacter freundii complex)`=isolate_identified___10,
      #   `Isolate Name (choice=Citrobacter koseri)`=isolate_identified___11,
      #   `Isolate Name (choice=Clostridium perfringens)`=isolate_identified___12,
      #   `Isolate Name (choice=Eggerthella lenta)`=isolate_identified___13,
      #   `Isolate Name (choice=Enterobacter aerogenes)`=isolate_identified___14,
      #   `Isolate Name (choice=Enterobacter cloacae)`=isolate_identified___15,
      #   `Isolate Name (choice=Enterococcus faecalis)`=isolate_identified___16,
      #   `Isolate Name (choice=Enterococcus faecium)`=isolate_identified___17,
      #   `Isolate Name (choice=Escherichia coli)`=isolate_identified___18,
      #   `Isolate Name (choice=Haemophilus influenzae)`=isolate_identified___19,
      #   `Isolate Name (choice=Haemophilus parainfluenzae)`=isolate_identified___20,
      #   `Isolate Name (choice=Klebsiella oxytoca)`=isolate_identified___21,
      #   `Isolate Name (choice=Klebsiella pneumoniae)`=isolate_identified___22,
      #   `Isolate Name (choice=Morganella morganii)`=isolate_identified___23,
      #   `Isolate Name (choice=Peptostreptococcus anaerobius)`=isolate_identified___24,
      #   `Isolate Name (choice=Proteus mirabilis)`=isolate_identified___25,
      #   `Isolate Name (choice=Pseudomonas aeruginosa)`=isolate_identified___26,
      #   `Isolate Name (choice=Serratia marcescens)`=isolate_identified___27,
      #   `Isolate Name (choice=Staphylcoccus aureus)`=isolate_identified___28,
      #   `Isolate Name (choice=Streptococcus anginosus)`=isolate_identified___29,
      #   `Isolate Name (choice=Streptococcus pneumoniae)`=isolate_identified___30,
      #   `Isolate Name (choice=Streptococcus salivarius group)`=isolate_identified___31,
      #   `Isolate Name (choice=Stenotrophomonas maltophilia)`=isolate_identified___32,
      #   `Isolate Name (choice=Acinetobacter)`=isolate_identified___33,
      #   `Isolate Name (choice=Aspergillus niger)`=isolate_identified___34,
      #   `Isolate Name (choice=Bacillus)`=isolate_identified___35,
      #   `Isolate Name (choice=Candida albicans)`=isolate_identified___36,
      #   `Isolate Name (choice=Candida glabrata)`=isolate_identified___37,
      #   `Isolate Name (choice=Candida tropicalis)`=isolate_identified___38,
      #   `Isolate Name (choice=Coagulase negative staphylcoccus)`=isolate_identified___39,
      #   `Isolate Name (choice=Corynebacterium)`=isolate_identified___40,
      #   `Isolate Name (choice=Enterococcus)`=isolate_identified___41,
      #   `Isolate Name (choice=Gram positive coccus)`=isolate_identified___42,
      #   `Isolate Name (choice=Klebsiella)`=isolate_identified___43,
      #   `Isolate Name (choice=Legionella pneumoniae)`=isolate_identified___44,
      #   `Isolate Name (choice=MRSA)`=isolate_identified___45,
      #   `Isolate Name (choice=MSSA)`=isolate_identified___46,
      #   `Isolate Name (choice=Moraxella catarrhalis)`=isolate_identified___47,
      #   `Isolate Name (choice=Mycobacterium tuberculosis)`=isolate_identified___48,
      #   `Isolate Name (choice=Pneumocytis jirovecii)`=isolate_identified___49,
      #   `Isolate Name (choice=Pseudomonas)`=isolate_identified___50,
      #   `Isolate Name (choice=Pseudomonas fluorescens)`=isolate_identified___51,
      #   `Isolate Name (choice=Staphylcoccus)`=isolate_identified___52,
      #   `Isolate Name (choice=Staphylococcus capitis)`=isolate_identified___53,
      #   `Isolate Name (choice=Staphylococcus epidermidis)`=isolate_identified___54,
      #   `Isolate Name (choice=Staphylococcus hominis)`=isolate_identified___55,
      #   `Isolate Name (choice=Streptococcus)`=isolate_identified___56,
      #   `Isolate Name (choice=Streptococcus beta-hemolytic)`=isolate_identified___57,
      #   `Isolate Name (choice=Streptococcus pyogenes)`=isolate_identified___58,
      #   `Isolate Name (choice=Streptococcus viridans)`=isolate_identified___59,
      #   `Isolate Name (choice=Yeast)`=isolate_identified___60,
      #   `Isolate Name (choice=Other)`=isolate_identified___61,
      #   `If other, please specify`=micro_other,
      #   `Isolate Classification`=isolate_class,
      #   `Sent to Central Lab`=micro_lab,
      #   `Pneumococcal result`=pn_result,
      #   `Serotype`=pn_st,
      #   `Penicillin Susceptibility (choice=Unknown)`=pen_susceptibility___1,
      #   `Penicillin Susceptibility (choice=Not susceptible)`=pen_susceptibility___2,
      #   `Penicillin Susceptibility (choice=Not applicable)`=pen_susceptibility___3,
      #   `Penicillin Susceptibility (choice=Resistant)`=pen_susceptibility___4,
      #   `Penicillin Susceptibility (choice=Intermediate)`=pen_susceptibility___5,
      #   `Penicillin Susceptibility (choice=Susceptible)`=pen_susceptibility___6,
      #   `Penicillin Susceptibility (choice=Susceptible (dose dependent))`=pen_susceptibility___7,
      #   `Co-trimoxazole (Trimethorpim/Sulfamethoxazole) Susceptibility (choice=Unknown)`=septrin_susceptibility___1,
      #   `Co-trimoxazole (Trimethorpim/Sulfamethoxazole) Susceptibility (choice=Not susceptible)`=septrin_susceptibility___2,
      #   `Co-trimoxazole (Trimethorpim/Sulfamethoxazole) Susceptibility (choice=Not applicable)`=septrin_susceptibility___3,
      #   `Co-trimoxazole (Trimethorpim/Sulfamethoxazole) Susceptibility (choice=Resistant)`=septrin_susceptibility___4,
      #   `Co-trimoxazole (Trimethorpim/Sulfamethoxazole) Susceptibility (choice=Intermediate)`=septrin_susceptibility___5,
      #   `Co-trimoxazole (Trimethorpim/Sulfamethoxazole) Susceptibility (choice=Susceptible)`=septrin_susceptibility___6,
      #   `Co-trimoxazole (Trimethorpim/Sulfamethoxazole) Susceptibility (choice=Susceptible (dose dependent))`=septrin_susceptibility___7,
      #   `Doxycycline Susceptibility (choice=Unknown)`=doxy_susceptibility___1,
      #   `Doxycycline Susceptibility (choice=Not susceptible)`=doxy_susceptibility___2,
      #   `Doxycycline Susceptibility (choice=Not applicable)`=doxy_susceptibility___3,
      #   `Doxycycline Susceptibility (choice=Resistant)`=doxy_susceptibility___4,
      #   `Doxycycline Susceptibility (choice=Intermediate)`=doxy_susceptibility___5,
      #   `Doxycycline Susceptibility (choice=Susceptible)`=doxy_susceptibility___6,
      #   `Doxycycline Susceptibility (choice=Susceptible (dose dependent))`=doxy_susceptibility___7,
      #   `Levofloxacin Susceptibility (choice=Unknown)`=levoflox_suscept___1,
      #   `Levofloxacin Susceptibility (choice=Not susceptible)`=levoflox_suscept___2,
      #   `Levofloxacin Susceptibility (choice=Not applicable)`=levoflox_suscept___3,
      #   `Levofloxacin Susceptibility (choice=Resistant)`=levoflox_suscept___4,
      #   `Levofloxacin Susceptibility (choice=Intermediate)`=levoflox_suscept___5,
      #   `Levofloxacin Susceptibility (choice=Susceptible)`=levoflox_suscept___6,
      #   `Levofloxacin Susceptibility (choice=Susceptible (dose dependent))`=levoflox_suscept___7,
      #   `Ceftriaxone Susceptibility (choice=Unknown)`=cef_susceptibility___1,
      #   `Ceftriaxone Susceptibility (choice=Not susceptible)`=cef_susceptibility___2,
      #   `Ceftriaxone Susceptibility (choice=Not applicable)`=cef_susceptibility___3,
      #   `Ceftriaxone Susceptibility (choice=Resistant)`=cef_susceptibility___4,
      #   `Ceftriaxone Susceptibility (choice=Intermediate)`=cef_susceptibility___5,
      #   `Ceftriaxone Susceptibility (choice=Susceptible)`=cef_susceptibility___6,
      #   `Ceftriaxone Susceptibility (choice=Susceptible (dose dependent))`=cef_susceptibility___7,
      #   `Bacterial fungal culture complete?`=bacterialfungal_culture_complete,
      #   `Viral testing performed`=viral_testing_performed,
      #   `Virology Date of Assessment`=virology_date_of_assessmen,
      #   `Specimen Type`=specimen_type,
      #   `Virus Isolated`=virus_isolated,
      #   `Specimen Sample Location`=specimen_sample_location,
      #   `Test Type`=test_type,
      #   `Virus Pathogen (choice=Influenza A)`=virus_pathogen___1,
      #   `Virus Pathogen (choice=Influenza B)`=virus_pathogen___2,
      #   `Virus Pathogen (choice=RSV)`=virus_pathogen___3,
      #   `Virus Pathogen (choice=Adenovirus)`=virus_pathogen___4,
      #   `Virus Pathogen (choice=Human metapneumovirus)`=virus_pathogen___5,
      #   `Virus Pathogen (choice=Rhinovirus)`=virus_pathogen___6,
      #   `Virus Pathogen (choice=Parainfluenza Type 1)`=virus_pathogen___7,
      #   `Virus Pathogen (choice=Parainfluenza Type 2)`=virus_pathogen___8,
      #   `Virus Pathogen (choice=Parainfluenza Type 3)`=virus_pathogen___9,
      #   `Virus Pathogen (choice=Parainfluenza Type 4)`=virus_pathogen___10,
      #   `Virus Pathogen (choice=COVID-19)`=virus_pathogen___11,
      #   `Virus Pathogen (choice=Other)`=virus_pathogen___12,
      #   `If other, please specify:`=viral_other,
      #   `Virology results complete?`=virology_results_complete,
      #   `PneumoVax (PPV23)`=pneumovax_ppv23,
      #   `Date of PneumoVax Vaccination`=ppv23_date,
      #   `Seasonal Influenza Vaccination (in last 12 months)`=flu_vaccine,
      #   `Date of Last Flu Vaccination`=flu_date,
      #   `COVID-19 Vaccination`=covid19_vax,
      #   `Date of COVID19 Vaccination`=covidvax_date,
      #   `Brand of COVID19 vaccination`=brand_of_covid19_vaccinati,
      #   `Vaccination complete?`=vaccination_status_complete,
      #   `Inpatient admission`=inpatient_admission,
      #   `COVID-19 diagnosis`=covid_19_diagnosis,
      #   `Final Standard-of-Care LRTD-related diagnosis (choice=CAP - radiologically confirmed)`=final_standard_of_care_lrt___1,
      #   `Final Standard-of-Care LRTD-related diagnosis (choice=CAP - clinically confirmed (but not on radiology))`=final_standard_of_care_lrt___2,
      #   `Final Standard-of-Care LRTD-related diagnosis (choice=CAP - no radiology performed)`=final_standard_of_care_lrt___3,
      #   `Final Standard-of-Care LRTD-related diagnosis (choice=Acute bronchitis)`=final_standard_of_care_lrt___4,
      #   `Final Standard-of-Care LRTD-related diagnosis (choice=Exacerbation of COPD)`=final_standard_of_care_lrt___5,
      #   `Final Standard-of-Care LRTD-related diagnosis (choice=Empyema/lung abscess)`=final_standard_of_care_lrt___6,
      #   `Final Standard-of-Care LRTD-related diagnosis (choice=LRTI - not further specified)`=final_standard_of_care_lrt___7,
      #   `Final Standard-of-Care LRTD-related diagnosis (choice=Other LRTI specified)`=final_standard_of_care_lrt___8,
      #   `Final Standard-of-Care LRTD-related diagnosis (choice=Congestive heart failure)`=final_standard_of_care_lrt___9,
      #   `Final Standard-of-Care LRTD-related diagnosis (choice=Non-infectious process)`=final_standard_of_care_lrt___10,
      #   `LRTD Outcome (at 30 days)`=dlrtd_outcome_at_30_days,
      #   `Highest Level Care Required`=highest_level_care_require,
      #   `Days in ICU`=days_in_icu,
      #   `Did the patient receive ECMO?`=did_the_patient_receive_ec,
      #   `Inotropic Support required`=inotropic_support_required,
      #   `Did the patient have Respiratory Failure during admission? Sats < 90% with ≥6L/min Oxygen OR Resp rate > 30/min`=did_the_patient_have_respi,
      #   `Ventilatory Support`=ventilatory_support,
      #   `Number of Days of Ventilatory Support`=number_of_days_of_ventilat,
      #   `Renal replacement therapy required (haemofiltration and/or new dialysis)`=renal_replacement_therapy,
      #   `Complications   (choice=Acute renal failure)`=complications___1,
      #   `Complications   (choice=Liver dysfunction)`=complications___2,
      #   `Complications   (choice=Hospital acquired infection)`=complications___3,
      #   `Complications   (choice=ARDS (acute respiratory distress syndrome))`=complications___4,
      #   `Complications   (choice=NSTEMI (non-ST elevation MI))`=complications___5,
      #   `Complications   (choice=STEMI (ST elevation MI))`=complications___6,
      #   `Complications   (choice=New episode of atrial fibrillation)`=complications___7,
      #   `Complications   (choice=Stroke or brain haemorrhage)`=complications___8,
      #   `Complications   (choice=DVT (deep vein thrombus))`=complications___9,
      #   `Complications   (choice=PE (pulmonary embolus))`=complications___10,
      #   `Complications   (choice=New or worsening congestive heart failure)`=complications___11,
      #   `Complications   (choice=Fall in hospital)`=complications___12,
      #   `Complications   (choice=Reduced mobility)`=complications___13,
      #   `Complications   (choice=Increasing care requirement on discharge)`=complications___14,
      #   `Complications   (choice=None)`=complications___15,
      #   `Did patient die in hospital?`=ip_death,
      #   `Discharge Date (or date of death)`=discharge_date,
      #   `Hospital Length of Stay`=hospital_length_of_stay,
      #   `Outcome data complete?`=outcome_data_complete
      # )
      
      return(data)
    },
    
    #' @description Load the CHESS dataset from a path
    #' 
    #' @param path - a path to the chess csv file
    #' @return raw CHESS data set
    getCHESS = function() {
      path = self$getLatest(self$filter$chess)
      out = readr::read_csv(path.expand(path), col_types = readr::cols(.default = col_character()))
      for (col in colnames(out)) {
        if (stringr::str_detect(col, "date")) {
          out = out %>% mutate(!!col := as.Date(stringr::str_extract(out[[col]], "[0-9]{4}-[0-9]{2}-[0-9]{2}"), format = "%Y-%m-%d"))
        } else {
          out = out %>% mutate(!!col := type.convert(out[[col]], as.is=TRUE))
        }
      }
      return(out)
    },
    
    #' @description Load Chess summary file
    #' 
    #' @param path - path to the ff100 file
    
    #' @return raw FF100 data set
    
    getCHESSSummary = function() {
      path = self$getLatest(self$filter$chessSummary)
      readr::read_csv(path, col_types = readr::cols(
        DateRange = readr::col_date("%d-%m-%Y"),
        DateOfAdmission = readr::col_date("%d-%m-%Y"),
        YearofAdmission = readr::col_integer(),
        TrustName = readr::col_character(),
        Code = readr::col_character(),
        .default = readr::col_integer()))
      chessSummary = chessSummary %>% dplyr::select(-X67) %>% tidyr::pivot_longer(cols = c(everything(),-all_of(c("DateRange","DateOfAdmission","YearofAdmission","TrustName","Code","Total"))), names_to = "variable", values_to = "count")
      chessSummary = chessSummary %>% dplyr::filter(Code != "Total")
      tmp = chessSummary %>% dplyr::mutate(
        toAge = str_replace(variable,"^.*_([^_]+)$","\\1"),
        fromAge = str_replace(variable,"^.*_([^_]+)_[^_]+$","\\1"),
        variable = str_replace(variable,"^(.*)_[^_]+_[^_]+$","\\1")
      )
      tmp = tmp %>% dplyr::mutate(fromAge = ifelse(fromAge=="GreaterThanEqual", toAge, fromAge))
      tmp = tmp %>% dplyr::mutate(fromAge = ifelse(fromAge=="LessThan", 0, fromAge))
      chessSummary = tmp %>% dplyr::mutate(toAge = ifelse(fromAge==toAge, 120, toAge))
      return(chessSummary)
    },
      
    #### Get processed SPIM data ----
    
    #' @description Load Bristol survival subset
    #' 
    #' @param path - path to the ff100 file
    
    #' @return Bristol survival subset
    
    bristolSurvivalSubset = function(bristolDf) {
      bristolDf %>% 
        filter(is.na(redcap_repeat_instrument)) %>% 
        select(
          record_number,
          enrollment_date, 
          admission_date, 
          age = age_at_admission, 
          sex = gender.factor, 
          ip_death.factor, discharge_date, 
          outcome_data_complete.factor, 
          days_of_symptoms_before_ad)
    },
    
    getOneOneOneIncidence = function(dateFrom=Sys.Date()-28, ...) {
      self$getDaily("SPIM-111-BREAKDOWN", params=list(dateFrom), ..., orElse = function(...) covidTimeseriesFormat({
        tmp3 = self$getOneOneOneLineList(dateFrom,...)
        tmp4 = tmp3 %>% 
          dplyr::filter(!is.na(code)) %>%
          dplyr::group_by(code,codeType,name,date,ageCat, gender,subgroup) %>% 
          dplyr::summarise(value = n()) %>% 
          dplyr::mutate(source="111 line list",statistic = "triage", type="incidence") %>%
          self$fillAbsentAllRegions() %>% self$completeAllRegions()
        return(tmp4)
      })) 
    },
    
    #' @description Load incidence from line list
    #' 
    #' @param ageBreaks - a list of ages which form the cut points for breaking continuous ages into ranges (or NULL for a single age category)
    
    #' @return a covidTimeseriesFormat dataframe
    
    getLineListIncidence = function(ageBreaks = NULL, specimenOrReport="specimen", ...) {
      self$getDaily("LINE-LIST-INCIDENCE", params=list(ageBreaks, specimenOrReport), ..., orElse = function (...) covidTimeseriesFormat({
        tmp = self$getLineList(...) %>% 
          dplyr::mutate(ageCat = age %>% self$cutByAge(ageBreaks), gender=self$normaliseGender(sex), subgroup=pillar)
        if(specimenOrReport == "report")
          tmp = tmp %>% dplyr::mutate(date = as.Date(lab_report_date))
        else
          tmp = tmp %>% dplyr::mutate(date = as.Date(specimen_date))
        
        england = tmp %>% dplyr::mutate(code = "E92000001", codeType= "CTRY", name="England") %>% 
          dplyr::group_by(code,codeType,name,date, ageCat, gender,subgroup) %>% 
          dplyr::summarise(value = n())
        
        selectByRegion = function(df, code, codeType, name) {
          code = ensym(code)
          name = ensym(name)
          # check column exists
          if(!(as_label(code) %in% colnames(df))) return(tibble())
          df = df %>% dplyr::mutate(code = !!code, codeType=codeType, name=!!name) %>% 
            dplyr::mutate(
              code = ifelse(is.na(code),"E99999999",code),
              name = ifelse(is.na(code),"Unknown (England)",name)
            ) %>%
            dplyr::group_by( code,codeType,name,date, ageCat, gender,subgroup) %>% 
            dplyr::summarise(value = n()) 
          return(df)
        }
        
        out = bind_rows(
          england,
          tmp %>% selectByRegion(NHSER_code, "NHSER19CDH", NHSER_name) %>% 
            dplyr::inner_join(
              self$codes$getMappings() %>% dplyr::filter(fromCodeType=="NHSER19CDH" & toCodeType=="NHSER"), 
              by=c("code"="fromCode")
            ) %>%
            dplyr::ungroup() %>%
            dplyr::select(-code,-codeType, -fromCodeType,-rel,-weight) %>%
            dplyr::rename(code = toCode, codeType=toCodeType),
          tmp %>% selectByRegion(PHEC_code, "PHEC", PHEC_name),
          tmp %>% selectByRegion(UTLA_code, "UA", UTLA_name),
          tmp %>% selectByRegion(LTLA_code, "LAD", LTLA_name),
          tmp %>% selectByRegion(LSOA_code, "LSOA", LSOA_name)
        )
        
        out = out %>% 
          dplyr::mutate(source="line list",statistic = "case", type="incidence") %>%
          self$fillAbsent() %>%
          #tidyr::complete(tidyr::nesting(code,codeType,name,source,statistic,type),subgroup,gender,ageCat,date = as.Date(min(date):max(date),"1970-01-01"), fill=list(value=0)) %>%
          dplyr::ungroup()
        return(out %>% self$fillAbsent() %>% self$fixDatesAndNames(4) %>% self$complete())
      }))
    },
    
    #' @description Load deaths data from linelist - does not preserve ethnicity
    #' @param ageBreaks - a list of ages which form the cut points for breaking continuous ages into ranges (or NULL for a single age category)
    #' @return a covidTimeseriesFormat dataframe
    getDeathsLineListIncidence = function(ageBreaks = NULL, deathOrReport="death", cutoff=60, ...) {
      self$getDaily(id = "DEATHS-LINE-LIST-INCIDENCE", params=list(ageBreaks, deathOrReport,cutoff), ..., orElse = function (...) covidTimeseriesFormat({
        tmp = self$getDeathsLineList(...) %>% 
          dplyr::filter(is.na(specimen_date) | as.integer(dod-specimen_date)<cutoff) %>%
          dplyr::mutate(
            ageCat = age %>% self$cutByAge(ageBreaks), 
            subgroup=ifelse(is.na(ons_pod),"unknown",ons_pod), 
            note=ethnicity_final)
        if(deathOrReport == "death") 
          tmp = tmp %>% dplyr::mutate(date = as.Date(dod))
        else
          tmp = tmp %>% dplyr::mutate(date = 
            as.Date(pmin(NHSdeathreportdate, DBSdeathreportdate, HPTdeathreportdate, ONS_death_registration_date, na.rm = TRUE),"1970-01-01")
          ) %>% dplyr::filter(!is.na(date))
        
        england = tmp %>% 
          dplyr::mutate(code = "E92000001", codeType= "CTRY", name="England") %>% 
          dplyr::group_by(code,codeType,name,date, ageCat, gender,subgroup) %>% 
          dplyr::summarise(value = n())
        
        selectByRegion = function(df, code, codeType, name) {
          code = ensym(code)
          name = ensym(name)
          # check column exists
          if(!(as_label(code) %in% colnames(df))) return(tibble())
          df = df %>% dplyr::mutate(code = !!code, codeType=codeType, name=!!name) %>% 
            dplyr::mutate(
              code = ifelse(is.na(code),"E99999999",code),
              name = ifelse(is.na(code),"Unknown (England)",name)
            ) %>%
            dplyr::group_by( code,codeType,name,date, ageCat, gender,subgroup) %>% 
            dplyr::summarise(value = n()) 
          return(df)
        }
        out = bind_rows(
          england,
          tmp %>% selectByRegion(code = nhser_ecode, codeType="NHSER", name=nhser_name),
          tmp %>% selectByRegion(code = phec_code, codeType="PHEC", name=phec_name),
          tmp %>% selectByRegion(code = utla_code, codeType="UA", name=utla_name),
          tmp %>% selectByRegion(code = ltla_code, codeType="LAD", name=ltla_name),
          tmp %>% selectByRegion(code = lsoa_code, codeType="LSOA", name=lsoa_name),
        )
        
        
        
        out = out %>% 
          dplyr::mutate(source="deaths line list",statistic = "death", type="incidence") %>%
          self$fillAbsent() %>%
          self$fixDatesAndNames(4) %>% 
          self$complete() %>%
          dplyr::ungroup()
        return(out)
      }))
    },
    
    #' @description Load seroprevalence data from linelist
    #' @param ageBreaks - a list of ages which form the cut points for breaking continuous ages into ranges (or NULL for a single age category)
    #' @return a covidTimeseriesFormat dataframe
    getSeroprevalenceTestIncidence = function(ageBreaks = NULL, ...) {
      warning("Need to update this for newer seroprevalence data")
      self$getDaily("SEROPREVALENCE-INCIDENCE", params=list(ageBreaks), ..., orElse = function (...) covidTimeseriesFormat({
        data2 = self$getSeroprevalence(...)
        
        # data2 %>% group_by(EuroImm_outcome) %>% summarise(low_cutoff = min(EuroImm_Units,na.rm=TRUE),high_cutoff = max(EuroImm_Units,na.rm=TRUE))
        # cut offs are: Negative > Borderline > Positive; 0.8 -> 1.1 
        # data2 %>% group_by(RBD_outcome) %>% summarise(low_cutoff = min(RBD_Units,na.rm=TRUE),high_cutoff = max(RBD_Units,na.rm=TRUE))
        # cut offs are: Negative > Borderline > Positive; 3.3 -> 4.9 
        
        data3 = data2 %>%
          dplyr::mutate(ageCat = age %>% self$cutByAge(ageBreaks), gender=self$normaliseGender(Sex), date = SampleDate)
        if ("EuroImm_outcome" %in% colnames(data3)) data3 %>% rename(EuroImmun_outcome = EuroImm_outcome)
        if ("EuroImm_Units" %in% colnames(data3)) data3 %>% rename(EuroImmun_units = EuroImm_Units)
        if ("RBD_Units" %in% colnames(data3)) data3 %>% rename(RBD_units = RBD_Units)
        
        data3 = data3 %>% mutate(
          subgroup = case_when(
            (!is.na(RBD_outcome) & RBD_outcome=="Failed QC") | (!is.na(EuroImmun_outcome) & EuroImmun_outcome == "Insufficient") ~ "no result", # if either insufficient then insufficient
            (is.na(RBD_units) & is.na(EuroImmun_units)) ~ "no result", # if both NA then some problem
            (!is.na(RBD_units) & RBD_units > 3.3 & RBD_units <= 4.9) | (!is.na(EuroImmun_units) & EuroImmun_units > 0.8 & EuroImmun_units < 1.1) ~ "borderline", # if either is borderline its borderline
            (is.na(RBD_units) | RBD_units > 4.9) & (is.na(EuroImmun_units) | EuroImmun_units > 1.1) ~ "positive", # if both either positive or one NA and one positive its positive
            (is.na(RBD_units) | RBD_units <= 3.3) & (is.na(EuroImmun_units) | EuroImmun_units <= 0.8 ) ~ "negative", # if both either negative or one NA and one negative its positive
            (RBD_units <= 3.3 & EuroImmun_units > 1.1 | RBD_units > 4.9 & EuroImmun_units <= 0.8) ~ "no result", # if one positive and one negative there is a disagreement
            TRUE ~ "no result"
          ),
          Region= ifelse(is.na(Region),sample_region,Region)
        ) %>%
          self$codes$findCodesByName(nameVar = Region, codeTypes = c("CTRY","NHSER")) %>%
          self$codes$findNamesByCode(codeVar = ccg, outputNameVar = ccgName, outputCodeTypeVar = ccgCodeType, codeTypes = "CCG")
        
        
        out = bind_rows(
          data3 %>% dplyr::mutate(code = ccg, codeType = ccgCodeType, name = ccgName) %>% dplyr::filter(!is.na(code)) %>% dplyr::group_by( code,codeType,name,date, ageCat, gender,subgroup) %>% dplyr::summarise(value = n()), 
          data3 %>% dplyr::mutate(code = code, codeType = codeType, name = Region) %>% dplyr::filter(!is.na(code)) %>% dplyr::group_by( code,codeType,name,date, ageCat, gender,subgroup) %>% dplyr::summarise(value = n())
        ) %>% 
          dplyr::mutate(statistic = "serology", type = "incidence", source="SPIM seroprevalence") %>%
          self$complete() %>%
          #tidyr::complete(tidy::nesting(code,codeType,name,source,statistic,type),subgroup,gender,ageCat,date = as.Date(min(date):max(date),"1970-01-01"), fill=list(value=0)) %>%
          dplyr::ungroup()
        
        return(out %>% self$fillAbsent() %>% self$fixDatesAndNames(0) %>% self$complete())
      }))
    },
    
    #' @description Load 111 data
    #' 
    
    #' @return a covidTimeseriesFormat dataframe
    
    getOneOneOne = function(...) {
      path = self$getLatest(self$filter$oneOneOne)
      self$getDaily("SPIM-111", ..., orElse = function (...) covidTimeseriesFormat({
        
        oneoneone <- readxl::read_excel(path.expand(path), sheet = "Extracted Data", col_types = "text")
        
        # make zeros explicit
        
        # for(outcome in colnames(oneoneone)[colnames(oneoneone) %>% stringr::str_starts("111-Outcome")]) {
        #   outcome = as.symbol(outcome)
        #   oneoneone = oneoneone %>% mutate(!!outcome := ifelse(!is.na(`111-COVID-TOTAL`) & is.na(!!outcome),0,!!outcome))
        # }
        # 
        # for(outcome in colnames(oneoneone)[colnames(oneoneone) %>% stringr::str_starts("999-Outcome")]) {
        #   outcome = as.symbol(outcome)
        #   oneoneone = oneoneone %>% mutate(!!outcome := ifelse(!is.na(`999-COVID-TOTAL`) & is.na(!!outcome),0,!!outcome))
        # }
        # 
        # for(outcome in colnames(oneoneone)[colnames(oneoneone) %>% stringr::str_starts("111-ONLINE") & colnames(oneoneone) != "111-ONLINE-Total Result"]) {
        #   outcome = as.symbol(outcome)
        #   oneoneone = oneoneone %>% mutate(!!outcome := ifelse(!is.na(`111-ONLINE-Total Result`) & is.na(!!outcome),0,!!outcome))
        # }
        
        ts111 = oneoneone %>% 
          dplyr::mutate(Geography = ifelse(Geography=="England: Unknown", "Unknown (England)", Geography)) %>% #TODO fix this ugly hack.
          self$codes$findCodesByName(nameVar = Geography,codeTypes = c("CTRY","NHSER","PSEUDO")) %>%
          dplyr::select(-ReportLevel) %>% 
          dplyr::rename(name=Geography) %>%
          dplyr::mutate(date = as.Date(paste0(Year,"-",Month,"-",Day),"%Y-%m-%d")) %>% 
          dplyr::select(-DateVal,-Day,-Month,-Year) %>% 
          tidyr::pivot_longer(cols = c(-date,-code,-name,-codeType), names_to = "category", values_to = "value") %>% 
          dplyr::mutate(
            value = suppressWarnings(as.numeric(value)),
            source = case_when(
              category %like% "111-ONLINE%" ~ "online",
              category %like% "111-Outcome%" ~ "111",
              category %like% "999-Outcome%" ~ "999",
              category %like% "All-111-Number_of_calls_where_person_triaged" ~ "111",
              TRUE ~ "other 111" # as.character(NA)
            ),
            subgroup = case_when(
              category %like% "%Clinical Assessment service 1 hour%" ~ "urgent clinical review",
              category %like% "%Clinical Assessment service 2 hour%" ~ "urgent clinical review",
              category %like% "%Clinical Assessment%" ~ "clinical review",
              category %like% "%Emergency Ambulance%" ~ "emergency ambulance",
              (category %like% "%Self Care%" | category %like% "%isolate%") ~ "self care",
              TRUE ~ "other" #as.character(NA)
            ),
            statistic = case_when(
              category %like% "111-ONLINE%" ~ "triage",
              category %like% "111-Outcome%" ~ "triage",
              category %like% "999-Outcome%" ~ "triage",
              TRUE ~ "information seeking"
            ),
            gender = NA_character_,
            ageCat = NA_character_,
            type = case_when(
              category %like% "All-111-Number_of_calls_where_person_triaged" ~ "background",
              TRUE ~ "incidence"
            )
          ) %>%
          dplyr::mutate(subgroup = factor(subgroup,levels=c("self care", "clinical review", "urgent clinical review", "emergency ambulance", "other"), ordered = TRUE))
        #browser()
        ts111 = ts111 %>% 
          dplyr::filter(!is.na(value)) %>%
          dplyr::rename(note = category) %>%
          dplyr::group_by(code,codeType,name,source,subgroup,statistic,gender,ageCat,type,date) %>% 
          dplyr::summarise(
            value = sum(value), 
            note = paste0(note, collapse = "; "))
        
        return(ts111 %>% self$fillAbsent() %>% self$fixDatesAndNames(0) %>% self$complete())
      }))
    },
    
    getFourNationsCases = function(...) {
      path = self$getLatest(self$filter$fourNationsCases)
      self$getDaily("SPIM-4-NATIONS", ..., orElse = function(...)  covidTimeseriesFormat({
        tmp = readxl::read_excel(path, sheet = "Extracted Data", col_types = "text", na = c("n/a",""))
        tmp2 = tmp %>% 
          tidyr::pivot_longer(
            cols=c(-DateVal,-Day,-Month,-Year,-Geography),
            names_to = "variable",
            values_to = "value"
          ) %>% dplyr::mutate(
            date = suppressWarnings(as.Date(DateVal)),
            value = suppressWarnings(as.numeric(value))
          ) %>% dplyr::select(-Day,-Month,-Year) %>%
          dplyr::mutate(Geography = ifelse(Geography %in% c("England: Unknown","England: Other"), "Unknown (England)", Geography)) %>% #TODO fix this ugly hack.
          dplyr::rename(name= Geography) %>% 
          self$codes$findCodesByName(codeTypes = c("CTRY","PSEUDO")) %>%
          dplyr::mutate(
            statistic = "case",
            type = case_when(
              stringr::str_detect(variable,"umula") ~ "cumulative",
              TRUE ~ "incidence"
            ),
            source = "casedata allnations",
            subgroup = variable
          )
        tmp3 = tmp2 %>% dplyr::select(-DateVal,-name.original, -variable) %>% mutate(ageCat=NA,gender=NA)
        tmp4 = tmp3 %>% filter(!is.na(value)) %>% self$fillAbsentByRegion() %>% self$fixDatesAndNames(4) %>% self$complete()
        return(tmp4)
        
        # CHESS_LL_lab_date_cases_P1
        # CHESS_LL_specimen_date_cases_P1
        # CHESS_LL_lab_date_cases_P2
        # CHESS_LL_specimen_date_cases_P2
        # RCGP_Pos_cases
        # RCGP_Neg_cases
        # Admitted Patients with Lab Confirmed COVID19
        # Dashboard_daily_confirmed - wales
        # Dashboard_cumulative_confirmed - wales
        # Positives_Spec_Date - scotland
        # Positives_Cumulative_Spec_Date - scotland
        # SitRep_Daily_Positive_tests - n ireland
        # SitRep_Cumulative_Positive_tests - n ireland
        
      }))
    },
    
    
    #' @description Load the SPI-M aggregated data spreadsheet
    #' @return a covidTimeseriesFormat dataframe
    # TODO: fix Couldn't match the following names: England: Unknown, England: Other, Golden Jubilee National Hospital, Velindre University NHS Trust
    getSPIMextract = function(...) {
      path = self$getLatest(self$filter$trust)
      self$getDaily("SPIM-TRUST", ..., orElse = function (...) covidTimeseriesFormat({
        tmp = readxl::read_excel(path, sheet = "Extracted Data", col_types = "text", na = c("n/a",""))
        tmp2 = tmp %>% 
          tidyr::pivot_longer(
            cols=c(-DateVal,-Day,-Month,-Year,-ReportLevel,-Geography,-TrustCode,-TrustName),
            names_to = "variable",
            values_to = "value"
          ) %>% 
          dplyr::filter(!is.na(value)) %>% dplyr::mutate(
            variable = variable %>% stringr::str_replace("acute1","acuteOne")
          ) %>% 
          dplyr::mutate(
            ageCat = stringr::str_extract(variable,"(<|>|Under )?[0-9]?[0-9]-?[0-9]?[0-9]?\\+?( age| year)?$") %>% stringr::str_trim(),
            source = stringr::str_remove(variable,"(<|>|Under )?[0-9]?[0-9]-?[0-9]?[0-9]?\\+?( age| year)?$")
          ) %>% 
          dplyr::mutate(
            ageCat = ifelse(stringr::str_detect(variable,"unknown age"),"unknown",ageCat %>% stringr::str_remove("age|year") %>% stringr::str_trim()),
            gender = self$normaliseGender(variable %>% stringr::str_extract("male|female")),
            source = source %>% stringr::str_remove("unknown age") %>% stringr::str_remove_all("males?|females?") %>% stringr::str_remove_all("[^a-zA-Z]+$") %>% stringr::str_to_lower()
          ) 
        #TODO: fix >84 in ageCat instead of 85+
        tmp3 = tmp2 %>% dplyr::mutate(
          date = suppressWarnings(as.Date(DateVal)),
          value = suppressWarnings(as.numeric(value))
        ) %>% dplyr::select(-Day,-Month,-Year)
        
        tmp4 = tmp3 %>% dplyr::mutate(
          type = case_when(
            stringr::str_detect(source,"cum") ~ "cumulative",
            stringr::str_detect(source,"total") ~ "cumulative",
            stringr::str_detect(source,"inc") ~ "incidence",
            stringr::str_detect(source,"prev") ~ "prevalence",
            stringr::str_detect(source,"weekly") ~ "incidence",
            stringr::str_detect(source,"admissions") ~ "incidence",
            stringr::str_detect(source,"daily") ~ "incidence",
            stringr::str_detect(source,"test") ~ "incidence",
            stringr::str_detect(source,"case") ~ "incidence",
            TRUE ~ as.character(NA)
          ),
          statistic = case_when(
            stringr::str_detect(source,"eath") ~ "death",
            stringr::str_detect(source,"icu") ~ "icu admission",
            stringr::str_detect(source,"osp") ~ "hospital admission",
            stringr::str_detect(source,"test") ~ "test",
            stringr::str_detect(source,"case") ~ "case",
            stringr::str_detect(source,"carehome") ~ "case",
            TRUE ~ as.character(NA)
          ),
          subgroup=NA_character_,
          
        )
        
        # scotland weekly NRS has age breakdowm, and gender breakdown which causes duplication issues....
        # here we exclude them...
        tmp4 = tmp4 %>% filter(!(source == "nrs_weeklydeath" & (!is.na(ageCat) | !is.na(gender))))
        
        browser(expr=self$debug)
        
        # Trusts
        tmp5 = tmp4 %>% 
          dplyr::filter(!is.na(TrustCode)) %>%
          self$codes$findNamesByCode(TrustCode,outputNameVar = name) %>%
          dplyr::mutate(name= ifelse(is.na(name), "Unknown NHS trust", name)) %>% 
          dplyr::select(-TrustName, -Geography,-ReportLevel,-DateVal) %>%
          dplyr::rename(note=variable,code = TrustCode)
        
        
        tmp6 = tmp4 %>% 
          dplyr::filter(is.na(TrustCode)) %>%
          dplyr::mutate(Geography = ifelse(Geography %in% c("England: Unknown","England: Other"), "Unknown (England)", Geography)) %>% #TODO fix this ugly hack.
          dplyr::mutate(name= Geography) %>% 
          dplyr::select(-TrustName) %>%
          self$codes$findCodesByName(codeTypes = c("LHB","HB","NHSER","CTRY","PSEUDO")) %>%
          dplyr::select(-name.original, -TrustCode,-Geography,-ReportLevel,-DateVal) %>%
          dplyr::rename(note=variable) %>%
          dplyr::filter(!is.na(code)) # 2 missing hospital trusts - Velindre and Golden jubilee
        #browser()
        tmp7 = dplyr::bind_rows(tmp5,tmp6) %>% self$fillAbsentByRegion() %>% self$fixDatesAndNames(4) %>% self$complete()
        
        return(tmp7)
      })) 
    },
    
    #' @description Load the SPI-M and public data
    #' @return a covidTimeseriesFormat dataframe
    getTheSPIMFireHose = function(...) {
      self$getDaily("SPIM-FIRE-HOSE", ..., orElse = function (...) covidTimeseriesFormat({
        bind_rows(
          self$datasets$getTheFireHose(),
          self$getOneOneOne(),
          self$getSPIMextract(),
          self$getLineListIncidence(...),
          self$getDeathsLineListIncidence(...),
          self$getSeroprevalenceTestIncidence(...)
        )
      }))
    }
))


#### SPIM specific function ----

#' reformats an Rt data set to SPI-M template
#' 
#' @param df - the dataframe with Rt estimates
#' @param geographyExpr - the derivation of the geographic name
#' @param modelName - the model
#' @param modelTypeExpr: ModelType - Multiple, Deaths, Cases, Survey, Emergency, Pillar Two Testing
#' @param dateVar - the column with dates
#' @param groupName - the group submitting the estimate
#' @return a tibble of the formatted estimates
#' @export
convertRtToSPIM = function(df, geographyExpr, modelExpr, modelTypeExpr, dateVar = "date", groupName = "Exeter", version="0.1") {
  dateVar = ensym(dateVar)
  geographyExpr = enexpr(geographyExpr)
  modelExpr = enexpr(modelExpr)
  modelTypeExpr = enexpr(modelTypeExpr)
  return(
    df %>% mutate(
      `Group`=groupName,
      `Model`=!!modelExpr,
      `ModelType`=!!modelTypeExpr,
      `Version`=version,
      `Creation Day` = Sys.Date() %>% format("%d") %>% as.integer(),
      `Creation Month` = Sys.Date() %>% format("%m") %>% as.integer(),
      `Creation Year` = Sys.Date() %>% format("%Y") %>% as.integer(),
      `Day of Value` = !!dateVar %>% format("%d") %>% as.integer(),
      `Month of Value` = !!dateVar %>% format("%m") %>% as.integer(),
      `Year of Value` = !!dateVar %>% format("%Y") %>% as.integer(),
      `Geography` = !!geographyExpr,
      `ValueType` = "R",
      `Value` = `Mean(R)`,# %>% round(2),
      `Quantile 0.05` = `Quantile.0.05(R)`,# %>% round(2),
      `Quantile 0.1` = NA,
      `Quantile 0.15` = NA,	
      `Quantile 0.2` = NA,	
      `Quantile 0.25` = `Quantile.0.25(R)`,# %>% round(2),
      `Quantile 0.3` = NA,
      `Quantile 0.35` = NA,	
      `Quantile 0.4` = NA,	
      `Quantile 0.45` = NA,
      `Quantile 0.5` = `Median(R)`,# %>% round(2),
      `Quantile 0.55` = NA,
      `Quantile 0.6` = NA,	
      `Quantile 0.65` = NA,	
      `Quantile 0.7` = NA,	
      `Quantile 0.75` = `Quantile.0.75(R)`,# %>% round(2),
      `Quantile 0.8` = NA,	
      `Quantile 0.85` = NA,	
      `Quantile 0.9` = NA,	
      `Quantile 0.95` = `Quantile.0.95(R)`,# %>% round(2)
    ) %>% select(
      `Group`,`Model`,`ModelType`,`Version`,`Creation Day`,`Creation Month`,
      `Creation Year`,`Day of Value`,`Month of Value`,`Year of Value`,
      `Geography`,`ValueType`,`Value`,`Quantile 0.05`,`Quantile 0.1`,
      `Quantile 0.15`,`Quantile 0.2`,`Quantile 0.25`,`Quantile 0.3`,`Quantile 0.35`,	
      `Quantile 0.4`,`Quantile 0.45`,`Quantile 0.5`,`Quantile 0.55`,
      `Quantile 0.6`,`Quantile 0.65`,`Quantile 0.7`,`Quantile 0.75`,
      `Quantile 0.8`,`Quantile 0.85`,`Quantile 0.9`,`Quantile 0.95`
    ))
}

#' reformats an Rt data set to SPI-M template
#' 
#' @param df - the dataframe with Rt estimates
#' @param geographyExpr - the derivation of the geographic name
#' @param modelName - the model
#' @param dateVar - the column with dates
#' @param groupName - the group submitting the estimate

#' @return a tibble of the formatted estimates
#' @export
convertGrowthRateToSPIM = function(df, geographyExpr, modelExpr, modelTypeExpr, growthVar = "Growth.windowed.value", growthSEVar = "Growth.windowed.SE.value",  dateVar = "date", groupName = "Exeter", version="0.1") {
  dateVar = ensym(dateVar)
  growthVar = ensym(growthVar)
  growthSEVar = ensym(growthSEVar)
  geographyExpr = enexpr(geographyExpr)
  modelExpr = enexpr(modelExpr)
  modelTypeExpr = enexpr(modelTypeExpr)
  df %>% mutate(
    `Group`=groupName,
    `Model`=!!modelExpr,
    `ModelType`=!!modelTypeExpr,
    `Version`=version,
    `Creation Day` = Sys.Date() %>% format("%d") %>% as.integer(),
    `Creation Month` = Sys.Date() %>% format("%m") %>% as.integer(),
    `Creation Year` = Sys.Date() %>% format("%Y") %>% as.integer(),
    `Day of Value` = !!dateVar %>% format("%d") %>% as.integer(),
    `Month of Value` = !!dateVar %>% format("%m") %>% as.integer(),
    `Year of Value` = !!dateVar %>% format("%Y") %>% as.integer(),
    `Geography` = !!geographyExpr,
    `ValueType` = "growth_rate",
    `Value` = !!growthVar,# %>% round(2),
    `Quantile 0.05` = qnorm(0.05, !!growthVar, !!growthSEVar),# %>% round(2),
    `Quantile 0.1` = NA,
    `Quantile 0.15` = NA,	
    `Quantile 0.2` = NA,	
    `Quantile 0.25` = qnorm(0.25, !!growthVar, !!growthSEVar),# %>% round(2),
    `Quantile 0.3` = NA,	
    `Quantile 0.35` = NA,	
    `Quantile 0.4` = NA,	
    `Quantile 0.45` = NA,
    `Quantile 0.5` = qnorm(0.5, !!growthVar, !!growthSEVar),# %>% round(2),
    `Quantile 0.55` = NA,
    `Quantile 0.6` = NA,	
    `Quantile 0.65` = NA,	
    `Quantile 0.7` = NA,	
    `Quantile 0.75` = qnorm(0.75, !!growthVar, !!growthSEVar),# %>% round(2),
    `Quantile 0.8` = NA,	
    `Quantile 0.85` = NA,	
    `Quantile 0.9` = NA,	
    `Quantile 0.95` = qnorm(0.95, !!growthVar, !!growthSEVar),# %>% round(2)
  ) %>% select(
    `Group`,`Model`,`ModelType`,`Version`,`Creation Day`,`Creation Month`,
    `Creation Year`,`Day of Value`,`Month of Value`,`Year of Value`,
    `Geography`,`ValueType`,`Value`,`Quantile 0.05`,`Quantile 0.1`,
    `Quantile 0.15`,`Quantile 0.2`,`Quantile 0.25`,`Quantile 0.3`,`Quantile 0.35`,	
    `Quantile 0.4`,`Quantile 0.45`,`Quantile 0.5`,`Quantile 0.55`,
    `Quantile 0.6`,`Quantile 0.65`,`Quantile 0.7`,`Quantile 0.75`,
    `Quantile 0.8`,`Quantile 0.85`,`Quantile 0.9`,`Quantile 0.95`
  )
}

#' reformats an Rt data set to SPI-M template
#' 
#' @param df - the dataframe with Rt estimates
#' @param geographyExpr - the derivation of the geographic name
#' @param modelName - the model
#' @param dateVar - the column with dates
#' @param groupName - the group submitting the estimate

#' @return a tibble of the formatted estimates
#' @export
convertDoublingTimeToSPIM = function(df, geographyExpr, modelExpr, modelTypeExpr, growthVar = "Growth.windowed.value", growthSEVar = "Growth.windowed.SE.value", dateVar = "date", groupName = "Exeter", version="0.1") {
  dateVar = ensym(dateVar)
  growthVar = ensym(growthVar)
  growthSEVar = ensym(growthSEVar)
  geographyExpr = enexpr(geographyExpr)
  modelExpr = enexpr(modelExpr)
  modelTypeExpr = enexpr(modelTypeExpr)
  df %>% mutate(
    `Group`=groupName,
    `Model`=!!modelExpr,
    `ModelType`=!!modelTypeExpr,
    `Version`=version,
    `Creation Day` = Sys.Date() %>% format("%d") %>% as.integer(),
    `Creation Month` = Sys.Date() %>% format("%m") %>% as.integer(),
    `Creation Year` = Sys.Date() %>% format("%Y") %>% as.integer(),
    `Day of Value` = !!dateVar %>% format("%d") %>% as.integer(),
    `Month of Value` = !!dateVar %>% format("%m") %>% as.integer(),
    `Year of Value` = !!dateVar %>% format("%Y") %>% as.integer(),
    `Geography` = !!geographyExpr,
    `ValueType` = "doubling_time",
    `Value` = log(2)/qnorm(0.5, !!growthVar, !!growthSEVar),
    `Quantile 0.05` = log(2)/qnorm(0.95, !!growthVar, !!growthSEVar),# %>% round(2),
    `Quantile 0.1` = NA,
    `Quantile 0.15` = NA,	
    `Quantile 0.2` = NA,	
    `Quantile 0.25` = log(2)/qnorm(0.75, !!growthVar, !!growthSEVar),# %>% round(2),
    `Quantile 0.3` = NA,	
    `Quantile 0.35` = NA,	
    `Quantile 0.4` = NA,	
    `Quantile 0.45` = NA,
    `Quantile 0.5` = log(2)/qnorm(0.5, !!growthVar, !!growthSEVar),# %>% round(2),
    `Quantile 0.55` = NA,
    `Quantile 0.6` = NA,	
    `Quantile 0.65` = NA,	
    `Quantile 0.7` = NA,	
    `Quantile 0.75` = log(2)/qnorm(0.25, !!growthVar, !!growthSEVar),# %>% round(2),
    `Quantile 0.8` = NA,	
    `Quantile 0.85` = NA,	
    `Quantile 0.9` = NA,	
    `Quantile 0.95` = log(2)/qnorm(0.05, !!growthVar, !!growthSEVar),# %>% round(2)
  ) %>% select(
    `Group`,`Model`,`ModelType`,`Version`,`Creation Day`,`Creation Month`,
    `Creation Year`,`Day of Value`,`Month of Value`,`Year of Value`,
    `Geography`,`ValueType`,`Value`,`Quantile 0.05`,`Quantile 0.1`,
    `Quantile 0.15`,`Quantile 0.2`,`Quantile 0.25`,`Quantile 0.3`,`Quantile 0.35`,	
    `Quantile 0.4`,`Quantile 0.45`,`Quantile 0.5`,`Quantile 0.55`,
    `Quantile 0.6`,`Quantile 0.65`,`Quantile 0.7`,`Quantile 0.75`,
    `Quantile 0.8`,`Quantile 0.85`,`Quantile 0.9`,`Quantile 0.95`
  )
}


#' reformats an EpiEstim config file to SPI-M template
#' 
#' @param cfg - the serial interval object
#' @param modelName - the model
#' @param groupName - the group submitting the estimate

#' @return a tibble of the formatted estimates
#' @export
convertSerialIntervalToSPIM = function(serial, modelName, groupName = "Exeter", version="0.1") {
  cfg = serial$getSummary()
  quant = function(q) {
    m = msm::qtnorm(q, cfg$meanOfMean, cfg$sdOfMean, lower=cfg$minOfMean, upper=cfg$maxOfMean)
    v = msm::qtnorm(q, cfg$meanOfSd, cfg$sdOfSd, lower=cfg$minOfSd, upper=cfg$maxOfSd)
    
    k_mean = (cfg$meanOfMean / cfg$meanOfSd)^2
    k_sd = sdFromRatio(mu_x = cfg$meanOfSd, sig_x = cfg$sdOfSd, mu_y = cfg$meanOfMean, sig_y = cfg$sdOfMean ) ^ 2
    
    k = qnorm(q, k_mean, k_sd)
    
    # v = scales::squish(v, range = c(cfg$min_std_si,cfg$max_std_si))
    # m = scales::squish(m, range = c(cfg$min_mean_si,cfg$max_mean_si))
    #return(c(m,v^2) %>% round(3))
    return(c(m,k,v^2)) # %>% round(3))
  }
  return(tibble(
    `Group`=groupName,
    `Model`=modelName,
    `ModelType`="Multiple",
    `Version`=version,
    `Creation Day` = Sys.Date() %>% format("%d") %>% as.integer(),
    `Creation Month` = Sys.Date() %>% format("%m") %>% as.integer(),
    `Creation Year` = Sys.Date() %>% format("%Y") %>% as.integer(),
    `Day of Value` = Sys.Date() %>% format("%d") %>% as.integer(),
    `Month of Value` = Sys.Date() %>% format("%m") %>% as.integer(),
    `Year of Value` = Sys.Date() %>% format("%Y") %>% as.integer(),
    `Geography` = "United Kingdom",
    `ValueType` = c("mean_generation_time","kappa","var_generation_time"),
    `Value` = quant(0.5),
    `Quantile 0.05` = quant(0.05),
    `Quantile 0.1` = NA,
    `Quantile 0.15` = NA,	
    `Quantile 0.2` = NA,	
    `Quantile 0.25` = quant(0.25),
    `Quantile 0.3` = NA,	
    `Quantile 0.35` = NA,	
    `Quantile 0.4` = NA,	
    `Quantile 0.45` = NA,
    `Quantile 0.5` = quant(0.5),
    `Quantile 0.55` = NA,
    `Quantile 0.6` = NA,	
    `Quantile 0.65` = NA,	
    `Quantile 0.7` = NA,	
    `Quantile 0.75` = quant(0.75),
    `Quantile 0.8` = NA,	
    `Quantile 0.85` = NA,	
    `Quantile 0.9` = NA,	
    `Quantile 0.95` = quant(0.95)
  ))
}