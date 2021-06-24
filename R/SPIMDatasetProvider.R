#' SPIM private data
#' @export
SPIMDatasetProvider = R6::R6Class("SPIMDatasetProvider", inherit=CovidTimeseriesProvider, public = list(
  
  
    # directory=NULL,
    # 
    # initialize = function(providerController, path, ...) {
    #   self$directory = path.expand(path)
    #   super$initialize(providerController, ...)
    # },
    
  fileProvider=NULL,
    
  initialize = function(providerController, fileProvider, ...) {
    self$fileProvider = fileProvider
    super$initialize(providerController, ...)
  },
  
  ### filters ----
  
  filter=list(
    chess="CHESS COVID19", #~/S3/encrypted/5Apr/NHS/CHESS COVID19 CaseReport 20200405.csv",
    sari="SARI COVID19 CaseReport",
    lineList="Anonymised .*Line List [0-9]{8}",
    rcgp="RCGP",
    deathsLineList="COVID19 Deaths",
    ff100="FF100",
    chessSummary="CHESS Aggregate Report",
    sariSummaryArchive="SARI Archive Aggregate Report",
    sariSummaryCurrent="SARI Aggregate Report",
    oneOneOne = "SPIM-111-999",
    onsWeekly = "SPIM_ONS",
    aeSitrep = "AESitrep",
    trust = "SPIM_trust_[0-9]{3}.xlsx",
    seroprevalence = "seroprev",
    negPillar1 = "Negatives pillar1",
    negPillar2 = "Negatives pillar2",
    oneOneOneLineList = "111telephony_CLEANSED",
    fourNationsCases = "Casedata_AllNations",
    sgene = "SGTF_linelist",
    immunization = "immunisations SPIM.csv",
    voc351 = "VOC202012_02_linelist",
    ctasLineList = "CTAS SGTF data.zip",
    vamLineList = "VAM line list"
  ),
    
  #### Get raw file paths ----
  
  #' @description Search a file path for the 
  #' @param path - path to the line list file
  #' @return raw line list data set
  
  getPaths = function(...) {
    path = self$directory
    return(self$getDaily("DATAFILES", ..., orElse = function (...) {
      tmp = self$fileProvider$listAllFiles()
      return(tmp %>% filter(!isDir) %>% pull(path))
    }))
  },
    
  getLatest = function(search) {
    tmp2 = self$getPaths() %>% stringr::str_subset(search)
    tmp2Date = tmp2 %>% stringr::str_extract_all("20[1-2][0-9]-?[0-1][0-9]-?[0-3][0-9]") 
    tmp2Date = sapply(tmp2Date, function(x) {
      x = x %>% stringr::str_remove_all("-")
      y = unique(x[x==max(x)])
      return(y)
    })
    tmp3 = tmp2[tmp2Date == max(tmp2Date)]
    if(length(tmp3)==0) {
      warning("Missing file: ",search)
      return(NA_character_)
    }
    if(length(tmp3)>1) {
      warning("Multiple matches, using first: ",paste0(tmp3,collapse="; "))
      tmp3 = tmp3[[1]]
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
      return(NA_character_)
    }
    return(paste0(self$directory,"/",tmp3))
  },
    
  getLatestRawFile = function(filter, to = getwd()) {
    path = self$getLatest(filter)
    if(!stringr::str_ends(to,"/")) to = paste0(to,"/")
    dir.create(to, recursive = TRUE, showWarnings = FALSE)
    tmpFile = self$fileProvider$getFile(path)
    if (stringr::str_detect(path,"zip")) {
      zipPath = fs::path_file(path) %>% stringr::str_replace("\\.zip",".csv")
      unzip(tmpFile, files=zipPath,exdir = to, junkpaths = TRUE)
      return(paste0(to,zipPath))
    } else {
      fs::file_copy(path = tmpFile,new_path = paste0(to,fs::path_file(path)))
      return(paste0(to,fs::path_file(path)))
    }
  },
    
  #### One one one ----
  
  #' @description Load 111 data
  #' 
  #' @return a covidTimeseriesFormat dataframe
  getOneOneOne = function(...) {
    path = self$getLatest(self$filter$oneOneOne)
    message("Using: ",path)
    tmp = self$getSaved("SPIM-111", params = list(path), ..., orElse = function (...) covidTimeseriesFormat({
      oneoneone <- readxl::read_excel(self$fileProvider$getFile(path), sheet = "Extracted Data", col_types = "text")
      
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
    attr(tmp,"paths") = path
    return(tmp %>% as_tibble())
  },
  
  getOneOneOneLineList = function(dateFrom=Sys.Date()-28, ...) {
    paths = self$getNewerThan(search = self$filter$oneOneOneLineList, date = dateFrom)
    tmp = self$getSaved(id = "SPIM-111-LINE-LIST",params=list(paths,dateFrom),...,orElse= function(...) {
      
      readCsvAsText = function(conn) {readr::read_csv(conn, col_types = readr::cols(.default = readr::col_character()))}
      
      tmp = self$fileProvider$processFiles(func = readCsvAsText, paths)
      tmp2 = tidyr::unnest(tmp) %>% select(-path)
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
    attr(tmp,"paths") = paths
    return(tmp %>% as_tibble())
  },

  getOneOneOneIncidence = function(dateFrom=Sys.Date()-28, ...) {
    tmp3 = self$getOneOneOneLineList(dateFrom,...)
    out = self$getSaved("SPIM-111-BREAKDOWN", params=list(tmp3,dateFrom), ..., orElse = function(...) covidTimeseriesFormat({
      tmp4 = tmp3 %>% 
        dplyr::filter(!is.na(code)) %>%
        dplyr::group_by(code,codeType,name,date,ageCat, gender,subgroup) %>% 
        dplyr::summarise(value = n()) %>% 
        dplyr::mutate(source="111 line list",statistic = "triage", type="incidence") %>%
        self$fillAbsentAllRegions() %>% 
        self$completeAllRegions() #self$fixDatesAndNames(1) %>% self$completeAllRegions()
      return(tmp4)
    }))
    attr(out,"paths") = attr(tmp3,"paths")
    return(out)
  },
  
  #### Deaths ----
  
  #' @description Load line list
  #' 
  #' @param path - path to the line list file
  #' @return raw line list data set
  getDeathsLineList = function(...) {
    path = self$getLatest(self$filter$deathsLineList)
    message("Using: ",path)
    tmp = self$getSaved("DEATHS-LINE-LIST", params = list(path), ..., orElse = function (...) {
      
      tmp = readxl::read_excel(self$fileProvider$getFile(path), col_types = "text")
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
    attr(tmp,"paths") = path
    return(tmp %>% as_tibble())
  },
  
  #' @description Load deaths data from linelist - does not preserve ethnicity
  #' @param ageBreaks - a list of ages which form the cut points for breaking continuous ages into ranges (or NULL for a single age category)
  #' @return a covidTimeseriesFormat dataframe
  getDeathsLineListIncidence = function(ageBreaks = NULL, deathOrReport="death", cutoff=28, subgroup=NULL, gender=FALSE, filterExpr=!(is.na(death_type28) & is.na(death_type60cod) & is.na(covidcod)), codeTypes = c("CTRY","NHSER"), truncate=NULL, ...) {
    filterExpr = enexpr(filterExpr)
    subgroup = tryCatch(ensym(subgroup), error=function(e) NULL)
    tmp = self$getDeathsLineList(...)
    output = self$getSaved(id = "DEATHS-LINE-LIST-INCIDENCE", params=list(tmp,ageBreaks, deathOrReport, gender, cutoff,as_label(subgroup),as_label(filterExpr),codeTypes), ..., orElse = function (...) covidTimeseriesFormat({
      if (!identical(filterExpr,NULL))
        tmp = tmp %>% filter(!!filterExpr)
      tmp = tmp %>% 
        dplyr::filter(is.na(specimen_date) | as.integer(dod-specimen_date)<=cutoff) %>%
        dplyr::mutate(
          ageCat = age %>% self$cutByAge(ageBreaks)
        )
      if (!identical(subgroup,NULL)) {
        tmp = tmp %>% mutate(subgroup=ifelse(is.na(!!subgroup),"unknown",!!subgroup))
      } else {
        tmp = tmp %>% mutate(subgroup = NA_character_)
      }
      if (!gender) {
        tmp = tmp %>% dplyr::mutate(gender=NA_character_)
      }
      if(deathOrReport == "death") 
        tmp = tmp %>% dplyr::mutate(date = as.Date(dod))
      else
        tmp = tmp %>% dplyr::mutate(date = 
                                      as.Date(pmin(report_date_earliest,NHSdeathreportdate, DBSdeathreportdate, HPTdeathreportdate, ONS_death_registration_date, na.rm = TRUE),"1970-01-01")
        ) %>% dplyr::filter(!is.na(date))
      
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
      
      out = NULL
      if ("CTRY" %in% codeTypes) {
        england = tmp %>% 
          dplyr::mutate(code = "E92000001", codeType= "CTRY", name="England") %>% 
          dplyr::group_by(code,codeType,name,date, ageCat, gender,subgroup) %>% 
          dplyr::summarise(value = n())
        out = out %>% bind_rows(england)
      }
      
      if ("NHSER" %in% codeTypes) {
        nhser = tmp %>% selectByRegion(nhser_code, "NHSER", nhser_name)
        isNhser = nhser %>% self$codes$allPresentAndCorrect(codeTypes=c("NHSER","PSEUDO"))
        
        if(!isNhser) {
          nhser = tmp %>% selectByRegion(nhser_code, "NHSER19CDH", nhser_name) %>% 
            dplyr::inner_join(
              self$codes$getMappings() %>% dplyr::filter(fromCodeType=="NHSER19CDH" & toCodeType=="NHSER"), 
              by=c("code"="fromCode")
            ) %>%
            dplyr::ungroup() %>%
            dplyr::select(-code,-codeType, -fromCodeType,-rel,-weight) %>%
            dplyr::rename(code = toCode, codeType=toCodeType)
        }
        out = out %>% bind_rows(nhser)
      }
      
      if ("PHEC" %in% codeTypes) {out = out %>% bind_rows(tmp %>% selectByRegion(phec_code, "PHEC", phec_name))}
      if ("UA" %in% codeTypes) {out = out %>% bind_rows(tmp %>% selectByRegion(utla_code, "UA", utla_name))}
      if ("LAD" %in% codeTypes) {out = out %>% bind_rows(tmp %>% selectByRegion(ltla_code, "LAD", ltla_name))}
      if ("LSOA" %in% codeTypes) {out = out %>% bind_rows(tmp %>% selectByRegion(lsoa_code, "LSOA", lsoa_name))}
      
      out = out %>% dplyr::mutate(source="deaths line list",statistic = "death", type="incidence")
      out = out %>% self$codes$findNamesByCode() %>% select(-ends_with(".original"))
      out = out %>% self$fixDatesAndNames(truncate)
      out = out %>% self$fillAbsent(completeDates=TRUE)
      #out = out %>% self$complete()
      out = out %>% dplyr::ungroup()
      return(out)
    }))
    attr(output,"paths") = attr(tmp,"paths")
    return(output %>% as_tibble())
  },

  #### Variants / genomics ----
  
  getVoc351LineList = function(...) {
    path = self$getLatest(self$filter$voc351)
    message("Using: ",path)
    tmp = self$getSaved("VOC351", params = list(path), ...,  orElse = function (...) {
      tmp = readxl::read_excel(self$fileProvider$getFile(path), col_types = "text", sheet = "Linelist")
      datecols = c("earliest_specimen_date")
      
      for(datecol in datecols) {
        tmp[[datecol]] = suppressWarnings(as.Date(as.numeric(tmp[[datecol]]),"1899-12-30"))
      }
      tmp = tmp %>% 
        dplyr::mutate(
          finalid=as.integer(finalid)
        )
      return(tmp %>% dplyr::ungroup())
    })
    attr(tmp,"paths") = path
    return(tmp %>% as_tibble())
  },
  
  getVAMLineList = function(...) {
    path = self$getLatest(self$filter$vamLineList)
    message("Using: ",path)
    tmp = self$getSaved("VAM", params = list(path), ...,  orElse = function (...) {
      tmp = self$fileProvider$getFile(path)
      tmp2 = readr::read_csv(tmp, col_types = readr::cols(.default = readr::col_character()))
      # tmp2 = tmp2 %>% mutate(genomic_specimen_date = suppressWarnings(as.Date(genomic_specimen_date,"%Y%m%d")))
      datecols = colnames(tmp2)[colnames(tmp2) %>% stringr::str_detect("date|_at")]
      for(datecol in datecols) {
        tmp2[[datecol]] = suppressWarnings(as.Date(tmp2[[datecol]],"%Y-%m-%d"))
      }
      idcols = colnames(tmp2)[colnames(tmp2) %>% stringr::str_detect("id")]
      for(idcol in idcols) {
        tmp2[[idcol]] = suppressWarnings(as.integer(tmp2[[idcol]]))
      }
      tmp2 = tmp2 %>% mutate(
        age = suppressWarnings(as.integer(age))
      )
      if(file.exists(tmp)) unlink(tmp)
      return(tmp2)
    })
    attr(tmp,"paths") = path
    return(tmp %>% as_tibble())
  },

  #### Test and trace ----
  
  getCTASLineList = function(...) {
    #/home/terminological/S3/encrypted/2021-03-29/20210329 CTAS SGTF data.zip
    path = self$getLatest(self$filter$ctasLineList)
    message("Using: ",path)
    tmp = self$getSaved("CTAS", params = list(path), ...,  orElse = function (...) {
      tmpPath = self$fileProvider$getFile(path)
      zipPath = fs::path_file(path) %>% stringr::str_replace("\\.zip",".csv")
      tmp2 = readr::read_csv(unz(tmpPath,filename=zipPath), col_types = readr::cols(.default = readr::col_character()))
      # tmp2 = tmp2 %>% mutate(genomic_specimen_date = suppressWarnings(as.Date(genomic_specimen_date,"%Y%m%d")))
      datecols = colnames(tmp2)[colnames(tmp2) %>% stringr::str_detect("date|_at")]
      for(datecol in datecols) {
        tmp2[[datecol]] = suppressWarnings(as.Date(tmp2[[datecol]],"%Y-%m-%d"))
      }
      idcols = colnames(tmp2)[colnames(tmp2) %>% stringr::str_detect("id")]
      for(idcol in idcols) {
        tmp2[[idcol]] = suppressWarnings(as.integer(tmp2[[idcol]]))
      }
      tmp2 = tmp2 %>% mutate(
        completed = as.logical(completed),
        sex = self$normaliseGender(sex),
        sgtf = as.integer(sgtf),
        sgtf_under30ct = as.integer(sgtf_under30ct),
        p2ch1cq = as.double(p2ch1cq),
        p2ch2cq = as.double(p2ch2cq),
        p2ch3cq = as.double(p2ch3cq),
        p2ch4cq = as.double(p2ch4cq),
        age = as.integer(age)
      )
      if(file.exists(tmpPath)) unlink(tmpPath)
      return(tmp2)
    })
    attr(tmp,"paths") = path
    return(tmp %>% as_tibble())
  },
  
  #### Immunisations ----

  #' @description Load immunizations line list
  #' 
  #' @return raw line list data set
  getImmunizationLineList = function(...) {
    path = self$getLatest(self$filter$immunization)
    message("Using: ",path)
    out = self$getSaved("IMMUNIZATIONS", params = list(path), ...,  orElse = function (...) {
      tmp = readr::read_csv(self$fileProvider$getFile(path), col_types = readr::cols(.default = readr::col_character()))
      tmp = tmp %>% 
        as_tibble() %>% 
        mutate(
          patient_pseudo_id = as.numeric(patient_pseudo_id),
          age = suppressWarnings(as.numeric(age)),
          finalid = as.numeric(finalid),
          finalid2 = as.numeric(finalid2),
          vaccination_date = as.Date(vaccination_date, "%d%b%Y")
        )
    })
    attr(out,"paths") = path
    return(out %>% as_tibble())
  },
  
  #' @description Load incidence from line list
  #' 
  #' @param ageBreaks - a list of ages which form the cut points for breaking continuous ages into ranges (or NULL for a single age category)
  #' @return a covidTimeseriesFormat dataframe
  getImmunizationLineListIncidence = function(ll=NULL, ageBreaks = NULL, filterExpr=NULL, subgroup="string_dose_number", ...) {
    filterExpr = enexpr(filterExpr)
    subgroup = tryCatch(ensym(subgroup), error = function(e) NULL)
    # TODO: do we need the ll option here. It is not cached
    tmp = self$getDaily("IMMUNIZATIONS-INCIDENCE", params=list(ageBreaks, as_label(filterExpr), as_label(subgroup)), ..., orElse = function (...) covidTimeseriesFormat({
      if(!identical(ll,NULL)) {
        tmp = ll
      } else {
        tmp = self$getImmunizationLineList(...) 
      }
      if(!identical(filterExpr,NULL)) tmp = tmp %>% filter(!!filterExpr)
      tmp = tmp %>% dplyr::mutate(ageCat = age %>% self$cutByAge(ageBreaks), gender=self$normaliseGender(gender,na.value="unknown"))
      if(!identical(subgroup,NULL)) {
        tmp = tmp %>% mutate(subgroup=!!subgroup)
      } else {
        tmp = tmp %>% mutate(subgroup=NA_character_)
      }
      tmp = tmp %>% dplyr::mutate(date = as.Date(vaccination_date))
      
      out = tmp %>% dplyr::mutate(code = ltla_code, codeType="LAD", name=ltla_name) %>% 
          dplyr::mutate(
            code = ifelse(is.na(code) | code=="Unknown","E99999999",code),
            name = ifelse(is.na(code) | code=="Unknown","Unknown (England)",name)
          ) %>%
          dplyr::group_by(code,codeType,name,date,ageCat,gender,subgroup) %>% 
          dplyr::summarise(value = n())
      
      out = out %>% dplyr::mutate(source="immunization",statistic = "immunization", type="incidence")
      out = out %>% self$fixDatesAndNames(0)
      out = out %>% self$fillAbsent(completeDates=TRUE)
      out = out %>% dplyr::ungroup()
      return(out)
    }))
    attr(tmp,"paths") = path
    return(tmp %>% as_tibble())
  },
  
  getImmunizationFraction = function(ageBreaks = NULL,...) {
    self$getDaily("IMMUNIZATIONS-FRACTION", params=list(ageBreaks), ..., orElse = function (...) covidTimeseriesFormat({
      tmp2 = dpc$spim$getImmunizationLineListIncidence(ageBreaks=ageBreaks)
      tmp3 = tmp2 %>% tsp$aggregateGender()
      tmp4 = tmp3 %>% tsp$cumulativeFromIncidence()
      tmp4 = tmp4 %>% self$demog$findDemographics()
      deaths = dpc$spim$getDeathsLineListIncidence(ageBreaks = ageBreaks,codeTypes = "LAD")
      deathsCum = deaths %>% tsp$aggregateGender() %>% tsp$cumulativeFromIncidence()
      tmp5 = tmp4 %>% inner_join(deathsCum %>% select(code,date,ageCat,cumdeaths = value), by=c("code","date","ageCat"))
      tmp5 = tmp5 %>% mutate(vaccPercent = value/(population-cumdeaths)) %>% mutate(vaccPercent = ifelse(vaccPercent>1,1,vaccPercent))
      tmp5 = tmp5 %>% mutate(immunized = value, value=vaccPercent, type="fraction") %>% select(-vaccPercent)
      return(tmp5)
    }))
  },
  
  #### S-gene line list ----

  #' @description Load line list
  #' 
  #' @return raw line list data set
  getSGeneLineList = function(...) {
    path = self$getLatest(self$filter$sgene)
    message("Using: ",path)
    tmp = self$getSaved("SGENE-LINE-LIST", params = list(path), ..., orElse = function (...) {
      if (stringr::str_detect(path,"zip")) {
        tmpFile = self$fileProvider$getFile(path)
        zipPath = fs::path_file(path) %>% stringr::str_replace("\\.zip",".csv")
        tmp = readr::read_csv(unz(tmpFile, filename=zipPath))
      } else {
        tmp = readr::read_csv(self$fileProvider$getFile(path))
      }
      return(tmp %>% dplyr::ungroup())
    })
    attr(tmp,"paths") = path
    return(tmp %>% as_tibble())
  },
  
  #' Interpret S gene status according to various cut off values
  #' function to help interpret S gene CT values in context of N gene and ORF gene to give S gene status. 
  #' With the defaults this produces the same result as the sgtf_30 column in the source SGTF line list
  #' Defaults are S:30,ORF:30,N:30,Control:Inf
  #'
  #' @param sGeneLineList - a dataframe includeing 
  #' @param S_CT - S gene detected when P2CH3CQ <= this value
  #' @param ORF1ab_CT - ORF1ab gene detected when P2CH1CQ <= this value
  #' @param N_CT - N gene detected when P2CH2CQ <= this value
  #' @param Control_CT - control sample is positive when P2CH4CQ <= this value
  #'
  #' @return - the same dataframe with additional columns including "sGene" and "result"
  #'
  #' @examples coxData = coxData %>% interpretSGene()
  interpretSGene = function(sGeneLineList, S_CT = 30, ORF1ab_CT = 30, N_CT = 30, Control_CT = Inf, ...) {
    sGeneLineList %>% 
      mutate(
        ORF1ab_CT_threshold = ORF1ab_CT,
        N_CT_threshold = N_CT,
        S_CT_threshold = S_CT,
        S_pos = P2CH3CQ > 0 & P2CH3CQ <= S_CT,
        S_undetect = P2CH3CQ == 0,
        N_pos = P2CH2CQ > 0 & P2CH2CQ <= N_CT,
        ORF1ab_pos = P2CH1CQ > 0 & P2CH1CQ <= ORF1ab_CT,
        Control_pos = P2CH4CQ > 0 & P2CH4CQ <= Control_CT,
        sGene = case_when(
          is.na(P2CH1CQ) ~ "Unknown",
          S_pos & N_pos & ORF1ab_pos & Control_pos ~ "Positive",
          S_undetect & N_pos & ORF1ab_pos & Control_pos ~ "Negative",
          TRUE ~ "Equivocal"
        ),
        CT_N = ifelse(P2CH2CQ > 0, P2CH2CQ, 40)
      ) %>% 
      mutate(
        result = case_when(
          is.na(P2CH1CQ) ~ "Unknown",
          !Control_pos ~ "No control",
          TRUE ~ paste0(ifelse(S_pos,"S+","S-"),ifelse(N_pos,"N+","N-"),ifelse(ORF1ab_pos,"ORF+","ORF-")))
      ) %>%
      mutate(
        sGene = sGene %>% ordered(c("Positive","Negative","Equivocal","Unknown")),
        relativeCopyNumber = 2^(median(CT_N,na.rm=TRUE)-CT_N)
      )
  },
  
  
  getSGeneEras = function(cutoff = 28, S_CT = 30, ORF1ab_CT = 30, N_CT = 30, Control_CT = Inf, ...) {
    path = self$getLatest(self$filter$sgene)
    tmp = self$getSaved("SGENE-ERAS", params = list(path), ..., orElse = function (...) {
      sgll = self$getSGeneLineList() %>% self$interpretSGene(S_CT, ORF1ab_CT, N_CT, Control_CT)
      # group by patient and find time delay between tests (where there are more than one)
      tmp = sgll %>% arrange(FINALID,specimen_date) %>% mutate(delay = ifelse(FINALID==lag(FINALID), as.numeric(specimen_date - lag(specimen_date)), NA_real_))
      # TODO: there is some interesting properties of the delay
      # ggplot(tmp, aes(x=delay))+geom_density()+scale_x_continuous(trans="log1p",breaks=c(0,10,20,50,100,200,500,1000))+facet_wrap(vars(sgtf_under30CT))
      # ggplot(tmp, aes(x=delay,y=P2CH1CQ))+geom_density_2d()
      # apply some heuristics to determine whether a test is part of the same infection or a new one
      tmp2 = tmp %>% mutate(era = case_when(
        is.na(delay) ~ "new",
        is.na(sgtf_under30CT) & delay < cutoff*2 ~ "same", # prolonged recovery
        delay < cutoff ~ "same",
        TRUE ~ "new"
      ))
      # assign an eraIndex - essentially the count of novel infection episodes
      tmp3 = tmp2 %>% group_by(FINALID) %>% arrange(specimen_date) %>% mutate(eraIndex = cumsum(ifelse(era=="new",1,0)))
      # summarise sGene data into a single value for each era
      # TODO: each era may have multiple positive tests there is an opportunity to look at the CT values over time and 
      # fit some sort of model here
      tmp4 = tmp3 %>% group_by(FINALID,eraIndex) %>% 
        mutate(
          earliest_specimen_date = min(specimen_date,na.rm=TRUE), 
          latest_specimen_date = max(specimen_date,na.rm=TRUE), 
          tests=n(), 
          anyPosSGene = any(sGene == "Positive"),
          anyNegSGene = any(sGene == "Negative"),
          anyEquivSGene = any(sGene == "Equivocal"),
          anyUnknSGene = any(sGene == "Unknown"),
        ) %>% ungroup() %>% mutate(
          sGene=case_when(
            anyPosSGene & !anyNegSGene ~ "Positive",
            anyNegSGene & !anyPosSGene ~ "Negative",
            anyEquivSGene ~ "Equivocal",
            TRUE ~ "Unknown"
          ) %>% ordered(c("Positive","Negative","Equivocal","Unknown"))
        )
      return(tmp4 %>% select(-anyPosSGene, -anyNegSGene, -anyEquivSGene, -anyUnknSGene))
    })
    attr(tmp,"paths") = path
    return(tmp %>% as_tibble())
  },
  
  getSDropoutFreqency = function(codeTypes = c("NHSER"),  ageBreaks = NULL, S_CT = 30, ORF1ab_CT = 30, N_CT = 30, equivocal.rm=TRUE, window=7, ll=NULL, sgll=NULL, ...) {
    
    if (identical(ll,NULL)) ll = self$getLineList() %>% ungroup()
    if (identical(sgll,NULL)) sgll = self$getSGeneLineList()
    
    path = c(attr(ll,"paths"),attr(sgll,"paths"))
    
    tmp = self$getDaily("SGENE-DROPOUT",params=list(ORF1ab_CT, N_CT, codeTypes,ageBreaks,equivocal.rm,window,path), ..., orElse = function (...) {
      
      groupByVars = ll %>% groups()
      
      tmp2 = sgll %>% 
        left_join(ll, by="FINALID", suffix=c("",".ll")) # %>%
        # group_by(FINALID) %>% arrange(desc(specimen_date)) %>% filter(row_number()==1)
      tmp3 = tmp2 %>% ungroup() %>% mutate(
        ageCat = age %>% self$cutByAge(ageBreaks)
      ) %>% self$interpretSGene(S_CT,ORF1ab_CT,N_CT)
      
      fn = function(tmpDf, codeCol = "NHSER_code", nameCol = "NHSER_name", codeType="NHSER") {
        codeCol = ensym(codeCol)
        nameCol = ensym(nameCol)
        grps = tmpDf %>% groups()
        joins = unlist(sapply(grps,as_label))
        
        tmpDf = tmpDf %>% 
          rename(code = !!codeCol,name = !!nameCol,date=specimen_date) %>%
          group_by(!!!grps,code,name,ageCat,date,sGene) %>% 
          summarise(count = n()) %>% ungroup()
        tmpDf2 = tmpDf %>% select(code,name) %>% distinct() %>% filter(!is.na(code)) %>% 
        left_join(
          tmpDf %>% select(!!!grps) %>% distinct(), by=character()
        ) %>% left_join(
          tibble(date = as.Date(min(tmpDf$date):max(tmpDf$date),"1970-01-01")), by=character()
        ) %>% left_join(
          tmpDf %>% select(ageCat) %>% distinct(), by=character()
        ) %>% left_join(
          tibble(sGene=c("Positive","Negative","Equivocal")), by=character()
        ) %>% left_join(tmpDf, by=c(joins,"code","name","ageCat","sGene","date")) %>% mutate(count = ifelse(is.na(count),0,count))
        
        if (equivocal.rm) tmpDf2 = tmpDf2 %>% filter(sGene != "Equivocal")
        tmpDf2 = tmpDf2 %>% group_by(!!!grps,code,name,ageCat,sGene) %>% arrange(date) %>%
          mutate(Roll.count = stats::filter(count,filter=rep(1,window),sides=1)) %>%
          filter(!is.na(Roll.count))
        tmpDf2 = tmpDf2 %>% 
          group_by(!!!grps,code,name,ageCat,date) %>%
          mutate(binom::binom.confint(Roll.count, sum(Roll.count), conf.level = 0.95, methods = "wilson")) %>% 
          rename(Roll.mean = mean,Roll.lower = lower,Roll.upper=upper) %>%
          mutate(binom::binom.confint(count, sum(count), conf.level = 0.95, methods = "wilson")) %>% 
          select(-x,-n,-method) %>% 
          mutate(codeType = codeType)
        return(tmpDf2)
      }
      
      tmp3 = tmp3 %>% ungroup() %>% group_by(!!!groupByVars)
      out = NULL
      if ("NHSER" %in% codeTypes) out = out %>% bind_rows(tmp3 %>% fn(codeCol = NHSER_code, nameCol=NHSER_name, codeType = "NHSER"))
      if ("PHEC" %in% codeTypes) out = out %>% bind_rows(tmp3 %>% fn(codeCol = PHEC_code, nameCol=PHEC_name, codeType = "PHEC"))
      if ("UA" %in% codeTypes) out = out %>% bind_rows(tmp3 %>% fn(codeCol = UTLA_code, nameCol=UTLA_name, codeType = "UA"))
      if ("LAD" %in% codeTypes) out = out %>% bind_rows(tmp3 %>% fn(codeCol = LTLA_code, nameCol=LTLA_name, codeType = "LAD"))
      if ("CTRY" %in% codeTypes) out = out %>% bind_rows(tmp3 %>% mutate(code = "E92000001",name="England") %>% fn(codeCol = code, nameCol=name, codeType = "CTRY"))
      
      return(out)
      
    })
    attr(tmp,"paths") = path
    return(tmp %>% as_tibble())
  },
  
  #### Cases ----

  #' @description Load line list
  #' 
  #' @return raw line list data set
  getLineList = function(...) {
    path = self$getLatest(self$filter$lineList)
    message("Using: ",path)
    out = self$getSaved("LINE-LIST", params = list(path), ..., orElse = function (...) {
      if (stringr::str_detect(path,"zip")) {
        tmpFile = self$fileProvider$getFile(path)
        zipPath = fs::path_file(path) %>% stringr::str_replace("\\.zip",".csv")
        tmp = readr::read_csv(unz(tmpFile, filename=zipPath), col_types = readr::cols(.default = readr::col_character()))
        tmp = tmp %>% 
          dplyr::mutate(
            Onsetdate = maybeDMYorMDY(Onsetdate),
            specimen_date = maybeDMYorMDY(specimen_date),
            lab_report_date = maybeDMYorMDY(lab_report_date)
          ) 
        
      } else if (stringr::str_detect(path,"csv")) {
        tmp = readr::read_csv(self$fileProvider$getFile(path), col_types = readr::cols(.default = readr::col_character()))
        tmp = tmp %>% 
          dplyr::mutate(
            Onsetdate = maybeDMYorMDY(Onsetdate),
            specimen_date = maybeDMYorMDY(specimen_date),
            lab_report_date = maybeDMYorMDY(lab_report_date)
          ) 
        
      } else {
        tmp = readxl::read_excel(path.expand(path), 
                 col_types = "text") #c("numeric", "text", "text", "text", "text", "text", "text", "text", "text", "text", "numeric", "date", "date", "date"))
        tmp = tmp %>% 
          dplyr::mutate(
            Onsetdate = suppressWarnings(as.Date(as.numeric(Onsetdate),"1899-12-30")),
            specimen_date = suppressWarnings(as.Date(as.numeric(specimen_date),"1899-12-30")),
            lab_report_date = suppressWarnings(as.Date(as.numeric(lab_report_date),"1899-12-30"))
          )
      }
      
      if(any(is.na(tmp$specimen_date))) warning("NA sprecimen dates in cases file")
      
      if ("finalid" %in% colnames(tmp)) tmp = tmp %>% rename(FINALID = finalid)
      
      return(tmp %>% mutate(
        pillar_2_testingkit = tolower(pillar_2_testingkit),
        age = suppressWarnings(as.numeric(age)),
        FINALID = as.numeric(FINALID),
        imd_rank = as.integer(imd_rank),
        imd_decile = as.integer(imd_decile),
        ethnicity_final = case_when(
          ethnicity_final %in% c("African (Black or Black British)","Any other Black background","Caribbean (Black or Black British)") ~ "Afro-caribbean",
          ethnicity_final %in% c("Any other Asian background","Bangladeshi (Asian or Asian British)","Indian (Asian or Asian British)","Pakistani (Asian or Asian British)") ~ "Asian",
          ethnicity_final %in% c("Any other White background","British (White)","Irish (White)") ~ "White",
          ethnicity_final %in% c("Any other Mixed background","Any other ethnic group","White and Black Caribbean (Mixed)","White and Black African (Mixed)","Chinese (other ethnic group)") ~ "Other",
          TRUE ~ "Unknown"),
        residential_category = case_when(
          cat == 'Residential dwelling (including houses, flats, sheltered accommodation)' ~ "Residential",
          cat == 'Care/Nursing home' ~ "Care home",
          cat == 'Undetermined'~"Other/Unknown",
          cat == 'Medical facilities (including hospitals and hospices, and mental health)'~"Other/Unknown",
          cat == 'Other property classifications'~"Other/Unknown",
          cat == 'House in multiple occupancy (HMO)' ~ "Residential",
          cat == 'Prisons, detention centres, secure units'~"Other/Unknown",
          cat == 'Residential institution (including residential education)'~"Other/Unknown",
          cat == 'No fixed abode'~"Other/Unknown",
          cat == 'Overseas address'~"Other/Unknown",
          TRUE ~ "Other/Unknown"
        )
      ) %>% dplyr::ungroup())
    })
    attr(out,"paths") = path
    return(out %>% as_tibble())
    # TODO: https://github.com/sarahhbellum/NobBS
  },
  
    
  augmentLineListWithLSOA = function(ll, ltlaCodeCol = "LTLA_code", imdRankCol="imd_rank") {
    ltlaCodeCol = ensym(ltlaCodeCol)
    imdRankCol = ensym(imdRankCol)
    imd = self$demog$getIMDData() %>% select(
      !!ltlaCodeCol := `Local Authority District code (2019)`,
      !!imdRankCol := `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`,
      LSOA_code = `LSOA code (2011)`,
      LSOA_name = `LSOA name (2011)`
    )
    return(ll %>% left_join(imd, by=c(as_label(ltlaCodeCol),as_label(imdRankCol))))
  },
  
  #' @description Load incidence from line list
  #' 
  #' @param ageBreaks - a list of ages which form the cut points for breaking continuous ages into ranges (or NULL for a single age category)
  #' @return a covidTimeseriesFormat dataframe
  getLineListIncidence = function(ll=NULL, ageBreaks = NULL, gender=FALSE, specimenOrReport="specimen", subgroup="pillar", filterExpr=NULL, codeTypes = c("CTRY","NHSER"), truncate=NULL, ...) {
    filterExpr = enexpr(filterExpr)
    subgroup = tryCatch(ensym(subgroup), error = function(e) NULL)
    if(!identical(ll,NULL)) {
      tmp = ll
    } else {
      tmp = self$getLineList(...) 
    }
    path = attr(tmp,"paths")
    out2 = self$getSaved("LINE-LIST-INCIDENCE", params=list(tmp, ageBreaks, specimenOrReport,as_label(subgroup), as_label(filterExpr), codeTypes, gender), ..., orElse = function (...) covidTimeseriesFormat({
      if(!identical(filterExpr,NULL)) 
        tmp = tmp %>% filter(!!filterExpr)
      tmp = tmp %>% dplyr::mutate(ageCat = age %>% self$cutByAge(ageBreaks)) 
      if (gender) {
        tmp = tmp %>% dplyr::mutate(gender=self$normaliseGender(sex))
      } else {
        tmp = tmp %>% dplyr::mutate(gender=NA_character_)
      }
      if(!identical(subgroup,NULL)) {
        tmp = tmp %>% dplyr::mutate(subgroup=!!subgroup)
      } else {
        tmp = tmp %>% dplyr::mutate(subgroup=NA_character_)
      }
      if(specimenOrReport == "report")
        tmp = tmp %>% dplyr::mutate(date = as.Date(lab_report_date))
      else
        tmp = tmp %>% dplyr::mutate(date = as.Date(specimen_date))
      
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
      
      out = NULL
      if ("CTRY" %in% codeTypes) {
        england = tmp %>% dplyr::mutate(code = "E92000001", codeType= "CTRY", name="England") %>% 
          dplyr::group_by(code,codeType,name,date, ageCat, gender,subgroup) %>% 
          dplyr::summarise(value = n())
        out = out %>% bind_rows(england)
      }
      
      if ("NHSER" %in% codeTypes) {
        nhser = tmp %>% selectByRegion(NHSER_code, "NHSER", NHSER_name)
        isNhser = nhser %>% self$codes$allPresentAndCorrect(codeTypes=c("NHSER","PSEUDO"))
        
        if(!isNhser) {
          nhser = tmp %>% selectByRegion(NHSER_code, "NHSER19CDH", NHSER_name) %>% 
            dplyr::inner_join(
              self$codes$getMappings() %>% dplyr::filter(fromCodeType=="NHSER19CDH" & toCodeType=="NHSER"), 
              by=c("code"="fromCode")
            ) %>%
            dplyr::ungroup() %>%
            dplyr::select(-code,-codeType, -fromCodeType,-rel,-weight) %>%
            dplyr::rename(code = toCode, codeType=toCodeType)
        }
        out = out %>% bind_rows(nhser)
      }
      
      if ("PHEC" %in% codeTypes) {out = out %>% bind_rows(tmp %>% selectByRegion(PHEC_code, "PHEC", PHEC_name))}
      if ("UA" %in% codeTypes) {out = out %>% bind_rows(tmp %>% selectByRegion(UTLA_code, "UA", UTLA_name))}
      if ("LAD" %in% codeTypes) {out = out %>% bind_rows(tmp %>% selectByRegion(LTLA_code, "LAD", LTLA_name))}
      if ("LSOA" %in% codeTypes) {out = out %>% bind_rows(tmp %>% selectByRegion(LSOA_code, "LSOA", LSOA_name))}
      
      out = out %>% dplyr::mutate(source="line list",statistic = "case", type="incidence")
      out = out %>% self$fixDatesAndNames(truncate)
      out = out %>% self$fillAbsent(completeDates=TRUE)
      out = out %>% dplyr::ungroup()
      return(out)
    }))
    attr(out2,"paths") = path
    return(out2 %>% as_tibble())
  },
  
  #### Episodes ----
  
  #' @description Combine line list and S-gene line list to get a list of infection episodes
  #' 
  #' this defines how long between tests before two tests are regarded as a new episode.
  #' if the tests are sgtf equivocal double this is allowed.
  #' calculate the individual episodes of covid resulting from runs of sequential positive tests <28 days apart.
  #' 
  #' @param cutoff - the time gap between sequential tests after which two tests are said to be from a new episode
  #' @param S_CT - S gene detected when P2CH3CQ <= this value
  #' @param ORF1ab_CT - ORF1ab gene detected when P2CH1CQ <= this value
  #' @param N_CT - N gene detected when P2CH2CQ <= this value
  #' @param Control_CT - control sample is positive when P2CH4CQ <= this value
  #' @return a covidTimeseriesFormat dataframe
  getInfectionEpisodes = function(cutoff=28, S_CT = 30, ORF1ab_CT = 30, N_CT = 30, Control_CT = Inf, ...) {
    path1 = self$getLatest(self$filter$sgene)
    path2 = self$getLatest(self$filter$lineList)
    message("Using: ",path1," and ",path2)
    # TODO: this is a bit slow and maybe could be improved
    # the dtplyr has some sort of bug in it connected with: https://github.com/tidyverse/dtplyr/issues/164
    # which only appears when wrapped in a function so code below works when 
    # dtplyr does not seem production ready. maybe need to learn data.table.
    out = self$getSaved(id = "EPISODES", params = list(cutoff,path1,path2,S_CT,ORF1ab_CT,N_CT), ..., orElse=function(...) {
      ll = self$getLineList()
      sgll = self$getSGeneLineList()
      
      # Split line list out into normalised parts.
      llDemog = ll %>% select(FINALID, NHSER_code, NHSER_name, PHEC_code, PHEC_name, UTLA_code, UTLA_name, LTLA_code, LTLA_name, sex, age, ethnicity_final, imd_decile, imd_rank, residential_category, cat) %>% distinct()
      # llEpisode = ll %>% select(FINALID, specimen_date, asymptomatic_indicator, pillar, lab_report_date, pillar_2_testingkit, testcentreid, case_category) %>% mutate(episode_type="first positive",episode=1) %>% distinct()
      llTest = ll %>% select(FINALID, specimen_date, case_category,asymptomatic_indicator) %>% mutate(linelist = TRUE)
      
      # Split s gene line list out into normalised parts.
      sgllTest = sgll %>% 
        self$interpretSGene(S_CT, ORF1ab_CT, N_CT, Control_CT) %>%
        select(FINALID, specimen_date, sGene, result) %>% 
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
          sGene = ifelse(is.na(sGene),"Unknown",as.character(sGene)),
          case_category = ifelse(sglinelist,"TaqPath",case_category)
        )
      
      tests2 = tests # dtplyr::lazy_dt(tests) weirdly not working inside a function....
      # browser()
      # look for sequential tests with the same id and calulate time difference between them.
      # we do this avoiding group by as it becomes very slow.
      tests3 = tests2 %>% arrange(FINALID,specimen_date) %>% 
        mutate(
          delay = ifelse(FINALID == lag(FINALID), as.numeric(specimen_date - lag(specimen_date)), NA_integer_)
        ) %>%
        mutate(era = case_when(
          is.na(delay) ~ "new",
          sGene == "Equivocal" & delay < cutoff*2 ~ "same", # prolonged recovery allowed for Equivocal test results
          delay < cutoff ~ "same",
          TRUE ~ "new"
        )) %>%
        # assign an eraIndex - essentially the count of novel infection episodes
        group_by(FINALID) %>% arrange(specimen_date) %>% 
        mutate(eraIndex = cumsum(ifelse(era=="new",1,0)))
      
      # TODO: each era may have multiple positive tests there is an opportunity to look at the CT values over time and 
      # fit some sort of model here
      tests4 = tests3 %>% group_by(FINALID,eraIndex) %>% 
        summarise(
          earliest_specimen_date = min(specimen_date,na.rm=TRUE), 
          latest_specimen_date = max(specimen_date,na.rm=TRUE), 
          tests=n(),
          anyPosSGene = any(sGene == "Positive"),
          anyNegSGene = any(sGene == "Negative"),
          anyEquivSGene = any(sGene == "Equivocal"),
          anyUnknSGene = any(sGene == "Unknown"),
          asymptomatic_indicator = first(na.omit(asymptomatic_indicator),default="U"),
          lft_only = all(na.omit(case_category=="LFT_Only"))
        )
      
      tests5 = tests4 %>% mutate(
        sGene=
          case_when(
            anyPosSGene & !anyNegSGene ~ "Positive",
            anyNegSGene & !anyPosSGene ~ "Negative",
            anyEquivSGene ~ "Equivocal",
            TRUE ~ "Unknown"
          ) %>% ordered(c("Positive","Negative","Equivocal","Unknown")),
        ) %>% as_tibble()
      
      return(tests5 %>% inner_join(llDemog, by="FINALID") %>% select(-anyPosSGene, -anyNegSGene, -anyEquivSGene, -anyUnknSGene))
    })
    attr(out,"paths") = c(path1,path2)
    return(out %>% as_tibble()) 
  },
  
  #### Negatives ----

  getNegatives = function(codeTypes = c("CTRY","NHSER"), truncate=NULL, ...) {
    # TODO:
    stop("needs updating as formats changed at some point")
    path1 = self$getLatest(self$filter$negPillar1)
    path2 = self$getLatest(self$filter$negPillar2)
    tmp = self$getSaved("NEGATIVES", params=list(codeTypes, path1, path2), ..., orElse = function(...) {
        #TODO: extra layer of caching in here?
        neg1 = readr::read_csv(self$fileProvider$getFile(path1))
        neg2 = readr::read_csv(self$fileProvider$getFile(path2))
        neg = bind_rows(neg1 %>% mutate(subgroup="Pillar 1"),neg2 %>% mutate(subgroup="Pillar 2"))
        neg = neg %>% mutate(ageCat = case_when(
          agegroup == "0 to 4" ~ "<5",
          agegroup == "5 to 9" ~ "5-14",
          agegroup == "10 to 14" ~ "5-14",
          agegroup == "15 to 19" ~ "15-24",
          agegroup == "20 to 24" ~ "15-24",
          agegroup == "25 to 29" ~ "25-34",
          agegroup == "30 to 34" ~ "25-34",
          agegroup == "35 to 39" ~ "35-44",
          agegroup == "40 to 44" ~ "35-44",
          agegroup == "45 to 49" ~ "45-54",
          agegroup == "50 to 54" ~ "45-54",
          agegroup == "55 to 59" ~ "55-64",
          agegroup == "60 to 64" ~ "55-64",
          agegroup == "65 to 69" ~ "65-74",
          agegroup == "70 to 74" ~ "65-74",
          agegroup == "75 to 79" ~ "75-84",
          agegroup == "80 to 84" ~ "75-84",
          agegroup == "85 to 89" ~ "85+",
          agegroup == "90 or older" ~ "85+",
          TRUE ~ "unknown"
        ))
        neg = neg %>% mutate(gender = self$normaliseGender(gender))
        neg = neg %>% rename(value = negative, date = earliestspecimendate) %>% select(-agegroup)
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
        neg = NULL
        if ("CTRY" %in% codeTypes) {
          england = tmp %>% dplyr::mutate(code = "E92000001", codeType= "CTRY", name="England") %>% 
            dplyr::group_by(code,codeType,name,date,ageCat,gender,subgroup) %>% 
            dplyr::summarise(value = sum(value))
          neg = neg %>% bind_rows(england)
        }
        if ("PHEC" %in% codeTypes) {
          phec = tmp %>% dplyr::rename(name=phecentre) %>% 
            dplyr::group_by(name,date,ageCat,gender,subgroup) %>% 
            dplyr::summarise(value = sum(value)) %>% 
            dpc$codes$findCodesByName(codeTypes = "PHEC",outputCodeVar = "code",outputCodeTypeVar = "codeType")
          neg = neg %>% bind_rows(phec)
        }
        if ("NHSER" %in% codeTypes) {
          nhser = tmp %>% dplyr::rename(name=nhsregion) %>% 
            dplyr::group_by(name,date,ageCat,gender,subgroup) %>% 
            dplyr::summarise(value = sum(value)) %>% 
            dpc$codes$findCodesByName(codeTypes = "NHSER",outputCodeVar = "code",outputCodeTypeVar = "codeType")
          neg = neg %>% bind_rows(nhser)
        }
        if ("LAD" %in% codeTypes) {
          ltla = neg %>% dplyr::mutate(code = ltla, codeType= "LAD", name=ltlaname) %>% 
            dplyr::group_by(code,codeType,name,date,ageCat,gender,subgroup) %>% 
            dplyr::summarise(value = sum(value))
          neg = neg %>% bind_rows(ltla)
        }
        if ("UA" %in% codeTypes) {
          utla = tmp %>% dplyr::mutate(code = utla, codeType= "UA", name=utlaname) %>% 
            dplyr::group_by(code,codeType,name,date,ageCat,gender,subgroup) %>% 
            dplyr::summarise(value = sum(value))
          neg = neg %>% bind_rows(utla)
        }
        neg = neg %>% 
          mutate(source = "negatives", statistic = "negative test",type = "incidence") %>%
          select(-any_of("name.original")) %>% 
          mutate(note=NA_character_) %>%
          filter(!is.na(code))
        neg = neg %>% 
          self$fixDatesAndNames(truncate) %>% 
          self$fillAbsent(completeDates = TRUE) %>%
          dplyr::ungroup()
        return(neg)
    })
    attr(tmp,"paths") = c(path1,path2)
    return(tmp %>% as_tibble())
  },
    
  #### Seroprevalence ----

  #' @description Load the seroprevalance file
  #' 
  #' @return raw FF100 data set
  getSeroprevalence = function(...) {
    path = self$getLatest(self$filter$seroprevalence)
    message("Using: ",path)
    tmp = self$getSaved("SEROPREV", params = list(path), ..., orElse = function (...) {
      # ID	Barcode	surv	age	age_m	Sex	Region	Location	sample_region	SampleDate	isoweek_sample	EuroImm_outcome	EuroImm_Units	RBD_outcome	RBD_Units
      #Barcode	Collection	Sex	Location	NHS_Region	SampleDate	sample_region	isoweek_sample	age	age_m	study_id	Abbott_outcome	Abbott_units	EuroImmun_outcome	EuroImmun_units	RBD_outcome	RBD_units	RocheN_outcome	RocheN_units	RocheS_outcome	RocheS_units	Ethnicity	study_visit	firstvaccinationdate	secondvaccinationdate	firstvaccinationbrand	secondvaccinationbrand	ONS_Region

      #xlsCon = self$fileProvider$getFile(path)
      
      
      for (sheet in readxl::excel_sheets(self$fileProvider$getFile(path))) {
        a1 = readxl::read_excel(self$fileProvider$getFile(path), sheet = sheet, range = "A1",col_names = FALSE)
        if (a1[[1]]=="Barcode") break
      }
      
      data = readxl::read_excel(self$fileProvider$getFile(path), sheet = sheet, col_types = "text") %>% dplyr::mutate(
        SampleDate = suppressWarnings(as.Date(as.numeric(SampleDate),"1899-12-30")),
      ) %>% dplyr::mutate(
        age = suppressWarnings((as.numeric(age)))
      )
      if ("Abbott_units" %in% colnames(data)) data = data %>% dplyr::mutate(Abbott_units = suppressWarnings(as.numeric(Abbott_units)))
      if ("EuroImmun_units" %in% colnames(data)) data = data %>% dplyr::mutate(EuroImmun_units = suppressWarnings(as.numeric(EuroImmun_units)))
      if ("RBD_units" %in% colnames(data)) data = data %>% dplyr::mutate(RBD_units = suppressWarnings(as.numeric(RBD_units)))
      if ("RocheN_units" %in% colnames(data)) data = data %>% dplyr::mutate(RocheN_units = suppressWarnings(as.numeric(RocheN_units)))
      if ("RocheS_units" %in% colnames(data)) data = data %>% dplyr::mutate(RocheS_units = suppressWarnings(as.numeric(RocheS_units)))
      
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
    attr(tmp,"paths") = path
    return(tmp %>% as_tibble())
  },
  
  #' @description Load seroprevalence data from linelist
  #' @param ageBreaks - a list of ages which form the cut points for breaking continuous ages into ranges (or NULL for a single age category)
  #' @return a covidTimeseriesFormat dataframe
  getSeroprevalenceTestIncidence = function(ageBreaks = NULL, ...) {
    stop("Need to update this for newer seroprevalence data")
    data2 = self$getSeroprevalence(...)
    self$getSaved("SEROPREVALENCE-INCIDENCE", params=list(data2, ageBreaks), ..., orElse = function (...) covidTimeseriesFormat({
      
      
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
    
  #### FF 100 ----
  
  #' @description Load ff100 file
  #' 
  #' @return raw FF100 data set
  getFF100 = function() {
    path = self$getLatest(self$filter$ff100)
    message("Using: ",path)
    tmp = readr::read_csv(self$fileProvider$getFile(path),
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
    attr(tmp,"paths") = c(path1,path2)
    return(tmp %>% as_tibble())
  },
    
  #### CHESS / SARI ----
  
  #' @description Load the CHESS dataset from a path
  #' 
  #' @param path - a path to the chess csv file
  #' @return raw CHESS data set
  getCHESS = function() {
    path = self$getLatest(self$filter$chess)
    message("Using: ",path)
    out = readr::read_csv(self$fileProvider$getFile(path), col_types = readr::cols(.default = readr::col_character()))
    for (col in colnames(out)) {
      if (stringr::str_detect(col, "date")) {
        out = out %>% mutate(!!col := as.Date(stringr::str_extract(out[[col]], "[0-9]{4}-[0-9]{2}-[0-9]{2}"), format = "%Y-%m-%d"))
      } else {
        out = out %>% mutate(!!col := type.convert(out[[col]], as.is=TRUE))
      }
    }
    attr(out,"paths") = c(path)
    return(out)
  },
    
  #' @description Load the CHESS dataset from a path
  #' 
  #' @param path - a path to the chess csv file
  #' @return raw CHESS data set
  getSARI = function() {
    path = self$getLatest(self$filter$sari)
    message("Using: ",path)
    out = readr::read_csv(self$fileProvider$getFile(path), col_types = readr::cols(.default = readr::col_character()))
    for (col in colnames(out)) {
      if (stringr::str_detect(col, "date")) {
        out = out %>% mutate(!!col := as.Date(stringr::str_extract(out[[col]], "[0-9]{4}-[0-9]{2}-[0-9]{2}"), format = "%Y-%m-%d"))
      } else {
        out = out %>% mutate(!!col := type.convert(out[[col]], as.is=TRUE))
      }
    }
    attr(out,"paths") = c(path)
    return(out)
  },
    
  #' @description Load Chess summary file
  #' 
  getCHESSSummary = function() {
    path = self$getLatest(self$filter$chessSummary)
    message("Using: ",path)
    chessSummary = readr::read_csv(self$fileProvider$getFile(path), col_types = readr::cols(
      DateRange = readr::col_date("%d-%m-%Y"),
      DateOfAdmission = readr::col_date("%d-%m-%Y"),
      YearofAdmission = readr::col_integer(),
      TrustName = readr::col_character(),
      Code = readr::col_character(),
      .default = readr::col_integer()))
    chessSummary = chessSummary %>% dplyr::select(-X67) %>% tidyr::pivot_longer(cols = c(everything(),-all_of(c("DateRange","DateOfAdmission","YearofAdmission","TrustName","Code","Total"))), names_to = "variable", values_to = "count")
    chessSummary = chessSummary %>% dplyr::filter(Code != "Total")
    tmp = chessSummary %>% dplyr::mutate(
      toAge = stringr::str_replace(variable,"^.*_([^_]+)$","\\1"),
      fromAge = stringr::str_replace(variable,"^.*_([^_]+)_[^_]+$","\\1"),
      variable = stringr::str_replace(variable,"^(.*)_[^_]+_[^_]+$","\\1")
    )
    tmp = tmp %>% dplyr::mutate(fromAge = ifelse(fromAge=="GreaterThanEqual", toAge, fromAge))
    tmp = tmp %>% dplyr::mutate(fromAge = ifelse(fromAge=="LessThan", 0, fromAge))
    chessSummary = tmp %>% dplyr::mutate(toAge = ifelse(fromAge==toAge, 120, toAge))
    attr(chessSummary,"paths") = c(path)
    return(chessSummary)
  },
    
  #' @description Load Sari summary file
  #' 
  getSARISummary = function(truncate = NULL,...) {
    path1 = self$getLatest(self$filter$sariSummaryArchive)
    path2 = self$getLatest(self$filter$sariSummaryCurrent)
    out = self$getSaved(id = "SARI-SUMMARY", params = list(path1,path2), ..., orElse = function (...) covidTimeseriesFormat({
      fn = function (path) {
        return(readr::read_csv(self$fileProvider$getFile(path), col_types = readr::cols(
          DateRange = readr::col_date("%d-%m-%Y"),
          DateOfAdmission = readr::col_date("%d-%m-%Y"),
          YearofAdmission = readr::col_integer(),
          TrustName = readr::col_character(),
          Code = readr::col_character(),
          .default = readr::col_integer()))
        )}
      tmp1 = fn(path1)
      tmp2 = fn(path2)
      sariSummary = bind_rows(tmp1 %>% anti_join(tmp2,by=c("DateOfAdmission","TrustName","Code")),tmp2)
      #sariSummary = sariSummary %>% group_by(DateOfAdmission,TrustName,Code) %>% arrange(desc(DateRange)) %>% filter(row_number() == 1) %>% ungroup()
      #browser()
      sariSummary = sariSummary %>% tidyr::pivot_longer(cols = c(everything(),-all_of(c("DateRange","DateOfAdmission","YearofAdmission","TrustName","Code"))), names_to = "variable", values_to = "count")
      sariSummary = sariSummary  %>% mutate(variable = stringr::str_to_lower(variable)) %>% group_by(DateOfAdmission,TrustName,Code,variable) %>% arrange(desc(count)) %>% filter(row_number() == 1) %>% ungroup()
      sariSummary = sariSummary %>% dplyr::filter(Code != "Total") #%>% mutate(count = ifelse(is.na(count),0,count))
      tmp = sariSummary %>% dplyr::mutate(
        toAge = stringr::str_replace(variable,"^.*_([^_]+)$","\\1"),
        fromAge = stringr::str_replace(variable,"^.*_([^_]+)_[^_]+$","\\1") #,
        #variable = str_replace(variable,"^(.*)_[^_]+_[^_]+$","\\1")
      )
      tmp = tmp %>% dplyr::mutate(ageCat = case_when(
          fromAge %>% stringr::str_detect("mos") ~ "<1",
          toAge %>% stringr::str_detect("mos") ~ "<1",
          fromAge=="greaterthanequal" ~ paste0(toAge,"+"), 
          fromAge=="lessthan" ~  paste0("<",toAge),
          fromAge=="45" ~ "45-54",
          toAge=="54" ~ "45-54", # 3 combinations in data 45-49, 45-54, 50-54 - merged into one category
          TRUE ~  paste0(fromAge,"-",toAge)
          ))
      tmp = tmp %>% dplyr::select(-fromAge,-toAge)
      sariSummary = tmp %>% mutate(
        type = case_when(
          stringr::str_detect(variable,"newhospitaladmissionswithacuterespiratoryinfection") ~ "background",
          stringr::str_detect(variable,"alladmittedpatientstestedforcovid19") ~ "background",
          stringr::str_detect(variable,"alladmittedpatientswithnewlabconfirmed") ~ "incidence",
          stringr::str_detect(variable,"newicu_hduadmissionswithacuterespiratoryinfection") ~ "background",
          stringr::str_detect(variable,"newlabconfirmedcovid19patientsonicu_hdu") ~ "incidence",
          stringr::str_detect(variable,"alllabconfirmedcovid19patientscurrentlyonicu_hdu") ~ "prevalence",
          TRUE ~ NA_character_
        ),
        statistic = case_when(
          stringr::str_detect(variable,"newhospitaladmissionswithacuterespiratoryinfection") ~ "hospital admission",
          stringr::str_detect(variable,"alladmittedpatientstestedforcovid19") ~ "test",
          stringr::str_detect(variable,"alladmittedpatientswithnewlabconfirmed") ~ "hospital admission",
          stringr::str_detect(variable,"newicu_hduadmissionswithacuterespiratoryinfection") ~ "icu admission",
          stringr::str_detect(variable,"newlabconfirmedcovid19patientsonicu_hdu") ~ "icu admission",
          stringr::str_detect(variable,"alllabconfirmedcovid19patientscurrentlyonicu_hdu") ~ "icu admission",
          TRUE ~ NA_character_
        ),
        subgroup = case_when(
          stringr::str_detect(variable,"icu") ~ "icu",
          TRUE ~ "hospital"
        ),
        note = variable
      )
      sariSummary = sariSummary %>% select(date = DateOfAdmission, name = TrustName, code = Code, value=count, ageCat,type,statistic,subgroup, note) %>% mutate(codeType = "NHS trust",gender=NA,source = "sari summary")
      sariSummary = sariSummary %>% group_by(date,code,name,codeType,ageCat,gender,source,subgroup,type,statistic) %>% summarise(note = paste0(note,collapse = "|"), value=sum(value,na.rm=TRUE), tmpCount = n())
      if (any(sariSummary$tmpCount > 1 & (sariSummary$ageCat != "<1" & sariSummary$ageCat != "45-54"))) warning("duplicates present in sari output where none were expected")
      sariSummary = sariSummary %>% self$fillAbsent() %>% self$fixDatesAndNames(truncate) # do;nt have any good info for reporting delay
      return(sariSummary %>% select(-tmpCount))
    }))
    attr(out,"paths") = c(path1,path2)
    return(out)
  },
    
  #### Get processed SPIM data ----
    
  
    
  #### DSTL files ----
  
  getFourNationsCases = function(truncate=NULL, ...) {
    path = self$getLatest(self$filter$fourNationsCases)
    message("Using: ",path)
    out = self$getSaved("SPIM-4-NATIONS", params = list(path), ..., orElse = function(...)  covidTimeseriesFormat({
      tmp = readxl::read_excel(self$fileProvider$getFile(path), sheet = "Extracted Data", col_types = "text", na = c("n/a",""))
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
      tmp3 = tmp2 %>% dplyr::select(-DateVal,-name.original, -variable) %>% mutate(ageCat=NA_character_,gender=NA_character_)
      tmp4 = tmp3 %>% filter(!is.na(value)) %>% self$fillAbsentByRegion() %>% self$fixDatesAndNames(truncate) %>% self$complete()
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
    attr(out,"paths") = c(path)
    return(out)
  },
    
  #' @description Load the SPI-M aggregated data spreadsheet
  #' @return a covidTimeseriesFormat dataframe
  # TODO: fix Couldn't match the following names: England: Unknown, England: Other, Golden Jubilee National Hospital, Velindre University NHS Trust
  getSPIMextract = function(truncate=NULL,...) {
    path = self$getLatest(self$filter$trust)
    message("Using: ",path)
    out = self$getSaved("SPIM-TRUST", params = list(path), ..., orElse = function (...) covidTimeseriesFormat({
      tmp = readxl::read_excel(self$fileProvider$getFile(path), sheet = "Extracted Data", col_types = "text", na = c("n/a",""))
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
          stringr::str_detect(source,"discharges") ~ "incidence",
          TRUE ~ NA_character_
        ),
        statistic = case_when(
          stringr::str_detect(source,"eath") ~ "death",
          stringr::str_detect(source,"icu") ~ "icu admission",
          stringr::str_detect(source,"osp") ~ "hospital admission",
          stringr::str_detect(source,"test") ~ "test",
          stringr::str_detect(source,"case") ~ "case",
          stringr::str_detect(source,"carehome") ~ "case",
          stringr::str_detect(source,"discharges") ~ "discharge",
          source == "positive_admissions_inpatients" ~ "hospital admission",
          TRUE ~ NA_character_
        ),
        subgroup=NA_character_,
      )
      
      # scotland weekly NRS has age breakdowm, and gender breakdown which causes duplication issues....
      # here we exclude them...
      tmp4 = tmp4 %>% filter(
          !(source == "nrs_weeklydeath" & (!is.na(ageCat) | !is.na(gender)))
        ) %>% filter(
          !is.na(statistic) &
            !is.na(type)
        )
      
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
      tmp7 = dplyr::bind_rows(tmp5,tmp6) %>% self$fillAbsentByRegion() %>% self$fixDatesAndNames(truncate) %>% self$complete()
      
      return(tmp7)
    }))
    attr(out,"paths") = c(path)
    return(out)
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


