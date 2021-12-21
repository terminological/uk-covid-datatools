#' SPIM private data
#' @import dbplyr
#' @export
SPIMDatasetProvider = R6::R6Class("SPIMDatasetProvider", inherit=CovidTimeseriesProvider, public = list(
  
  
    # directory=NULL,
    # 
    # initialize = function(providerController, path, ...) {
    #   self$directory = path.expand(path)
    #   super$initialize(providerController, ...)
    # },
    
  fileProvider=NULL,
  con=NULL,
  reproduceAt=NULL,
    
  initialize = function(providerController, fileProvider, ...) {
    self$fileProvider = fileProvider
    super$initialize(providerController, ...)
    self$reproduceAt = getOption("ukcovid.reproduce.at",Sys.Date())
  },
  
  finalize = function() {
    if(!identical(self$con,NULL)) {
      DBI::dbDisconnect(self$con)
    }
  },
  
  ### filters ----
  
  filter=list(
    chess="CHESS COVID19", #~/S3/encrypted/5Apr/NHS/CHESS COVID19 CaseReport 20200405.csv",
    sari="SARI COVID19 CaseReport",
    lineList="/Anonymised .*Line List [0-9]{8}",
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
    immunization = "immunisations SPIM",
    voc351 = "VOC202012_02_linelist",
    ctasLineList = "CTAS SGTF data.zip",
    vamLineList = "VAM line list",
    admissionsLineList = "SUS ECDS linked data",
    reinfections = "reinfection SPIM",
    travellers = "Modellers_rapid_travel"
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
  
  getLineListLatest = function() {
    return(self$getAllLatest(c("lineList","deathsLineList","sgene","immunization","admissionsLineList","ctasLineList","vamLineList")))
  },
  
  getAllLatest = function(filters = names(self$filter)) {
    tmp = bind_rows(lapply(filters, function(x) {
      filt = self$filter[[x]]
      return(tibble(path=self$getPaths() %>% stringr::str_subset(filt),name = x))
    }))
    
    tmp2Date = tmp$path %>% stringr::str_extract("/([^/]+)\\.") %>% stringr::str_extract_all("20[1-2][0-9]-?[0-1][0-9]-?[0-3][0-9]")
    tmp2Date = sapply(tmp2Date, function(x) {
      x = x %>% stringr::str_remove_all("-")
      y = suppressWarnings(unique(x[x==max(x)]))
      return(y)
    })
    tmp2Date = suppressWarnings(as.Date.character(tmp2Date, format="%Y%m%d",optional = TRUE))
    
    tmp = tmp %>% mutate(date = tmp2Date)
    
    reproDate = getOption("ukcovid.reproduce.at",Sys.Date())
    if (reproDate != Sys.Date()) message("Reproducing results from: ",reproDate," ( set options('ukcovid.reproduce.at'=NULL) to get the latest version )")
    
    tmp = tmp %>% filter(date<=reproDate)
    
    #tmp = tmp %>% group_by(name) %>% arrange(desc(date)) %>% filter(row_number()==1)
    tmp = tmp %>% group_by(name) %>% filter(date==max(date))
    tmp = tmp %>% mutate(path = paste0(self$directory,"/",path))
    return(tmp)
  },
    
  getLatest = function(search) {
    tmp2 = self$getAllLatest()
    tmp2 = tmp2 %>% 
      filter(stringr::str_detect(path,search)) %>%
      filter(!stringr::str_detect(path,"\\.docx")) %>%
      filter(!stringr::str_detect(path,"READ"))
    tmp3 = tmp2 %>% pull(path)
    
    if(length(tmp3)==0) {
      warning("Missing file: ",search)
      return(NA_character_)
    }
    if(length(tmp3)>1) {
      warning("Multiple matches: ",paste0(tmp3,collapse="; "))
      tmp3 = sort(tmp3,decreasing = TRUE)[[1]]
      warning("Using last: ",tmp3)
    }
    return(tmp3)
  },
    
  getNewerThan = function(search, date = as.Date("2020-01-01")) {
    reproDate = getOption("ukcovid.reproduce.at",Sys.Date())
    if (reproDate != Sys.Date()) message("Reproducing results from: ",reproDate," ( set options('ukcovid.reproduce.at'=NULL) to get the latest version )")
    return(self$getSpecificDates(search,(date+1):reproDate))
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
    return(self$getRawFile(path,to))
  },
  
  getRawFile = function(path, to = getwd()) {
    if(!stringr::str_ends(to,"/")) to = paste0(to,"/")
    dir.create(to, recursive = TRUE, showWarnings = FALSE)
    tmpFile = self$fileProvider$getZip(path, asFile=TRUE)
    fs::file_move(path = tmpFile,new_path = paste0(to,fs::path_file(tmpFile)))
    return(paste0(to,fs::path_file(path)))
  },
  
  #### Database functions ----
  # TODO: refactor this to PassthroughFilesystemCache
  getSQLLite = function(reset = FALSE, nocache=FALSE) {
    
    if (self$reproduceAt != getOption("ukcovid.reproduce.at",Sys.Date())) {
      self$reproduceAt = getOption("ukcovid.reproduce.at",Sys.Date())
      reset=TRUE
    }
    
    if (reset & !identical(self$con,NULL)) {
      try({DBI::dbDisconnect(self$con)})
      message("Switching database to reproduce results from: ",self$reproduceAt)
    }
    
    if (!reset & !identical(self$con,NULL)) return(self$con)
      
    dbname = self$getDatabaseName()
    sources = self$getLineListLatest()
    
    if(nocache) {
      warning("Full database rebuild requested: ",dbname)
      unlink(dbname)
      self$con = NULL
    }
    if( identical(self$con,NULL) || !DBI::dbIsValid(self$con) ) {
      self$con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbname, cache_size = -4*1024*1024, extended_types = TRUE, synchronous="off")
      tables = DBI::dbListTables(self$con)
      if (!("sources" %in% tables)) {
        dbplyr::db_copy_to(self$con, table="sources", values=sources, temporary=FALSE)
      }
    }
    return(self$con)
  },
  
  getDatabaseName = function() {
    sources = self$getLineListLatest()
    id = sources$date %>% max(na.rm = TRUE)
    dbname = paste0(self$wd, "/spim-",id,".sqlite3")
    return(dbname)
  },
  
  getTable = function(table, params=NULL,...,orElse,nocache=NULL, debug=NULL, con=self$getSQLLite(),  indexes=list(), uniqueIndexes=list()) {
    if(!identical(params,NULL)) {
      table = paste0(table,"_",digest::digest(params,algo="md5"))
      # digest::digest(deparse(orElse), algo="md5"))
    }
    if (identical(nocache,NULL)) {
      nocache = getOption("ukcovid.cache.disabled", self$nocache)
    }
    if (identical(debug,NULL)) {
      debug = getOption("ukcovid.cache.debug", self$debug)
    }
    tables = DBI::dbListTables(con)
    if (table %in% tables & nocache) {
      DBI::dbRemoveTable(con, table)
      tables = DBI::dbListTables(con)
    }
    if (table %in% tables) {
      if(debug) message("Using existing table: ",table)
      return(tbl(con,table))
    } else {
      if(debug) message("Executing query for table: ",table)
      orElse(con = con, table = table,...)
      lapply(indexes, function(index) {
        try({
          DBI::dbSendStatement(con,paste0("CREATE INDEX IF NOT EXISTS X_",table,"_",index," ON ",table," (",index,")")) %>% DBI::dbClearResult()
        })
      })
      lapply(uniqueIndexes, function(index) {
        try({
          DBI::dbSendStatement(con,paste0("CREATE UNIQUE INDEX IF NOT EXISTS X_",table,"_",index," ON ",table," (",index,")")) %>% DBI::dbClearResult()
        })
      })
    }
    tables = DBI::dbListTables(con)
    if (!(table %in% tables)) {
      stop("Table ",table, " not available")
    }
    return(tbl(con,table))
  },
  
  batchLoadTable = function(table, csvPath, wrangleBatch, indexes="FINALID", uniqueIndexes="record_id", ...) {
    self$getTable(table, ..., con=self$getSQLLite(), col_types = readr::cols(.default = readr::col_character()), indexes=indexes, uniqueIndexes = uniqueIndexes, orElse = function(con,table, ...) {
      message("Loading ",table," from ",csvPath)
      input = self$fileProvider$getZip(csvPath)
      writeToTable = function(x, pos) {
        #message(".",appendLF = pos %% 5000000 == 1)
        df = wrangleBatch(x) %>% mutate(record_id=as.integer(pos+row_number()-1))
        tables = DBI::dbListTables(con)
        if (!(table %in% tables)) {
          dbplyr::db_copy_to(con, table, values=df, temporary=FALSE)
        } else {
          DBI::dbAppendTable(con, table, df)
        }
      }
      readr::read_csv_chunked(file= input, callback = readr::SideEffectChunkCallback$new(writeToTable), chunk_size = 100000, ...)
    })
  },
  
  #### Line lists ----
  
  #' @description Load line list
  #' 
  #' @param path - path to the line list file
  #' @return raw line list data set
  getDeathsLineList = function(...) {
    path = self$getLatest(self$filter$deathsLineList)
    message("Using: ",path)
    tmp = self$getTable("deaths", ..., indexes="FINALID", uniqueIndexes="record_id", orElse = function (con,table, ...) {
      
      message("Loading deaths from ",path)
      input = self$fileProvider$getZip(path, asFile = TRUE)
      if (input %>% stringr::str_ends(".xlsx")) {
      
        tmp2 = readxl::read_excel(input, col_types = "text")
        datecols = c(colnames(tmp2) %>% stringr::str_subset("date"),"dod")
        for(datecol in datecols) {
          tmp2[[datecol]] = suppressWarnings(as.Date(as.numeric(tmp2[[datecol]]),"1899-12-30"))
        }
      
      } else {
        
        tmp2 = readr::read_csv(input, col_types = readr::cols(.default = readr::col_character()))
        datecols = c(colnames(tmp2) %>% stringr::str_subset("date"),"dod")
        for(datecol in datecols) {
          tmp2[[datecol]] = as.Date(tmp2[[datecol]], "%d/%m/%Y")
        }
        
      }
      
      tmp2 = tmp2 %>% 
        self$normaliseDemographics() %>%
        dplyr::mutate(
          age = as.integer(age),
          record_id = as.integer(row_number()),
          FINALID = as.integer(finalid)
        ) %>% select(-finalid)
      
      #tmp2 = tmp2 %>% mutate(across(where(lubridate::is.Date), ~ as.POSIXct(.x)))
      #tmp2 = tmp2 %>% mutate(across(where(lubridate::is.Date), ~ as.integer(.x)+2458849.5-18262))
      tmp2 = tmp2 %>% mutate(across(where(lubridate::is.Date), ~ format(.x, "%Y-%m-%d")))
      
      dbplyr::db_copy_to(con, table, values=tmp2, temporary=FALSE)
      #DBI::dbAppendTable(con,table,tmp2)
      
    })
    
    attr(tmp,"paths") = path
    return(tmp)
  },
  
  getVAMLineList = function(...) {
    path = self$getLatest(self$filter$vamLineList)
    tmp = self$batchLoadTable("genomics", csvPath = path, ...,  wrangleBatch = function (tmp2) {
      
      datecols = colnames(tmp2)[colnames(tmp2) %>% stringr::str_detect("date|_at")]
      for(datecol in datecols) {
        tmp2[[datecol]] = suppressWarnings(as.Date(tmp2[[datecol]],"%Y-%m-%d"))
      }
      idcols = colnames(tmp2)[colnames(tmp2) %>% stringr::str_detect("id")]
      for(idcol in idcols) {
        tmp2[[idcol]] = suppressWarnings(as.integer(tmp2[[idcol]]))
      }
      tmp2 = tmp2 %>% 
        self$normaliseDemographics() %>%
        mutate(
          age = suppressWarnings(as.integer(age)),
          FINALID = as.integer(finalid)
      ) %>% select(-finalid)
      
      tmp2 = tmp2 %>% mutate(across(where(lubridate::is.Date), ~ format(.x, "%Y-%m-%d")))
      
      return(tmp2)
    })
    attr(tmp,"paths") = path
    return(tmp)
  },
  
  getTravellerLineList = function(...) {
    path = self$getLatest(self$filter$travellers)
    tmp = self$batchLoadTable("travellers", csvPath = path, ...,  wrangleBatch = function (tmp2) {
      
      datecols = colnames(tmp2)[colnames(tmp2) %>% stringr::str_detect("date|_at")]
      for(datecol in datecols) {
        tmp2[[datecol]] = suppressWarnings(as.Date(tmp2[[datecol]],"%Y%m%d"))
      }
      idcols = colnames(tmp2)[colnames(tmp2) %>% stringr::str_detect("id")]
      for(idcol in idcols) {
        tmp2[[idcol]] = suppressWarnings(as.integer(tmp2[[idcol]]))
      }
      tmp2 = tmp2 %>% 
        mutate(
          FINALID = as.integer(finalid)
        ) %>% select(-finalid)
      
      tmp2 = tmp2 %>% mutate(across(where(lubridate::is.Date), ~ format(.x, "%Y-%m-%d")))
      
      return(tmp2)
    })
    attr(tmp,"paths") = path
    return(tmp)
  },
  
  getReinfectionLineList = function(...) {
    path = self$getLatest(self$filter$reinfections)
    out = self$batchLoadTable("reinfections", csvPath = path, ..., wrangleBatch = function (tmp) {
        tmp = tmp %>% 
          dplyr::mutate(
            specimen_date = as.Date(SPECIMEN_DATE), #maybeDMYorMDY(SPECIMEN_DATE)
          ) 
        #if(any(is.na(tmp$specimen_date))) warning("NA specimen dates in cases file")
        tmp = tmp %>% filter(!is.na(specimen_date))
        tmp = tryCatch({
          tmp %>% rename(FINALID = finalid_primary_case)
        }, error=function (e) {
          tmp %>% rename(FINALID = finalid)
        })
        tmp = tmp %>% 
          rename(
            NHSER_code = nhser_code,
            NHSER_name = nhser_name,
            PHEC_code = phec_code,
            PHEC_name = phec_name,
            UTLA_code = utla_code,
            ULTA_name = utla_name,
            LTLA_code = ltla_code,
            LTLA_name = ltla_name,
            asymptomatic_indicator = Asymptomatic_Indicator
          )
        tmp = tmp %>% 
          self$normaliseDemographics() %>%
          mutate(
            age = suppressWarnings(as.numeric(age)),
            FINALID = as.integer(FINALID),
            imd_rank = as.integer(imd_rank),
            imd_decile = as.integer(imd_decile)
          ) %>% select(-sex, -SPECIMEN_DATE)
        
        tmp = tmp %>% mutate(across(where(lubridate::is.Date), ~ format(.x, "%Y-%m-%d")))
        
        return(tmp)
    })
    attr(out,"paths") = path  
    return(out)
  },
  
  getCTASLineList = function(...) {
    #/home/terminological/S3/encrypted/2021-03-29/20210329 CTAS SGTF data.zip
    path = self$getLatest(self$filter$ctasLineList)
    tmp = self$batchLoadTable("ctas", csvPath=path, ...,  wrangleBatch = function (tmp2) {
      
      datecols = colnames(tmp2)[colnames(tmp2) %>% stringr::str_detect("date|_at")]
      for(datecol in datecols) {
        tmp2[[datecol]] = suppressWarnings(as.Date(tmp2[[datecol]],"%Y-%m-%d"))
      }
      idcols = colnames(tmp2)[colnames(tmp2) %>% stringr::str_detect("id")]
      for(idcol in idcols) {
        tmp2[[idcol]] = suppressWarnings(as.integer(tmp2[[idcol]]))
      }
      tmp2 = tmp2 %>% 
        self$normaliseDemographics() %>%
        mutate(
          completed = as.logical(completed),
          sgtf = as.integer(sgtf),
          sgtf_under30ct = as.integer(sgtf_under30ct),
          p2ch1cq = as.double(p2ch1cq),
          p2ch2cq = as.double(p2ch2cq),
          p2ch3cq = as.double(p2ch3cq),
          p2ch4cq = as.double(p2ch4cq),
          age = as.integer(age),
          FINALID = ifelse(is.na(sgtf_finalid),sgtf_finalid,genomic_finalid)
        )
      
      tmp2 = tmp2 %>% mutate(across(where(lubridate::is.Date), ~ format(.x, "%Y-%m-%d")))
      
      return(tmp2)
    })
    attr(tmp,"paths") = path
    return(tmp)
  },
  
  #' @description Load immunizations line list
  #' 
  #' @return raw line list data set
  getVaccinationLineList = function(...) {
    path = self$getLatest(self$filter$immunization)
    out = self$batchLoadTable("vaccinations", csvPath = path, ...,  wrangleBatch = function(tmp) {
      
      tmp = tmp %>% 
        self$normaliseDemographics() %>%
        mutate(
          age = suppressWarnings(as.numeric(age)),
          FINALID = suppressWarnings(as.integer(finalid)),
          finalid2 = suppressWarnings(as.integer(finalid2)),
          vaccination_date = as.Date(vaccination_date, tryFormats = c("%Y-%m-%d","%d%b%Y"), optional=TRUE),
          clinically_vulnerable = suppressWarnings(as.integer(Clinically_Extremely_Vulnerable)), 
          covid_risk = suppressWarnings(as.integer(COVID19_AtRisk)), 
          nhs_worker = suppressWarnings(as.integer(NHS_worker))
        ) %>%
        select(-finalid,-Clinically_Extremely_Vulnerable,-COVID19_AtRisk,-NHS_worker)
      
      tmp = tmp %>% mutate(across(where(lubridate::is.Date), ~ format(.x, "%Y-%m-%d")))
      
      return(tmp)
      
    })
    attr(out,"paths") = path
    return(out)
  },
  
  #' @description Load line list
  #' 
  #' @return raw line list data set
  getSGeneLineList = function(...) {
    path = self$getLatest(self$filter$sgene)
    tmp = self$batchLoadTable("sgene", csvPath = path, ..., wrangleBatch = function (tmp) {
      tmp = tmp %>% 
        self$normaliseDemographics() %>%
        mutate(
          age = suppressWarnings(as.integer(age)),
          FINALID = suppressWarnings(as.integer(FINALID)),
          specimen_date = as.Date(specimen_date, tryFormats = c("%Y-%m-%d","%d%b%Y"), optional=TRUE),
          sgtf = as.integer(sgtf),
          sgtf_under30CT = as.integer(sgtf_under30CT),
          P2CH1CQ = as.numeric(P2CH1CQ),
          P2CH2CQ = as.numeric(P2CH2CQ),
          P2CH3CQ = as.numeric(P2CH3CQ),
          P2CH4CQ = as.numeric(P2CH4CQ),
        )
      
      tmp = tmp %>% mutate(across(where(lubridate::is.Date), ~ format(.x, "%Y-%m-%d")))
      
      return(tmp %>% select(-sex))
    })
    attr(tmp,"paths") = path
    return(tmp)
  },
  
  #' @description Load line list
  #' 
  #' @return raw line list data set
  getLineList = function(...) {
    path = self$getLatest(self$filter$lineList)
    out = self$batchLoadTable("cases", csvPath = path, ..., wrangleBatch = function (tmp) {
      tmp = tmp %>% 
          dplyr::mutate(
            Onsetdate = maybeDMYorMDY(Onsetdate),
            specimen_date = maybeDMYorMDY(specimen_date),
            lab_report_date = maybeDMYorMDY(lab_report_date)
          ) 
      if(any(is.na(tmp$specimen_date))) warning("NA specimen dates in cases file")
      if ("finalid" %in% colnames(tmp)) tmp = tmp %>% rename(FINALID = finalid)
      
      tmp = tmp %>% 
        self$normaliseDemographics() %>%
        mutate(
          pillar_2_testingkit = tolower(pillar_2_testingkit),
          age = suppressWarnings(as.numeric(age)),
          FINALID = as.integer(FINALID),
          imd_rank = as.integer(imd_rank),
          imd_decile = as.integer(imd_decile)
        ) %>% select(-sex)
      
      tmp = tmp %>% mutate(across(where(lubridate::is.Date), ~ format(.x, "%Y-%m-%d")))
      
      return(tmp)
    })
    attr(out,"paths") = path
    return(out)
    # TODO: https://github.com/sarahhbellum/NobBS
  },
  
  getAdmissionLineList = function(...) {
    path = self$getLatest(self$filter$admissionsLineList)
    
    hrgs = self$codes$getHRGCodes()
    covidHrg = hrgs %>% filter(code %>% stringr::str_starts("DX")) %>% mutate(covid_hrg = 1L) %>% select(spell_core_hrg = code, covid_hrg)
    respiratoryInfectionHrg = hrgs %>% filter(code %>% stringr::str_starts("DZ11|DZ22|DZ23|DZ27")) %>% mutate(resp_hrg = 1L) %>% select(spell_core_hrg = code, resp_hrg)
    
    tmp = self$batchLoadTable("admissions", csvPath = path, ...,  wrangleBatch = function (tmp2) {
      
      datecols = colnames(tmp2)[colnames(tmp2) %>% stringr::str_detect("date|_in|_out|wk_start")]
      for(datecol in datecols) {
        tmp2[[datecol]] = suppressWarnings(as.Date(tmp2[[datecol]],tryFormats = c("%Y-%m-%d","%d%b%Y"), optional=TRUE))
      }
      
      # "sex","agegrp","ethnicity_final","linkset","hoslink","specimen_date","pillar","hospital_event_rank","mega_spell_id","diag_n","charlson_score","charlson_index","spell_start_date","spell_end_date","spell_core_hrg","arrival_date","departure_date","proxy_missing","hospital_in","hospital_out","currently_admitted","diff_pos_admit","diff_pos_discharge","pos_in_hospital","still_in_hospital","pos14_order","inpatient_stay","covidICD","length_of_stay","ecds_discharge","admit_source","admit_method","discharge_method","discharge_destination","discharge_destination_desc","discharge_destination_grp","onset_category","onset_category_simple","valid_length_of_stay","provider_code","trust_name","nhs_region","trust_type","wk_start","wk","FINALID","record_id"
      intcols = c("hospital_event_rank","diag_n","charlson_score","charlson_index","proxy_missing","currently_admitted","diff_pos_admit","diff_pos_discharge",
                  "pos14_order","inpatient_stay","covidICD","length_of_stay",                 "valid_length_of_stay")
      for(intcol in intcols) {
        tmp2[[intcol]] = suppressWarnings(as.integer(tmp2[[intcol]]))
      }
      tmp2 = tmp2 %>% mutate(
        FINALID = as.integer(final_id)
      ) %>% select(-final_id)
      
      tmp2 = tmp2 %>%
        self$normaliseDemographics() %>%
        left_join(covidHrg, by = "spell_core_hrg") %>% 
        left_join(respiratoryInfectionHrg, by = "spell_core_hrg") %>%
        mutate(covid_hrg = case_when(
          !is.na(covid_hrg) ~ covid_hrg,
          !is.na(spell_core_hrg) ~ 0L,
          TRUE ~ NA_integer_
        )) %>% 
        mutate(resp_hrg = case_when(
          !is.na(resp_hrg) ~ resp_hrg,
          !is.na(spell_core_hrg) ~ 0L,
          TRUE ~ NA_integer_
        )) %>% 
        mutate(discharge_method_cat = case_when(
          discharge_method == "1" ~ "Discharged",
          discharge_method == "2" ~ "Discharged",
          discharge_method == "3" ~ "Discharged",
          discharge_method == "4" ~ "Died",
          discharge_method == "5" ~ "Died",
          discharge_method == "6" ~ "Discharged",
          discharge_method == "7" ~ "Discharged",
          discharge_method == "8" ~ "Not applicable",
          discharge_method == "9" ~ "Unknown",
          TRUE ~ NA_character_
        )) %>%
        mutate(admit_method_cat = case_when(
          admit_method=="11" ~ "Elective",
          admit_method=="12" ~ "Elective",
          admit_method=="13" ~ "Elective",
          admit_method=="21" ~ "Emergency",
          admit_method=="22" ~ "Emergency",
          admit_method=="23" ~ "Emergency",
          admit_method=="24" ~ "Emergency",
          admit_method=="25" ~ "Emergency",
          admit_method=="2A" ~ "Emergency",
          admit_method=="2B" ~ "Emergency",
          admit_method=="2C" ~ "Emergency",
          admit_method=="2D" ~ "Emergency",
          admit_method=="28" ~ "Emergency",
          admit_method=="31" ~ "Maternity",
          admit_method=="32" ~ "Maternity",
          admit_method=="82" ~ "Other",
          admit_method=="83" ~ "Other",
          admit_method=="81" ~ "Other",
          admit_method=="98" ~ "Not applicable",
          admit_method=="99" ~ "Not known",
          TRUE ~ NA_character_
        )) %>%
        mutate(admit_source_cat = case_when(
          admit_source=="19" ~ "Usual residence",
          admit_source=="29" ~ "Temporary residence",
          admit_source=="51" ~ "Emergency Dept",
          admit_source=="52" ~ "Maternity",
          admit_source=="54" ~ "Care home",
          admit_source=="65" ~ "Care home",
          admit_source=="85" ~ "Care home",
          admit_source=="98" ~ "Not applicable",
          is.na(admit_source) ~ NA_character_,
          TRUE ~ "Other"
        )) %>%
        mutate(discharge_destination_cat = case_when(
          discharge_destination=="19" ~ "Usual residence",
          discharge_destination=="29" ~ "Temporary residence",
          discharge_destination=="51" ~ "Other hospital",
          discharge_destination=="52" ~ "Other hospital",
          discharge_destination=="53" ~ "Other hospital",
          discharge_destination=="87" ~ "Other hospital",
          discharge_destination=="54" ~ "Care home",
          discharge_destination=="65" ~ "Care home",
          discharge_destination=="85" ~ "Care home",
          discharge_destination=="98" ~ "Not applicable",
          is.na(admit_source) ~ NA_character_,
          TRUE ~ "Other"
        ))
      #   admit_method=="11" ~ "Elective Admission: Waiting list",
      # admit_method=="12" ~ "Elective Admission: Booked",
      # admit_method=="13" ~ "Elective Admission: Planned",
      # admit_method=="21" ~ "Emergency Admission: Emergency Care Department or dental casualty department of the Health Care Provider  ",
      # admit_method=="22" ~ "Emergency Admission: GENERAL PRACTITIONER: after a request for immediate admission has been made direct to a Hospital Provider, i.e. not through a Bed bureau, by a GENERAL PRACTITIONER or deputy",
      # admit_method=="23" ~ "Emergency Admission: Bed bureau",
      # admit_method=="24" ~ "Emergency Admission: Consultant Clinic, of this or another Health Care Provider  ",
      # admit_method=="25" ~ "Emergency Admission: Admission via Mental Health Crisis Resolution Team",
      # admit_method=="2A" ~ "Emergency Admission: Emergency Care Department of another provider where the PATIENT  had not been admitted",
      # admit_method=="2B" ~ "Emergency Admission: Transfer of an admitted PATIENT from another Hospital Provider in an emergency",
      # admit_method=="2C" ~ "Emergency Admission: Baby born at home as intended"
      # admit_method=="2D" ~ "Emergency Admission: Other emergency admission",
      # admit_method=="28" ~ "Emergency Admission: Other means",
      # admit_method=="31" ~ "Maternity Admission: Admitted ante partum",
      # admit_method=="32" ~ "Maternity Admission: Admitted post partum",
      # admit_method=="82" ~ "Other Admission: The birth of a baby in this Health Care Provider",
      # admit_method=="83" ~ "Other Admission: Baby born outside the Health Care Provider except when born at home as intended",
      # admit_method=="81" ~ "Other Admission: Transfer of any admitted PATIENT from other Hospital Provider other than in an emergency",
      # admit_method=="98" ~ "Not applicable",
      # admit_method=="99" ~ "ADMISSION METHOD  not known"
        
      tmp2 = tmp2 %>% mutate(across(where(lubridate::is.Date), ~ format(.x, "%Y-%m-%d")))
      
      return(tmp2)
    })
    attr(tmp,"paths") = path
    return(tmp)
  },
  
  ## Filtered line lists ----
  
  getLinkedVaccinations = function(..., immunisations = self$getVaccinationLineList()) {
    linked_vaccination = self$getTable("linked_vaccination", ..., orElse = function (con,table, ...) {
      
      tmp = immunisations %>% filter(!is.na(FINALID)) %>% select(-finalid2) %>% rename(from_record_id = record_id) %>% union(
        immunisations %>% filter(!is.na(finalid2)) %>% select(-FINALID) %>% rename(FINALID = finalid2, from_record_id = record_id)
      ) %>% mutate(record_id = row_number())
      
      tmp %>% compute(indexes=list("record_id","FINALID"), temporary=FALSE, name=table)
      
    })
  },
  
  getLinkedCtas = function(..., ctas = self$getCTASLineList()) {
    linked_ctas = self$getTable("linked_ctas", ..., orElse = function (con,table, ...) {
      tmp = ctas %>% filter(!is.na(FINALID))
      tmp %>% compute(unique_indexes=list("record_id"), indexes=list("FINALID"), temporary=FALSE, name=table)
    })
  },
  
  getLinkedDeaths = function(..., deaths = self$getDeathsLineList()) {
    linked_deaths = self$getTable("linked_deaths", ..., orElse = function (con,table, ...) {
      tmp = deaths %>% filter(!is.na(FINALID))
      tmp %>% compute(unique_indexes=list("record_id"), indexes=list("FINALID"), temporary=FALSE, name=table)
    })
  },
  
  getLinkedGenomics = function(..., genomics = self$getVAMLineList()) {
    linked_genomics = self$getTable("linked_genomics", ..., orElse = function (con,table, ...) {
      tmp = genomics %>% filter(!is.na(FINALID))
      tmp %>% compute(unique_indexes=list("record_id"), indexes=list("FINALID"), temporary=FALSE, name=table)
    })
  },
  
  ## Genomics ----
  
  getCombinedGenomics = function(...,
     variant_designation = ukcovidtools::variantDesignation,
     linked_genomics = self$getLinkedGenomics(),
     linked_ctas = self$getLinkedCtas(),
     ignore_missing = FALSE
  ) {
    
    combined_genomics = self$getTable("combined_genomics", ..., orElse = function (con,table, ...) {
      # if("variant_designation" %in% DBI::dbListTables(con)) {
      #   DBI::dbRemoveTable(con,"variant_designation")
      # }
      # db_copy_to(con, table="variant_designation", values = variant_designation)
      # variant_designation
      
      fromVam = linked_genomics %>%
        select(
          record_id,
          FINALID,
          date = specimen_date_sk,
          phe_name = vam,
          exposure_type,
          seq_result
        ) %>%
        mutate(
          source = "linked_genomics"
        ) %>% compute(temporary = TRUE, indexes = list(c("FINALID","date")))
      
      fromCtas =  linked_ctas %>% 
        filter(!is.na(genomic_variant)) %>% 
        # exclude records already found. We don't trust CTAS dates for genomics
        left_join(fromVam %>% select(FINALID) %>% mutate(tmp=1), by="FINALID") %>% 
        filter(is.na(tmp)) %>% 
        select(
          record_id,
          FINALID,
          date = genomic_specimen_date,
          phe_name = genomic_variant,
          exposure_type = genomic_exp_type,
          seq_result = genomic_seq_result
        ) %>%
        mutate(
          source = "linked_ctas"
        ) %>% 
        compute(temporary = TRUE)
      
      combined = fromVam %>% 
        union(fromCtas) %>% 
        mutate(phe_name = replace(replace(phe_name,"VOC-",""),"VUI-","")) %>%
        left_join(variant_designation %>% mutate(matched=1), by="phe_name",copy=TRUE) %>%
        compute(indexes=list("FINALID","date","phe_name","who_name",c("record_id","source")), temporary=FALSE, name=table)
      
      if(!ignore_missing & combined %>% filter(is.na(matched)) %>% count() %>% pull(n)>0) {
        stop("An unmatched PHE variant was detected. Please update 'variant_designation'")
      }
    })
  },
  
  getCombinedSGene = function(...,
      sgene = self$getSGeneLineList(),
      linked_ctas = self$getLinkedCtas()
  ) {
    
    combined_sgene = self$getTable("combined_sgene", ..., orElse = function (con,table, ...) {
      # if("variant_designation" %in% DBI::dbListTables(con)) {
      #   DBI::dbRemoveTable(con,"variant_designation")
      # }
      # db_copy_to(con, table="variant_designation", values = variant_designation)
      # variant_designation
      
      fromSgene = sgene %>%
        select(
          record_id,
          FINALID,
          date = specimen_date,
          Specimen_Number,
          sgtf,
          sgtf_under30CT,
          P2CH3CQ,
          P2CH1CQ,
          P2CH2CQ,
          P2CH4CQ
        ) %>%
        mutate(
          source = "sgene"
        ) %>% compute(temporary = TRUE, indexes = list("Specimen_Number"))
      
      fromCtas =  linked_ctas %>% 
        left_join(fromSgene %>% select(Specimen_Number) %>% mutate(tmp=1), by=c("sgtf_specimen_number"="Specimen_Number")) %>% 
        filter(is.na(tmp)) %>% 
        select(
          record_id,
          FINALID,
          date = sgtf_specimen_date,
          Specimen_Number = sgtf_specimen_number,
          sgtf,
          sgtf_under30CT = sgtf_under30ct,
          P2CH3CQ = p2ch3cq,
          P2CH1CQ = p2ch1cq,
          P2CH2CQ = p2ch2cq,
          P2CH4CQ = p2ch4cq
        ) %>%
        mutate(
          source = "linked_ctas"
        ) %>% 
        compute(temporary = TRUE)
      
      combined = fromSgene %>% 
        union(fromCtas) %>% 
        compute(indexes=list("FINALID","date","sgtf_under30CT",c("record_id","source")), temporary=FALSE, name=table)
      
    })
  },
  
  #### Linkage analysis ----
  
  getSimplePersonIndex = function(
    ...,
    cases = self$getLineList()
  ) {
    mpi = self$getTable("person_simple", ..., orElse = function (con,table, ...) {
      cases %>% 
        mutate(
          yob = year(specimen_date)-age,
          clinically_vulnerable = NA_integer_, 
          covid_risk = NA_integer_, 
          nhs_worker = NA_integer_
        ) %>%
        select(
          FINALID,
          NHSER_code,
          LTLA_code,
          yob,
          gender,
          ethnic_cat,
          ethnic_subcat,
          residential_category,
          imd_decile,
          imd_rank,
          vaccine_linked,
          clinically_vulnerable, 
          covid_risk, 
          nhs_worker
      ) %>%
      compute(name=table, 
          unique_indexes=list("FINALID"), 
          indexes=list("yob","gender","ethnic_cat","NHSER_code","LTLA_code","ethnic_subcat","residential_category"))
      
    })
  },
  
  getLsoaPatientIndex = function(..., mpi = self$getSimplePersonIndex()) {
    lsoaMap = self$getTable("lsoa_lookup", ..., orElse = function(con,table,...) {
      imd = self$demog$getIMDData()
      imd = imd %>% select(
        LTLA_code = `Local Authority District code (2019)`,
        imd_rank = `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`,
        LSOA_code = `LSOA code (2011)`)
      imdTbl = copy_to(con, df=imd, name=table, overwrite=TRUE, 
                       unique_indexes=list(c("LTLA_code","imd_rank")))
      return(imdTbl)
    })
    return(mpi %>% left_join(lsoaMap, by=c("LTLA_code","imd_rank")))
  },
  
  # TODO: work out how to split MPI function to 2 parts:
  # A linkage QA function which does merging analysis and looks at conflicts and blacklists 
  # poorly linked FINALIDS from specific tables. This would need to be integrated into the
  # timeline analysis to prevent timelines incorporating data from those unreliably linked tables
  
  getMasterPersonIndex = function(
    ...,
    admissions = self$getAdmissionLineList(),
    cases = self$getLineList(),
    linked_ctas = self$getLinkedCtas(),
    linked_deaths = self$getLinkedDeaths(),
    linked_genomics = self$getLinkedGenomics(),
    sgene = self$getSGeneLineList(),
    linked_vaccinations = self$getLinkedVaccinations()
  ) {
    mpi = self$getTable("person", ..., orElse = function (con,table, ...) {
      
      ## Person details ---- 
      
      decodeTablecode = function(df) {
        # Doesn't need to be collected
        df %>% collect() %>% mutate(
          admissions = bitwAnd(as.integer(tablecode),1L) > 0,
          cases = bitwAnd(as.integer(tablecode),2L) > 0,
          ctas = bitwAnd(as.integer(tablecode),4L) > 0,
          deaths = bitwAnd(as.integer(tablecode),8L) > 0,
          genomics = bitwAnd(as.integer(tablecode),16L) > 0,
          sgene = bitwAnd(as.integer(tablecode),32L) > 0,
          vaccinations = bitwAnd(as.integer(tablecode),64L) > 0
        )
      }
      
      ## Ids ----
      
      allIds = 
        admissions %>% select(FINALID) %>% mutate(tablecode = 1L) %>% 
        union(cases %>% select(FINALID) %>% mutate(tablecode = 2L)) %>% 
        union(linked_ctas %>% select(FINALID) %>% mutate(tablecode = 4L)) %>% 
        union(linked_deaths %>% select(FINALID) %>% mutate(tablecode = 8L)) %>% 
        union(linked_genomics %>% select(FINALID) %>% mutate(tablecode = 16L)) %>% 
        union(sgene %>% select(FINALID) %>% mutate(tablecode = 32L)) %>% 
        union(linked_vaccinations %>% select(FINALID) %>% mutate(tablecode = 64L)) %>%
        group_by(FINALID) %>% summarise(tablecode = sum(tablecode, na.rm=TRUE)) %>%
        compute(unique_indexes=list("FINALID"))
      
      tmp = allIds %>% group_by(tablecode) %>% count() %>% decodeTablecode()
      
      message("Writing linkage count data to:",paste0(self$todayWd,"/linkage-counts.csv"))
      readr::write_csv(tmp, paste0(self$todayWd,"/linkage-counts.csv"))
      
      ## Ages ----
      
      allAges = 
        cases %>% rename(date=specimen_date) %>% mutate(yob = year(date)-age, tablecode=2L) %>% select(FINALID,yob,tablecode) %>% 
        union(linked_genomics %>% rename(date=specimen_date_sk) %>% mutate(yob = year(date)-age, tablecode=16L) %>% select(FINALID,yob,tablecode)) %>% 
        union(linked_ctas %>% rename(date=date_created) %>% mutate(yob = year(date)-age, tablecode=4L) %>% select(FINALID,yob,tablecode)) %>% 
        union(linked_vaccinations  %>% rename(date=vaccination_date) %>% mutate(yob = year(date)-age, tablecode=64L) %>% select(FINALID,yob,tablecode)) %>%
        group_by(FINALID) %>%
        mutate(mean_yob = mean(yob,na.rm = TRUE)) %>%
        ungroup() %>%
        compute(indexes = list("FINALID","yob"))
      
      ageAgree = allAges %>% 
        filter(abs(yob-mean_yob) <= 2) %>%
        group_by(FINALID) %>%
        summarise(
          yob = as.integer(mean(yob,na.rm = TRUE))
        ) %>%
        compute(indexes = list("FINALID"))
      
      ageConflict = allAges %>% 
        filter(abs(yob-mean_yob) > 2) %>%
        select(-mean_yob) %>%
        distinct() %>% 
        group_by(FINALID, yob) %>%
        summarise(
          tablecode = sum(tablecode,na.rm=TRUE)
        ) %>%
        compute()
      
      ## Merge functions ----
      
      uniqueValues = function(table, column) {
        column = ensym(column)
        if(!as_label(column) %in% colnames(table)) {
          df = table %>% mutate(!!column := NA_character_)
        } else {
          df = table
        } 
        columnname = as_label(column)
        tablename = as.character(table$ops$x)
        tablecode = case_when(
          tablename == "admissions" ~ 1L,
          tablename == "cases" ~ 2L,
          tablename == "linked_ctas" ~ 4L,
          tablename == "linked_deaths" ~ 8L,
          tablename == "linked_genomics" ~ 16L,
          tablename == "sgene" ~ 32L,
          tablename == "linked_vaccination" ~ 64L,
          TRUE ~ NA_integer_
        )
        browser(expr = is.na(tablecode))
        df %>% filter(!is.na(FINALID)) %>% filter(!is.na(!!column)) %>% select(FINALID, value = !!column) %>% distinct() %>% mutate(tablecode = tablecode) %>% compute(unique_indexes=list(c("FINALID","value"))) %>% return()
      }
      
      mergeValue = function(columnName, ...) {
        union = NULL
        dots = rlang::list2(...)
        for (table in dots) {
          # tablename = as.character(table$ops$x)
          # colname = paste0("value.",tablename)
          
          if(identical(union,NULL)) {
            union = table
          } else {
            union = union %>% union(table)
          }
        }
        #browser()
        allValues = union %>% compute(indexes = list(c("FINALID","value")),temporary = TRUE) %>% group_by(FINALID,value) %>% summarise(tablecode = sum(tablecode,na.rm=TRUE)) %>% ungroup() %>% compute(indexes = list("FINALID"))
        agreements = allValues %>% group_by(FINALID) %>% filter(n() == 1) %>% rename(!!columnName := value) %>% select(-tablecode) %>% ungroup() %>% compute()
        disagreements = allValues %>% group_by(FINALID) %>% filter(n() > 1)  %>% select(FINALID) %>% distinct()  %>% ungroup() %>% compute(temporary=TRUE)
        conflicts = allValues %>% rename(!!columnName := value) %>% inner_join(disagreements, by="FINALID") %>% compute()
        return(list(agree = agreements, conflicts = conflicts))
      }
      
      ## Geography ----
      
      NHSER = mergeValue(
        "NHSER_code",
        uniqueValues(cases, NHSER_code),
        uniqueValues(linked_deaths, nhser_code)
      )
      
      LTLA = mergeValue(
        "LTLA_code",
        uniqueValues(cases, LTLA_code),
        uniqueValues(linked_deaths, ltla_code),
        uniqueValues(linked_vaccinations, ltla_code),
        uniqueValues(linked_genomics, ltlacode)
        # ctas has ltlanm
      )
      
      ## Demographics ----
      
      gender = mergeValue(
        "gender",
        uniqueValues(cases, gender),
        uniqueValues(linked_deaths, gender),
        uniqueValues(linked_vaccinations, gender),
        uniqueValues(admissions, gender),
        uniqueValues(linked_ctas, gender),
        uniqueValues(linked_genomics, gender)
      )
      
      ethnic_cat = mergeValue(
        "ethnic_cat",
        uniqueValues(cases, ethnic_cat),
        uniqueValues(linked_deaths, ethnic_cat),
        uniqueValues(linked_vaccinations, ethnic_cat),
        uniqueValues(admissions, ethnic_cat),
        uniqueValues(linked_ctas, ethnic_cat),
        uniqueValues(linked_genomics, ethnic_cat)
      )
      
      ethnic_subcat = mergeValue(
        "ethnic_subcat",
        uniqueValues(cases, ethnic_subcat),
        uniqueValues(linked_deaths, ethnic_subcat),
        uniqueValues(linked_vaccinations, ethnic_subcat),
        uniqueValues(admissions, ethnic_subcat),
        uniqueValues(linked_ctas, ethnic_subcat),
        uniqueValues(linked_genomics, ethnic_subcat)
      )
      
      residential_category = mergeValue(
        "residential_category",
        uniqueValues(cases, residential_category),
        uniqueValues(linked_deaths, residential_category),
      )
      
      ## Versions without vaccaintion linkage problems ----
      # TODO: most recent LTLA & NHSER rather than setting LTLA to NULL if disagreement.
      
      LTLA_restricted = mergeValue(
        "LTLA_code",
        uniqueValues(cases, LTLA_code),
        uniqueValues(linked_deaths, ltla_code),
        uniqueValues(linked_genomics, ltlacode)
        # ctas has ltlanm
      )
      
      ethnic_cat_restricted = mergeValue(
        "ethnic_cat",
        uniqueValues(cases, ethnic_cat),
        uniqueValues(linked_deaths, ethnic_cat),
        uniqueValues(admissions, ethnic_cat),
        uniqueValues(linked_ctas, ethnic_cat),
        uniqueValues(linked_genomics, ethnic_cat)
      )
      
      ethnic_subcat_restricted = mergeValue(
        "ethnic_subcat",
        uniqueValues(cases, ethnic_subcat),
        uniqueValues(linked_deaths, ethnic_subcat),
        uniqueValues(admissions, ethnic_subcat),
        uniqueValues(linked_ctas, ethnic_subcat),
        uniqueValues(linked_genomics, ethnic_subcat)
      )
      
      ## Debug output ----
      # Also find poorly linked vaccination records for exclusion
      
      mismatches = list(
        age = ageConflict %>% decodeTablecode(),
        gender = gender$conflicts %>% decodeTablecode(),
        ethnic_cat = ethnic_cat$conflicts %>% decodeTablecode(),
        NHSER_code = NHSER$conflicts %>% decodeTablecode(),
        LTLA_code = LTLA$conflicts %>% decodeTablecode(),
        ethnic_subcat = ethnic_subcat$conflicts %>% decodeTablecode(),
        residential_category = residential_category$conflicts %>% decodeTablecode()
      )
      
      message("Writing mismatch data to:",paste0(self$todayWd,"/mismatches.Rdata"))
      saveRDS(mismatches, paste0(self$todayWd,"/mismatches.Rdata"))
      
      # TODO: more meaningful output of mismatches
      tmp = sapply(names(mismatches), function(x) {
        mismatches[[x]] %>% ungroup() %>% select(FINALID) %>% distinct() %>% count() %>% pull(n) %>% sprintf(fmt="Conflicts in %1.0f entries for %s", x)
      },USE.NAMES = FALSE)
      
      message("Writing mismatch counts to:",paste0(self$todayWd,"/mismatch-counts.csv"))
      write.csv(tibble(mismatches=tmp), paste0(self$todayWd,"/mismatch-counts.csv"))
      
      # mismatches$gender %>% group_by(FINALID) %>% summarise(lhs = min(tablecode), rhs=max(tablecode)) %>% group_by(lhs,rhs) %>% count() %>% View()
      
      badVaccinationMatches = bind_rows(lapply(names(mismatches), function(x) {
        mismatches[[x]] %>% filter(tablecode==64L) %>% select(FINALID)
      })) %>% group_by(FINALID) %>% count()
      
      # badVaccinationMatches %>% group_by(mismatches = n) %>% count()
      # bad vaccination linkages are defined as those which have 2 or more mismatched items.
      
      bad_vacc = badVaccinationMatches %>% filter(n>=2) %>% select(FINALID) %>% ungroup()
      write.csv(bad_vacc, paste0(self$todayWd,"/dodgy_vaccination_linkages.csv"))
      db_copy_to(con = con, table="tmp_bad_vacc", values=bad_vacc, overwrite=TRUE, indexes=list("FINALID"))
      bad_vacc = tbl(con, "tmp_bad_vacc")
      
      ## Assemble ----
      
      vacc_linked = cases %>% filter(!is.na(FINALID)) %>% 
        left_join(bad_vacc %>% mutate(tmp=1L), by="FINALID") %>% 
        filter(is.na(tmp)) %>% 
        select(FINALID, vaccine_linked) %>% 
        compute(indexes=list("FINALID"))
      vacc_reason = linked_vaccinations %>% filter(!is.na(FINALID)) %>% left_join(bad_vacc %>% mutate(tmp=1L), by="FINALID") %>% filter(is.na(tmp)) %>% select(FINALID, clinically_vulnerable, covid_risk, nhs_worker) %>% distinct() %>% compute(indexes=list("FINALID"))
      
      #TODO: IMD decile appears in other sets
      imd = cases %>% filter(!is.na(FINALID)) %>% 
        select(FINALID,imd_decile,imd_rank) %>% 
        compute(indexes=list("FINALID"))
      
      mpi = 
        allIds %>%
        left_join(ageAgree, by="FINALID") %>%
        left_join(gender$agree, by="FINALID") %>%
        left_join(NHSER$agree, by="FINALID") %>%
        left_join(LTLA_restricted$agree, by="FINALID") %>%
        left_join(ethnic_cat_restricted$agree, by="FINALID") %>%
        left_join(ethnic_subcat_restricted$agree, by="FINALID") %>%
        left_join(residential_category$agree, by="FINALID") %>%
        left_join(vacc_linked, by="FINALID") %>%
        left_join(vacc_reason, by="FINALID") %>%
        left_join(imd, by="FINALID") %>%
        compute(name=table, 
                unique_indexes=list("FINALID"), 
                indexes=list("yob","gender","ethnic_cat","NHSER_code","LTLA_code","ethnic_subcat","residential_category"))
      
      deleteTempTables(con)
      rm(allIds,ageConflict,ageAgree,allAges,residential_category,ethnic_subcat,ethnic_cat,gender,NHSER,LTLA,vacc_linked,vacc_reason,LTLA_restricted,ethnic_cat_restricted,ethnic_subcat_restricted,bad_vacc, badVaccinationMatches)
      
    })
  },
  
  
  #### Episode analysis ----
  
  getDiagnosisEpisodes = function(..., delay=56,
      diagnoses = self$getDiagnosisEvents(), 
      sgene = self$getCombinedSGene(),
      genomics = self$getCombinedGenomics()
    ) {
    
    tableName = paste0("diagnosis_episodes_",delay)
    linkTableName = paste0("diagnosis_episodes_link_",delay)
    diag_episodes = self$getTable(tableName, ..., orElse = function (con,table, ...) {
      
      # get the diagnoses in order and detect boundaries
      tmp = diagnoses %>% 
        filter(event %in% c("positive test","sequencing")) %>%
        left_join(
          sgene %>% self$interpretSGene() %>% select(FINALID, record_id, sGene, sgtf_under30CT, result, CT_N) %>% mutate(source = "sgene"), by=c("FINALID", "source","record_id")
        ) %>% 
        left_join( 
          genomics %>% select(FINALID, record_id,phe_name,pango_lineage,who_name) %>% mutate(source = "genomics"), by=c("FINALID", "source","record_id")
        ) %>% 
        mutate(
          sGene = ifelse(is.na(sGene),"Unknown",sGene)
        ) %>% 
        group_by(FINALID) %>%
        window_order(date) %>%
        mutate(newEraFlag = case_when(
          # no previous test
          is.na(lag(date)) ~ 1,
          # >2 delay since previous
          julianday(lag(date)) < julianday(date)-2*delay ~ 1,
          # 1-2 delay since previous & previous is not equivocal
          julianday(lag(date)) < julianday(date)-delay & sGene != "Equivocal" ~ 1,
          TRUE ~ 0
        )) %>%
        mutate(
          infection_number = cumsum(newEraFlag)
        ) %>%
        compute(prefix = "zz_tmp_diag_episode")
        
      tmp2 = tmp %>% group_by(FINALID, infection_number) %>%
        summarise(
          sGene = case_when(
            sum(sGene=="Positive",na.rm = TRUE)>0 & sum(sGene == "Negative",na.rm = TRUE)==0 ~ "Positive",
            sum(sGene=="Positive",na.rm = TRUE)==0 & sum(sGene == "Negative",na.rm = TRUE)>0 ~ "Negative",
            sum(sGene=="Positive",na.rm = TRUE)>0 & sum(sGene == "Negative",na.rm = TRUE)>0 ~ "Equivocal",
            sum(sGene!="Unknown",na.rm = TRUE)>0 ~ "Equivocal",
            TRUE ~ "Unknown"
          ),
          sGene_detail = group_concat(distinct(result)),
          min_nGene_CT = min(CT_N,na.rm=TRUE), 
          phe_name = group_concat(distinct(phe_name)),
          pango_lineage = group_concat(distinct(pango_lineage)),
          who_name = group_concat(distinct(who_name)),
          date = min(date,na.rm=TRUE),
          .groups="drop"
        ) %>%
        ungroup() %>%
        mutate(record_id=row_number()) %>%
        compute(
          unique_indexes=list("record_id", c("FINALID","infection_number")), 
          indexes=list("sGene","pango_lineage","who_name"), temporary=FALSE, name=tableName)
      
      tmp3 = tmp %>% 
        select(FINALID,infection_number,source, record_id) %>%
        inner_join(tmp2 %>% select(FINALID,infection_number,to_record_id = record_id), by=c("FINALID","infection_number")) %>%
        mutate(to_source = tableName) %>%
        compute(
          indexes=list(c("FINALID","record_id","source"),"to_record_id"), temporary=FALSE, name=linkTableName)
      
      deleteTempTables(con,prefix = "zz_tmp_diag_episode")
    })
  },
  
  getDiagnosisEpisodesLink = function(..., delay=56) {
    linkTableName = paste0("diagnosis_episodes_link_",delay)
    diag_episodes = self$getTable(linkTableName, ..., orElse = function (con,table, ...) {
      # The linkage table is written elsewhere. This just makes it happen if not already written
      self$getDiagnosisEpisodes(..., delay=delay)
    })
  },
  
  getAdmissionEpisodes = function(..., admissions = self$getAdmissionLineList()) {
    admit_episodes = self$getTable("admit_episodes", ..., orElse = function (con,table, ...) {
      
      # The logic here is to order a patients spells into ascending start date order and descending end date order
      # spells that are contained within a master spell or episode will have a start date >= containing and end date <= containing
      # new master spells will have end date > containing end date. the cummax term gives us the rolling largest end date = contianing spell end date.
      tmp = admissions %>% filter(!is.na(spell_start_date)) %>%
        mutate(start=julianday(spell_start_date), end=julianday(spell_end_date)) %>%
        mutate(source = "admissions") %>%
        group_by(FINALID) %>%
        window_order(start,desc(end)) %>%
        mutate(
          last = cummax(end),
          covidICD = sum(covidICD,na.rm = TRUE),
          covid_hrg = sum(covid_hrg,na.rm = TRUE),
          resp_hrg = sum(resp_hrg,na.rm = TRUE),
          covid_linked = sum(onset_category_simple!="Unlinked",na.rm = TRUE)
        ) %>%
        filter(end>lag(last,default = 0)) %>%
        mutate(admission_number = row_number()) %>%
        select(
          FINALID, admission_number, record_id, source, start, end,  
          provider_code, discharge_destination, discharge_destination_cat,
          discharge_method_cat, admit_method_cat, admit_source_cat, covidICD, covid_hrg, resp_hrg, covid_linked) 
      
      tmp %>% compute(unique_indexes=list("record_id"), indexes=list(c("FINALID", "admission_number")), temporary=FALSE, name=table)
      
    })
  },
  
  getAdmissionEpisodesLink = function(..., admissions = self$getAdmissionLineList()) {
    admit_episodes_link = self$getTable("admit_episodes_link", ..., orElse = function (con,table, ...) {
      
      tmp = admissions %>% filter(!is.na(spell_start_date)) %>%
        mutate(start=julianday(spell_start_date), end=julianday(spell_end_date)) %>%
        mutate(source = "admissions") %>%
        group_by(FINALID) %>%
        window_order(start,desc(end)) %>%
        mutate(last = cummax(end)) %>%
        mutate(new = end>lag(last,default = 0)) %>%
        mutate(admission_number = cumsum(new)) %>%
        select(FINALID, record_id, admission_number)
      
      tmp %>% compute(unique_indexes=list("record_id"), indexes=list(c("FINALID", "admission_number")), temporary=FALSE, name=table)
    
    })
  },
  
  ## Incidences from line list ----
  
  getIncidenceFromLineList = function(
    linelist,
    dateVar,
    codeExpr,
    codeType,
    statistic,
    ageBreaks = NULL,
    genderExpr = FALSE,
    excludeExpr = FALSE,
    subgroupExpr = NULL,
    noteExpr = NULL,
    unmatchedCode = NA_character_,
    unmatchedName = NA_character_,
    source = "phe linked data",
    validCodes = NULL,
    ...
  ) {
    
    excludeExpr = enexpr(excludeExpr)
    subgroupExpr = enexpr(subgroupExpr)
    genderExpr = enexpr(genderExpr)
    noteExpr = enexpr(noteExpr)
    codeExpr = enexpr(codeExpr)
    dateVar = ensym(dateVar)
    
    
    if (!statistic %in% allowableStatistics) {
      stop("statistic must be one of: ",paste0(allowableStatistics,collapse = ", "))
    }
    
    name = paste0("xx_incid_",tolower(codeType),"_",stringr::str_replace_all(statistic," ","_"))
    
    self$getTable(table = name, params=list(dateVar,codeExpr,codeType,ageBreaks,excludeExpr,subgroupExpr,genderExpr,noteExpr,unmatchedCode,validCodes), ..., orElse = function(con,table,...) {
      message("Calculating incidence: ",table)
      ct = codeType
      if(identical(validCodes,NULL)) validCodes = self$codes$getCodes() %>% filter(codeType==ct & status=="live") %>% select(code, codeType, name) 
      if(!is.na(unmatchedCode)) validCodes = validCodes %>% bind_rows(tibble(code=unmatchedCode, codeType=ct, name=unmatchedName)) 
      
      ageMapping = tibble(
        age = 0L:120L,
        ageCat = as.character(self$cutByAge(0:120, ageBreaks)),
        ageCatOrder = as.integer(self$cutByAge(0:120, ageBreaks))
      )
      tmp = linelist %>% 
        filter(!(!!excludeExpr)) %>%
        mutate(
          date = !!dateVar,
          code = !!codeExpr,
          gender = !!genderExpr,
          subgroup = !!subgroupExpr,
          note = !!noteExpr,
        ) %>%
        left_join(ageMapping, by="age", copy=TRUE, suffix = c(".old","")) %>%
        left_join(validCodes, by="code", copy=TRUE, suffix = c(".old","")) %>%
        mutate(code = ifelse(is.na(codeType), unmatchedCode, code)) %>%
        filter(!is.na(code)) %>%
        group_by(code,codeType,name,gender,ageCat,ageCatOrder,subgroup,note,date) %>% 
        summarise(value = n()) %>%
        ungroup() %>%
        compute(prefix = "zz_tmp_incid")
      
      if (tmp %>% count() %>% pull(n) == 0) stop("No matching geographies. Do your validCodes match your data?")
      
      minMaxDates = tmp %>% summarise(min_date = min(date,na.rm=TRUE), max_date = max(date,na.rm = TRUE)) %>% collect()
      
      tryCatch({
        dates = tibble(date=format(as.Date(as.Date(minMaxDates$min_date):as.Date(minMaxDates$max_date),"1970-01-01"),"%Y-%m-%d"))
      }, error = function(e) {
        message("No dates found");
        browser()
      })
      
      complete = tmp %>% select(gender) %>% distinct() %>%
        inner_join(tmp %>% select(ageCat,ageCatOrder) %>% distinct(), by=character()) %>%
        inner_join(tmp %>% select(subgroup) %>% distinct(), by=character()) %>%
        inner_join(tmp %>% select(note) %>% distinct(), by=character()) %>%
        inner_join(validCodes, copy=TRUE, by=character()) %>%
        inner_join(dates, copy=TRUE, by=character()) %>%
        compute(prefix = "zz_tmp_incid")
      
      out = complete %>% 
        left_join(tmp, by = c("name","code","codeType","gender","ageCat","ageCatOrder","subgroup","note","date"), na_matches="na") %>%
        mutate(
          value = ifelse(is.na(value),0,value),
          statistic = statistic,
          type = "incidence",
          source = source,
        ) %>%
        compute(name=table)
      
      deleteTempTables(con, prefix = "zz_tmp_incid")
      return(out)
    })
  },
  
  #' @description Load deaths data from linelist - does not preserve ethnicity
  #' @param ageBreaks - a list of ages which form the cut points for breaking continuous ages into ranges (or NULL for a single age category)
  #' @return a covidTimeseriesFormat dataframe
  getDeathsLineListIncidence = function(dll=NULL, ageBreaks = NULL, deathOrReport="death", cutoff=28, subgroup=NULL, gender=FALSE, 
                                        filterExpr=!(is.na(death_type28) & is.na(death_type60cod) & is.na(covidcod)), codeTypes = c("CTRY","NHSER"), truncate=NULL, ...) {
    filterExpr = enexpr(filterExpr)
    
    if(identical(dll,NULL)) {
      tmp = self$getDeathsLineList(...)
    } else {
      tmp = dll
    }
    
    if (!identical(filterExpr,NULL)) tmp = tmp %>% filter(!!filterExpr)
    tmp = tmp %>% 
      dplyr::filter(is.na(specimen_date) | as.integer(dod-specimen_date)<=cutoff) 
    
    if (!identical(subgroup,NULL)) {
      tmp = tmp %>% mutate(subgroup=ifelse(is.na(!!subgroup),"unknown",!!subgroup))
    } else {
      tmp = tmp %>% mutate(subgroup = NA_character_)
    }
    if (!gender) {
      tmp = tmp %>% dplyr::mutate(gender=NA_character_)
    }
    
    if(deathOrReport=="death") {
      tmp = tmp %>% mutate(date = dod)
    } else {
      tmp = tmp %>% mutate(date = pmin(report_date_earliest,NHSdeathreportdate, DBSdeathreportdate, HPTdeathreportdate, ONS_death_registration_date, na.rm = TRUE))
    }
    
    #tmp = tmp %>% compute(prefix="yy_derived_incid")
    
    codeTypeToColumn = list(
      CTRY = expr("E92000001"),
      NHSER = expr(nhser_code),
      PHEC = expr(phec_code),
      UA = expr(utla_code),
      LAD = expr(ltla_code)
    )
    
    out = bind_rows(lapply(codeTypes, function(codeType) {
      tmp %>% self$getIncidenceFromLineList(
        dateVar = date,
        codeExpr = !!codeTypeToColumn[[codeType]],
        codeType = codeType,
        statistic = "death",
        ageBreaks = ageBreaks,
        genderExpr = gender,
        subgroupExpr = subgroup,
        noteExpr = NA_character_,
        excludeExpr = FALSE
      ) %>% collect()
    }))
    
    out = out %>% filter(code %>% stringr::str_starts("E")) %>% self$fixDates(truncate) %>%
      mutate(ageCat = ordered(ageCat, levels = unique(ageCat[order(ageCatOrder)]))) %>% select(-ageCatOrder)
    
    return(out)
    
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
    
    if(!identical(filterExpr,NULL)) 
      tmp = tmp %>% filter(!!filterExpr)
    
    if (!gender) {
        tmp = tmp %>% dplyr::mutate(gender=NA_character_)
    }
    
    if(!identical(subgroup,NULL)) {
      tmp = tmp %>% dplyr::mutate(subgroup=!!subgroup)
    } else {
      tmp = tmp %>% dplyr::mutate(subgroup=NA_character_)
    }
    
    if(specimenOrReport == "report") {
      tmp = tmp %>% dplyr::mutate(date = lab_report_date)
    } else {
      tmp = tmp %>% dplyr::mutate(date = specimen_date)
    }
    
    #tmp = tmp %>% compute(prefix="yy_derived_incid")
    
    codeTypeToColumn = list(
      CTRY = list(codes = expr("E92000001"), mapId = "CTRY19"),
      NHSER = list(codes = expr(NHSER_code), mapId = "NHSER20"),
      PHEC = list(codes = expr(PHEC_code), mapId = "PHEC16"),
      UA = list(codes = expr(UTLA_code), mapId = "CTYUA19"),
      LAD = list(codes = expr(LTLA_code), mapId = "LAD19")
    )
    
    out = bind_rows(lapply(codeTypes, function(codeType) {
      tmp %>% self$getIncidenceFromLineList(
        dateVar = date,
        codeExpr = !!codeTypeToColumn[[codeType]]$codes,
        codeType = codeType,
        statistic = "case",
        ageBreaks = ageBreaks,
        genderExpr = gender,
        subgroupExpr = subgroup,
        noteExpr = NA_character_,
        excludeExpr = FALSE,
        validCodes = self$geog$getValidCodesForMap(codeTypeToColumn[[codeType]]$mapId)
      ) %>% collect()
    }))
    
    out = out %>% filter(code %>% stringr::str_starts("E")) %>% self$fixDates(truncate) %>%
      mutate(ageCat = ordered(ageCat, levels = unique(ageCat[order(ageCatOrder)])))
    return(out)
    
  },
  
  
  ## Timeline ----
  
  getOutcomeEvents = function(..., admissions = self$getAdmissionLineList(), deaths = self$getDeathsLineList(), admit_episodes = self$getAdmissionEpisodes()) {
    outcome_events = self$getTable("outcome_events", ..., orElse = function (con,table, ...) {
      # # These are episodes that start with a transfer in from a different trust
      xfer = admit_episodes %>% 
        mutate(from_record_id = lag(record_id)) %>%
        filter(start >= lag(end,default = 0) & start <= lag(end,default = 0)+1 & lag(discharge_destination_cat)=="Other hospital" & provider_code != lag(provider_code)) %>%
        mutate(event = "transfer", subgroup=NA_character_, source="admissions", from_source="admissions") %>%
        select(FINALID, event, date=start, source, record_id, from_source, from_record_id,subgroup) %>%
        compute(unique_indexes=list("record_id"))
      # 
      # # we will keep these ones as needed for knowing which hospital transferred from (in from_record_id)
      # 
      # # These are episodes that start with an admission from home
      adm = admit_episodes %>%
        filter(!(start >= lag(end,default = 0) & start <= lag(end,default = 0)+1 & lag(discharge_destination_cat)=="Other hospital")) %>%
        mutate(event = "admission", subgroup=admit_method_cat, source="admissions", from_source=NA_character_, from_record_id=NA_character_) %>%
        select(FINALID, event, date=start, source, record_id, from_source, from_record_id,subgroup) %>%
        compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))
      # 
      # # these are episodes that end in a transfer out to a different unit 
      # # episodes %>% filter(discharge_destination %in% c("51","52","53","87") & provider_code != lead(provider_code))
      # # we are not using this as this info is in transfer in
      # 
      # # these are episodes that end in a discharge from hospital (maybe be home (19) or to care home (54,85))
      disch = admit_episodes %>%
        filter(!(discharge_destination_cat=="Other hospital" | discharge_method_cat == "Died")) %>%
        mutate(event = "discharge", subgroup=admit_method_cat, source="admissions", from_source=NA_character_, from_record_id=NA_character_) %>%
        select(FINALID, event, date=end, source, record_id, from_source, from_record_id,subgroup) %>%
        compute(unique_indexes=list("record_id"))
      # 
      # these and in and out of hospital deaths from the death line list
      died = deaths %>% filter(!is.na(dod) & !is.na(FINALID)) %>%
        mutate(event = "death", subgroup=case_when(
          !is.na(ons_pod) ~ tolower(ons_pod),
          !is.na(NHSE) ~ "hospital",
          !is.na(DBSdeathreportdate) & pillars=="PILLAR1" ~ "hospital",
          !is.na(DBSdeathreportdate) ~ "community",
          !is.na(HPT) ~ "community",
          !is.na(P2deathreportdate) ~ "community",
          TRUE ~ "unknown"
        ), source="deaths", from_source=NA_character_, from_record_id=NA_character_) %>%
        mutate(date = julianday(dod)) %>%
        select(FINALID, event, date, source, record_id, from_source, from_record_id,subgroup) %>%
        compute(unique_indexes=list("record_id"), indexes=list("FINALID"))
      # 
      # these ard in hospital deaths from the admission line list
      hospDied = admit_episodes %>% 
        filter(discharge_method_cat == "Died") %>%
        mutate(event = "death", subgroup="hospital", source="admissions", from_source=NA_character_, from_record_id=NA_character_) %>%
        select(FINALID, event, date=end, source, record_id, from_source, from_record_id,subgroup) %>%
        compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))
      # 
      # an A&E attendance has an arrival date from ECDS: 
      aeAttend = admissions %>% filter(!is.na(arrival_date)) %>%
        mutate(event = "a&e visit", subgroup=NA_character_, source="admissions", from_source=NA_character_, from_record_id=NA_character_) %>%
        mutate(date = julianday(arrival_date)) %>%
        select(FINALID, event, date, source, record_id, from_source, from_record_id,subgroup) %>%
        compute(unique_indexes=list("record_id"))
      # 
      # 
      # # TODO think about A&E transfer more
      # 
      # # admissions %>% filter(!is.na(departure_date)) %>% group_by(ecds_discharge) %>% count()
      # 
      # an admission from A&E has an ecds departure date and dischage type of admitted
      aeAdmit = admissions %>%
        filter(!is.na(departure_date) & ecds_discharge %in% c("Admitted")) %>% #,"Ambulatory/Short stay","Transfer")) %>%
        mutate(event = "admission", subgroup="emergency", source="admissions", from_source=NA_character_, from_record_id=NA_character_) %>%
        mutate(date = julianday(departure_date)) %>%
        select(FINALID, event, date, source, record_id, from_source, from_record_id,subgroup) %>%
        
        compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))
      # 
      aeDied = admissions %>%
        filter(!is.na(departure_date) & ecds_discharge %in% c("Died")) %>%
        mutate(event = "death", subgroup="a&e", source="admissions", from_source=NA_character_, from_record_id=NA_character_) %>%
        mutate(date = julianday(departure_date)) %>%
        select(FINALID, event, date, source, record_id, from_source, from_record_id,subgroup) %>%
        compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))
      # 
      addECDSAdm = aeAdmit %>%
        anti_join(adm, by=c("FINALID","date")) %>%
        anti_join(adm %>% mutate(date = date-1), by=c("FINALID","date")) %>%
        anti_join(adm %>% mutate(date = date+1), by=c("FINALID","date")) %>%
        anti_join(adm %>% mutate(date = date-2), by=c("FINALID","date")) %>%
        anti_join(adm %>% mutate(date = date+2), by=c("FINALID","date")) %>%
        compute()
      # 
      addHospDied = hospDied %>%
        anti_join(died, by=c("FINALID")) %>%
        compute()
      # 
      addECDSDied = aeDied %>%
        anti_join(died, by=c("FINALID")) %>%
        anti_join(hospDied, by=c("FINALID")) %>%
        compute()
      # 
      # # adm %>% ungroup() %>% count() %>% pull(n)
      # # addECDSAdm %>% ungroup() %>% count() %>% pull(n)
      # # disch %>% ungroup() %>% count() %>% pull(n)
      # # died %>% ungroup() %>% count() %>% pull(n)
      # # addECDSDied %>% ungroup() %>% count() %>% pull(n)
      # # xfer %>% ungroup() %>% count() %>% pull(n)
      # 
      admissionEvents = adm %>%
        union(aeAttend) %>%
        union(addECDSAdm) %>%
        union(disch) %>%
        union(died) %>%
        union(addHospDied) %>%
        union(addECDSDied) %>%
        union(xfer) %>%
        ungroup() %>%
        arrange(FINALID, date) %>%
        # show_query()
        compute(indexes=list("record_id","FINALID","date"), temporary=FALSE, name=table)
      # 
      # admissionEvents %>% filter(FINALID == -5623049) %>% glimpse() 
      # 
      
      deleteTempTables(con)
      rm(adm,aeAttend,aeAdmit,disch,died,hospDied,aeDied,addHospDied,addECDSDied,xfer,addECDSAdm)
      
    })
  },
   
  getDiagnosisEvents = function(..., cases = self$getLineList(), sgene = self$getSGeneLineList(), genomics = self$getVAMLineList(), ctas = self$getLinkedCtas(), reinfect = self$getReinfectionLineList()) {
    diagnosis_events = self$getTable("diagnosis_events", ..., orElse = function (con,table, ...) { 
      onsets = cases %>%
        filter(!is.na(Onsetdate)) %>%
        mutate(event = "symptom onset", subgroup=NA_character_, source="cases", from_source=NA_character_, from_record_id=NA_character_) %>%
        mutate(date = julianday(Onsetdate)) %>%
        select(FINALID, event, date, source, record_id, from_source, from_record_id,subgroup) %>%
        compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))
      
      # 
      # 
      # First positive date from cases 
      firstPos = cases %>%
        filter(!is.na(specimen_date)) %>%
        mutate(event = "positive test", subgroup=tolower(pillar), source="cases", from_source=NA_character_, from_record_id=NA_character_) %>%
        mutate(date = julianday(specimen_date)) %>%
        select(FINALID, event, date, source, record_id, from_source, from_record_id,subgroup) %>%
        compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))
      
      # repeat infections
      reinfectPos = reinfect %>%
        filter(!is.na(specimen_date)) %>%
        mutate(event = "positive test", subgroup=tolower(pillar), source="reinfections", from_source=NA_character_, from_record_id=NA_character_) %>%
        mutate(date = julianday(specimen_date)) %>%
        select(FINALID, event, date, source, record_id, from_source, from_record_id,subgroup) %>%
        compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))
      
      # S-gene positive cases
      sgenePos = sgene %>%
        filter(!is.na(specimen_date)) %>%
        mutate(event = "positive test", subgroup="taqpath", source="sgene", from_source=NA_character_, from_record_id=NA_character_) %>%
        mutate(date = julianday(specimen_date)) %>%
        select(FINALID, event, date, source, record_id, from_source, from_record_id,subgroup) %>%
        compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))
      # 
      # Cases from genomics
      seq = genomics %>%
        filter(!is.na(specimen_date_sk) & !is.na(FINALID)) %>%
        mutate(event = "sequencing", subgroup=NA_character_, source="genomics", from_source=NA_character_, from_record_id=NA_character_) %>%
        mutate(date = julianday(specimen_date_sk)) %>%
        select(FINALID, event, date, source, record_id, from_source, from_record_id,subgroup) %>%
        compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))
      # 
      ctasSgene = ctas %>%
        filter(!is.na(sgtf_finalid) & !is.na(sgtf_specimen_date)) %>%
        mutate(event = "positive test", subgroup="taqpath", source="ctas", from_source=NA_character_, from_record_id=NA_character_) %>%
        mutate(date = julianday(sgtf_specimen_date)) %>%
        select(FINALID=sgtf_finalid, event, date, source, record_id, from_source, from_record_id,subgroup) %>%
        compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))
      # 
      ctasGenomics = ctas %>%
        filter(!is.na(genomic_finalid) & !is.na(genomic_specimen_date)) %>%
        mutate(event = "sequencing", subgroup=NA_character_, source="ctas", from_source=NA_character_, from_record_id=NA_character_) %>%
        group_by(genomic_cdr_specimen_request_sk) %>%
        window_order(genomic_specimen_date) %>%
        filter(row_number() == 1) %>%
        ungroup() %>%
        mutate(date = julianday(genomic_specimen_date)) %>%
        select(FINALID=genomic_finalid, event, date, source, record_id, from_source, from_record_id,subgroup) %>%
        compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))
      # 
      ctasSymptoms = ctas %>%
        filter(!is.na(first_symptomatic_at) & !is.na(FINALID)) %>%
        group_by(FINALID) %>%
        # TODO: this removes any info that might be present about second symptom onset dates
        window_order(first_symptomatic_at) %>%
        filter(row_number()==1) %>%
        mutate(event = "symptom onset", subgroup=NA_character_, source="ctas", from_source=NA_character_, from_record_id=NA_character_) %>%
        mutate(date = julianday(first_symptomatic_at)) %>%
        select(FINALID, event, date, source, record_id, from_source, from_record_id,subgroup) %>%
        compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))
      # 
      # 
      # # onsets %>% ungroup() %>% count()
      # # firstPos %>% ungroup() %>% count()
      # # sgenePos %>% ungroup() %>% count()
      # # seq %>% ungroup() %>% count()
      # # ctasSgene %>% ungroup() %>% count()
      # # ctasGenomics %>% ungroup() %>% count()
      # # ctasSymptoms %>% ungroup() %>% count()
      # 
      addCtasSgene = ctasSgene %>% anti_join(sgenePos, by=c("FINALID","date")) %>% compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))
      addCtasGenomics = ctasGenomics %>% anti_join(seq, by=c("FINALID","date")) %>% compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))
      addCtasSymptoms = ctasSymptoms %>% anti_join(onsets, by=c("FINALID")) %>% compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))
      # 
      # # addCtasSgene %>% ungroup() %>% count()
      # # addCtasGenomics %>% ungroup() %>% count()
      # # addCtasSymptoms %>% ungroup() %>% count()
      # 
      symptomEvents = onsets %>%
        union(addCtasSymptoms) %>%
        union(firstPos) %>%
        union(reinfectPos) %>%
        union(sgenePos) %>%
        union(addCtasSgene) %>%
        union(seq) %>%
        union(addCtasGenomics) %>%
        compute(indexes=list("record_id","FINALID","date"), temporary = FALSE, name=table)
      
      deleteTempTables(con)
      rm(addCtasGenomics,addCtasSymptoms,addCtasSgene, ctasGenomics, ctasSgene,ctasSymptoms, firstPos,sgenePos,onsets, seq)
    })
  },
  
  getVaccinationEvents = function(..., linked_vaccination = self$getLinkedVaccinations()) {
    vaccination_events = self$getTable("vaccination_events", ..., orElse = function (con,table, ...) { 
      
      imms1 = immunisations %>%
        filter(!is.na(vaccination_date) & !is.na(FINALID)) %>%
        mutate(event = "vaccination", subgroup=tolower(dose_number), source="linked_vaccination", from_source=NA_character_, from_record_id=NA_character_) %>%
        mutate(date = julianday(vaccination_date)) %>%
        select(FINALID, event, date, source, record_id, from_source, from_record_id,subgroup) %>%
        compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"), temporary = FALSE, name=table)
      
      # imms2 = immunisations %>%
      #   filter(!is.na(vaccination_date) & !is.na(finalid2)) %>%
      #   mutate(event = "vaccination", subgroup=tolower(dose_number), source="immunisations", from_source=NA_character_, from_record_id=NA_character_) %>%
      #   select(FINALID = finalid2, event, date=vaccination_date, source, record_id, from_source, from_record_id,subgroup) %>%
      #   compute(unique_indexes=list("record_id"), indexes=list("FINALID","date"))
      
      # immunisationEvents = imms1 %>%
      # union(imms2 %>% anti_join(imms1,by="FINALID")) %>%
      #  compute(indexes=list("record_id","FINALID","date"), temporary = FALSE, name=table)
       
      deleteTempTables(con)
      rm(imms1) #,imms2)
      
    })
  },
  
  # TODO: filter out bad linkages from vaccination and other tables?
  # merge this with following
  getEventTimeline = function(...,outcome_events = self$getOutcomeEvents(), diagnosis_events = self$getDiagnosisEvents(), vaccination_events = self$getVaccinationEvents()) {
    event_timeline = self$getTable("event_timeline", ..., orElse = function (con,table, ...) { 
      vaccination_events %>%
        union(diagnosis_events) %>%
        union(outcome_events) %>%
        mutate(event_type_order = case_when(
          event=="symptom onset" ~1L,
          event=="positive test" ~ 2L,
          event=="sequencing" ~ 3L,
          event=="a&e visit" ~ 4L,
          event=="admission" ~ 5L,
          event=="discharge" ~ 6L,
          event=="vaccination" ~ 7L,
          event=="death" ~ 8L
        )) %>%
        compute(indexes=list("record_id","FINALID","date","event_type_order"), name=table, temporary=FALSE)
    })
    
  },
  
  getAugmentedEventTimeline = function(..., event_timeline = self$getEventTimeline()) {
    
    augmented_timeline = self$getTable("augmented_event_timeline", ..., orElse = function (con,table, ...) {
      
      ## Copy <<<
      NONE_AFTER = julianday.Date(as.Date("2100-01-01"))
      NONE_BEFORE = julianday.Date(as.Date("1970-01-01"))
      GAP_BETWEEN_INFECTIONS = 28
      
      tmp1 = event_timeline %>%
        group_by(FINALID) %>%
        window_order(desc(date),desc(event_type_order)) %>%
        mutate(
          next_positive_date = cummin(ifelse(event %in% c("positive test","sequencing"),date,NONE_AFTER)),
          next_admission_date = cummin(ifelse(event %in% c("admission"),date,NONE_AFTER)),
          next_discharge_date = cummin(ifelse(event %in% c("discharge"),date,NONE_AFTER)),
          next_AE_visit_date = cummin(ifelse(event %in% c("a&e visit"),date,NONE_AFTER)),
          next_death_date = cummin(ifelse(event %in% c("death"),date,NONE_AFTER)),
          next_symptom_onset_date = cummin(ifelse(event %in% c("symptom onset"),date,NONE_AFTER)),
          # nextEra = cummin(era)
        ) %>%
        mutate(
          infection_episode_end = ifelse(
            event %in% c("positive test","sequencing") &
              date < lag(next_positive_date,default=NONE_AFTER)-GAP_BETWEEN_INFECTIONS # WINDOW FOR NEW
            # so this is lagged as we are in reverse date order
            # it is effectively testing whether a given test result is >28 days before the next positive test result, or if there is no next test result.
            ,TRUE,FALSE)
        ) %>%
        mutate(
          next_infection_episode_end_date =  cummin(ifelse(infection_episode_end,date,NONE_AFTER))
        ) %>%
        # needed to reset the ordering:
        compute(indexes=list("FINALID","date","event_type_order")) 
      
      tmp2 = tmp1 %>%
        group_by(FINALID) %>%
        window_order(date,event_type_order) %>%
        mutate(
          prev_positive_date = cummax(ifelse(event %in% c("positive test","sequencing"),date,NONE_BEFORE)),
          prev_admission_date = cummax(ifelse(event %in% c("admission"),date,NONE_BEFORE)),
          prev_discharge_date = cummax(ifelse(event %in% c("discharge"),date,NONE_BEFORE)),
          prev_symptom_onset_date = cummax(ifelse(event %in% c("symptom onset"),date,NONE_BEFORE)),
          prev_AE_visit_date = cummax(ifelse(event %in% c("a&e visit"),date,NONE_BEFORE)),
          prev_full_vaccination_date = cummax(ifelse(event %in% c("vaccination") & subgroup=="second",date,NONE_BEFORE)),
          prev_vaccination_date = cummax(ifelse(event %in% c("vaccination"),date,NONE_BEFORE))
        ) %>%
        mutate(
          # fully vaccinated if full vaccination date >14 days before now.
          is_fully_vaccinated = prev_full_vaccination_date!=NONE_BEFORE & prev_full_vaccination_date+14 <= date
          # 
        ) %>% 
        # needed to reset the ordering:
        compute(indexes=list("FINALID","date","event_type_order")) 
      
      tmp3 = tmp2 %>%
        mutate(covid_related = case_when(
          # an A&E visit more than 14 days before or more than 28 days after any positive test is not covid related
          event=="a&e visit" & date < next_positive_date-14 & date > prev_positive_date+28 ~ FALSE,
          # an admission with no discharge following it when admission more than 28 days after positive test is not covid related
          event=="admission" & next_discharge_date==NONE_AFTER & date > prev_positive_date+28 ~ FALSE,
          # an admission with subsequent discharged more than 14 days before positive test & admission date more than 28 days after positive test are not covid related
          event=="admission" & next_discharge_date < next_positive_date-14 & date > prev_positive_date+28 ~ FALSE,
          # an discharge more than 14 days before positive test & associated admission date more than 28 days after positive test are not covid related
          event=="discharge" & date < next_positive_date-14 & prev_admission_date > prev_positive_date+28 ~ FALSE,
          # symptoms falling outside of 28 days before, or 28 days after a positive test
          event=="symptom onset" & date < next_positive_date-28 & date > prev_positive_date+28 ~ FALSE,
          # deaths falling outside of 90 days after a positive test, however all deaths here are thought to be covid related
          # all other items are covid related at this point athough this will be changed later:
          # i.e. all positive tests, all vaccinations, all sequencing results, all sgene results, all deaths (will be adjusted later)
          TRUE ~ TRUE
        )) %>%
        group_by(FINALID) %>%
        window_order(date,event_type_order) %>%
        mutate(
          infection_episode_start = ifelse(
            event %in% c("positive test","sequencing") &
              date > lag(prev_positive_date,default=NONE_BEFORE)+GAP_BETWEEN_INFECTIONS # WINDOW FOR NEW
            ,TRUE,FALSE),
          infection_episode = cumsum(infection_episode_start),
          #TODO: could add in admission episodes here
          prev_covid_admission_date = cummax(ifelse(covid_related == TRUE & event %in% c("admission"),date,NONE_BEFORE)),
          prev_covid_discharge_date = cummax(ifelse(covid_related == TRUE & event %in% c("discharge"),date,NONE_BEFORE)),
          prev_covid_AE_visit_date = cummax(ifelse(covid_related == TRUE & event %in% c("a&e visit"),date,NONE_BEFORE)),
          prev_infection_episode_start_date = cummax(ifelse(infection_episode_start,date,NONE_BEFORE))
        ) %>%
        mutate(
          in_hospital_with_covid = case_when(
            # need to be between and admission and a discharge...
            # this does not include the admission and the discharge itself (and poss randomly )
            # disc -> adm -> event -> disc -> adm
            prev_discharge_date < prev_covid_admission_date & next_discharge_date == NONE_AFTER  ~ TRUE,
            prev_discharge_date < prev_covid_admission_date & next_discharge_date < next_admission_date  ~ TRUE,
            TRUE ~ FALSE
          )
        ) %>%
        mutate(
          # covid related deaths
          covid_related = case_when(
            # deaths from death line list are by definition covid related (according to PHE)
            # this is enforced above
            # deaths from in hospital records may be not covid related.
            # TODO: this should also include something to do with proximity to covid related admission?
            # if death occurs in someone that has not yet tested positive
            event=="death" & source=="admissions" & prev_positive_date == NONE_BEFORE ~ FALSE,
            # if the death is less than 28 days of since positive test or during a a covid-related hospital stay or within 28 days of a covid related hospital discharge then is covid related
            # expressed as complement.
            # i.e. if death is more that 28 days since positive test AND not in a covid hospital stay AND more that 28 days since a covid related hospital discharge it is not covid related.
            event=="death" & source=="admissions" & date > prev_positive_date+28 & !in_hospital_with_covid & date > prev_covid_discharge_date+28 ~ FALSE,
            TRUE ~ covid_related
          )
        ) %>%
        compute(indexes=list("FINALID","date","event_type_order","infection_episode","covid_related"), temporary=FALSE, name=table)
      ## Copy <<<
      deleteTempTables(con)
    })
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
  
  #' #' @description Load deaths data from linelist - does not preserve ethnicity
  #' #' @param ageBreaks - a list of ages which form the cut points for breaking continuous ages into ranges (or NULL for a single age category)
  #' #' @return a covidTimeseriesFormat dataframe
  #' getDeathsLineListIncidence = function(ageBreaks = NULL, deathOrReport="death", cutoff=28, subgroup=NULL, gender=FALSE, filterExpr=!(is.na(death_type28) & is.na(death_type60cod) & is.na(covidcod)), codeTypes = c("CTRY","NHSER"), truncate=NULL, ...) {
  #'   filterExpr = enexpr(filterExpr)
  #'   subgroup = tryCatch(ensym(subgroup), error=function(e) NULL)
  #'   tmp = self$getDeathsLineList(...) %>% collect()
  #'   output = self$getSaved(id = "DEATHS-LINE-LIST-INCIDENCE", params=list(tmp,ageBreaks, deathOrReport, gender, cutoff,as_label(subgroup),as_label(filterExpr),codeTypes), ..., orElse = function (...) covidTimeseriesFormat({
  #'     
  #'     if (!identical(filterExpr,NULL))
  #'       tmp = tmp %>% filter(!!filterExpr)
  #'     tmp = tmp %>% 
  #'       dplyr::filter(is.na(specimen_date) | as.integer(dod-specimen_date)<=cutoff) %>%
  #'       dplyr::mutate(
  #'         ageCat = age %>% self$cutByAge(ageBreaks)
  #'       )
  #'     if (!identical(subgroup,NULL)) {
  #'       tmp = tmp %>% mutate(subgroup=ifelse(is.na(!!subgroup),"unknown",!!subgroup))
  #'     } else {
  #'       tmp = tmp %>% mutate(subgroup = NA_character_)
  #'     }
  #'     if (!gender) {
  #'       tmp = tmp %>% dplyr::mutate(gender=NA_character_)
  #'     }
  #'     if(deathOrReport == "death") 
  #'       tmp = tmp %>% dplyr::mutate(date = as.Date(dod))
  #'     else
  #'       tmp = tmp %>% dplyr::mutate(date = 
  #'                                     as.Date(pmin(report_date_earliest,NHSdeathreportdate, DBSdeathreportdate, HPTdeathreportdate, ONS_death_registration_date, na.rm = TRUE),"1970-01-01")
  #'       ) %>% dplyr::filter(!is.na(date))
  #'     
  #'     selectByRegion = function(df, code, codeType, name) {
  #'       code = ensym(code)
  #'       name = ensym(name)
  #'       # check column exists
  #'       if(!(as_label(code) %in% colnames(df))) return(tibble())
  #'       df = df %>% dplyr::mutate(code = !!code, codeType=codeType, name=!!name) %>% 
  #'         dplyr::mutate(
  #'           code = ifelse(is.na(code),"E99999999",code),
  #'           name = ifelse(is.na(code),"Unknown (England)",name)
  #'         ) %>%
  #'         dplyr::group_by( code,codeType,name,date, ageCat, gender,subgroup) %>% 
  #'         dplyr::summarise(value = n()) 
  #'       return(df)
  #'     }
  #'     
  #'     out = NULL
  #'     if ("CTRY" %in% codeTypes) {
  #'       england = tmp %>% 
  #'         dplyr::mutate(code = "E92000001", codeType= "CTRY", name="England") %>% 
  #'         dplyr::group_by(code,codeType,name,date, ageCat, gender,subgroup) %>% 
  #'         dplyr::summarise(value = n())
  #'       out = out %>% bind_rows(england)
  #'     }
  #'     
  #'     if ("NHSER" %in% codeTypes) {
  #'       nhser = tmp %>% selectByRegion(nhser_code, "NHSER", nhser_name)
  #'       isNhser = nhser %>% self$codes$allPresentAndCorrect(codeTypes=c("NHSER","PSEUDO"))
  #'       
  #'       if(!isNhser) {
  #'         nhser = tmp %>% selectByRegion(nhser_code, "NHSER19CDH", nhser_name) %>% 
  #'           dplyr::inner_join(
  #'             self$codes$getMappings() %>% dplyr::filter(fromCodeType=="NHSER19CDH" & toCodeType=="NHSER"), 
  #'             by=c("code"="fromCode")
  #'           ) %>%
  #'           dplyr::ungroup() %>%
  #'           dplyr::select(-code,-codeType, -fromCodeType,-rel,-weight) %>%
  #'           dplyr::rename(code = toCode, codeType=toCodeType)
  #'       }
  #'       out = out %>% bind_rows(nhser)
  #'     }
  #'     
  #'     if ("PHEC" %in% codeTypes) {out = out %>% bind_rows(tmp %>% selectByRegion(phec_code, "PHEC", phec_name))}
  #'     if ("UA" %in% codeTypes) {out = out %>% bind_rows(tmp %>% selectByRegion(utla_code, "UA", utla_name))}
  #'     if ("LAD" %in% codeTypes) {out = out %>% bind_rows(tmp %>% selectByRegion(ltla_code, "LAD", ltla_name))}
  #'     if ("LSOA" %in% codeTypes) {out = out %>% bind_rows(tmp %>% selectByRegion(lsoa_code, "LSOA", lsoa_name))}
  #'     
  #'     out = out %>% dplyr::mutate(source="deaths line list",statistic = "death", type="incidence")
  #'     out = out %>% self$codes$findNamesByCode() %>% select(-ends_with(".original"))
  #'     out = out %>% self$fixDatesAndNames(truncate)
  #'     out = out %>% self$fillAbsent(completeDates=TRUE)
  #'     #out = out %>% self$complete()
  #'     out = out %>% dplyr::ungroup()
  #'     return(out)
  #'   }))
  #'   attr(output,"paths") = attr(tmp,"paths")
  #'   return(output %>% as_tibble())
  #' },

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
  
  

  #### Test and trace ----
  
  
  
  #### Immunisations ----

  
  
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
      tmp2 = self$getImmunizationLineListIncidence(ageBreaks=ageBreaks)
      tmp3 = tmp2 %>% tsp$aggregateGender()
      tmp4 = tmp3 %>% tsp$cumulativeFromIncidence()
      tmp4 = tmp4 %>% self$demog$findDemographics()
      deaths = self$getDeathsLineListIncidence(ageBreaks = ageBreaks,codeTypes = "LAD")
      deathsCum = deaths %>% tsp$aggregateGender() %>% tsp$cumulativeFromIncidence()
      tmp5 = tmp4 %>% inner_join(deathsCum %>% select(code,date,ageCat,cumdeaths = value), by=c("code","date","ageCat"))
      tmp5 = tmp5 %>% mutate(vaccPercent = value/(population-cumdeaths)) %>% mutate(vaccPercent = ifelse(vaccPercent>1,1,vaccPercent))
      tmp5 = tmp5 %>% mutate(immunized = value, value=vaccPercent, type="fraction") %>% select(-vaccPercent)
      return(tmp5)
    }))
  },
  
  #### S-gene line list ----

  
  
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
  
  #' #' @description Load incidence from line list
  #' #' 
  #' #' @param ageBreaks - a list of ages which form the cut points for breaking continuous ages into ranges (or NULL for a single age category)
  #' #' @return a covidTimeseriesFormat dataframe
  #' getLineListIncidence = function(ll=NULL, ageBreaks = NULL, gender=FALSE, specimenOrReport="specimen", subgroup="pillar", filterExpr=NULL, codeTypes = c("CTRY","NHSER"), truncate=NULL, ...) {
  #'   filterExpr = enexpr(filterExpr)
  #'   subgroup = tryCatch(ensym(subgroup), error = function(e) NULL)
  #'   if(!identical(ll,NULL)) {
  #'     tmp = ll  %>% collect()
  #'   } else {
  #'     tmp = self$getLineList(...) %>% collect()
  #'   }
  #'   path = attr(tmp,"paths")
  #'   out2 = self$getSaved("LINE-LIST-INCIDENCE", params=list(tmp, ageBreaks, specimenOrReport,as_label(subgroup), as_label(filterExpr), codeTypes, gender), ..., orElse = function (...) covidTimeseriesFormat({
  #'     if(!identical(filterExpr,NULL)) 
  #'       tmp = tmp %>% filter(!!filterExpr)
  #'     tmp = tmp %>% dplyr::mutate(ageCat = age %>% self$cutByAge(ageBreaks)) 
  #'     if (gender) {
  #'       tmp = tmp %>% dplyr::mutate(gender=self$normaliseGender(sex))
  #'     } else {
  #'       tmp = tmp %>% dplyr::mutate(gender=NA_character_)
  #'     }
  #'     if(!identical(subgroup,NULL)) {
  #'       tmp = tmp %>% dplyr::mutate(subgroup=!!subgroup)
  #'     } else {
  #'       tmp = tmp %>% dplyr::mutate(subgroup=NA_character_)
  #'     }
  #'     if(specimenOrReport == "report")
  #'       tmp = tmp %>% dplyr::mutate(date = as.Date(lab_report_date))
  #'     else
  #'       tmp = tmp %>% dplyr::mutate(date = as.Date(specimen_date))
  #'     
  #'     selectByRegion = function(df, code, codeType, name) {
  #'       code = ensym(code)
  #'       name = ensym(name)
  #'       # check column exists
  #'       if(!(as_label(code) %in% colnames(df))) return(tibble())
  #'       df = df %>% dplyr::mutate(code = !!code, codeType=codeType, name=!!name) %>% 
  #'         dplyr::mutate(
  #'           code = ifelse(is.na(code),"E99999999",code),
  #'           name = ifelse(is.na(code),"Unknown (England)",name)
  #'         ) %>%
  #'         dplyr::group_by( code,codeType,name,date, ageCat, gender,subgroup) %>% 
  #'         dplyr::summarise(value = n()) 
  #'       return(df)
  #'     }
  #'     
  #'     out = NULL
  #'     if ("CTRY" %in% codeTypes) {
  #'       england = tmp %>% dplyr::mutate(code = "E92000001", codeType= "CTRY", name="England") %>% 
  #'         dplyr::group_by(code,codeType,name,date, ageCat, gender,subgroup) %>% 
  #'         dplyr::summarise(value = n())
  #'       out = out %>% bind_rows(england)
  #'     }
  #'     
  #'     if ("NHSER" %in% codeTypes) {
  #'       nhser = tmp %>% selectByRegion(NHSER_code, "NHSER", NHSER_name)
  #'       isNhser = nhser %>% self$codes$allPresentAndCorrect(codeTypes=c("NHSER","PSEUDO"))
  #'       
  #'       if(!isNhser) {
  #'         nhser = tmp %>% selectByRegion(NHSER_code, "NHSER19CDH", NHSER_name) %>% 
  #'           dplyr::inner_join(
  #'             self$codes$getMappings() %>% dplyr::filter(fromCodeType=="NHSER19CDH" & toCodeType=="NHSER"), 
  #'             by=c("code"="fromCode")
  #'           ) %>%
  #'           dplyr::ungroup() %>%
  #'           dplyr::select(-code,-codeType, -fromCodeType,-rel,-weight) %>%
  #'           dplyr::rename(code = toCode, codeType=toCodeType)
  #'       }
  #'       out = out %>% bind_rows(nhser)
  #'     }
  #'     
  #'     if ("PHEC" %in% codeTypes) {out = out %>% bind_rows(tmp %>% selectByRegion(PHEC_code, "PHEC", PHEC_name))}
  #'     if ("UA" %in% codeTypes) {out = out %>% bind_rows(tmp %>% selectByRegion(UTLA_code, "UA", UTLA_name))}
  #'     if ("LAD" %in% codeTypes) {out = out %>% bind_rows(tmp %>% selectByRegion(LTLA_code, "LAD", LTLA_name))}
  #'     if ("LSOA" %in% codeTypes) {out = out %>% bind_rows(tmp %>% selectByRegion(LSOA_code, "LSOA", LSOA_name))}
  #'     
  #'     out = out %>% dplyr::mutate(source="line list",statistic = "case", type="incidence")
  #'     out = out %>% self$fixDatesAndNames(truncate)
  #'     out = out %>% self$fillAbsent(completeDates=TRUE)
  #'     out = out %>% dplyr::ungroup()
  #'     return(out)
  #'   }))
  #'   attr(out2,"paths") = path
  #'   return(out2 %>% as_tibble())
  #' },
  
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
    attr(tmp,"paths") = c(path)
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


