#' Get a provider of UK stats
#'
#' This function sets up a connection to the omop databse.
#' @keywords omop
#' @import dplyr
#' @export
UKCodeMappingProvider = R6::R6Class("UKCodeMappingProvider", inherit=PassthroughFilesystemCache, public = list(
  
  pcd = NULL,
  
  initialize = function(postcodeProvider, ...) {
    super$initialize(workingDirectory = postcodeProvider$wd, ...)
    self$pcd = postcodeProvider
  },
  
  #### code mapping ----
  mapping = list(
    PHEC19CDH_PHEC19CD = list(
      note="ODS codes (ANANA codes) to ONS codes for public health",
      url="https://opendata.arcgis.com/datasets/e1ab849323534ea7be1920a2598a4c30_0.csv", 
      fromCode="PHEC19CDH", 
      toCode="PHEC19CD",
      toCodeType = "PHEC",
      rel="synonym"
    ),
    LAD19CD_PHEC19CD = list(
      note="path from NHS trust postcode to LSOA to LAD to PHEC",
      url="https://opendata.arcgis.com/datasets/4da177ab2ab34edaba9d2696c3a6da64_0.csv",
      fromCode="LAD19CD", 
      fromCodeType="LAD",
      toCode="PHEC19CD",
      toCodeType = "PHEC",
      rel="parent"
    ),
    LSOA11CD_LAD19CD = list(
      note="path from postcode to LSOA to LAD to PHEC",
      url="https://opendata.arcgis.com/datasets/15299a7b8e6c498d94a08b687c75b73f_0.csv", 
      fromCode="LSOA11CD", 
      fromCodeType="LSOA",
      toCode="LAD19CD",
      toCodeType = "LAD",
      rel="best_fit"
    ),
    CCG20CDH_CCG20CD = list(
      note="ODS codes (ANANA codes) to ONS codes for CCGs",
      url="https://opendata.arcgis.com/datasets/bfb87228cf9e4c44bad0cffa353d0fc8_0.csv",
      fromCode="CCG20CDH",
      toCode="CCG20CD",
      toCodeType = "CCG",
      rel="synonym"
    ),
    NHSER19CDH_NHSER19CD = list(
      note="ODS codes (ANANA codes) to ONS codes for NHS regions",
      url = "https://opendata.arcgis.com/datasets/a84ae875f03c4553b49cdec08eb8e13c_0.csv",
      fromCode="NHSER19CDH",
      toCode="NHSER19CD", 
      toCodeType = "NHSER",
      rel="synonym"
    ),
    CCG20CD_NHSER20CD = list(
      note="CCGs to NHS regions in England only",
      url = "https://opendata.arcgis.com/datasets/888dc5cc66ba4ad9b4d935871dcce251_0.csv",
      fromCode = "CCG20CD",
      toCode = "NHSER20CD",
      fromCodeType = "CCG",
      toCodeType = "NHSER",
      rel = "parent"
    ),
    LSOA11CD_CCG20CD = list(
      note="LSOA to CCGs in England only",
      url="https://opendata.arcgis.com/datasets/1631beea57ff4e9fb90d75f9c764ce26_0.csv",
      fromCode = "LSOA11CD",
      toCode = "CCG20CD",
      fromCodeType = "LSOA",
      toCodeType = "CCG",
      rel = "parent"
    ),
    UA19CD_LHB19CD = list(
      note="wales UA to LHBs provide route for LSOA to LHB, using LSOA to UTLA (which includes UAs)",
      url="https://opendata.arcgis.com/datasets/680c9b730655473787cb594f328a86fa_0.csv",
      fromCode = "UA19CD",
      toCode = "LHB19CD",
      fromCodeType = "UA",
      toCodeType = "LHB",
      rel = "parent"
    ),
    DZ11CD_HB19CD = list(
      note="scot equivalent to LSOA to CCG, DZ to HB, this file also includes CA (council areas), HSCP (health social care)",
      url="https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/395476ab-0720-4740-be07-ff4467141352/download/dz2011_codes_and_labels_21042020.csv",
      fromCode = "DataZone",
      toCode = "HB",
      fromCodeType = "DZ",
      toCodeType = "HB",
      rel = "parent"
    ),
    LSOA11CD_UTLA19CD = list(
      note="mapping from LSOA to UTLA inclues UA codes needed for mapping to Welsh LHBs",
      url="https://opendata.arcgis.com/datasets/4c6f3314565e43c5ac7885fd71347548_0.csv",
      fromCode = "LSOA11CD",
      toCode = "UTLA19CD",
      fromCodeType = "LSOA",
      toCodeType = "UTLA",
      rel = "best_fit"
    )
  ),
  
  # OUT_CODE_TO_LSOA = ONSPD_NOV_2019_UK %>% dplyr::mutate(outcode = pcd %>% stringr::str_sub(1,4) %>% stringr::str_trim()) %>% dplyr::select(outcode,lsoa11) %>% dplyr::distinct()
  # LSOA_TO_IMD = ONSPD_NOV_2019_UK %>% dplyr::select(lsoa11,imd) %>% dplyr::distinct()
  # LSOA_TO_CCG = ONSPD_NOV_2019_UK %>% dplyr::select(lsoa11,ccg) %>% dplyr::distinct()
  # LSOA_TO_NHSER = ONSPD_NOV_2019_UK %>% dplyr::select(lsoa11,nhser) %>% dplyr::distinct()
  # 
  # NHS trusts  with postcodes
  
  getONSMappings = function() {
    self$getSaved("ONS_MAPPINGS",orElse = function() {
      out = NULL
      for (mapName in names(self$mapping)) {
        item = self$mapping[[mapName]]
        filename = self$download(mapName, item$url, "csv")
        map = readr::read_csv(filename)
        fromCode = as.symbol(item$fromCode)
        toCode = as.symbol(item$toCode)
        # browser(expr=self$debug)
        map = map %>% dplyr::rename(fromCode = !!fromCode, toCode = !!toCode) %>% dplyr::mutate(
          fromCodeType = ifelse(is.null(item$fromCodeType),item$fromCode,item$fromCodeType), 
          toCodeType = ifelse(is.null(item$toCodeType),item$toCode,item$toCodeType), 
          rel=item$rel) %>%
          dplyr::select(fromCodeType, fromCode, toCodeType, toCode, rel)
        out = out %>% dplyr::bind_rows(map)
      }
      tmp2 = self$getONSRegister()
      
      tmp3 = tmp2 %>% 
        dplyr::select(fromCode = code, fromCodeType = codeType, toCode = parent) %>% 
        dplyr::filter(!is.na(toCode)) %>% 
        dplyr::inner_join( 
          tmp2 %>% dplyr::select(toCode = code, toCodeType = codeType),
          by = "toCode"
        ) %>%
        dplyr::mutate(rel = "parent")
      out = out %>% dplyr::bind_rows(tmp3) %>% dplyr::distinct()
      browser(expr=self$debug)
      return(out)
    })
  },
  
  
  # idMapping <- read_csv(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vQj6X8rIlBlsD5bK-PMcBT9wjAWh60dTTJLfuczqsiKnYzYiN_4KjYAh4HWWkf4v1RH6ih7C78FhdiN/pub?gid=1853095988&single=true&output=csv","&nocache=",sample(1:1000000,1)))
  
  getONSRegister = function() {
    self$getSaved("CODE_REGISTER",orElse = function() {
      register = self$downloadAndUnzip("ONS_REGISTER", url="https://www.arcgis.com/sharing/rest/content/items/56a91921e10d4fb4b367ef592ceb0bab/data", pattern = "xlsx")
      sheets = readxl::excel_sheets(register)
      sheets = sheets[sheets %>% stringr::str_detect("[A-Z][0-9][0-9]_.*")]
      out = NULL
      for(sheet in sheets) {
        message(sheet)
        tmp = readxl::read_excel(register, sheet=sheet, col_types="text")
        prefix = sheet %>% stringr::str_remove("[A-Z][0-9][0-9]_")
        tmp = tmp %>% dplyr::select(
          code = `GEOGCD`,
          name = `GEOGNM`,
          # `GEOGNMW`,
          # `SI_ID`,
          # `SI_TITLE`,
          start = `OPER_DATE`,
          end = `TERM_DATE`,
          parent = `PARENTCD`,
          entity = `ENTITYCD`,
          # `OWNER`,
          status = `STATUS`
        ) %>% dplyr::mutate(
          start = as.Date(as.numeric(start),"1899-12-30"),
          end = as.Date(as.numeric(end),"1899-12-30"),
          status = status %>% stringr::str_to_lower(),
          codeType = prefix
        )
        out = dplyr::bind_rows(out,tmp)
      }
      
      scot_register = self$download("SCOT_REGISTER",url="https://www2.gov.scot/Resource/0054/00547737.xlsx",type="xlsx")
      sheets = readxl::excel_sheets(scot_register)
      sheets = sheets[sheets %>% stringr::str_detect("[A-Z][0-9][0-9]_.*")]
      for(sheet in sheets) {
        message(sheet)
        prefix = sheet %>% stringr::str_remove("[A-Z][0-9][0-9]_")
        tmp = readxl::read_excel(scot_register, sheet=sheet, col_types="text")
        tmp = tmp %>% dplyr::select(
          entity = `EntityCode`,
          code = `InstanceCode`,
          name = `InstanceName`,
          status = `Status`,
          start = `DateEnacted`,
          #`LegalDocOpen`,`NotesOpen`,
          end = `DateArchived`
          #`LegalDocClose`,`NotesClosed`
        ) %>% dplyr::mutate(
          start = as.Date(as.numeric(start),"1899-12-30"),
          end = as.Date(as.numeric(end),"1899-12-30"),
          status = status %>% stringr::str_to_lower(),
          codeType = prefix,
          parent=as.character(NA)
        )
        out = dplyr::bind_rows(out,tmp)
      }
      browser(expr=self$debug)
      return(out)
    })
    
  },
  
  getODSMappings = function() {
    self$getSaved("ODS_MAPPINGS",orElse = function() {
      tmp = self$getODSCodes()
      
      mapping = tmp %>% 
        dplyr::select(fromCodeType = codeType, fromCode = code, toCode=parent) %>% 
        dplyr::inner_join(tmp %>% dplyr::select(toCodeType=codeType, toCode=code), by="toCode") %>% 
        dplyr::mutate(rel="parent") %>% 
        dplyr::filter(!is.na(fromCodeType) & !is.na(toCodeType))
      idMapping <- read_csv(paste0("https://docs.google.com/spreadsheets/d/e/2PACX-1vQj6X8rIlBlsD5bK-PMcBT9wjAWh60dTTJLfuczqsiKnYzYiN_4KjYAh4HWWkf4v1RH6ih7C78FhdiN/pub?gid=1853095988&single=true&output=csv","&nocache=",sample(1:1000000,1)))
      idMapping = idMapping %>% dplyr::rename(fromCode = fromId, toCode=toId, fromCodeType = fromType, toCodeType = toType) %>% dplyr::mutate(rel="synonym") %>%
        dplyr::select(fromCode,fromCodeType,toCode,toCodeType,rel)
      
      tmp2 = tmp %>% dplyr::filter(!is.na(pcds) & codeType=="NHS trust") %>% self$pcd$lookupFeatures(pcds, dplyr::vars(ccg,lsoa11))
      tmp3 = tmp2 %>% dplyr::mutate(toCodeType = "CCG",rel="located_in") %>% dplyr::select(fromCodeType = codeType, fromCode = code, toCode=ccg, toCodeType, rel)
      tmp4 = tmp2 %>% dplyr::mutate(toCodeType = "LSOA",rel="located_in") %>% dplyr::select(fromCodeType = codeType, fromCode = code, toCode=lsoa11, toCodeType, rel)
      
      out = dplyr::bind_rows(mapping,idMapping,tmp3,tmp4) %>% dplyr::distinct()
      browser(expr=self$debug)
      return(out)
    })
  },
  
  # Codes from the ONS website
  getODSCodes = function() {
    self$getSaved("NHS_ODS",orElse = function() {
    
      standardFormat = c( "Organisation Code",  "Name",  "National Grouping",  "High Level Health Geography",  "Address Line 1",  "Address Line 2",  "Address Line 3",  "Address Line 4",
                          "Address Line 5",  "Postcode",  "Open Date",  "Close Date",  "Null 1",  "Organisation SubType Code",  "Parent Organisation Code",  "Null 2",  "Null 3",  "Contact Telephone Number",
                          "Null 4",  "Null 5",  "Null 6",  "Amended Record Indicator",  "Null 7",  "GOR Code",  "Null 8",  "Null 9",  "Null 10")
      
      etr = readr::read_csv("https://nhsenglandfilestore.s3.amazonaws.com/ods/etr.csv",col_names = standardFormat,col_types = cols(.default=col_character())) %>% dplyr::select(-starts_with("Null"))
      ets =  readr::read_csv("https://nhsenglandfilestore.s3.amazonaws.com/ods/ets.csv",col_names = standardFormat,col_types = cols(.default=col_character())) %>% dplyr::select(-starts_with("Null"))
      eccg = readr::read_csv("https://nhsenglandfilestore.s3.amazonaws.com/ods/eccg.csv",col_names = standardFormat,col_types = cols(.default=col_character())) %>% dplyr::select(-starts_with("Null"))
      eauth = readr::read_csv("https://nhsenglandfilestore.s3.amazonaws.com/ods/eauth.csv",col_names = standardFormat,col_types = cols(.default=col_character())) %>% dplyr::select(-starts_with("Null"))
      
      combined = dplyr::bind_rows(
        etr %>% dplyr::mutate(codeType="NHS trust"),
        ets %>% dplyr::mutate(codeType="NHS site"),
        eccg %>% dplyr::mutate(codeType="CCG20CDH"),
        eauth %>% dplyr::mutate(codeType=case_when(
          `Organisation Code` %>% stringr::str_starts("Q") ~ "NHSRLO19CDH",
          `Organisation Code` %>% stringr::str_starts("Y") ~ "NHSER19CDH",
          TRUE ~ NA_character_
        ))
      ) %>% dplyr::rename(
        code = `Organisation Code`, name = `Name`, pcds = Postcode
      ) %>% dplyr::mutate(
        parent = case_when(
          !is.na(`Parent Organisation Code`) ~ `Parent Organisation Code`,
          !is.na(`High Level Health Geography`) ~ `High Level Health Geography`,
          !is.na(`National Grouping`) ~ `National Grouping`,
          TRUE ~ NA_character_
        ),
        start = as.Date.character(`Open Date`,"%Y%m%d"),
        end = as.Date.character(`Close Date`,"%Y%m%d"),
        status = ifelse(is.na(`Close Date`),"live","terminated"),
        entity = NA
      ) %>% dplyr::select(
        code,name,pcds,codeType,parent,start,end,status,entity
      )
      browser(expr=self$debug)
      return(combined)
      
      # lookup long lat using postcodes for sites
      # 
      
    })
  },
  
  getMappings = function() {
    self$getSaved("CODE_MAPPINGS", orElse = function() {
      tmp_mappings = dplyr::bind_rows(
        self$getODSMappings(),
        self$getONSMappings()
      ) %>% dplyr::distinct()
      browser(expr=self$debug)
      return(tmp_mappings)
    })
  },
  
  getTransitiveClosure = function() {
    self$getSaved("TRANSITIVE_CLOSURE", orElse = function() {
      mappings = self$getMappings() %>% dplyr::select(-rel) %>% dplyr::semi_join(self$getCodes(), by=c("toCode"="code"))
      out = mappings %>% dplyr::mutate(distance = 0, path = paste0(fromCodeType,"-",toCodeType)) 
      tmp = out
      while(nrow(tmp) > 0) {
        message("building transitive closure; rows in this iteration: ",nrow(tmp))
        browser(expr=self$debug)
        tmp = tmp %>% 
          dplyr::rename(joinCode = toCode, joinCodeType = toCodeType) %>% 
          dplyr::inner_join(
            mappings %>% dplyr::rename(joinCode = fromCode) %>% dplyr::select(-fromCodeType),# joinCodeType = fromCodeType),# %>% dplyr::filter(rel=="parent"), 
            by=c("joinCode") #,"joinCodeType")
          ) %>%  dplyr::mutate(
            distance = distance+1,
            path = paste0(path,"-",toCodeType)
          ) %>% dplyr::select(-joinCode,-joinCodeType)
        
        tmp = tmp %>% dplyr::group_by(fromCode,toCode) %>% filter(path==min(path)) %>% ungroup()
        tmp = tmp %>% dplyr::anti_join(out, by=c("fromCode","toCode"))
        
        out = out %>% dplyr::bind_rows(tmp)
      }
      return(out)
    })
  },
  
  getCodes = function() {
    self$getSaved("CODE_LIST", orElse = function() {
      tmp_codes = dplyr::bind_rows(self$getONSRegister(), self$getODSCodes()) %>% dplyr::select(-pcds)
      synonyms = tmp_codes %>% dplyr::rename(fromCode = code) %>% dplyr::select(-codeType) %>% dplyr::inner_join(
        self$getMappings() %>% dplyr::filter(rel=="synonym") %>% dplyr::rename(code = toCode, codeType = toCodeType),
        by = "fromCode"
      )  %>% dplyr::select(-fromCode,-fromCodeType,-rel)
      synonyms2 = tmp_codes %>% dplyr::rename(toCode = code) %>% dplyr::select(-codeType) %>% dplyr::inner_join(
        self$getMappings() %>% dplyr::filter(rel=="synonym") %>% dplyr::rename(code = fromCode, codeType = fromCodeType),
        by = "toCode"
      )  %>% dplyr::select(-toCode,-toCodeType,-rel)
      return(dplyr::bind_rows(tmp_codes,synonyms,synonyms2) %>% 
               dplyr::filter(status=="live" & !is.na(name)) %>% 
               dplyr::select(-end, -parent, -entity, -status) %>%
               dplyr::mutate(name = name %>% stringr::str_to_title() %>% stringr::str_replace_all("Nhs","NHS")) %>%
               dplyr::distinct())
    })
  },
  
  findCodesByName = function(df, nameVar = "name", outputCodeVar = "code", codeTypes = c("LSOA","CCG","HB","LHB","NHSER","PHEC","LAD","UA","NHS site","NHS trust")) {
    nameVar = ensym(nameVar)
    outputCodeVar = ensym(outputCodeVar)
    tmp = self$getCodes()
    # browser(expr=self$debug)
    tmp2 = tmp %>% dplyr::mutate(tmp_name = name %>% stringr::str_to_lower() %>% stringr::str_remove_all("[^a-z0-9]")) %>% dplyr::rename(!!nameVar := name, !!outputCodeVar := code) %>% dplyr::select(-start) 
    tmp3 = tmp2 %>% dplyr::inner_join(
      df %>% dplyr::mutate(tmp_name = !!nameVar %>% stringr::str_to_lower() %>% stringr::str_remove_all("[^a-z0-9]")),
      by="tmp_name", suffix = c("",".original")) 
    return(tmp3 %>% dplyr::select(-tmp_name) %>% dplyr::filter(codeType %in% codeTypes))
  }
  
))

#upp = UKPostcodeProvider$new("~/Data/maps")
ump = UKCodeMappingProvider$new(upp)
odsCodes = ump$getODSCodes()
onsCodes = ump$getONSRegister()
onsMaps = ump$getONSMappings()
odsMaps = ump$getODSMappings()
codes = ump$getCodes()
maps = ump$getMappings()

# ump$debug = TRUE
#ump$getODSMappings()
#tmp = ump$getODSCodes()
#tmp = ump$getMappings()
#ump$findCodesByName(tibble(x="Musgrove Park Hospital"),nameVar = x)
tc = ump$getTransitiveClosure()
View(tc %>% group_by(fromCodeType, toCodeType) %>% count())
View(tc %>% filter(fromCodeType == "NHS site" & toCodeType=="CCG"))
View(tc %>% filter(fromCodeType == "NHS site" & toCodeType=="PHEC"))
