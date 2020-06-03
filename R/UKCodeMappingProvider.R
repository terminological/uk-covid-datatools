#' Get a provider of UK stats
#'
#' This function sets up a connection to the omop databse.
#' @keywords omop
#' @import dplyr
#' @export
UKCodeMappingProvider = R6::R6Class("UKCodeMappingProvider", inherit=PassthroughFilesystemCache, public = list(
  
  #### code mapping ----
  mapping = list(
    PHEC19CDH_to_PHEC19CD = list(
      url="https://opendata.arcgis.com/datasets/e1ab849323534ea7be1920a2598a4c30_0.csv", 
      fromCode="PHEC19CDH", 
      toCode="PHEC19CD", 
      rel="synonym"
    ),
    CCG20CDH_to_CCG20CD = list(
      url="https://opendata.arcgis.com/datasets/bfb87228cf9e4c44bad0cffa353d0fc8_0.csv",
      fromCode="CCG20CDH",
      toCode="CCG20CD",
      rel="synonym"
    ),
    NHSER19CDH_to_NHSER19CD = list(
      url = "https://opendata.arcgis.com/datasets/a84ae875f03c4553b49cdec08eb8e13c_0.csv",
      fromCode="NHSER19CD",
      toCode="NHSER19CDH", 
      rel="synonym"
    )
  ),
  
  # OUT_CODE_TO_LSOA = ONSPD_NOV_2019_UK %>% mutate(outcode = pcd %>% stringr::str_sub(1,4) %>% stringr::str_trim()) %>% select(outcode,lsoa11) %>% distinct()
  # LSOA_TO_IMD = ONSPD_NOV_2019_UK %>% select(lsoa11,imd) %>% distinct()
  # LSOA_TO_CCG = ONSPD_NOV_2019_UK %>% select(lsoa11,ccg) %>% distinct()
  # LSOA_TO_NHSER = ONSPD_NOV_2019_UK %>% select(lsoa11,nhser) %>% distinct()
  # 
  # NHS trusts  with postcodes
  
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
        tmp = tmp %>% select(
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
        ) %>% mutate(
          start = as.Date(as.numeric(start),"1899-12-30"),
          end = as.Date(as.numeric(end),"1899-12-30"),
          status = status %>% stringr::str_to_lower(),
          codeType = prefix
        )
        out = bind_rows(out,tmp)
      }
      
      scot_register = self$download("SCOT_REGISTER",url="https://www2.gov.scot/Resource/0054/00547737.xlsx",type="xlsx")
      sheets = readxl::excel_sheets(scot_register)
      sheets = sheets[sheets %>% stringr::str_detect("[A-Z][0-9][0-9]_.*")]
      for(sheet in sheets) {
        message(sheet)
        prefix = sheet %>% stringr::str_remove("[A-Z][0-9][0-9]_")
        tmp = readxl::read_excel(scot_register, sheet=sheet, col_types="text")
        tmp = tmp %>% select(
          entity = `EntityCode`,
          code = `InstanceCode`,
          name = `InstanceName`,
          status = `Status`,
          start = `DateEnacted`,
          #`LegalDocOpen`,`NotesOpen`,
          end = `DateArchived`
          #`LegalDocClose`,`NotesClosed`
        ) %>% mutate(
          start = as.Date(as.numeric(start),"1899-12-30"),
          end = as.Date(as.numeric(end),"1899-12-30"),
          status = status %>% stringr::str_to_lower(),
          codeType = prefix,
          parent=as.character(NA)
        )
        out = bind_rows(out,tmp)
      }
      return(out)
    })
    
  },
  
  getONSPDcodes = function() {
    csvfile = self$downloadAndUnzip("ONSPD20", "https://www.arcgis.com/sharing/rest/content/items/fb894c51e72748ec8004cc582bf27e83/data", pattern = "ONSPD.*UK\\.csv")
  },
  
  # Codes from the ONS website
  getODScodes = function() {
    #self$getSaved("NHS_ODS",orElse = function() {
    
    standardFormat = c( "Organisation Code",  "Name",  "National Grouping",  "High Level Health Geography",  "Address Line 1",  "Address Line 2",  "Address Line 3",  "Address Line 4",
                        "Address Line 5",  "Postcode",  "Open Date",  "Close Date",  "Null 1",  "Organisation SubType Code",  "Parent Organisation Code",  "Null 2",  "Null 3",  "Contact Telephone Number",
                        "Null 4",  "Null 5",  "Null 6",  "Amended Record Indicator",  "Null 7",  "GOR Code",  "Null 8",  "Null 9",  "Null 10")
    
    etr = readr::read_csv("https://nhsenglandfilestore.s3.amazonaws.com/ods/etr.csv",col_names = standardFormat,col_types = cols(.default=col_character())) %>% select(-starts_with("Null"))
    ets =  readr::read_csv("https://nhsenglandfilestore.s3.amazonaws.com/ods/ets.csv",col_names = standardFormat,col_types = cols(.default=col_character())) %>% select(-starts_with("Null"))
    eccg = readr::read_csv("https://nhsenglandfilestore.s3.amazonaws.com/ods/eccg.csv",col_names = standardFormat,col_types = cols(.default=col_character())) %>% select(-starts_with("Null"))
    eauth = readr::read_csv("https://nhsenglandfilestore.s3.amazonaws.com/ods/eauth.csv",col_names = standardFormat,col_types = cols(.default=col_character())) %>% select(-starts_with("Null"))
    
    return(list(
      etr = etr %>% rename(code = `Organisation Code`, name = `Name`, eauthCode = `High Level Health Geography`, eauthCode2 = `National Grouping`) %>% select(-starts_with("Null")),
      ets = ets %>% rename(code = `Organisation Code`, name = `Name`, eauthCode = `High Level Health Geography`, eauthCode2 = `National Grouping`) %>% select(-starts_with("Null")),
      eccg = eccg %>% rename(code = `Organisation Code`, name = `Name`, eauthCode = `High Level Health Geography`, eauthCode2 = `National Grouping`) %>% select(-starts_with("Null")),
      eauth = eauth %>% rename(code = `Organisation Code`, name = `Name`, eauthCode2 = `National Grouping`) %>% select(-starts_with("Null"))
    ))
    
    #TODO: locate these codes in ONS formats
    # join eauth to lower level organisationa
    # lookup long lat using postcodes for sites
    # 
    
    #})
  }
  
))

ump = UKCodeMappingProvider$new("~/Data/maps")
tmp = ump$getODScodes()
tmp = ump$getONSRegister()
