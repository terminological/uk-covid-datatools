#' Get a provider of UK stats
#'
#' This function sets up a connection to the omop databse.
#' @keywords omop
#' @import dplyr
#' @export
UKPostcodeProvider = R6::R6Class("UKPostcodeProvider", inherit=PassthroughFilesystemCache, public = list(
  
  getFullONS = function() {
    self$getSaved("ONSPD",orElse = function() {
      csvfile = self$downloadAndUnzip("ONSPD", "https://www.arcgis.com/sharing/rest/content/items/fb894c51e72748ec8004cc582bf27e83/data", pattern = "ONSPD.*UK\\.csv")
      ONSPD = readr::read_csv(csvfile[1], col_types = cols(
        pcd = col_character(), pcd2 = col_character(), pcds = col_character(), dointr = col_character(),
        doterm = col_character(), oscty = col_character(), ced = col_character(), oslaua = col_character(),
        osward = col_character(), parish = col_character(), usertype = col_character(), oseast1m = col_character(), osnrth1m = col_character(),
        osgrdind = col_character(), oshlthau = col_character(), nhser = col_character(), ctry = col_character(), rgn = col_character(),
        streg = col_character(), pcon = col_character(), eer = col_character(), teclec = col_character(), ttwa = col_character(),
        pct = col_character(), nuts = col_character(), statsward = col_character(), oa01 = col_character(), casward = col_character(),
        park = col_character(), lsoa01 = col_character(), msoa01 = col_character(), ur01ind = col_character(),oac01 = col_character(),
        oa11 = col_character(), lsoa11 = col_character(), msoa11 = col_character(), wz11 = col_character(), ccg = col_character(),
        bua11 = col_character(), buasd11 = col_character(), ru11ind = col_character(), oac11 = col_character(), lat = col_double(),
        long = col_double(), lep1 = col_character(), lep2 = col_character(), pfa = col_character(), imd = col_character(), calncv = col_character(),
        stp = col_character()
      )) %>% dplyr::mutate(
        outcode = pcd2 %>% stringr::str_sub(1,4) %>% stringr::str_trim()
      )
      return(ONSPD)
    })
  },
  
  getOutcodeCentroids = function() {
    self$getSaved("ONSPD_OUTCODES",orElse = function() {
      self$getFullONS() %>% dplyr::mutate(outcode = pcd2 %>% stringr::str_sub(1,4) %>% stringr::str_trim()) %>% dplyr::group_by(outcode) %>% dplyr::summarise(lat = mean(lat), long = mean(long))
    })
  },
  
  # OUT_CODE_TO_LSOA = ONSPD_NOV_2019_UK %>% dplyr::mutate(outcode = pcd %>% stringr::str_sub(1,4) %>% stringr::str_trim()) %>% dplyr::select(outcode,lsoa11) %>% dplyr::distinct()
  # LSOA_TO_IMD = ONSPD_NOV_2019_UK %>% dplyr::select(lsoa11,imd) %>% dplyr::distinct()
  # LSOA_TO_CCG = ONSPD_NOV_2019_UK %>% dplyr::select(lsoa11,ccg) %>% dplyr::distinct()
  # LSOA_TO_NHSER = ONSPD_NOV_2019_UK %>% dplyr::select(lsoa11,nhser) %>% dplyr::distinct()
  
  lookupFeatures = function(df, postcodeVar = "pcd", onspdVars) {
    postcodeVar = ensym(postcodeVar)
    tmp = self$getFullONS()
    df = df %>% dplyr::mutate(tmp_pcd = stringr::str_replace(!!postcodeVar, " ", strrep(" ",8-stringr::str_length(!!postcodeVar))))
    df = df %>% dplyr::left_join(tmp %>% dplyr::select(tmp_pcd = pcd, !!!onspdVars), by="tmp_pcd", suffix=c(".original","")) %>% dplyr::select(-tmp_pcd)
    return(df)
  },
  
  lookupLocation = function(df, postcodeVar = "pcd") {
    self$lookupFeatures(df, postcodeVar, vars(lat,long))
  }
  
))


# pcds = UKPostcodeProvider$new("~/Data/maps")
# tmp = pcds$getFullONS()
