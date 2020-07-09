#' Get a provider of UK postcodes
#'
UKPostcodeProvider = R6::R6Class("UKPostcodeProvider", inherit=PassthroughFilesystemCache, public = list(
  
  getFullONS = function(...) {
    self$getSaved("ONSPD",...,orElse = function() {
      csvfile = self$downloadAndUnzip("ONSPD", "https://www.arcgis.com/sharing/rest/content/items/fb894c51e72748ec8004cc582bf27e83/data", pattern = "ONSPD.*UK\\.csv")
      ONSPD = readr::read_csv(csvfile[1], col_types = readr::cols(
        pcd = readr::col_character(), pcd2 = readr::col_character(), pcds = readr::col_character(), dointr = readr::col_character(),
        doterm = readr::col_character(), oscty = readr::col_character(), ced = readr::col_character(), oslaua = readr::col_character(),
        osward = readr::col_character(), parish = readr::col_character(), usertype = readr::col_character(), oseast1m = readr::col_character(), osnrth1m = readr::col_character(),
        osgrdind = readr::col_character(), oshlthau = readr::col_character(), nhser = readr::col_character(), ctry = readr::col_character(), rgn = readr::col_character(),
        streg = readr::col_character(), pcon = readr::col_character(), eer = readr::col_character(), teclec = readr::col_character(), ttwa = readr::col_character(),
        pct = readr::col_character(), nuts = readr::col_character(), statsward = readr::col_character(), oa01 = readr::col_character(), casward = readr::col_character(),
        park = readr::col_character(), lsoa01 = readr::col_character(), msoa01 = readr::col_character(), ur01ind = readr::col_character(),oac01 = readr::col_character(),
        oa11 = readr::col_character(), lsoa11 = readr::col_character(), msoa11 = readr::col_character(), wz11 = readr::col_character(), ccg = readr::col_character(),
        bua11 = readr::col_character(), buasd11 = readr::col_character(), ru11ind = readr::col_character(), oac11 = readr::col_character(), lat = readr::col_double(),
        long = readr::col_double(), lep1 = readr::col_character(), lep2 = readr::col_character(), pfa = readr::col_character(), imd = readr::col_character(), calncv = readr::col_character(),
        stp = readr::col_character()
      )) %>% dplyr::mutate(
        outcode = pcd2 %>% stringr::str_sub(1,4) %>% stringr::str_trim()
      )
      return(ONSPD)
    })
  },
  
  getOutcodeCentroids = function(...) {
    self$getSaved("ONSPD_OUTCODES",.__C__.Other,orElse = function() {
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
  
  #TODO: weighted map LSOA -> CCG
  
  lookupWeightedFeatureByOutcode = function(df, outcodeVar = "outcode", onspdVar) {
    outcodeVar = ensym(outcodeVar)
    onspdVar = ensym(onspdVar)
    
    df = df %>% dplyr::mutate(tmp_outcode = !!outcodeVar)
    tmp = self$getFullONS()
    tmp = tmp %>% 
      dplyr::select(tmp_outcode = outcode, pcd, !!onspdVar) %>% 
      dplyr::semi_join(df, by="tmp_outcode") %>% 
      dplyr::group_by(tmp_outcode, !!onspdVar) %>% 
      dplyr::count() %>% 
      dplyr::group_by(tmp_outcode) %>% 
      dplyr::mutate(weight = n/sum(n)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-n)
    
    df = df %>% 
      dplyr::left_join(tmp, by="tmp_outcode", suffix=c(".original","")) %>% 
      dplyr::select(-tmp_outcode)
    return(df)
  },
  
  lookupLocation = function(df, postcodeVar = "pcd") {
    self$lookupFeatures(df, postcodeVar, vars(lat,long)) %>% sf::st_as_sf(coords=c("long","lat"), crs=4326)
  }
  
))


# pcds = UKPostcodeProvider$new("~/Data/maps")
# tmp = pcds$getFullONS()
