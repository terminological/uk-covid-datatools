#' Process chess data
#' @export
ChessProcessingPipeline = R6::R6Class("ChessProcessingPipeline", inherit=DataProvider, public = list(
  
  
  initialize = function(providerController, ...) {
    super$initialize(providerController, ...)
  },
  
  #' @description Chess data metrics on a hospital by hospital basis
  #' @details
  #' the chess data has some issues as
  #' different hospitals have different reporting strategies
  #' @param CHESSdf - the raw chess data set
  #' @return a data frame of stats by trust representing reporting quality.
  chessQuality = function(CHESSdf, updatedWithin = 14, date = max(CHESSdf$dateupdated,na.rm=TRUE), unknownDatesPercent = 0.2) {
    tmp2 = CHESSdf %>% dplyr::select(trustcode, contains("date"))
    tmp3 = tmp2 %>% tidyr::pivot_longer(cols = contains("date"), names_to = "type", values_to = "date")
    tmp4 = tmp3 %>% dplyr::group_by(trustcode) %>% dplyr::summarise(recentDate = max(date,na.rm = TRUE))
    
    CHESS_date = as.Date(date,"1970-01-01")
    
    hospAcc = CHESSdf %>% dplyr::group_by(trustcode, trustname) %>% dplyr::summarise(
      records = n(),
      patients = n_distinct(caseid),
      updatedRecords = sum(if_else(is.na(dateupdated),0,1)),
      knownOutcomes = sum(if_else(is.na(finaloutcomedate),0,1)),
      outcomeWithoutDates = sum(if_else(!is.na(finaloutcome) & is.na(finaloutcomedate),1,0)),
      knownAdmittedItu = sum(if_else(is.na(dateadmittedicu),0,1)),
      knownAdmittedItuPercent = knownAdmittedItu/records,
      knownOutcomePercent = knownOutcomes/records
    ) %>% dplyr::inner_join(tmp4, by="trustcode") %>% mutate(
      ituSubset = 
        as.Date(recentDate) >= CHESS_date-updatedWithin &  
        outcomeWithoutDates/knownOutcomes < unknownDatesPercent,
      admissionSubset = 
        as.Date(recentDate) >= CHESS_date-updatedWithin & 
        outcomeWithoutDates/knownOutcomes < unknownDatesPercent &
        knownAdmittedItuPercent < 0.5 &
        knownOutcomePercent > 0.1
    )
    return(hospAcc)
  },
  
  
  #' @description The subset of the CHESS data of patients which are admitted to ITU
  #' 
  #' the chess data has some issues as
  #' different hospitals have different reporting strategies
  #' 
  #' @param CHESSdf - the raw chess data set
  #' @param date - the date of the data set
  #' @return a data frame of stats by trust representing reporting quality.
  chessItuSubset = function(CHESSdf = self$getCHESS(), updatedWithin = 14, date = max(CHESSdf$dateupdated,na.rm=TRUE), unknownDatesPercent = 0.2) {
    
    # hospitals must have updated their data in last 14 days
    # and have fewer than 20% outcomes recorded with unknown dates
    incHosp = self$chessQuality(CHESSdf, updatedWithin, date, unknownDatesPercent) %>% filter(ituSubset)
    return(
      CHESSdf %>% self$chessDefaultFilter(as.numeric(labtestdate - hospitaladmissiondate) < 10) %>%
        inner_join(incHosp %>% dplyr::select(-trustname), by="trustcode") %>% 
        filter(!is.na(dateadmittedicu)) %>% 
        mutate(censorDate = as.Date(recentDate))
    )
  },
  
  #' @description The subset of the CHESS data of patients from hospitals that appear to be reporting all admissions
  #' 
  #' the chess data has some issues as
  #' different hospitals have different reporting strategies
  #' 
  #' @param CHESSdf - the raw chess data set
  #' @param date - the date of the data set
  #' @return a data frame of stats by trust representing reporting quality.
  chessAdmissionSubset = function(CHESSdf = self$getCHESS(), updatedWithin = 14, date = max(CHESSdf$dateupdated,na.rm = TRUE), unknownDatesPercent = 0.2) {
    # hospitals must have updated their data in last N days
    # and have fewer than 20% outcomes recorded with unknown dates
    # and have fewer than half of their patients admitted to ITU (remove hospitals that only submit ITU data)
    incHosp = self$chessQuality(CHESSdf, updatedWithin, date, unknownDatesPercent) %>% filter(admissionSubset)
    
    return(
      CHESSdf %>% self$chessDefaultFilter(as.numeric(labtestdate - hospitaladmissiondate) < 10) %>%
        inner_join(incHosp %>% dplyr::select(-trustname), by="trustcode") %>% 
        mutate(censorDate = as.Date(recentDate))
    )
  },
  
  # internal use
  chessDefaultFilter = function(CHESSdf = self$getCHESS(), ...) {
    # remove records with finaloutcome but no finaloutcomedate
    # remove records with outcome date earlier than admission date
    # remove unknown gender
    # create factors
    # create a decimal age
    CHESSdf %>% 
      filter(!(is.na(finaloutcomedate) & !is.na(finaloutcome) )) %>% 
      filter(is.na(finaloutcomedate) | hospitaladmissiondate <= finaloutcomedate) %>%
      filter(sex != "Unknown") %>% 
      filter(...) %>%
      mutate(
        sex = as.factor(sex),
        finaloutcome = as.factor(finaloutcome),
      ) %>% dplyr::mutate(
        sex = relevel(sex, ref="Male"),
        age = ageyear+agemonth/12
      )
  },
  
  ## Chess data
  
  #' @description Cleanses CHESS data set and adds a few fields
  #' 
  #' * Detects trusts that have not been updating records
  #' * establishes a censoring date on a trust by trust basis
  #' * checks all outcomes have an outcomedate
  #' * checks outcome dates are after admission date
  #' * excludes patients whose tests are >10 days after admission e.g. hospital acquired
  #' * excludes paediatric patients, and those with no known gender
  #' * adds censoring status (as status)
  #' * adds time from admission to outcome (as timeToOutcome)
  #' 
  #' @param path - path to the ff100 file
  
  #' @return cleansed CHESS data set
  cleanseCHESSData = function(CHESSdf, date, removeOutliers = TRUE) {
    
    hospAcc = self$chessQuality(CHESSdf)
    
    write.csv(hospAcc, "~/Dropbox/covid19/ventilator-demand/parameterisation/qualityCheck.csv")
    
    incHosp = hospAcc %>% dplyr::filter(
      recentDate >= CHESS_date-3 & outcomeWithoutDates < 5
    ) %>% dplyr::mutate(
      censorDate = as.Date(recentDate)
    )
    
    meanKnownOutcome = sum(incHosp$outcome)/sum(incHosp$records)
    sdKnownOutcome = sd(incHosp$knownOutcome)
    
    if (removeOutliers) {
      incHosp = incHosp %>% dplyr::filter(knownOutcome > meanKnownOutcome-sdKnownOutcome)
    }
    
    incHosp = incHosp %>% dplyr::select(
      trustcode, censorDate
    )
    
    CHESSClean = CHESSdf %>% dplyr::inner_join(incHosp, by="trustcode") %>% 
      filter(!(is.na(finaloutcomedate) & !is.na(finaloutcome) )) %>% 
      mutate(
        age = ageyear+agemonth/12
      ) %>% dplyr::filter(
        as.numeric(labtestdate - hospitaladmissiondate) < 10 &
          (is.na(finaloutcomedate) | hospitaladmissiondate <= finaloutcomedate) &
          sex != "Unknown"
      ) %>% dplyr::mutate(
        sex = as.factor(sex),
        finaloutcome = as.factor(finaloutcome)
      ) %>% dplyr::mutate(
        sex = relevel(sex, ref="Male")
      )
    
    return(CHESSClean)
  }
  
  
))