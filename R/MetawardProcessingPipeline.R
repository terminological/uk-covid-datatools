#' Process metawards output
#' @export
MetawardProcessingPipeline = R6::R6Class("MetawardProcessingPipeline", inherit=DataProvider, public = list(
  
  
  initialize = function(providerController, ...) {
    super$initialize(providerController, ...)
  },

  #' @description loads a spatial model file
  #' 
  #' @param fname - a filename containing a ward based metaward model .dat file
  
  #' @return a data frame containing the simulation results

  readSpatialFile = function(fname, ignoreErrors=FALSE) {        # read and process a single file
    #  Function to read in Work/PlayInfections.dat’ type files (many columns, one for each class only of infecteds, Time is row number)
    tmp = read.table(file=fname,sep = ' ') %>% # make long data frame
      mutate(time=row_number()) %>% # count up time, as row number
      pivot_longer(-time) %>%
      mutate(ward=as.integer(str_remove(name,'V'))) %>%
      select(-name)
    #tmp = tmp %>% left_join(UKWardLookup2011 %>% select(ward = FID,WD11CD,LAD11CD), by="ward")
    tmp2 = tmp %>% left_join(UKWardLookup2011 %>% select(ward = FID,WD11CD), by="ward") %>% left_join(WD11_to_LAD19, by="WD11CD")
    if(!ignoreErrors)
      tmp2 %>% ensurer::ensure(!any(is.na(.$LAD19CD)))
    return(tmp2)
  },
  
  #' @description loads an age time matrix from a csv
  #' 
  #' @param url - a url or filename for a csv file
  
  #' @return a data frame containing the filter
  loadAgeTimeMatrix = function(url) {
    tmp = read.csv(url) %>% 
      pivot_longer(cols=starts_with("X"), names_to = "days", values_to = "probability") %>%
      mutate(days = as.integer(str_remove(days,"X"))) 
    tmp = tmp %>% 
      rename(ageGroup = age)
    return(tmp)
  },
  
  
  #' @description applies a age time matrix to a dataframe containing an incidence, an age category column, and a date column
  #' @details
  #' Make sure your age categories are the saem as the matrix you are applying
  #' The age time matrix can implement a delay, aggregate over time and or filter by age
  #' @param inputDf - an input df containing an incidence col, a date col and a ageCategory col this must be grouped 
  #' @param matrixDf - a dataframe containing a sparse matrix of probabilities where age is labelled 
  #' @param oldIncidenceVar - the name of the input incidence col
  #' @param newIncidenceVar - the name of the output incidence col
  #' @param oldDateVar - the name of the input date column
  #' @param newDateVar - the name of the output date column
  #' @param ageCatVar - the age category variable
  
  #' @return a data frame containing the simulation results
  applyAgeTimeMatrix = function(inputDf, matrixDf, 
                                oldIncidenceVar = "incidence", newIncidenceVar = "incidence",
                                oldDateVar = "date", newDateVar = "date",
                                ageCatVar = "ageGroup") {
    
    oldIncidenceVar = ensym(oldIncidenceVar)
    newIncidenceVar = ensym(newIncidenceVar)
    oldDateVar = ensym(oldDateVar)
    newDateVar = ensym(newDateVar)
    ageCatGrp = ensym(ageCatVar)
    grps = inputDf %>% groups()
    
    if (length(grps)==0) stop("inputDf must be grouped to represent the geographical unit we are testing for")
    
    simEnd = max(inputDf %>% pull(!!oldDateVar))
    
    outputDf = inputDf %>% left_join(matrixDf %>% rename(!!ageCatVar := ageGroup), by=ageCatVar) %>% 
      mutate(
        !!newDateVar := !!oldDateVar+days,
        !!newIncidenceVar := !!oldIncidenceVar*probability
      ) %>%
      group_by(
        !!!grps, !!newDateVar, !!ageCatGrp
      ) %>%
      summarise (
        !!newIncidenceVar := sum(!!newIncidenceVar)
      ) %>% filter(
        simEnd >= !!newDateVar
      ) 
    outputDf %>% ensure(
      all(!is.na(.[as_label(newIncidenceVar)]))
    ) %>% ensure(
      nrow(.) == nrow(inputDf)
    ) %>% invisible()
    
    return(outputDf %>% group_by(!!!grps))
  },
  
  #' @description loads a ventilator demand matrices
  #' 
  
  #' @return a list of matrices

  loadDefaultAgeTimeMatrices = function() {
    
    infected2SymptomaticUrl = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSl_h_Vjag3N1bkfBlqCotUmB_Mg5BPfugGzhsmK-ByycxKGvODIGBD-IRtEwjOclu4UUchiZ3j45xU/pub?gid=2044085188&single=true&output=csv"
    symptomaticToHospitalisationUrl = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSl_h_Vjag3N1bkfBlqCotUmB_Mg5BPfugGzhsmK-ByycxKGvODIGBD-IRtEwjOclu4UUchiZ3j45xU/pub?gid=1708683854&single=true&output=csv"
    hospitalisedToInpatientUrl = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSl_h_Vjag3N1bkfBlqCotUmB_Mg5BPfugGzhsmK-ByycxKGvODIGBD-IRtEwjOclu4UUchiZ3j45xU/pub?gid=1784137714&single=true&output=csv"
    symptomaticToItuUrl = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSl_h_Vjag3N1bkfBlqCotUmB_Mg5BPfugGzhsmK-ByycxKGvODIGBD-IRtEwjOclu4UUchiZ3j45xU/pub?gid=895816136&single=true&output=csv"
    ituAdmitToItuInpatientUrl = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSl_h_Vjag3N1bkfBlqCotUmB_Mg5BPfugGzhsmK-ByycxKGvODIGBD-IRtEwjOclu4UUchiZ3j45xU/pub?gid=68691773&single=true&output=csv"
    
    matrix = list(
      infected2Symptomatic = loadAgeTimeMatrix(infected2SymptomaticUrl),
      symptomaticToHospitalisation = loadAgeTimeMatrix(symptomaticToHospitalisationUrl),
      hospitalisedToInpatient=loadAgeTimeMatrix(hospitalisedToInpatientUrl),
      symptomaticToItu = loadAgeTimeMatrix(symptomaticToItuUrl),
      ituAdmitToItuInpatient = loadAgeTimeMatrix(ituAdmitToItuInpatientUrl)
    )
    
    return(matrix)
  }
  
  
  
  
))
  
#TEST:
# groupedDf = bind_rows(
#   tibble(grouping="grp1",date=as.Date("2001-01-01"):as.Date("2001-03-30"), value=c(1:length(as.Date("2001-01-01"):as.Date("2001-03-30")))),
#   tibble(grouping="grp2",date=as.Date("2001-01-01"):as.Date("2001-02-28"), value=c(length(as.Date("2001-01-01"):as.Date("2001-02-28")):1))
# ) %>% group_by(grouping) %>% mutate(date=as.Date(date,"1970-01-01"))
# distributionsDf = tibble(grouping = c("grp1","grp2"), distribution = c("norm","exp"), mean=c(2,NA), sd=c(0.5,NA), rate=c(NA,2))
# distributionDistDf = tibble(grouping = c("grp1","grp2"), distribution = c("norm","exp"), 
#                             mean_mean=c(2,NA), 
#                             mean_sd=c(1.5,NA), 
#                             mean_max=c(4,NA), 
#                             mean_min=c(0,NA), 
#                             sd_mean=c(0.5,NA),
#                             sd_sd=c(0.5,NA), 
#                             sd_max=c(1,NA), 
#                             sd_min=c(0.25,NA), 
#                             rate_mean=c(NA,2),
#                             rate_sd=c(NA,0.5),
#                             rate_max=c(NA,3),
#                             rate_min=c(NA,1)
# )

#TODO: tsBootstrapFilter = function(groupedDf, binomialDistDf)