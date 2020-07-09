#' Process metawards output
#' @export
MetawardProcessingPipeline = R6::R6Class("MetawardProcessingPipeline", inherit=PassthroughFilesystemCache, public = list(
  
  
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
  },
  
  
  
  #' @description applies a set of parameterised convolution functions groupwise to input data
  #' 
  
  #' @param groupedDf - an optionally grouped dataframe, containing at dateVar, and a valueVar to be colvolved
  #' @param distributionsDf - a dataframe containing the same grouping columns as groupedDf plus "distribution", and relevant distribution parameter columns
  #' @param dateVar - 
  #' @param days - 
  #' @param timepoints - the times to 
  #' @param padLeft - what can we assume about the run in to the current values? default NA.
  #' @return a list of matrices

  tsParameterizedConvolution = function(groupedDf, distributionsDf, outputVar = "output", valueVar = "value", dateVar="date", days = 30, timepoints = 0:days, padLeft = NA, padRight = NA) {
    grps = groupedDf %>% groups()
    
    discreteDistDf = distributionsDf %>% group_by(!!!grps) %>% group_modify(function(d,g,...) {
      if(nrow(d) != 1) stop("A single covolution distribution must be defined for each group in groupedDf")
      func = paste0("p",d$distribution)
      funcCDF = paste0("p",d$distribution)
      params = formals(func)
      params2 = d %>% select(any_of(names(params))) %>% as.list()
      start = timepoints[1:(length(timepoints)-1)]
      end = timepoints[2:(length(timepoints))]
      discreteDist = tibble(
        start = start,
        end = end,
        prob = do.call(func, c(q=list(end),params2))-do.call(func, c(q=list(start),params2)),
        surv = 1-do.call(func, c(q=list(start),params2))
      )
      return(discreteDist)
    })
    
    tsDiscreteCovolution(groupedDf, discreteDistDf, {{outputVar}}, {{valueVar}},{{dateVar}},padLeft, padRight)
    
  },
  
  #' @description applies a set of parameterised convolution functions groupwise to input data
  #' 
  
  #' @param groupedDf - an optionally grouped dataframe, containing at dateVar, and a valueVar to be colvolved
  #' @param discreteDistDf - a dataframe containing the same grouping columns as groupedDf plus "start", "end" and "prob" columns
  #' @param dateVar - 
  #' @param padLeft - what can we assume about the run in to the current values? default NA.
  #' @param padRight - what can we assume about the run in to the current values? default NA.
  #' @return a list of matrices

  tsDiscreteConvolution = function(groupedDf, discreteDistDf, outputVar = "output", valueVar="value", dateVar="date", pExpr="prob", padLeft=NA, padRight=NA ) {
    grps = groupedDf %>% groups()
    valueVar = ensym(valueVar)
    dateVar = ensym(dateVar)
    outputVar = ensym(outputVar)
    pExpr = ensym(pExpr)
    joinCols = c(sapply(grps, as_label),"tmp_join")
    finalJoinCols = c(sapply(grps, as_label),as_label(dateVar))
    
    dateRanges = groupedDf %>% summarise(min_date = min(!!dateVar), max_date = max(!!dateVar)) %>% mutate(tmp_join=1)
    
    combinations = discreteDistDf %>% select(!!!grps,start) %>% mutate(tmp_join=1) %>% inner_join(dateRanges, by=joinCols)
    
    padLeftDf = combinations %>% mutate(tmp_value=padLeft, tmp_date=min_date-start-1) %>% select(-start)
    padRightDf = combinations %>% mutate(tmp_value=padRight, tmp_date=max_date+start+1) %>% select(-start)
    
    tmp = groupedDf %>% select(!!!grps,tmp_value=!!valueVar, tmp_date=!!dateVar) %>% group_by(!!!grps) %>%
      mutate(tmp_join=1, min_date = min(tmp_date), max_date = max(tmp_date))
    
    tmp = bind_rows(padLeftDf,tmp,padRightDf)
    
    tmp2 = tmp %>% 
      inner_join(discreteDistDf %>% mutate(tmp_join=1), by=joinCols)
    
    tmp3 = tmp2 %>% mutate(eff_date = tmp_date+start, eff_value = tmp_value*!!pExpr) %>% select(-tmp_join) %>%
      filter(eff_date <= max_date & eff_date >= min_date)
  
    tmp4 = tmp3 %>% group_by(!!!grps,eff_date) %>% summarise(!!outputVar := sum(eff_value)) %>% mutate(!!dateVar := as.Date(eff_date,"1970-01-01")) %>% select(-eff_date)
  
    return(groupedDf %>% inner_join(tmp4, by=finalJoinCols))
  },
  
  #' @description calculates a set of bootstrap parameter distributions
  #' 
  
  #' @param distributionDistDf - a grouped data frame containing the same columns as those grouped in groupedDf, plus "distribution" and columns for "<parameter>_mean","<parameter>_sd","<parameter>_min","<parameter>_max" e.g. shape_mean, shape_sd, shape_min, shape_max, rate_mean, rate_sd, rate_min, rate_max
  #' @param bootstraps - number of bootstrap iterations 
  #' @return a list of randomly selected bootstraps conforming to the specifications.

  bootstrapDistributions = function(distributionDistDf, bootstraps=100) {
    grps = distributionDistDf %>% groups()
    distributionsDf = distributionDistDf %>% group_by(!!!grps) %>% group_modify(function(d,g,...) {
      if(nrow(d) != 1) stop("A single covolution distribution must be defined for each group")
      func = paste0("p",d$distribution)
      out = tibble(
        distribution = d %>% pull(distribution),
        bootstrap_iteration = 1:bootstraps
      )
      for (param in names(formals(func))) {
        if(all(paste0(param,c("_mean","_sd","_min","_max")) %in% colnames(d))) {
          
          paramMean = d %>% pull(paste0(param,"_mean"))
          paramSd = d %>% pull(paste0(param,"_sd"))
          paramMin = d %>% pull(paste0(param,"_min"))
          paramMax = d %>% pull(paste0(param,"_max"))
          paramBoot = rnorm(bootstraps, paramMean, paramSd)
          paramBoot[paramBoot>paramMax]=paramMax
          paramBoot[paramBoot<paramMin]=paramMin
          out = out %>% add_column(!!param := paramBoot)
        }
      }
      return(out)
    })
    return(distributionsDf)
  },
  
  #' @description executes a convolution using bootstrapped parameterized distributions
  #' 
  
  #' @param distributionDistDf - a grouped data frame containing the same columns as those grouped in groupedDf, plus "distribution" and  columns for "<parameter>_mean","<parameter>_sd","<parameter>_min","<parameter>_max" e.g. shape_mean, shape_sd, shape_min, shape_max, rate_mean, rate_sd, rate_min, rate_max
  #' @param bootstraps - number of bootstrap iterations 
  #' @return the full bootstrap result (i.e. not summarized) which can be futher convoluted (asd long as the number of bootstrap iterations are kept the same).

  tsBootstrapConvolution = function(groupedDf, distributionDistDf, bootstraps=100, outputVar = "output", valueVar="value", dateVar="date", days = 30, timepoints = 0:days, padLeft=NA, padRight=NA) {
    grps = groupedDf %>% groups()
    tmp = groupedDf %>% crossing(tibble(bootstrap_iteration = 1:bootstraps)) %>% group_by(!!!grps,bootstrap_iteration)
    distributionDistDf = distributionDistDf %>% group_by(!!!grps)
    out = tmp %>% tsParameterizedConvolution(bootstrapDistributions(distributionDistDf, bootstraps = bootstraps), 
          outputVar = {{outputVar}}, valueVar = {{valueVar}}, dateVar = {{dateVar}}, days = days, timepoints = timepoints, padLeft = padLeft, padRight = padRight)
    out = out %>% ungroup() %>% group_by(!!!grps)
    return(out)
  },
  
  #' @description summarise the result of a bootstrapped convolution using parameterized distributions
  #' 
  
  #' @param distributionDistDf - a grouped data frame containing the same columns as those grouped in groupedDf, plus "distribution" and  columns for "<parameter>_mean","<parameter>_sd","<parameter>_min","<parameter>_max" e.g. shape_mean, shape_sd, shape_min, shape_max, rate_mean, rate_sd, rate_min, rate_max
  #' @param bootstraps - number of bootstrap iterations 
  #' @return the full bootstrap result (i.e. not summarized) which can be futher convoluted (asd long as the number of bootstrap iterations are kept the same).

  summariseBootstrap = function(groupedDf, outputVars = vars(output), dateVar = "date") {
    grps = groupedDf %>% groups()
    dateVar = ensym(dateVar)
    joinCols = c(sapply(grps,as_label),as_label(dateVar))
    out = groupedDf %>% select(!!!grps,!!dateVar) %>% distinct()
    p <- c(0.2, 0.5, 0.8)
    for(outputVar in outputVars) {
      
      tmp = groupedDf %>% group_by(!!!grps,!!dateVar) %>% summarise(
        !!(paste0("CI.0.025(",as_label(outputVar),")")) := quantile(!!outputVar,p=0.025),
        !!(paste0("CI.0.1(",as_label(outputVar),")")) := quantile(!!outputVar,p=0.1),
        !!(paste0("Median(",as_label(outputVar),")")) := quantile(!!outputVar,p=0.5),
        !!(paste0("CI.0.9(",as_label(outputVar),")")) := quantile(!!outputVar,p=0.9),
        !!(paste0("CI.0.975(",as_label(outputVar),")")) := quantile(!!outputVar,p=0.975),
      )
      
      out = out %>% inner_join(tmp, by=joinCols)
    }
    return(out)
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