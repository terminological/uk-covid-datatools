#### Common interface definitions ----

## Covid timeseries ----
#### Incidence data loaders ----
# These functions all aim to produce a standard format:
# "date"      
# "code" - the geographic code
# "name" - the geographic region name
# "codeType" - the geographic region type (ONS code or "NHS site", "NHS trust")
# "statistic" - the factor the data is measuring: one of: case|death|icu admission|hospital admission|symptom|triage|serology|test|information seeking
# "source" - the provenance of the source    
# "ageCat" - an age range e.g. ("<5", "5-10", "10+")  
# "gender" - male|female
# "type" - the type of the value one of incidence|prevalence|cumulative|background|bias
# "value" - the value of the statistic
# "subgroup" -  a subgroup for source specific factors (e.g. subtypes of symptoms, severity of cases) or NA if none
# "note" - any other info

#' The covid timeseries format 
#' @return A dataframe that is specific to the covid timeseries format.
#' @export
covidTimeseriesFormat = ensurer::ensures_that(
  is.data.frame(.) ~ "not a data frame",
  all(c("code","name","codeType","statistic","source","ageCat","gender","type","value","subgroup","date") %in% colnames(.)) ~ "missing columns",
  lubridate::is.Date(.$date) ~ "incorrect date format",
  all(unique(.$statistic) %in% c("case","death","icu admission","hospital admission","symptom","triage","serology","test","information seeking","negative test")) ~ "unknown statistic value",
  all(unique(.$type) %in% c("incidence","prevalence","cumulative","background","bias")) ~ "unknown type value",
  length(unique(.$code)) == length(unique(paste0(.$code,.$name))) ~ "more than one code/name combination per code"
)

as.CovidTimeseriesFormat = function(
  dataframe,
  dateExpr,
  valueExpr,
  statisticExpr,
  typeExpr,
  codeExpr="no code",
  nameExpr="no name",
  codeTypeExpr="undefined", 
  sourceExpr = "undefined",
  ageCatExpr = NA,
  genderExpr = NA,
  subgroupExpr = NA
) {
  codeExpr = enexpr(codeExpr)
  nameExpr = enexpr(nameExpr)
  codeTypeExpr = enexpr(codeTypeExpr)
  statisticExpr = enexpr(statisticExpr)
  valueExpr = enexpr(valueExpr)
  dateExpr = enexpr(dateExpr)
  ageCatExpr = enexpr(ageCatExpr)
  genderExpr = enexpr(genderExpr)
  typeExpr = enexpr(typeExpr)
  subgroupExpr = enexpr(subgroupExpr)
  sourceExpr = enexpr(sourceExpr)
  return(dataframe %>% dplyr::ungroup() %>% mutate(
    code = !!codeExpr, 
    name = !!nameExpr,
    codeType = !!codeTypeExpr, 
    statistic = !!statisticExpr,
    value = !!valueExpr,
    date = !!dateExpr,
    ageCat = !!ageCatExpr,
    gender = !!genderExpr,
    type = !!typeExpr,
    subgroup = !!subgroupExpr,
    source = !!sourceExpr
  )) 
}

covidStandardSelect = function(df) {
  return(df %>% dplyr::select(code, name, codeType, statistic,
                            value, date, ageCat, gender,
                            type, subgroup, source))
}

covidStandardGrouping = function(ts, ...) {
  vars = ensyms(...)
  names = ts %>% colnames()
  names = names[names %in% c("code","codeType","name","source","subgroup","statistic","gender","ageCat","type")]
  names = names[!(names %in% sapply(vars,as_label))]
  grps = sapply(names,as.symbol)
  return(ts %>% dplyr::ungroup() %>% dplyr::group_by(!!!grps))
}

covidStandardDateGrouping = function(ts, ...) {
  covidStandardGrouping(ts,...) %>% group_by(date, .add=TRUE)
}

## Uncertain distribution parameters ----

#' Check timeseries conforms
#' @return ensures params consistent
#' @export
parameterDefinition = ensurer::ensures_that(
  is.data.frame(.) ~ "not a data frame",
  all(c("param","mean","sd","lower","upper") %in% colnames(.)) ~ "missing columns",
  length(unique(.$param)) == length(.$param) ~ "multiple parameter definitions per distribution"
)

as.ParameterDefinition = function(dataframe, paramExpr, meanExpr, sdExpr = NA, lowerExpr = NA, upperExpr = NA) {
  paramExpr = enexpr(paramExpr)
  meanExpr = enexpr(meanExpr)
  sdExpr = enexpr(sdExpr)
  lowerExpr = enexpr(lowerExpr)
  upperExpr = enexpr(upperExpr)
  return(dataframe %>% dplyr::ungroup() %>% dplyr::mutate(
    param = !!paramExpr,
    mean = !!meanExpr,
    sd = !!sdExpr,
    lower = !!lowerExpr,
    upper = !!upperExpr
  ))
} 


## Uncertain distributions ----

distributionsDefinition = ensurer::ensures_that(
  is.data.frame(.) ~ "not a data frame",
  all(c("dist","param","mean","sd","lower","upper") %in% colnames(.)) ~ "missing columns",
  !any(duplicated(select(.,!!!groups(.),dist,param))) ~ "multiple parameter definitions for distribution"
)

as.DistributionsDefinition = function(dataframe, distExpr, paramExpr, meanExpr, sdExpr = NA, lowerExpr = NA, upperExpr = NA) {
  distExpr = enexpr(distExpr)
  paramExpr = enexpr(paramExpr)
  meanExpr = enexpr(meanExpr)
  sdExpr = enexpr(sdExpr)
  lowerExpr = enexpr(lowerExpr)
  upperExpr = enexpr(upperExpr)
  return(dataframe %>% dplyr::ungroup() %>% dplyr::mutate(
    dist = !!distExpr,
    param = !!paramExpr,
    mean = !!meanExpr,
    sd = !!sdExpr,
    lower = !!lowerExpr,
    upper = !!upperExpr
  ))
}