
#' ordered factor from age range labels
#' @param ageCat - a vector of age categories as strings
#' @param ageLabels - a vector of age range labels
#' @import dplyr
#' @return an ordered factor of age categories
#' @export 
ageCatToFactor = function(ageCat, ageLabels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")) {
  factor(
    ageCat,
    levels = ageLabels,
    ordered = TRUE
  )
}

#' create an ordered factor of ages from a continuous age 
#' 
#' @param age - a vector of ages
#' @param ageLabels - a vector of age range labels
#' @import dplyr
#' @return an ordered factor of age categories
#' @export
ageToAgeCat = function(age, ageLabels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80+")) {
  ageBreaks = c(ageLabels %>% stringr::str_extract("^[0-9]+") %>% as.integer(),Inf)
  return(cut(age,
             breaks = ageBreaks,
             labels = ageLabels,
             include.lowest = TRUE, ordered_result = TRUE
  ))
}

