#' loads a spatial model file
#' 
#' @param fname - a filename containing a ward based metaward model .dat file
#' @import dplyr
#' @return a data frame containing the simulation results
#' @export
readSpatialFile = function(fname, ignoreErrors=FALSE) {        # read and process a single file
  #  Function to read in Work/PlayInfections.dat’ type files (many columns, one for each class only of infecteds, Time is row number)
  tmp = read.table(file=fname,sep = ' ') %>% # make long data frame
    mutate(time=row_number()) %>% # count up time, as row number
    pivot_longer(-time) %>%
    mutate(ward=as.integer(str_remove(name,'V'))) %>%
    select(-name)
  #tmp = tmp %>% left_join(UKWardLookup %>% select(ward = FID,WD11CD,LAD11CD), by="ward")
  tmp2 = tmp %>% left_join(UKWardLookup %>% select(ward = FID,WD11CD), by="ward") %>% left_join(WD11_to_LAD19, by="WD11CD")
  if(!ignoreErrors)
    tmp2 %>% ensurer::ensure(!any(is.na(.$LAD19CD)))
  return(tmp2)
}



