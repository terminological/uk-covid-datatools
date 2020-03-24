readSpatialFile<-function (fname){        # read and process a single file
  #  Function to read in Work/PlayInfections.dat’ type files (many columns, one for each class only of infecteds, Time is row number)
  tmp = read.table(file=fname,sep = ' ') %>% # make long data frame
    mutate(time=row_number()) %>% # count up time, as row number
    pivot_longer(-time) %>%
    mutate(ward=as.integer(str_remove(name,‘V’))) %>%
    select(-name)
  
  tmp = tmp %>% left_join(UKWardLookup %>% select(ward = FID,WD11CD,LAD11CD), by="ward")
  
  return(tmp)
}



readSpatialFile("~/Dropbox/covid19/ventilator-demand/ForMattData.dat")