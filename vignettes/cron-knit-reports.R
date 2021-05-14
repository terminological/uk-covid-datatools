#!/usr/bin/Rscript

setwd("~/Git/uk-covid-datatools/vignettes/")



try({
  rmarkdown::render('current-rt-data.Rmd',output_dir = paste0("~/Dropbox/covid19/current-rt/",Sys.Date()), output_file=paste0('current-rt-data-',Sys.Date(),'.pdf'))
})

try({
  source("current-rt-export.R")
})

# try({
#   rmarkdown::render('growth-rates-by-age.Rmd',output_dir = paste0("~/Dropbox/covid19/current-rt/",Sys.Date()), output_file=paste0('covid-growth-by-age-',Sys.Date(),'.pdf'))
# })
  
try({
  rmarkdown::render('demographics-by-time.Rmd',output_dir = paste0("~/Dropbox/covid19/current-rt/",Sys.Date()), output_file=paste0('covid-demographics-',Sys.Date(),'.pdf'))
})

rm(list = ls())
try({
  source("./b-1-351-s-pos-leaflet.R")
})
  
# try({
#   rmarkdown::render('in-development/tiers-vs-growth-rates.Rmd',output_dir = paste0("~/Dropbox/covid19/current-rt/",Sys.Date()), output_file=paste0('tiers-vs-growth-rates-',Sys.Date(),'.pdf'))
# })
  
try({
  source("in-development/tiers-vs-growth-animation.R")
})

