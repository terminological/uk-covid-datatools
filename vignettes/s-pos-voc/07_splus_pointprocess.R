library(tidyverse)
library(lubridate)
library(INLA)
library(Matrix)
library(htmltools)

# load lsoa data (polygons, and adjacency matrix)
load('lsoa_sp.Rdata')
load('lsoa_adj.Rdata')
all_lsoas = rownames(lsoa_adj)

# index weeks starting Mon 1 Feb 2021
all_weeks = paste(seq.Date(from=as.Date('2021-02-01'), to=as.Date('2021-05-17'), by='7 days'))

# read id, lsoa, date, x, y
splus = read_csv('data/input-data-2021-05-03.zip', col_types='--cc---------c-dd') %>%
  mutate(response = 1) %>%
  rename(lsoa_code = LSOA_code) %>%
  mutate(week = paste(floor_date(as.Date(date), unit='week', week_start=1))) %>%
  filter(as.Date(week) >= as.Date('2021-02-01'))

# add lsoas and weeks where no event was observed, set response = 0
inla_data = splus %>% 
            group_by(lsoa_code, week) %>%
            summarise(response = sum(response), .groups='drop') %>%
            full_join(crossing(week = all_weeks, lsoa_code = all_lsoas), 
                      by=c('week', 'lsoa_code')) %>%
            mutate(response = ifelse(is.na(response), 0, response)) %>%
            mutate(i_week = as.numeric(factor(week, levels=all_weeks))) %>%
            mutate(i_lsoa = as.numeric(factor(lsoa_code, levels=all_lsoas)))



# spatial poisson process on only i_week=11 data, hyperparameters:
res = inla(response ~ f(i_lsoa, model='besag', graph=lsoa_adj), 
           data=filter(inla_data, i_week == 11), 
           family='poisson', 
           control.inla=list(strategy='gaussian', int.strategy='eb'), 
           verbose=TRUE)
# besag: log(theta) = -2.5
# bym: log(theta1,iid) = 5.6; log(theta2,car) = -2.5


i_week_fit = 9:12
week_fit = all_weeks[i_week_fit]

# spatio-temporal poisson process on 4 weeks
res = inla(
  formula = response ~ -1 + 
                       f(i_week, model='rw2', constr=FALSE, 
                         hyper=list(theta=list(initial=8, fixed=TRUE))) +
                       f(i_lsoa, model='besag', graph=lsoa_adj, 
                         #hyper=list(theta=list(initial=-2.5, fixed=TRUE))),
                         hyper=list(theta=list(initial=-1, fixed=TRUE))),
  data = filter(inla_data, i_week %in% i_week_fit),
  family='poisson', 
  verbose=TRUE,
  control.predictor=list(link=1), 
  control.compute=list(waic=TRUE),
  control.inla=list(strategy='gaussian', int.strategy='eb'))

inla_res = inla_data %>%
           filter(i_week %in% i_week_fit) %>% 
           mutate(fitted = res$summary.fitted.values$mean) %>% 
           arrange(i_week, i_lsoa)

n_lsoa = length(all_lsoas)

# merge fitted into lsoa_sp for plotting
for (ii in 1:length(i_week_fit)) {
  lsoa_sp[[ paste0('inten_w', i_week_fit[ii]) ]] = inla_res$fitted[1 : n_lsoa + (ii-1) * n_lsoa]
}

splus_sp = list()
labls = list()
for (w in i_week_fit) {
  splus_ = splus %>% filter(week == all_weeks[w]) %>%
                     mutate(x = x + rnorm(n(), 0, 5e-4),
                            y = y + rnorm(n(), 0, 5e-4))
  if (nrow(splus_) > 0) {
    coordinates(splus_) = ~x+y
    proj4string(splus_) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84")
  }
  splus_sp[[ paste0('week', w) ]] = splus_


  labl_ = 
  paste0('<strong>LSOA: </strong>', lsoa_sp$LSOA11CD,
         '<br>', 
         '<strong>Rate: </strong>', round(lsoa_sp[[ paste0('inten_w', w) ]], 3), '<br>') %>%
         as.list %>% lapply(HTML)

  labls[[ paste0('week', w) ]] = labl_

}



library(leaflet)
pal = colorNumeric(rev(rainbow(20))[-(1:3)], domain=c(0,2))
lft = leaflet() %>% 
  addTiles()
for (ii in 1:length(i_week_fit)) {
  iw = i_week_fit[ii]
  ww = week_fit[ii]
  lft = lft %>%
  addPolygons(data=lsoa_sp, fillColor=~pal(eval(parse(text=paste0('inten_w', iw)))), 
              stroke=FALSE, fillOpacity=.7, group=paste0('w/c ', ww), label=labls[[paste0('week',iw)]]) %>%
  addCircles(data=splus_sp[[paste0('week', iw)]], color='black',
             opacity=1, fillOpacity=1,
             group=paste0('w/c ', ww)) 
}
lft = lft %>%
  addLegend('topright', 
            pal=pal, 
            values=~splus_per_week, 
            data=tibble(splus_per_week=seq(0,2,.1)), 
            opacity=1) %>%
  addLayersControl(baseGroups=c(paste0('w/c ', week_fit), 'none'),
                   options=layersControlOptions(collapsed=FALSE, position='topleft')) 
  
  

