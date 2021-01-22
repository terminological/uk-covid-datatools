library(tidyverse)
devtools::load_all("~/Git/uk-covid-datatools/")
devtools::load_all("~/Git/standard-print-output/")

setwd("~/Git/uk-covid-datatools/")

ukcovidtools::setup()
ggplot2::theme_set(standardPrintOutput::defaultFigureLayout())
standardPrintOutput::setDefaults()

## Get data -----

ll = dpc$spim$getLineList()
#ll %>% filter(LTLA_code == "E06000001") %>% group_by(specimen_date,asymptomatic_indicator) %>% count() %>% pivot_wider(names_from=asymptomatic_indicator, values_from = n) %>% View()


ltla = dpc$spim$getLineListIncidence(specimenOrReport = "specimen", codeTypes = c("NHSER","LAD"),subgroup = asymptomatic_indicator) %>%
  filter(codeType %in% c("NHSER","LAD") & subgroup!="Y") %>% 
      tsp$aggregateAge() %>% 
      tsp$aggregateGender() %>% 
      tsp$aggregateSubgroup() %>%
      dpc$demog$findDemographics()

ltla2 = ltla %>% tsp$logIncidenceStats(growthRateWindow = 14, smoothingWindow = 14)
# Get NHSER parent for each LTLA 
ltla3 = ltla2 %>% tsp$codes$parentCode(parentCodeTypes = "NHSER")


# Tier data
tidyOut = dpc$datasets$getTiers()

ltla4 = ltla3 %>% left_join(tidyOut %>% select(-name,-codeType), by=c("code","date"))
ltla4$tier = factor(ltla4$tier, levels=c("none","one","one+","two","two+","local","three","three+","four+","lockdown"),ordered=TRUE)



ltlaByTier = ltla4 %>% filter(codeType!="NHSER" & name!="Unknown (England)") %>% group_by() %>% arrange(code,date)
pal = c("none"="#D0D0D0","one"="#A0A0A0","one+"="#40C040","two"="#6060C0","two+"="#4040C0","local"="#C08080","three"="#C06060","three+"="#C04040","four+"="#C040C0","lockdown"="#404040")

plotDates = c(as.Date(c("2020-08-14","2020-09-14","2020-10-07","2020-11-02","2020-11-23")),max(ltlaByTier$date))
dates = tibble::tibble(progress = seq(min(ltla$date),max(ltla$date),1))

## Animation by region ----

animBase = ggplot(
  ltlaByTier %>% 
    #filter(date == max(date)) %>%
    filter(!is.na(parentCode)), # There are some missing here
    aes(x=Growth.windowed.value, y=Est.value/population*1000000,size=population,colour=parentName),alpha=0.8) + 
  geom_vline(xintercept = 0, colour="grey40")+
  geom_point() + #alpha=0.2) + 
  ylab("Symptomatic covid per 1M per day")+
  xlab("7 day growth rate")+
  scale_size_area(max_size=3,guide="none")+
  scale_y_continuous(trans="log1p", breaks=c(0,2,5,20,50,200,500,2000,5000)) +
  scale_alpha_manual(values = c(1,0.3), guide="none") +
  scale_colour_brewer(palette = "Dark2", name="Region")+#facet_wrap(vars(parentName))+#, oob=scales::squish, lim=c(-0.10,0.10))+
  coord_cartesian(xlim=c(-0.25,0.25))+
  #guides(col=guide_legend(direction="horizontal",nrow=1,title.theme = element_blank(),label.theme = element_text(size = 12)))+
  theme(
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle=0,hjust=0.5),
    axis.title = element_text(size = 14),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text =  element_text(size = 12)
    )

anim = animBase +
  gganimate::transition_time(date)+
  gganimate::enter_fade() +
  gganimate::exit_fade()


mainGif = gganimate::animate(anim, renderer=gganimate::magick_renderer(), width=600, height=600, nframes=dates %>% nrow())

## Animation By tier ----

dates = tibble::tibble(progress = seq(min(ltla$date),max(ltla$date),1))


animTier = ggplot(
  ltlaByTier %>% 
    #filter(date == max(date)) %>%
    #filter(date == "2020-03-20") %>%
    filter(!is.na(parentCode)), # There are some missing here
    aes(x=Growth.windowed.value, y=Est.value/population*1000000,size=population,colour=tier),alpha=0.8) + 
  geom_vline(xintercept = 0, colour="grey40")+
  geom_point() + #alpha=0.2) + 
  ylab("Symptomatic covid per 1M per day")+
  xlab("7 day growth rate")+
  scale_size_area(max_size=3,guide="none")+
  scale_y_continuous(trans="log1p", breaks=c(0,2,5,20,50,200,500,2000,5000)) +
  scale_alpha_manual(values = c(1,0.1), guide="none") +
  scale_color_manual(values = pal)+
  #scale_colour_brewer(palette="Reds" ,na.value="grey80")+
  #scale_colour_viridis_d(direction = -1,na.value="grey80")+
  coord_cartesian(xlim=c(-0.25,0.25))+
  #guides(col=guide_legend(direction="horizontal",nrow=1,title.theme = element_blank(),label.theme = element_text(size = 12)))+
  theme(
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle=0,hjust=0.5),
    axis.title = element_text(size = 14),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text =  element_text(size = 12)
    )

anim2 = animTier +
  gganimate::transition_time(date)+
  gganimate::enter_fade() +
  gganimate::exit_fade()

lockdownGif = gganimate::animate(anim2, renderer=gganimate::magick_renderer(), width=600, height=600, nframes=dates %>% nrow())

## Timeline animation ----

events = dpc$datasets$getSignificantDates() %>% filter(Significance==1)

england = dpc$datasets$getPHEApiNations() %>% filter(name=="England" & statistic=="case" & date >= min(dates$progress) & date <= max(dates$progress))
england = england %>% ungroup() %>% tsp$logIncidenceStats()

slider= ggplot(dates,aes(xmin=progress,xmax=progress+1))+
  geom_vline(mapping=aes(xintercept=`Start date`,colour=Label),data=events)+
  geom_rect(colour="red",ymin=0,ymax=Inf)+
  geom_line(data=england,mapping=aes(x=date,y=Est.value),inherit.aes = FALSE)+
  #scale_y_continuous(trans="log1p")+
  standardPrintOutput::hideY()+
  scale_x_date(date_breaks = "1 month",date_labels = "%b")+
  #guides(col=guide_legend(direction="horizontal",nrow=1,title.theme = element_blank(),label.theme = element_text(size = 12)))+
  theme(
    axis.text.x = element_text(angle=0,hjust=0.5,size=12),
    axis.title.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text =  element_text(size = 12)
  )
#slider = slider+geom_rect(mapping = aes(xmin=progress,xmax=progress+1),ymin=-Inf,ymax=Inf,colour="red")

slideAnim = slider + gganimate::transition_time(progress)+
  gganimate::enter_fade() +
  gganimate::exit_fade()

#gganimate::animate(slideAnim, nframes=dates %>% nrow())
timeGif = gganimate::animate(slideAnim, renderer=gganimate::magick_renderer(), width=1200, height=100, nframes=dates %>% nrow())



## composite animation ----

layoutGifs = function(i) {
  magick::image_append(c(
    magick::image_append(c(
      mainGif[i], 
      lockdownGif[i]
    )),
    timeGif[i]), stack=TRUE)
}

new_gif <- layoutGifs(1)
for(i in 2:(dates %>% nrow())){ 
  combined <- layoutGifs(i)
  new_gif <- c(new_gif, combined)
}

magick::image_write_gif(new_gif,path=paste0("~/Dropbox/covid19/current-rt/",Sys.Date(),"/GrowthRatesVsIncidence-",Sys.Date(),".gif"))

