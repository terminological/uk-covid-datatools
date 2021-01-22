
devtools::load_all("~/Git/standard-print-output/")
devtools::load_all("~/Git/uk-covid-datatools/")

library(tidyverse)
library(patchwork)
library(rgdal)
library(ggplot2)
library(ggspatial)
library(rgeos)
library(maptools)
library(patchwork)
library(sp)
library(sf)

ggplot2::theme_set(standardPrintOutput::defaultFigureLayout())

standardPrintOutput::setDefaults()
ukcovidtools::setup()


breakdown = dpc$spim$getOneOneOneIncidence(dateFrom = as.Date("2020-07-01"))


byRegionAndOutcome = breakdown %>% tsp$aggregateAge() %>% tsp$logIncidenceStats()
currentGrowth = byRegionAndOutcome %>% filter(date == max(date)) %>% dpc$demog$findDemographics() %>% mutate(Est.value.per1M = Est.value/population*1000000)
ccgMap = dpc$geog$getMap("CCG20")

ccgMap = ccgMap %>% rmapshaper::ms_simplify()

ccgMap2 = ccgMap %>% left_join(currentGrowth %>% select(-codeType,-name), by="code")

probCcgMap = ccgMap2 %>% filter(subgroup != "other") %>% 
  mutate(
    Prob = cut(Growth.ProbPos.value,breaks = c(-Inf,0.5,0.75,0.9,0.95,0.975,Inf), labels=c("<50%","50-75%","75-90%","90-95%","95-97.5%","97.5%+")),
    maxIncid = case_when(
      subgroup=="self care"~90,
      subgroup=="urgent clinical review"~60,
      subgroup=="clinical review"~50,
      subgroup=="emergency ambulance"~10
    ),
    IncidPer1M = ifelse(Est.value.per1M > maxIncid, maxIncid, Est.value.per1M),
    Incid = ifelse(value > maxIncid, maxIncid, value),
    DailyGrowth = Growth.value*100
  )

interactive = function(map, col) {
  m1 = mapview::mapview(map %>% filter(subgroup=="self care"), zcol=col, layer.name=paste0("self care ",col), viewer.suppress=TRUE, burst=FALSE, alpha.regions=0.8, lwd=2, alpha=1, colour="black",popup=c("subgroup","name","population","value","Est.value.per1M","Growth.value","Growth.ProbPos.value","doublingTime"))
  m2 = mapview::mapview(map %>% filter(subgroup=="clinical review"), zcol=col, layer.name=paste0("clin rev ",col), viewer.suppress=TRUE, burst=FALSE, alpha.regions=0.8, lwd=2, alpha=1, colour="black",popup=c("subgroup","name","population","value","Est.value.per1M","Growth.value","Growth.ProbPos.value","doublingTime"))
  m3 = mapview::mapview(map %>% filter(subgroup=="urgent clinical review"), zcol=col, layer.name=paste0("urgent clin rev ",col), viewer.suppress=TRUE, burst=FALSE, alpha.regions=0.8, lwd=2, alpha=1, colour="black",popup=c("subgroup","name","population","value","Est.value.per1M","Growth.value","Growth.ProbPos.value","doublingTime"))
  m4 = mapview::mapview(map %>% filter(subgroup=="emergency ambulance"), zcol=col, layer.name=paste0("ambulance ",col), viewer.suppress=TRUE, burst=FALSE, alpha.regions=0.8, lwd=2, alpha=1, colour="black",popup=c("subgroup","name","population","value","Est.value.per1M","Growth.value","Growth.ProbPos.value","doublingTime"))
  return(m1+m2+m3+m4)
}

mxIncidPer1M = interactive(probCcgMap, col="IncidPer1M")
mxIncid = interactive(probCcgMap, col="Incid")
mxGrowth = interactive(probCcgMap, col="DailyGrowth")
mxProb = interactive(probCcgMap, col="Prob")

#### Leaflet timeline -------

# leaflet timeline is limited to points
# alternative is to dig into leaflet javascript internals
# another alternative is to use d3-geo and javascript to control colours:
# https://observablehq.com/@d3/world-choropleth?collection=@d3/d3-geo

currentGrowth2 = byRegionAndOutcome %>% dpc$demog$findDemographics() %>% mutate(Est.value.per1M = Est.value/population*1000000)
ccgMap3 = ccgMap %>% left_join(currentGrowth2 %>% select(-codeType,-name), by="code")
ccgMap3 = ccgMap3 %>% filter(subgroup != "other") %>% 
  mutate(
    Prob = cut(Growth.ProbPos.value,breaks = c(-Inf,0.5,0.75,0.9,0.95,0.975,Inf), labels=c("<50%","50-75%","75-90%","90-95%","95-97.5%","97.5%+")),
    maxIncid = case_when(
      subgroup=="self care"~90,
      subgroup=="urgent clinical review"~60,
      subgroup=="clinical review"~50,
      subgroup=="emergency ambulance"~10
    ),
    IncidPer1M = ifelse(Est.value.per1M > maxIncid, maxIncid, Est.value.per1M),
    Incid = ifelse(value > maxIncid, maxIncid, value),
    DailyGrowth = Growth.value*100,
    start = date,
    end = date+1
  )

#### City labels ----

cities = maps::world.cities %>% filter(country.etc=="UK") %>% arrange(desc(pop)) %>% head(10) %>% sf::st_as_sf(coords=c("long","lat"),crs=4326,remove=FALSE)
ggplot(ccgMap3 %>% filter(subgroup=="self care" & date == max(date)))+
  geom_sf(aes(fill=Prob))+
  geom_sf(data=cities,inherit.aes = FALSE,size=0.5)+
  scale_fill_viridis_d(drop=FALSE)+
  ggrepel::geom_text_repel(data = cities, aes(x = long, y = lat, label = name), size=2,fontface = "bold",colour="orange")



pal <- leaflet::colorFactor(
  palette = "RdYlBu",
  domain = probCcgMap$Prob,reverse = TRUE)

leaf = leaflet::leaflet(ccgMap3 %>% filter(subgroup=="self care" & date > max(date)-10 )) %>%
  leaflet::addPolygons(fillColor = ~pal(Prob), fill=TRUE, weight = 1, color = "black", opacity = 0.5, fillOpacity=0.2) %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Voyager) %>%
  leaflet::addLegend(pal = pal, values = ~Prob) %>%
  leaftime::addTimeline()
  #   sliderOpts = leaftime::sliderOptions(
  #     # formatOutput = htmlwidgets::JS(
  #     #   "function(date) {return new Date(date).toDateString()}
  #     # "),
  #     position = "bottomright",
  #     step = 10,
  #     duration = 3000,
  #     showTicks = FALSE
  #   )
  # )

# leaf$dependencies[[length(leaf$dependencies)+1]] <- htmltools::htmlDependency(
#   name = "leaflet-timeline",
#   version = "1.0.0",
#   src = c("href" = "http://skeate.github.io/Leaflet.timeline/"),
#   script = "javascripts/leaflet.timeline.js",
#   stylesheet = "stylesheets/leaflet.timeline.css"
# )

#leaf


#### shinyify ----

# Shiny app
# library(shiny); library(ggiraph); library(ggplot2); library(tidyverse)
# library(ggspatial); library(maptools); library(sp); library(rgdal); library(rgeos); library(patchwork); library(ggrepel)
# 
# load('R0timeseries.RData')
# data("UKCovidMaps")
# r0shapes = UKCovidMaps$unitaryAuthority %>% 
#   left_join(R0timeseries, by=c("ctyua19cd"="GSS_CD")) %>% 
#   mutate(ago=difftime(date,lubridate::now(),units="days")) %>% 
#   filter(!is.na(date))
# r0shapes = r0shapes %>% mutate(`Median(R)` = ifelse(`Median(R)`>10, 9.999,`Median(R)`))
# 
# keyDates = tibble(
#   date = as.Date(c("2020-03-13","2020-03-16","2020-03-19","2020-03-23")), #max(r0shapes$date-1, na.rm=TRUE)),
#   impactDate = as.Date(c("2020-03-14","2020-03-21","2020-03-24","2020-03-28")), #max(r0shapes$date-1, na.rm=TRUE)),
#   event = c("Inpatient only testing","Social isolation of vulnerable","Travel ban / school closure","Stay at home") #,"Latest")
# ) %>% mutate(label = paste0(date,": \n",event))
# r0shapes_key = r0shapes %>% inner_join(keyDates, by="date")
# 
# # Function for creating the map
# createMap <- function(r0shapes, Location, Date, Summary){
#   
#   # Subset data by date
#   r0shapes <- subset(r0shapes, date == Date)  
#   
#   if (Summary == "Median"){
#     ggfill <- geom_sf(aes(fill=`Median(R)`), data=r0shapes)
#   }
#   
#   if (Summary == "Lower"){
#     ggfill <- geom_sf(aes(fill=`Quantile.0.025(R)`), data=r0shapes)
#   }
#   
#   if (Summary == "Upper"){
#     ggfill <- geom_sf(aes(fill=`Quantile.0.975(R)`), data=r0shapes)
#   }
#   
#   # Map
#   mm <- ggplot(r0shapes)+
#     ggfill+
#     scale_fill_gradient2(
#       low="green",
#       mid="white",
#       high="red",
#       midpoint=0,
#       trans="log",
#       na.value = "grey80", 
#       limits=c(0.1,10), 
#       breaks=c(0.1,0.4,1,2.5,10), 
#       labels=c("<0.1","0.4","1","2.5",">10"),
#       name=paste(Summary))
#   
#   if (Location == "East of England"){
#     mm = mm + coord_sf(crs = 4326, xlim = c(-1, 2), ylim = c(51.25, 53.25), expand = FALSE)
#   }
#   
#   if (Location == "London"){
#     mm = mm + coord_sf(crs = 4326, xlim = c(-0.7, 0.5), ylim = c(51.25, 51.75), expand = FALSE)
#   }
#   
#   if (Location == "Midlands"){
#     mm = mm + coord_sf(crs = 4326, xlim = c(-3.5, 0.5), ylim = c(51.5, 53.75), expand = FALSE)
#   }
#   
#   if (Location == "North East and Yorkshire"){
#     mm = mm + coord_sf(crs = 4326, xlim = c(-2.75, 0.25), ylim = c(53.5, 56), expand = FALSE)
#   }
#   
#   if (Location == "North West"){
#     mm = mm + coord_sf(crs = 4326, xlim = c(-3.75, -1.75), ylim = c(52.75, 55.25), expand = FALSE)
#   }
#   
#   if (Location == "South East"){
#     mm = mm + coord_sf(crs = 4326, xlim = c(-1.5, 1.5), ylim = c(50.5, 51.75), expand = FALSE)
#   }
#   
#   if (Location == "South West"){
#     mm = mm + coord_sf(crs = 4326, xlim = c(-6, -1), ylim = c(49.75, 51.75), expand = FALSE)
#   }
#   
#   return(mm)
# }
# 
# 
# TODO: Zoomable plot example::
# https://shiny.rstudio.com/gallery/plot-interaction-zoom.html
# https://gallery.shinyapps.io/093-plot-interaction-basic/?_ga=2.50933801.1211594543.1607506809-1617147825.1573819738
# https://github.com/jquery/jquery-mousewheel
# https://www.rdocumentation.org/packages/shinyjs/versions/2.0.0
# https://www.rdocumentation.org/packages/shinyjs/versions/2.0.0/topics/onevent
# https://bookdown.org/yihui/rmarkdown/shiny-start.html
# https://bookdown.org/yihui/rmarkdown/shiny-widgets.html <- this one 

# # Define the UI
# ui = fluidPage(
#   
#   # App title
#   titlePanel("R(t) by unitary authority over time"),
#   
#   # Sidebar layout with input and output definitions
#   sidebarLayout(
#     
#     # Sidebar panel for inputs 
#     sidebarPanel(
#       selectInput(inputId = "Location",
#                   label = "Select a region:",
#                   choices = list("England" = "England", 
#                                  "East of England" = "East of England", 
#                                  "London" = "London", 
#                                  "Midlands" = "Midlands",
#                                  "North East and Yorkshire" = "North East and Yorkshire",
#                                  "North West" = "North West",
#                                  "South East" = "South East",
#                                  "South West" = "South West")),
#       
#       sliderInput(inputId = "Date",
#                   label = "Date:",
#                   min = min(r0shapes$date),
#                   max = max(r0shapes$date),
#                   value = max(r0shapes$date),
#                   step = 1),
#       
#       selectInput(inputId = "Summary",
#                   label = "Select statistic:",
#                   choices = list("Lower" = "Lower",
#                                  "Median" = "Median",
#                                  "Upper" = "Upper"),
#                   selected = "Median")
#     ),
#     
#     # Main panel for displaying outputs
#     mainPanel(
#       plotOutput("map")
#     )
#   )
# )
# 
# # Define the server
# server = function(input, output) {
#   
#   # Create the map
#   output$map <- renderPlot({createMap(r0shapes, input$Location, input$Date, input$Summary) })
#   
#   #output$map <- renderGirafe({
#   #  ggiraph(code = print(createMap(r0shapes, input$Location, input$Date)), width_svg = 8, height_svg = 8)
#   #})
#   
# }
# 
# # Finally, we can run our app by either clicking "Run App" in the top of our RStudio IDE, or by running
# shinyApp(ui = ui, server = server)

### Feature details ----
#https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html#1_Introduction
osmdata::available_features()
bb_df <- osmdata::getbb(place_name = "England, United Kingdom", format_out = "data.frame")
bb <- osmdata::getbb(place_name = "England, United Kingdom")
tmp = osmdata::opq(bbox = bb) %>% osmdata::add_osm_feature(key = "highway", value="Motorway") %>% osmdata::osmdata_sf()


### rayshader
# needs R > 4.0.0
# https://www.tylermw.com/3d-ggplots-with-rayshader/
# 2 plots - one with fill = height the other with fill = fill


