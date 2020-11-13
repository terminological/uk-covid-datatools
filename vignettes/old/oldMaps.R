<!-- # Old -->
  
  <!-- In the accompanying supplementary materials we present an animation of -->
  <!-- the *R~t~* over time, and in the rate of change in *R~t~* over time. This -->
  <!-- animation raises the possibility of waves of increasing and decreasing -->
  <!-- *R~t~* spreading throughout the UK. If such waves can be identified and -->
  <!-- predicted we may be able to intercept them in the future, through a -->
  <!-- targeted application of community testing and a more localised social -->
  <!-- distancing intervention. For this to be an option however we will need -->
  <!-- much more granular data on the location of confirmed and suspected -->
  <!-- cases, and to as far as possible reduce the time delay between infection -->
  <!-- and detection. -->
  
  <!-- In the last part of our analysis we look at the rate of change in *R~t~* -->
  <!-- estimated using a simple linear regression. We calculated a regional -->
  <!-- estimate of the current rate of change of *R~t~*, by fitting a linear -->
  <!-- equation to the last 14 points, and extracting the derivative of *R~t~*, -->
  <!-- associated confidence intervals and measures of goodness of fit. -->
  
  
  <!-- TODO: These trends of -->
  <!-- the differences between the 27th March to the 9th April were further -->
  <!-- analysed with a pairwise t-Test and presented in table 3 where we can -->
  <!-- see that the *R~t~* differences observed in London, the East, the North -->
  <!-- East and North West of England over this time period are statistically -->
  <!-- significant. The mean difference in *R~t~* between London and the North -->
  <!-- West could be about 0.35, which could have important consequences for -->
  <!-- the outbreak progression. -->
  
  <!-- In Figure 2 we plot the absolute difference of *R~t~* in the 7 NHS England -->
  <!-- administrative regions, from the *R~t~* of England overall, as a baseline. -->
  <!-- This demonstrates the volatility described above and highlights the -->
  <!-- regional differences in *R~t~* over time. Prior to the imposition of -->
  <!-- social distancing the patterns observed are dominated by noise, as in -->
  <!-- the early phase of the outbreak the case numbers in individual regions -->
  <!-- were small. However from the 27th March onwards we can see a clearer -->
  <!-- trend emerging with the East of England, Midlands, South East and South -->
  <!-- Western regions approximately tracking the England baseline. London is -->
  <!-- consistently below this baseline and the North West, and less so North -->
  <!-- East & Yorkshire are consistently above the baseline. These trends of -->
  <!-- the differences between the 27th March to the 9th April were further -->
  <!-- analysed with a pairwise t-Test and presented in table 3 where we can -->
  <!-- see that the *R~t~* differences observed in London, the East, the North -->
  <!-- East and North West of England over this time period are statistically -->
  <!-- significant. The mean difference in *R~t~* between London and the North -->
  <!-- West could be about 0.35, which could have important consequences for -->
  <!-- the outbreak progression. -->
  
  <!-- Due to the fragmented nature of reporting of the outbreak across the 4 -->
  <!-- main countries of the UK there is insufficient time series data -->
  <!-- available to perform a UK wide regional breakdown of *R~t~*. The -->
  <!-- availability of time series data is particularly challenging in Northern -->
  <!-- Ireland and Wales where we only have a very limited data set. -->
  
  <!-- This introduces -->
  <!-- further time delays which may vary in length from individual to -->
  <!-- individual. The delay between onset of symptoms and admission has been -->
  <!-- estimated at 5 to 9 days [@lintonIncubationPeriodOther2020]. -->
  <!-- The delay induced by the test processing is unknown. If we assume a 2 -->
  <!-- day delay for testing, we would estimate that interventions begin to -->
  <!-- have an effect from 9 days after the intervention, have half of their -->
  <!-- impact in 14 days, and begin to have full impact after 22 days. -->
  
  <!-- and is seen in all regions to be decreasing overall. *R~t~* -->
  <!-- remains above or equal to 1 at all times and hence within the region of -->
  <!-- exponential growth of COVID-19 cases. Over the period from March 19th to -->
  <!-- March 25th it is notable that, whilst decreasing in England, the -->
  <!-- reproduction number rose in Northern Ireland, Scotland and Wales before -->
  <!-- falling again. On the 8th April the *R~t~* was close to one in all regions -->
  <!-- apart from England. Two significant dates are marked on the time series. -->
  <!-- Firstly, the recommendation for specific social isolation of the -->
  <!-- vulnerable on the 16th March, and secondly, the widespread order to -->
  <!-- remain at home on the 23rd March. These dates represent the initial and -->
  <!-- final dates of implementation of social restrictions. The date of one -->
  <!-- serial interval post-lock-down  is also shown. -->
  
  <!-- Over the period from March 19th to -->
  <!-- March 25th it is notable that, whilst decreasing in England, the -->
  <!-- reproduction number rose in Northern Ireland, Scotland and Wales before -->
  <!-- falling again. On the 8th April the *R~t~* was close to one in all regions -->
  <!-- apart from England. -->
  
  
  
  <!-- The results presented above demonstrate a regional and time based -->
  <!-- variation in the reproduction number of SARS-CoV-2. Currently the -->
  <!-- reproduction number is close to 1 in most regions and the rate of change -->
  <!-- is negative suggesting that we will see a peak to the infection shortly. -->
  
  <!-- the state of the ongoing crisis of -->
  <!-- COVID-19 in the UK, but with a sense that the effective reproduction -->
  <!-- number is slowly coming under control. The interpretation of the time -->
  <!-- series data presented is particularly challenging due to the extensive -->
  <!-- time delay in identifying positive cases. As such it is impossible to -->
  <!-- conclude whether social distancing is the cause of this improvement. -->
  
  <!-- The current trends in improvement are encouraging and we hope to see -->
  <!-- bigger changes in the near future, however we note that this improvement -->
  <!-- is not uniformly distributed across the UK and policy makers will have -->
  <!-- to be cautious until we can be certain that COVID-19 is under control in -->
  <!-- all regions. The difference in apparent *R~t~* between London and the -->
  <!-- North Western region of the UK is currently approximately 0.35. Until -->
  <!-- that gap has closed it will be difficult to ease the current country -->
  <!-- wide restrictions. -->
  
  <!-- ## *R~t~* by Lower Tier Local Authority in England, and Local Health Boards in Wales and Scotland -->
  
  <!-- The following maps provide the most recent (from `r latestDate`) observation of the time varying reproduction number in all the different local authority districts in England, and health boards in Scotland and Wales. Full regional breakdown is not published on a daily basis in Northern Ireland. In panel A and D we see the low confidence interval estimate for *R~t~*, in B and E, the median, and in C and F the high confidence interval. In panels G,H and I the relative frequency of the *R~t~* observation for all regions is plotted, a curve to the left of 1 implies the majority of regions implies case numbers are overall decreasing, a curve to the right of one implies case numbers are overall increasing. -->
  
  <!-- ```{r} -->
  <!-- data("UKCovidMaps") -->
  
  <!-- # https://github.com/tidyverse/ggplot2/issues/3391 -->
  <!-- # some issues joining tibble onto sf - which  -->
  
  <!-- r0shapes = UKCovidMaps$reportingRegionsLTLA %>%  -->
    <!--   # fill in missing dates to prevent the map having disappearing / reappearing regions -->
    <!--   crossing(tibble(date=unique(r0CombinedUK_LTLA$date))) %>%  -->
    <!--   left_join(keyDates, by="date") %>% -->
    <!--   left_join( -->
                        <!--     r0CombinedUK_LTLA, -->
                        <!--     by=c("code","date"), suffix=c("",".dup")) %>%  -->
    <!--   mutate(ago=difftime(date,lubridate::now(),units="days")) %>%  -->
    <!--   sf::st_as_sf() -->
    
    
    
    <!-- ``` -->
    
    <!-- ```{r} -->
    <!-- defaultR0Map = function(data, facet="Median(R)") { -->
        <!--   facet = ensym(facet) -->
          <!--   return( -->
                           <!--     ggplot(data)+ -->
                           <!--     geom_sf(aes(fill=!!facet),size=0.1)+ -->
                           <!--     scale_fill_gradient2( -->
                                                            <!--       low="green", -->
                                                            <!--       mid="white", -->
                                                            <!--       high="red", -->
                                                            <!--       midpoint=0, -->
                                                            <!--       trans="log", -->
                                                            <!--       #na.value = "black",  -->
                                                            <!--       limits=c(0.2,5),  -->
                                                            <!--       breaks=c(0.2,0.5,1,2,5),  -->
                                                            <!--       labels=c("<0.2","0.5","1","2",">5"), -->
                                                            <!--       oob=scales::squish)+ -->
                           <!--     standardPrintOutput::narrowAndTall()+ -->
                           <!--     standardPrintOutput::mapTheme() -->
                           <!--   ) -->
          <!-- } -->
      <!-- ``` -->
      
      <!-- ```{r fig4, fig.cap="A geographical breakdown of *R~t~* observations in the UK based on cases. Left left column shows lower confidence limits, the central column is the median, and the right column the high confidence limit."} -->
      <!-- map1 = defaultR0Map(r0shapes %>% filter(date==latestDate) %>% mutate(`Median(R)` = ifelse(incidence < 2 , NA, `Median(R)`)), `Median(R)`)+guides(fill=guide_colourbar("Observed *R~t~*")) -->
        <!-- map2 = defaultR0Map(r0shapes %>% filter(date==latestDate) %>% mutate(`Quantile.0.025(R)` = ifelse(incidence < 2 , NA, `Quantile.0.025(R)`)), `Quantile.0.025(R)`)+guides(fill="none") -->
          <!-- map3 = defaultR0Map(r0shapes %>% filter(date==latestDate) %>% mutate(`Quantile.0.975(R)` = ifelse(incidence < 2 , NA, `Quantile.0.975(R)`)), `Quantile.0.975(R)`)+guides(fill="none") -->
            
            <!-- map4 = map1 + coord_sf(crs = 4326,xlim = c(-0.7, 0.5), ylim = c(51.25, 51.75)) -->
              <!-- map5 = map2 + coord_sf(crs = 4326,xlim = c(-0.7, 0.5), ylim = c(51.25, 51.75)) -->
                <!-- map6 = map3 + coord_sf(crs = 4326,xlim = c(-0.7, 0.5), ylim = c(51.25, 51.75)) -->
                  
                  <!-- map7 = ggplot(r0shapes %>% filter(date==latestDate), aes(x=`Median(R)`))+geom_density()+geom_vline(xintercept = 1,colour="blue")+coord_cartesian(xlim = c(0,2))+xlab("Median")+standardPrintOutput::hideY() -->
                    <!-- map8 = ggplot(r0shapes %>% filter(date==latestDate), aes(x=`Quantile.0.025(R)`))+geom_density()+geom_vline(xintercept = 1,colour="blue")+coord_cartesian(xlim = c(0,2))+xlab("Low CI")+standardPrintOutput::hideY() -->
                      <!-- map9 = ggplot(r0shapes %>% filter(date==latestDate), aes(x=`Quantile.0.975(R)`))+geom_density()+geom_vline(xintercept = 1,colour="blue")+coord_cartesian(xlim = c(0,2))+xlab("High CI")+standardPrintOutput::hideY() -->
                        
                        <!-- out = (map2 + map1 + map3) / (map5+map4+map6) / (map8 + map7 + map9) / patchwork::guide_area() +patchwork::plot_annotation(tag_levels = "A") + patchwork::plot_layout(guides = "collect", heights = c(1,0.36,0.45,0.05))  -->
                          
                          <!-- out %>% standardPrintOutput::saveFullPageFigure("~/Dropbox/covid19/current-rt/Fig4_Map") -->
                          
                          <!-- ``` -->
                          
                          
                          <!-- This analysis covers the last 4 days. There is quite a lot of missing data in recent days. -->
                          
                          <!-- ```{r fig4, fig.cap="A map of *R~t~* estimates for the Unitary Authority and Local Health Boards of England, Scotland, and Wales"} -->
                          
                          <!-- listAppend = function(list1,list2) { -->
                              <!--   list1[[length(list1)+1]] <- list2 -->
                                <!--   return(list1) -->
                                <!-- } -->
                            
                            <!-- plotCols = list() -->
                              <!-- plotDates = c(latestDate-3,latestDate-2,latestDate-1,latestDate) -->
                                
                                <!-- ukwide = lapply(plotDates, function(plotDate) { -->
                                    <!--   defaultR0Map(r0shapes %>% filter(date==plotDate))+guides(fill="none")+labs(subtitle = plotDate) -->
                                    <!-- }) -->
                                  <!-- plotCols = ukwide #%>% standardPrintOutput::saveHalfPageFigure("~/Dropbox/covid19/lock-down -impact/Fig3_englandMap") -->
                                  
                                  <!-- london = lapply(plotDates, function(plotDate) { -->
                                      <!--   defaultR0Map(r0shapes %>% filter(date ==plotDate ))+guides(fill="none")+ coord_sf(crs = 4326,xlim = c(-0.7, 0.5), ylim = c(51.25, 51.75), expand = FALSE) -->
                                      <!-- }) -->
                                    <!-- plotCols = plotCols %>% append(london) -->
                                      
                                      <!-- rtDensity = lapply(plotDates, function(plotDate) { -->
                                          <!--   ggplot(r0shapes %>% filter(date==plotDate), aes(x=`Median(R)`))+geom_density()+geom_vline(xintercept = 1,colour="blue")+coord_cartesian(xlim = c(0,3))+ -->
                                          <!--     theme(axis.text.y=element_blank(),axis.title.y=element_blank()) -->
                                          <!-- }) -->
                                        <!-- plotCols = plotCols %>% append(rtDensity) -->
                                          
                                          <!-- tmp = patchwork::wrap_plots(plotCols,ncol=4,byrow = TRUE, tag_level = "new")+plot_annotation(tag_levels = "A")  -->
                                            <!-- tmp %>% standardPrintOutput::saveHalfPageFigure("~/Dropbox/covid19/current-rt/Fig4_Map") -->
                                            
                                            <!-- ``` -->
                                            
                                            
                                            
                                            <!-- ### Regions with highest *R~t~* -->
                                            
                                            <!-- In the following table the unitary authority areas with the highest median time varying reproduction number are listed, based on case numbers. As some of these areas are quite small there is a good deal of uncertainty in these observations. This list has been restricted to areas which have had more than 200 cases altogether, as areas with very few cases trajectory is heavily influenced by single cases.  As this *R~t~* value is a single observation we expect this to vary significantly from day to day, and these numbers need to be interpreted with caution. -->
                                            
                                            <!-- Observations which have a high confidence of being high or low are marked with a double asterisk. The observed time varying reproductive rate (*R~t~*) are shown as well as an estimate of the rate of change. An high *R~t~* which is worsening is worth monitoring.  -->
                                            
                                            <!-- ```{r} -->
                                            
                                            <!-- r0CombinedUK %>% filter(date == latestDate & cumulative_cases>200 & incidence > 2) %>% arrange(desc(`Median(R)`)) %>% head(10) %>% mutate( -->
                                                                                                                                                                                              
                                                                                                                                                                                              <!--   `*R~t~* 95% CI` = paste0( tdp(`Median(R)`,`Quantile.0.025(R)`,`Quantile.0.975(R)`), if_else(`Quantile.0.025(R)`>1," **","")), -->
                                                                                                                                                                                              <!--   `Worsening?` = if_else(slope>0,ifelse(`slopeLowerCi`>0,"yes","possibly"),""), -->
                                                                                                                                                                                              <!--   `Delta *R~t~* 95% CI` = paste0(tdp(`slope`,`slopeLowerCi`,`slopeUpperCi`),if_else(`slopeLowerCi`>0," **","")) -->
                                                                                                                                                                                              <!--   ) %>% select(Date = date, Region = name, `*R~t~* 95% CI`, `Worsening?`, `Delta *R~t~* 95% CI`) %>% group_by(Date) %>% standardPrintOutput::saveTable("~/Dropbox/covid19/current-rt/Table3_top10Rt", tableWidth = 1, defaultFontSize = 8, colWidths = c(0.7,1.5,1,0.8,1)) -->
                                            
                                            <!-- ```  -->
                                            
                                            <!-- ### Regions with the fastest increase in *R~t~* -->
                                            
                                            <!-- ```{r} -->
                                            
                                            <!-- r0CombinedUK %>% filter(date == latestDate & cumulative_cases>200) %>% arrange(desc(slope)) %>% head(10) %>% mutate( -->
                                                                                                                                                                        <!--   `*R~t~* 95% CI` = paste0( tdp(`Median(R)`,`Quantile.0.025(R)`,`Quantile.0.975(R)`), if_else(`Quantile.0.025(R)`>1," **","")), -->
                                                                                                                                                                        <!--   `Increasing?` = if_else(`Median(R)`>1,ifelse(`Quantile.0.025(R)`>1,"yes","possibly"),""), -->
                                                                                                                                                                        <!--   `Delta *R~t~* 95% CI` = paste0(tdp(`slope`,`slopeLowerCi`,`slopeUpperCi`),if_else(`slopeLowerCi`>0," **","")) -->
                                                                                                                                                                        <!--   ) %>% select(Date = date, Region = name, `*R~t~* 95% CI`, `Increasing?`, `Delta *R~t~* 95% CI`) %>% group_by(Date) %>%  -->
                                            <!--   standardPrintOutput::saveTable("~/Dropbox/covid19/current-rt/Table4_top10DeltaRt", tableWidth = 1, defaultFontSize = 8, colWidths = c(0.7,1.5,1,0.8,1)) -->
                                            
                                            <!-- ```  -->
                                            
                                            <!-- ### Regions with lowest *R~t~* -->
                                            
                                            <!-- In the following table the unitary authority areas with the lowest median time varying reproduction number are listed, based on case numbers. As above this list has been restricted to areas which have had more than 200 cases altogether.  -->
                                            
                                            <!-- Observations which have a high confidence of being high or low are marked with a double asterisk. The observed time varying reproductive rate (*R~t~*) are shown as well as an estimate of the rate of change. A low *R~t~* which continues to improve (which includes the majority of this table) is reassuring.  -->
                                            
                                            <!-- ```{r} -->
                                            
                                            <!-- r0CombinedUK %>% filter(date == latestDate & cumulative_cases>200 & incidence > 2) %>% arrange(`Median(R)`) %>% head(10) %>% mutate( -->
                                                                                                                                                                                        <!--   `*R~t~* 95% CI` = paste0( tdp(`Median(R)`,`Quantile.0.025(R)`,`Quantile.0.975(R)`), if_else(`Quantile.0.975(R)`<1," **","")), -->
                                                                                                                                                                                        <!--   `Improving?` = if_else(slope<0,ifelse(`slopeUpperCi`<0,"yes","possibly"),""), -->
                                                                                                                                                                                        <!--   `Delta *R~t~* 95% CI` = paste0(tdp(`slope`,`slopeLowerCi`,`slopeUpperCi`),if_else(`slopeUpperCi`<0," **","")) -->
                                                                                                                                                                                        <!--   ) %>% select(Date = date, Region = name, `*R~t~* 95% CI`, `Improving?`, `Delta *R~t~* 95% CI`) %>% group_by(Date) %>%  -->
                                            <!--   standardPrintOutput::saveTable("~/Dropbox/covid19/current-rt/Table5_bottom10Rt", tableWidth = 1, defaultFontSize = 8, colWidths = c(0.7,1.5,1,0.8,1)) -->
                                            
                                            <!-- ```  -->
                                            
                                            <!-- ### Regions with the fastest decrease in *R~t~* -->
                                            
                                            <!-- ```{r} -->
                                            
                                            <!-- r0CombinedUK %>% filter(date == latestDate & cumulative_cases>200) %>% arrange(slope) %>% head(10) %>% mutate( -->
                                                                                                                                                                  <!--   `*R~t~* 95% CI` = paste0( tdp(`Median(R)`,`Quantile.0.025(R)`,`Quantile.0.975(R)`), if_else(`Quantile.0.975(R)`<1," **","")), -->
                                                                                                                                                                  <!--   `Decreasing?` = if_else(`Median(R)`<1,ifelse(`Quantile.0.975(R)`<1,"yes","possibly"),""), -->
                                                                                                                                                                  <!--   `Delta *R~t~* 95% CI` = paste0(tdp(`slope`,`slopeLowerCi`,`slopeUpperCi`),if_else(`slopeUpperCi`<0," **","")) -->
                                                                                                                                                                  <!--   ) %>% select(Date = date, Region = name, `*R~t~* 95% CI`, `Decreasing?`, `Delta *R~t~* 95% CI`) %>% group_by(Date) %>%  -->
                                            <!--   standardPrintOutput::saveTable("~/Dropbox/covid19/current-rt/Table3_bottom10DeltaRt", tableWidth = 1, defaultFontSize = 8, colWidths = c(0.7,1.5,1,0.8,1)) -->
                                            
                                            <!-- ```  -->
                                            <!-- In figures 3 and 4 we present the detailed regional breakdown of -->
                                            <!-- reproduction number in the 149 unitary authorities in England. These are -->
                                            <!-- illustrated at time points representing the start of the 2 social -->
                                            <!-- distancing measures implemented by the UK government and described -->
                                            <!-- above, and marked on figures 1 and 2. In the individual unitary -->
                                            <!-- authority regions case numbers may be quite small so the estimates of -->
                                            <!-- *R~t~* may have wide confidence intervals which are not shown. The full -->
                                            <!-- regional breakdown including confidence intervals is available as -->
                                            <!-- supplementary materials. In the vast majority of regions and time points -->
                                            <!-- the reproduction number is greater than 1, but the same decreasing trend -->
                                            <!-- is present. -->
                                            
                                            <!-- ```{r} -->
                                            <!-- data("UKCovidMaps") -->
                                            
                                            <!-- # https://github.com/tidyverse/ggplot2/issues/3391 -->
                                            <!-- # some issues joining tibble onto sf - which  -->
                                            
                                            <!-- r0shapes = UKCovidMaps$reportingRegions %>%  -->
                                              <!--   # fill in missing dates to prevent the map having disappearing / reappearing regions -->
                                              <!--   crossing(tibble(date=unique(ts$r0CombinedUK$date))) %>%  -->
                                              <!--   left_join(keyDates, by="date") %>% -->
                                              <!--   left_join( -->
                                                                  <!--     ts$r0CombinedUK, -->
                                                                  <!--     by=c("code","date"), suffix=c("",".dup")) %>%  -->
                                              <!--   mutate(ago=difftime(date,lubridate::now(),units="days")) %>%  -->
                                              <!--   sf::st_as_sf() -->
                                              
                                              <!-- defaultR0Map = function(data) { -->
                                                  <!--   return( -->
                                                                   <!--     ggplot(data)+ -->
                                                                   <!--     geom_sf(aes(fill=`Median(R)`))+ -->
                                                                   <!--     scale_fill_gradient2( -->
                                                                                                    <!--       low="green", -->
                                                                                                    <!--       mid="white", -->
                                                                                                    <!--       high="red", -->
                                                                                                    <!--       midpoint=0, -->
                                                                                                    <!--       trans="log", -->
                                                                                                    <!--       na.value = "grey80",  -->
                                                                                                    <!--       limits=c(0.2,5),  -->
                                                                                                    <!--       breaks=c(0.2,0.5,1,2,5),  -->
                                                                                                    <!--       labels=c("<0.2","0.5","1","2",">5"), -->
                                                                                                    <!--       oob=scales::squish)+ -->
                                                                   <!--     standardPrintOutput::narrowAndTall()+ -->
                                                                   <!--     standardPrintOutput::mapTheme() -->
                                                                   <!--   ) -->
                                                  <!-- } -->
                                                
                                                <!-- defaultDeltaR0Map = function(data) { -->
                                                    <!--   return( -->
                                                                     <!--     ggplot(data)+ -->
                                                                     <!--     geom_sf(aes(fill=slope))+ -->
                                                                     <!--     scale_fill_gradient2( -->
                                                                                                      <!--       low="cyan", -->
                                                                                                      <!--       mid="white", -->
                                                                                                      <!--       high="magenta", -->
                                                                                                      <!--       midpoint=0, -->
                                                                                                      <!--       na.value = "grey80", -->
                                                                                                      <!--       limits=c(-0.5,0.5),  -->
                                                                                                      <!--       breaks=c(-0.5,-0.25,0,0.25,0.5),  -->
                                                                                                      <!--       labels=c("<-0.5","-0.25","0","0.25",">0.5"), -->
                                                                                                      <!--       oob=scales::squish)+ -->
                                                                     <!--     standardPrintOutput::narrowAndTall()+ -->
                                                                     <!--     standardPrintOutput::mapTheme() -->
                                                                     <!--   ) -->
                                                    <!-- } -->
                                                  <!-- ``` -->
                                                  
                                                  <!-- *Figure 3 - the median value of *R~t~* at the time of announcement of -->
                                                  <!-- different social interventions designed to prevent spread of SARS-CoV-2, -->
                                                  <!-- by England* *Unitary Authority regions. Not all regions provided time -->
                                                  <!-- regional series data over the period under investigation and these areas -->
                                                  <!-- are represented in grey.* -->
                                                  
                                                  <!-- ```{r} -->
                                                  <!-- ukwide =  -->
                                                    <!--   defaultR0Map(r0shapes %>% filter(!is.na(label)))+ -->
                                                    <!--   facet_wrap(vars(label), nrow = 1) -->
                                                    <!-- ukwide %>% standardPrintOutput::saveHalfPageFigure("~/Dropbox/covid19/regional-infectivity/Fig3_englandMap") -->
                                                    
                                                    <!-- ``` -->
                                                    <!-- *Figure 4 - the median value of *R~t~* at the time of announcement of -->
                                                    <!-- different social interventions designed to prevent spread of SARS-CoV-2, -->
                                                    <!-- by England Unitary Authority region in London* -->
                                                    
                                                    <!-- ```{r} -->
                                                    <!-- london = ukwide + coord_sf(crs = 4326,xlim = c(-0.7, 0.5), ylim = c(51.25, 51.75), expand = FALSE) + standardPrintOutput::mapTheme() -->
                                                      <!-- london %>% standardPrintOutput::saveThirdPageFigure("~/Dropbox/covid19/regional-infectivity/Fig4_londonMap") -->
                                                      <!-- ``` -->
                                                      <!-- ```{r} -->
                                                      <!-- # birmingham = ukwide + coord_sf(crs = 4326,xlim = c(-2.3, -1.5), ylim = c(52.25, 52.75), expand = FALSE) + standardPrintOutput::mapTheme() -->
                                                      <!-- # birmingham %>% standardPrintOutput::saveThirdPageFigure("~/Dropbox/covid19/regional-infectivity/Fig4_birminghamMap") -->
                                                      <!-- ``` -->
                                                      <!-- In figure 5 we present the regional analysis of the current rate of -->
                                                      <!-- change of *R~t~*. This aims to demonstrate the magnitude in change of *R~t~* -->
                                                      <!-- over time and hence give us a sense of whether social distancing -->
                                                      <!-- measures are currently having the desired effect of reducing the overall -->
                                                      <!-- velocity of infection. In this time series, if the rate of change of -->
                                                      <!-- *R~t~* is positive, the infection is accelerating. Negative values, on the -->
                                                      <!-- other hand, represent deceleration. We can see that in England, the -->
                                                      <!-- viral infection has been decelerating from before the onset of the full -->
                                                      <!-- social distancing policies of the 23rd March. In the other regions of -->
                                                      <!-- the UK on the other hand we see a continued acceleration of the viral -->
                                                      <!-- spread until approximately one serial interval after the 23rd March -->
                                                      <!-- after which the infection begins to decelerate. -->
                                                      
                                                      
                                                      <!-- *Figure 5 - the rate of change of *R~t~* per day in different regions of -->
                                                      <!-- the UK.* -->
                                                      
                                                      <!-- In figures 6 and 7 we present a more detailed regional analysis of the -->
                                                      <!-- rate of change of *R~t~* by Unitary Authority in England or Local Health -->
                                                      <!-- Board in Scotland and Wales. We do not have complete data for this -->
                                                      <!-- regional breakdown in Wales, but in the far right panel representing the -->
                                                      <!-- most up to date time point, we see that in the vast majority of areas in -->
                                                      <!-- England the rate of change is negative (cyan) representing a continued -->
                                                      <!-- day on day fall in transmission. However there is seen to be regional -->
                                                      <!-- variation in the preceding days and weeks, and that the widespread -->
                                                      <!-- negative rate of change of *R~t~* is a relatively recent feature. -->
                                                      
                                                      
                                                      <!-- *Figure 6 - the median value of *R~t~* at the time of announcement of -->
                                                      <!-- different social interventions designed to prevent spread of SARS-CoV-2, -->
                                                      <!-- by England Unitary Authority regions (England), and Local Health Boards -->
                                                      <!-- (Scotland and Wales). Not all regions provided time regional series data -->
                                                      <!-- over the period under investigation and these areas are represented in -->
                                                      <!-- grey.* -->
                                                      
                                                      
                                                      
                                                      <!-- ```{r} -->
                                                      <!-- ukwideDelta =  -->
                                                        <!--   defaultDeltaR0Map(r0shapes %>% filter(!is.na(label)))+ -->
                                                        <!--   facet_wrap(vars(label), nrow = 1) -->
                                                        <!-- ukwideDelta %>% standardPrintOutput::saveHalfPageFigure("~/Dropbox/covid19/regional-infectivity/Fig6_englandMapDelta") -->
                                                        <!-- ``` -->
                                                        
                                                        <!-- *Figure 7 - the median value of *R~t~* at -->
                                                        <!-- the time of announcement of different social interventions designed to -->
                                                        <!-- prevent spread of SARS-CoV-2, by England Unitary Authority region in -->
                                                        <!-- London* -->
                                                        
                                                        <!-- ```{r} -->
                                                        <!-- londonDelta = ukwideDelta + coord_sf(crs = 4326,xlim = c(-0.7, 0.5), ylim = c(51.25, 51.75), expand = FALSE) + standardPrintOutput::mapTheme() -->
                                                          <!-- londonDelta %>% standardPrintOutput::saveThirdPageFigure("~/Dropbox/covid19/regional-infectivity/Fig7_londonMapDelta") -->
                                                          <!-- ``` -->
                                                          <!-- ```{r} -->
                                                          <!-- # birminghamDelta = ukwideDelta + coord_sf(crs = 4326,xlim = c(-2.3, -1.5), ylim = c(52.25, 52.75), expand = FALSE) + standardPrintOutput::mapTheme() -->
                                                          <!-- # birminghamDelta %>% standardPrintOutput::saveThirdPageFigure("~/Dropbox/covid19/regional-infectivity/Fig7_birminghamMapDelta") -->
                                                          <!-- ``` -->
                                                          
                                                          
                                                          <!-- ## Create and save animated map -->
                                                          
                                                          <!-- Animated maps are viewable: -->
                                                          
                                                          <!-- * https://github.com/terminological/uk-covid-datatools/blob/master/vignettes/UK_Rt_over_time.gif -->
                                                          
                                                          <!-- ```{r eval=FALSE} -->
                                                          <!-- dates = tibble(date1=as.Date(min(r0shapes$date):max(r0shapes$date),origin=as.Date("1970-01-01"))) -->
                                                            <!-- barAnim = ggplot(dates,aes(xmax=date1+1,xmin=date1))+ -->
                                                              <!--   geom_rect(ymin=0,ymax=1) +  -->
                                                              <!--   ylim(c(0,1))+ -->
                                                              <!--   geom_vline(aes(xintercept=date,colour=event),data=keyDates,size=4,show.legend = FALSE)+ -->
                                                              <!--   # ggrepel::geom_text_repel( -->
                                                              <!--   #   aes(x=date, y=1.25, colour=event, label=event),data=keyDates, hjust=0.5,vjust=0, show.legend = FALSE,box.padding=0.05,inherit.aes = FALSE, -->
                                                              <!--   #         size=(10/ggplot2:::.pt/(96/72)))+ -->
                                                              <!--   theme( -->
                                                                              <!--     axis.text.y = element_blank(), -->
                                                                              <!--     axis.ticks.y = element_blank(),  -->
                                                                              <!--     panel.grid.major.y = element_blank(),  -->
                                                                              <!--     panel.grid.minor.y = element_blank(),  -->
                                                                              <!--     axis.title.x = element_blank() -->
                                                                              <!--   )+ -->
                                                              <!--   coord_cartesian(ylim = c(0, 1), clip = 'off')+ -->
                                                              <!--   gganimate::transition_time(date1) -->
                                                              
                                                              <!-- timeGif = gganimate::animate(barAnim, renderer=gganimate::magick_renderer(), width=800, height=60, nframes=200) -->
                                                                
                                                                
                                                                <!-- ukwideAnim =  -->
                                                                  <!--   defaultR0Map(r0shapes)+ -->
                                                                  <!--   guides(fill="none")+labs(subtitle="A")+ -->
                                                                  <!--   gganimate::transition_time(date) -->
                                                                  
                                                                  <!-- londonAnim =  -->
                                                                    <!--   defaultR0Map(r0shapes)+ -->
                                                                    <!--   coord_sf(crs = 4326,xlim = c(-0.7, 0.5), ylim = c(51.25, 51.75), expand = FALSE) +  -->
                                                                    <!--   standardPrintOutput::mapTheme()+labs(subtitle="C")+ -->
                                                                    <!--   gganimate::transition_time(date) -->
                                                                    
                                                                    <!-- ukwideDeltaAnim =  -->
                                                                      <!--   defaultDeltaR0Map(r0shapes)+ -->
                                                                      <!--   guides(fill="none")+labs(subtitle="B")+ -->
                                                                      <!--   gganimate::transition_time(date) -->
                                                                      
                                                                      <!-- londonDeltaAnim =  -->
                                                                        <!--   defaultDeltaR0Map(r0shapes)+ -->
                                                                        <!--   coord_sf(crs = 4326,xlim = c(-0.7, 0.5), ylim = c(51.25, 51.75), expand = FALSE) +  -->
                                                                        <!--   standardPrintOutput::mapTheme()+labs(subtitle="D")+ -->
                                                                        <!--   gganimate::transition_time(date) -->
                                                                        
                                                                        <!-- ukWideGif = gganimate::animate(ukwideAnim, renderer=gganimate::magick_renderer(), width=400, height=800, nframes=200) -->
                                                                          <!-- londonGif = gganimate::animate(londonAnim, renderer=gganimate::magick_renderer(), width=400,height=300, nframes=200) -->
                                                                            <!-- ukWideDeltaGif = gganimate::animate(ukwideDeltaAnim, renderer=gganimate::magick_renderer(), width=400, height=800, nframes=200) -->
                                                                              <!-- londonDeltaGif = gganimate::animate(londonDeltaAnim, renderer=gganimate::magick_renderer(), width=400,height=300, nframes=200) -->
                                                                                
                                                                                <!-- captionTif = magick::image_read(path = "~/Git/uk-covid-datatools/vignettes/suppFig1Caption.png") -->
                                                                                  
                                                                                  <!-- layoutGifs = function(i) { -->
                                                                                      <!--   magick::image_append(c( -->
                                                                                                                       <!--   magick::image_append(c(ukWideGif[i], ukWideDeltaGif[i])), -->
                                                                                                                       <!--   magick::image_append(c(londonGif[i], londonDeltaGif[i])), -->
                                                                                                                       <!--   timeGif[i], captionTif[1]), stack=TRUE) -->
                                                                                      <!-- } -->
                                                                                    
                                                                                    <!-- new_gif <- layoutGifs(1) -->
                                                                                      <!-- for(i in 2:200){ -->
                                                                                          <!--   combined <- layoutGifs(i) -->
                                                                                            <!--   new_gif <- c(new_gif, combined) -->
                                                                                              <!-- } -->
                                                                                      
                                                                                      <!-- #gganimate::anim_save("~/Git/uk-covid-datatools/vignettes/UK_Rt_over_time.gif",new_gif) -->
                                                                                      <!-- magick::image_write_gif(new_gif,path="~/Git/uk-covid-datatools/vignettes/UK_Rt_over_time.gif") -->
                                                                                      <!-- # ggplot(r0shapes, aes(x=date,y=`Median(R)`,colour=code))+geom_line(alpha=0.2,show.legend = FALSE) -->
                                                                                      
                                                                                      <!-- ``` -->
                                                                                      
                                                                                      <!-- ```{r} -->
                                                                                      
                                                                                      <!-- UKCovidNeghbours = UKCovidMaps$reportingRegions %>% createNeighbourNetwork(code) -->
                                                                                        <!-- write.csv(UKCovidNeghbours,"~/Git/uk-covid-datatools/vignettes/Rt_Timeseries_Neighbours.csv") -->
                                                                                        
                                                                                        <!-- ``` -->
                                                                                        
                                                                                        <!-- ```{r} -->
                                                                                        
                                                                                        <!-- # captionTif = magick::image_read(path = "~/Git/uk-covid-datatools/vignettes/suppFig1Caption.png") -->
                                                                                        <!-- # manGif = magick::image_read(path = "~/Dropbox/covid19/regional-infectivity/eurosurveillanceSubmission/UK_Rt_over_time_no_label.gif") -->
                                                                                        <!-- #  -->
                                                                                        <!-- # new_gif <- magick::image_append(c(manGif[1], captionTif[1]), stack=TRUE) -->
                                                                                        <!-- # for(i in 2:100){ -->
                                                                                        <!-- #   combined <- magick::image_append(c(manGif[i], captionTif[1]), stack=TRUE) -->
                                                                                        <!-- #   new_gif <- c(new_gif, combined) -->
                                                                                        <!-- # } -->
                                                                                        <!-- #  -->
                                                                                        <!-- # magick::image_write_gif(new_gif,path="~/Git/uk-covid-datatools/vignettes/UK_Rt_over_time_2.gif") -->
                                                                                        
                                                                                        
                                                                                        <!-- # rateOfChangeDist = ggplot(rateOfChange, aes(x=slope))+geom_density()+geom_rug()+geom_vline(xintercept = 0, colour="blue")+coord_cartesian(xlim=c(-0.5,0.5)) -->
                                                                                        <!-- #  -->
                                                                                        <!-- # ukRateOfChange = ggplot(rateOfChangeUKregion, aes(x=uk_region,y=slope,fill=slope))+ -->
                                                                                        <!-- #   scale_fill_gradient2( -->
                                                                                        <!-- #     low="cyan", -->
                                                                                        <!-- #     mid="white", -->
                                                                                        <!-- #     high="magenta", -->
                                                                                        <!-- #     midpoint=0, -->
                                                                                        <!-- #     #trans="log", -->
                                                                                        <!-- #     na.value = "grey80", -->
                                                                                        <!-- #     limits=c(-1,1),  -->
                                                                                        <!-- #     breaks=c(-1,-0.5,0,0.5,1),  -->
                                                                                        <!-- #     labels=c("<-1","-0.5","0","0.5",">1") -->
                                                                                        <!-- #     ) +  -->
                                                                                        <!-- #   geom_bar(stat="identity", colour="black")+geom_errorbar(aes(ymin=slopeLowerCi,ymax=slopeUpperCi), width=0.2) -->
                                                                                        <!-- #  -->
                                                                                        <!-- # (ukwideRateOfChange + londonRateofChange  + ukRateOfChange + rateOfChangeDist + plot_annotation(tag_levels = 'A')  + plot_layout(nrow = 2, guides="collect")) %>% standardPrintOutput::saveHalfPageFigure("~/Dropbox/covid19/regional-infectivity/Fig4_RateOfChangeMap") -->
                                                                                        <!-- #  -->
                                                                                        
                                                                                        <!-- ``` -->
                                                                                        
                                                                                        <!-- ```{r} -->
                                                                                        
                                                                                        <!-- # rateOfChange %>% ungroup() %>% filter(slopeLowerCi > 0) %>% arrange(desc(slope)) %>% head(5) %>% mutate(`95% CI` = sprintf("%1.2f; %1.2f",slopeLowerCi,slopeUpperCi)) %>% select(`Unitary authority`=name,`dR/dt`=slope, `95% CI` ,`R^2`=r_squared) %>% standardPrintOutput::saveTable("~/Dropbox/covid19/regional-infectivity/Table2_Top5UnitaryAuthoritiesByDeltaR_t") -->
                                                                                        
                                                                                        <!-- ``` -->
                                                                                        
                                                                                        <!-- ```{r} -->
                                                                                        
                                                                                        <!-- # rateOfChange %>% ungroup() %>% filter(slopeUpperCi < 0) %>% arrange(slope) %>% head(5) %>% mutate(`95% CI` = sprintf("%1.2f; %1.2f",slopeLowerCi,slopeUpperCi)) %>% select(`Unitary authority`=name,`dR/dt`=slope, `95% CI` ,`R^2`=r_squared) %>% standardPrintOutput::saveTable("~/Dropbox/covid19/regional-infectivity/Table3_Bottom5UnitaryAuthoritiesByDeltaR_t") -->
                                                                                        
                                                                                        <!-- ``` -->
                                                                                        
                                                                                        
                                                                                        
                                                                                        
                                                                                        <!-- ```{r} -->
                                                                                        
                                                                                        <!-- # tmpDeltaR0timeseries =  -->
                                                                                        <!-- #  -->
                                                                                        <!-- # rateOfChangeShapes = UKCovidMaps$reportingRegions %>% -->
                                                                                        <!-- #   crossing(tibble(date=unique(tmpDeltaR0timeseries$date))) %>%  -->
                                                                                        <!-- #   left_join(tmpDeltaR0timeseries, by=c("code","date")) %>% -->
                                                                                        <!-- #   sf::st_as_sf()  -->
                                                                                        <!-- #  -->
                                                                                        <!-- # ukwideRateOfChange = ggplot(rateOfChangeShapes)+ -->
                                                                                        <!-- #   geom_sf(aes(fill=slope))+ -->
                                                                                        <!-- #   scale_fill_gradient2( -->
                                                                                        <!-- #     low="cyan", -->
                                                                                        <!-- #     mid="white", -->
                                                                                        <!-- #     high="magenta", -->
                                                                                        <!-- #     midpoint=0, -->
                                                                                        <!-- #     trans="pseudo_log", -->
                                                                                        <!-- #     na.value = "grey80", -->
                                                                                        <!-- #     limits=c(-1,1),  -->
                                                                                        <!-- #      -->
                                                                                        <!-- #     breaks=c(-1,-0.5,0,0.5,1),  -->
                                                                                        <!-- #     labels=c("<-1","-0.5","0","0.5",">1") -->
                                                                                        <!-- #     )+ -->
                                                                                        <!-- #   standardPrintOutput::mapTheme() -->
                                                                                        <!-- #  -->
                                                                                        <!-- # anim5 = ukwideRateOfChange + guides(fill="none")+#labs(title = 'Date: {frame_time}')+ -->
                                                                                        <!-- #   gganimate::transition_time(date) -->
                                                                                        <!-- # gif5 = gganimate::animate(anim5, renderer=gganimate::magick_renderer(), width=400, height=800) -->
                                                                                        <!-- #  -->
                                                                                        <!-- # anim6 = ukwideRateOfChange + coord_sf(crs = 4326,xlim = c(-0.7, 0.5), ylim = c(51.25, 51.75), expand = FALSE)+ -->
                                                                                        <!-- #   gganimate::transition_time(date) -->
                                                                                        <!-- # gif6 = gganimate::animate(anim6, renderer=gganimate::magick_renderer(), duration = 10, fps=10, width=400, height=300) -->
                                                                                        
                                                                                        <!-- ``` -->
                                                                                        
                                                                                        <!-- ```{r} -->
                                                                                        <!-- # dates = tibble(date1=as.Date(min(rateOfChangeShapes$date):max(rateOfChangeShapes$date),origin=as.Date("1970-01-01"))) -->
                                                                                        <!-- # barAnim = ggplot(dates,aes(xmax=date1+1,xmin=date1))+ -->
                                                                                        <!-- #   geom_rect(ymin=0,ymax=1) +  -->
                                                                                        <!-- #   ylim(c(0,1))+ -->
                                                                                        <!-- #   geom_vline(aes(xintercept=date,colour=event),data=keyDates,size=4,show.legend = FALSE)+ -->
                                                                                        <!-- #   # ggrepel::geom_text_repel( -->
                                                                                        <!-- #   #   aes(x=date, y=1.25, colour=event, label=event),data=keyDates, hjust=0.5,vjust=0, show.legend = FALSE,box.padding=0.05,inherit.aes = FALSE, -->
                                                                                        <!-- #   #         size=(10/ggplot2:::.pt/(96/72)))+ -->
                                                                                        <!-- #   theme( -->
                                                                                        <!-- #     axis.text.y = element_blank(), -->
                                                                                        <!-- #     axis.ticks.y = element_blank(),  -->
                                                                                        <!-- #     panel.grid.major.y = element_blank(),  -->
                                                                                        <!-- #     panel.grid.minor.y = element_blank(),  -->
                                                                                        <!-- #     axis.title.x = element_blank() -->
                                                                                        <!-- #   )+ -->
                                                                                        <!-- #   coord_cartesian(ylim = c(0, 1), clip = 'off')+ -->
                                                                                        <!-- #   gganimate::transition_time(date1) -->
                                                                                        <!-- #  -->
                                                                                        <!-- # timeGif = gganimate::animate(barAnim, renderer=gganimate::magick_renderer(), width=800, height=60) -->
                                                                                        <!-- ``` -->
                                                                                        
                                                                                        <!-- ```{r eval=FALSE} -->
                                                                                        
                                                                                        
                                                                                        
                                                                                        <!-- # ukRateOfChange = ggplot(deltaR0Regionaltimeseries, aes(x=uk_region,y=slope,fill=slope))+ -->
                                                                                        <!-- #   scale_fill_gradient2( -->
                                                                                        <!-- #     low="cyan", -->
                                                                                        <!-- #     mid="white", -->
                                                                                        <!-- #     high="magenta", -->
                                                                                        <!-- #     midpoint=0, -->
                                                                                        <!-- #     #trans="log", -->
                                                                                        <!-- #     na.value = "grey80", -->
                                                                                        <!-- #     limits=c(-0.5,0.5),  -->
                                                                                        <!-- #     breaks=c(-0.5,-0.25,0,0.25,0.5),  -->
                                                                                        <!-- #     labels=c("<-0.5","-0.25","0","0.25",">0.5") -->
                                                                                        <!-- #     ) + geom_bar(stat="identity", colour="black")+geom_errorbar(aes(ymin=slopeLowerCi,ymax=slopeUpperCi), width=0.2)+ -->
                                                                                        <!-- #   coord_cartesian(ylim=c(-0.25,0.25))#+ -->
                                                                                        <!-- #   #labs(title = 'Date: {frame_time}') -->
                                                                                        <!-- #  -->
                                                                                        <!-- # anim3 = ukRateOfChange+gganimate::transition_time(date) -->
                                                                                        <!-- # gif3 = gganimate::animate(anim3, renderer=gganimate::magick_renderer(), width=400, height = 300) -->
                                                                                        <!-- # #gganimate::anim_save("~/Git/uk-covid-datatools/vignettes/delta_Rt_by_uk_region_over_time.gif",gif) -->
                                                                                        <!-- #  -->
                                                                                        <!-- # rateOfChangeDist = ggplot(deltaR0timeseries %>% filter(!is.na(slope)), aes(x=slope))+geom_density()+geom_rug()+geom_vline(xintercept = 0, colour="blue")+coord_cartesian(xlim=c(-0.5,0.5))+labs(title = " ") -->
                                                                                        <!-- # anim4 = rateOfChangeDist+gganimate::transition_time(date) -->
                                                                                        <!-- # gif4 = gganimate::animate(anim4, renderer=gganimate::magick_renderer(), width=400, height = 200) -->
                                                                                        <!-- #  -->
                                                                                        <!-- # new_gif <- magick::image_append(c( -->
                                                                                        <!-- #       magick::image_append(c( -->
                                                                                        <!-- #         gif5[1], -->
                                                                                        <!-- #         magick::image_append(c( -->
                                                                                        <!-- #           gif3[1],  -->
                                                                                        <!-- #           gif4[1], -->
                                                                                        <!-- #           gif6[1] -->
                                                                                        <!-- #         ),stack=TRUE) -->
                                                                                        <!-- #       )), -->
                                                                                        <!-- #       timeGif[1] -->
                                                                                        <!-- # ),stack=TRUE)  -->
                                                                                        <!-- # for(i in 2:100){ -->
                                                                                        <!-- #   combined <- magick::image_append(c( -->
                                                                                        <!-- #       magick::image_append(c( -->
                                                                                        <!-- #         gif5[i], -->
                                                                                        <!-- #         magick::image_append(c( -->
                                                                                        <!-- #           gif3[i],  -->
                                                                                        <!-- #           gif4[i], -->
                                                                                        <!-- #           gif6[i] -->
                                                                                        <!-- #         ),stack=TRUE) -->
                                                                                        <!-- #       )), -->
                                                                                        <!-- #       timeGif[i] -->
                                                                                        <!-- # ),stack=TRUE) -->
                                                                                        <!-- #      -->
                                                                                        <!-- #   new_gif <- c(new_gif, combined) -->
                                                                                        <!-- # } -->
                                                                                        <!-- #  -->
                                                                                        <!-- # magick::image_write_gif(new_gif,path="~/Git/uk-covid-datatools/vignettes/delta_Rt_over_time.gif") -->
                                                                                        <!-- #gganimate::anim_save("~/Git/uk-covid-datatools/vignettes/delta_Rt_over_time.gif",new_gif) -->
                                                                                        <!-- #gganimate::anim_save("~/Git/uk-covid-datatools/vignettes/delta_Rt_distribution_over_time.gif",gif) -->
                                                                                        <!-- ``` -->
                                                                                        
                                                                                        
                                                                                        <!-- ``` -->
                                                                                        
                                                                                        
                                                                                        