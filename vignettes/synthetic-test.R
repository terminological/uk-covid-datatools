# synthetic-test

here::i_am("vignettes/synthetic-test.R")

devtools::load_all()
ukcovidtools::reload()

synth = SyntheticDatasetProvider$new(dpc)

tmp = synth$generateGrowthRate()
tmp2 = synth$generateRt()
tmp3 = synth$getGrowthRateBasedDataset(bootstraps = 5,weekendEffect = 0)

tmp4 = tmp3$ts %>%  tsp$estimateGrowthRate(window = 21,nearestNeigbours = TRUE)

tmp4 %>% tsp$plotIncidenceQuantiles(events=tmp3$events)+facet_wrap(vars(subgroup))+scale_y_continuous(trans="log1p")


(tmp4 %>% tsp$plotGrowthRate()+facet_wrap(vars(subgroup)))+geom_line(data=tmp3$ts, mapping=aes(x=date,y=Growth.actual), colour="red")

tmp %>% View()