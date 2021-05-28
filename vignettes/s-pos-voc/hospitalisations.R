

tmp = dpc$spim$getSPIMextract()
tmp %>% filter(name %like% "Bolton" & statistic=="hospital admission") %>% tsp$describe()


ggplot(tmp %>% filter(code == "RMC" & source=="hospital_inc" & !is.na(ageCat)), aes(x=date, y=value, fill=ageCat))+geom_bar(stat="identity")+coord_cartesian(xlim=c(as.Date("2021-03-01"),NA))
ggplot(tmp %>% filter(code == "RMC" & source=="hospital_inc" & !is.na(ageCat)), aes(x=date, y=value, fill=ageCat))+geom_bar(stat="identity")+coord_cartesian(xlim=c(as.Date("2021-03-01"),NA))
