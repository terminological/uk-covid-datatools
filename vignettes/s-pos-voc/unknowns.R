breakdownVOC = combinedCases %>% mutate(
  subgroup = case_when(
    !is.na(type) & type == "B.1.617.2" ~ "B.1.617.2",
    !is.na(type) & type == "B.1.1.7" ~ "B.1.1.7",
    !is.na(type) ~ "other sequenced",
    is.na(type) & sGene == "positive" ~ "S+",
    is.na(type) & sGene == "negative" ~ "S-",
    TRUE ~ "unknown"
  ) %>% ordered(c("B.1.1.7","S-","unknown","S+","other sequenced","B.1.617.2"))
)


breakdownVOCCount = breakdownVOC %>% group_by(code = LTLA_code, name = LTLA_name, subgroup, date = earliest_specimen_date) %>% summarise(value=n())

dates = as.Date(min(breakdownVOCCount$date,na.rm=TRUE):max(breakdownVOCCount$date,na.rm=TRUE),"1970-01-01")

breakdownVOCCount = breakdownVOCCount %>% ungroup()%>% tidyr::complete(nesting(code,name),subgroup,date=dates,fill=list(value=0))

window = 21

breakdownVOCCount = breakdownVOCCount %>%
  group_by(code,name,subgroup) %>%
  arrange(date) %>%
  mutate(
    #subgroup.rate = signal::sgolayfilt(value, p=1, n=window)
    subgroup.rate = exp(signal::sgolayfilt(log(value+exp(-1)), p=1, n=window))-exp(-1)
  ) %>% mutate(
    subgroup.rate = ifelse(subgroup.rate<0,0,subgroup.rate),
    subgroup.roll = round(subgroup.rate*window)
  ) 

breakdownVOCCount2 = breakdownVOCCount %>%
  group_by(code,name,date) %>%
  mutate(
    beta.alpha = subgroup.rate,
    beta.beta = sum(subgroup.rate)-subgroup.rate
  ) %>%
  mutate(
    mean = subgroup.rate/sum(subgroup.rate),
    lower = qbeta(0.05,beta.alpha,beta.beta),
    upper = qbeta(0.95,beta.alpha,beta.beta),
    n = sum(subgroup.rate),
  ) %>%
  #mutate(binom::binom.confint(x=ifelse(is.na(subgroup.roll),0,subgroup.roll) ,n=sum(subgroup.roll,na.rm = TRUE),methods = "wilson")) %>% 
  rename(total.roll = n, subgroup.proportion = mean,subgroup.proportion.lower = lower, subgroup.proportion.upper = upper)# %>% select(-x)

breakdownVOCKnown = breakdownVOC %>% 
  mutate(subgroup = case_when(
      !is.na(type) & type == "B.1.617.2" ~ "B.1.617.2",
      !is.na(type) & type == "B.1.1.7" ~ "B.1.1.7",
      !is.na(type) ~ NA_character_,
      is.na(type) & sGene == "positive" ~ "B.1.617.2",
      is.na(type) & sGene == "negative" ~ "B.1.1.7",
      TRUE ~ NA_character_
    )) %>%
  filter(!is.na(subgroup)) %>%
  group_by(code = LTLA_code, name = LTLA_name, subgroup, date = earliest_specimen_date) %>% 
  summarise(value=n()) %>%
  ungroup() %>%
  tidyr::complete(nesting(code,name),subgroup,date=dates,fill=list(value=0)) %>%
  group_by(code,name,subgroup) %>%
  arrange(date) %>%
  mutate(
    #subgroup.rate = signal::sgolayfilt(value, p=1, n=window)
    subgroup.rate = exp(signal::sgolayfilt(log(value+exp(-1)), p=1, n=window))-exp(-1)
  ) %>% mutate(
    subgroup.rate = ifelse(subgroup.rate<0,0,subgroup.rate),
    subgroup.roll = round(subgroup.rate*window)
  ) %>%
  group_by(code,name,date) %>%
  mutate(
    # https://stats.stackexchange.com/questions/354214/continuous-approximation-to-binomial-distribution#:~:text=This%20approximation%20is%20good%20if,normal%20distribution%20has%20infinite%20support.
    beta.alpha = subgroup.rate,
    beta.beta = sum(subgroup.rate)-subgroup.rate
  ) %>%
  mutate(
    mean = subgroup.rate/sum(subgroup.rate),
    lower = qbeta(0.05,beta.alpha,beta.beta),
    upper = qbeta(0.95,beta.alpha,beta.beta),
    n = sum(subgroup.rate),
  ) %>%
  # mutate(binom::binom.confint(x=ifelse(is.na(subgroup.roll),0,subgroup.roll) ,n=sum(subgroup.roll,na.rm=TRUE),methods = "wilson")) %>% 
  # mutate(
  #   x=subgroup.roll,
  #   n=sum(subgroup.roll),
  #   mean=subgroup.roll/sum(subgroup.roll),
  #   lower = ifelse(is.na(n),0,lower),
  #   upper = ifelse(is.na(n),0,upper)
  # ) %>%
  # select(-x) %>%
  rename(total.roll = n, subgroup.proportion = mean,subgroup.proportion.lower = lower, subgroup.proportion.upper = upper)
  

filterExpr = expr(name %in% c("Bolton","Burnley","Hackney","Hillingdon"))
filterExpr=TRUE



# Data driven estimates ----

totalCount = breakdownVOC %>% 
  mutate(subgroup = case_when(
    (type == "B.1.617.2" | (is.na(type) & sGene == "positive")) ~ "B.1.617.2",
    (type == "B.1.1.7" | (is.na(type) & sGene == "negative")) ~ "B.1.1.7",
    sGene == "unknown" ~ "unknown",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(subgroup)) %>%
  group_by(code = LTLA_code, name = LTLA_name, subgroup, date = earliest_specimen_date) %>% 
  summarise(value=n()) %>%
  ungroup() %>%
  tidyr::complete(nesting(code,name),subgroup,date=dates,fill=list(value=0))

estimatedVOC = totalCount %>% left_join(
  breakdownVOCKnown %>% 
    select(code, name, inferred.subgroup = subgroup, date, beta.alpha, beta.beta) %>% 
    mutate(subgroup="unknown"),
  by=c("code","name","date","subgroup")) %>%
  mutate(
    beta.alpha = ifelse(is.na(beta.alpha), 1, beta.alpha),
    beta.beta = ifelse(is.na(beta.beta), 0, beta.beta),
    inferred = ifelse(is.na(inferred.subgroup),subgroup,paste(subgroup,inferred.subgroup,sep=":")),
    subgroup = ifelse(!is.na(inferred.subgroup),inferred.subgroup,subgroup)
  ) %>% 
  # group_by(code,name,subgroup,inferred,inferred.subgroup) %>% 
  # arrange(date) %>%
  ungroup()

estimatedVOC2 = estimatedVOC %>%
  group_by(code,name,subgroup,inferred,inferred.subgroup,date) %>% 
  group_modify(function(d,g,...) {
    tibble(
      sample = 1:10,
      value = rbinom(10,d$value,qbeta(runif(10,min = 0.05,max=0.95), d$beta.alpha, d$beta.beta))
    )
  }) %>% 
  group_by(code,name,subgroup,date,sample) %>% 
  summarise(value = sum(value)) 

# TODO: This seems to work.
# Seems to be 
window = 15
bayes.window = 15
mu.prior = 0
var.prior = 10
# 
# 
estimatedVOC3 = estimatedVOC2 %>%
  ungroup() %>%
  filter(date<max(date)-4) %>%
  group_by(code,name,subgroup,sample) %>%
  arrange(date) %>%
  fill(value) %>%
  mutate(
    subgroup.growth = signal::sgolayfilt(log(pmax(value,exp(-1))), p = 1, n = window,m = 1),
    subgroup.rate = exp(signal::sgolayfilt(log(value+exp(-1)), p=1, n=window))-exp(-1)
  ) %>% mutate(
    subgroup.rate = ifelse(subgroup.rate<0,0,subgroup.rate)
  ) %>%
  #   subgroup.lograte = stats::filter(log(pmax(value,exp(-1))), filter=rep(1,window)/window)
  # ) %>%
  # mutate(
  #   subgroup.growth = subgroup.lograte-lag(subgroup.lograte)
  # ) %>%
  
  mutate(
    subgroup.growth.mu = stats::filter(x = subgroup.growth, filter=rep(1/bayes.window,bayes.window)), # rolling mean
    subgroup.growth.var = stats::filter(x = ((subgroup.growth-subgroup.growth.mu)^2)/(bayes.window-1), filter=rep(1,bayes.window)), #rolling sd
    # https://towardsdatascience.com/a-bayesian-approach-to-estimating-revenue-growth-55d029efe2dd
    # bayes normal normal - prior mu = 0, prior variance = 1.
    subgroup.growth.mu.posterior =
      (mu.prior/var.prior + bayes.window*subgroup.growth.mu/subgroup.growth.var) /
        (1/var.prior+bayes.window/subgroup.growth.var),
    subgroup.growth.var.posterior = 1 /
      (1/var.prior+bayes.window/subgroup.growth.var)
  ) %>%
  ungroup()

filterExpr = expr(name %in% c("Ealing","Croydon"))

#ordering = estimatedVOC3 %>% filter(subgroup=="B.1.617.2" & !is.na(name)) %>% group_by(name) %>% summarise(order = max(subgroup.growth.mu)) %>% arrange(desc(order)) %>% pull(name)
ordering = breakdownVOCCount2 %>% filter(subgroup=="B.1.617.2" & !is.na(name)) %>% group_by(name) %>% summarise(order = sum(value)) %>% arrange(desc(order)) %>% pull(name)

estimatedVOC3 = estimatedVOC3 %>% filter(!is.na(name)) %>% mutate(name = name %>% ordered(ordering))
breakdownVOCCount2 = breakdownVOCCount2 %>% filter(!is.na(name)) %>% mutate(name = name %>% ordered(ordering))
breakdownVOCKnown = breakdownVOCKnown %>% filter(!is.na(name)) %>% mutate(name = name %>% ordered(ordering))

filterExpr = TRUE


p1 = ggplot(breakdownVOCCount2 %>% filter(!!filterExpr), aes(x=date, y=subgroup.proportion, fill=subgroup))+
  geom_area()+
  geom_line(data=breakdownVOCKnown %>% filter(subgroup=="B.1.617.2") %>% filter(!!filterExpr),mapping = aes(y=subgroup.proportion),colour="cyan")+
  geom_line(data=breakdownVOCKnown %>% filter(subgroup=="B.1.617.2") %>% filter(!!filterExpr),mapping = aes(y=subgroup.proportion.lower),colour="cyan",linetype="twodash")+
  geom_line(data=breakdownVOCKnown %>% filter(subgroup=="B.1.617.2") %>% filter(!!filterExpr),mapping = aes(y=subgroup.proportion.upper),colour="cyan",linetype="twodash")+
  coord_cartesian(ylim=c(0,1),xlim=as.Date(c("2021-03-01",NA)))+
  facet_wrap(vars(name),ncol=15)+
  theme(
    strip.text = element_text(size=unit(4,"pt")),
    axis.text.x = element_text(angle=90,vjust=0.5)
  )+
  standardPrintOutput::smallLegend()

p1 %>% standardPrintOutput::saveFigure("~/Dropbox/covid19/sa-variant/ProportionsIncludingUnknownsData",maxWidth = 10, maxHeight = 20) %>% invisible()

p2 = ggplot(estimatedVOC3 %>% filter(!!filterExpr), aes(x=date,colour = subgroup,group=paste0(sample,subgroup)))+
  # geom_point(aes(y=value),alpha=0.2,size=0.5)+
  geom_line(aes(y=subgroup.rate),alpha=0.5)+scale_y_continuous(trans="log1p",breaks=c(0,2,5,20,50,200,500,2000))+
  coord_cartesian(xlim=as.Date(c("2021-03-01",NA)))+
  facet_wrap(vars(name),ncol=15)+theme(strip.text = element_text(size=unit(4,"pt")))+
  scale_x_date(breaks=seq(as.Date("2021-02-04"),Sys.Date(),by = 14),date_labels="%d-%m")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  standardPrintOutput::smallLegend()+
  ylab("predicted counts")

p2 %>% standardPrintOutput::saveFigure("~/Dropbox/covid19/sa-variant/CountsIncludingUnknownsData",maxWidth = 10, maxHeight = 20) %>% invisible()



p3 = ggplot(estimatedVOC3 %>% filter(!!filterExpr), aes(x=date,colour = subgroup,group=paste0(sample,subgroup)))+
  geom_ribbon(aes(
    ymin=subgroup.growth.mu.posterior-1.96*sqrt(subgroup.growth.var.posterior),
    ymax=subgroup.growth.mu.posterior+1.96*sqrt(subgroup.growth.var.posterior),
  ),alpha=0.2,colour=NA,fill="grey")+ 
  geom_line(aes(y=subgroup.growth.mu.posterior),alpha=0.5)+
  scale_y_continuous(
    sec.axis = dup_axis( 
      breaks = log(2)/c(5,10,Inf,-10,-5), 
      labels = c("5","10","\u221E","-10","-5"), 
      name="doubling time")
  )+
  facet_wrap(vars(name),ncol=15)+theme(strip.text = element_text(size=unit(4,"pt")))+
  scale_x_date(breaks=seq(as.Date("2021-02-04"),Sys.Date(),by = 14),date_labels="%d-%m")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  coord_cartesian(xlim=as.Date(c("2021-03-01",NA),ylim=c(-0.2,0.2)))+
  ylab("growth rate,r")

p3 %>% standardPrintOutput::saveFigure("~/Dropbox/covid19/sa-variant/GrowthIncludingUnknownsData",maxWidth = 10, maxHeight = 20) %>% invisible()



## Model dirvent estimates


lastDate = max(breakdownVOCKnown$date)-4
poissonVOC = breakdownVOCKnown %>%
  group_by(code,name,subgroup) %>%
  group_modify(function(d,g,...) {
    d = d %>% mutate(time = as.numeric(date-max(date)))
    tmp = d %>% filter(date<lastDate)
    
    #fit = glm(value ~ time,family = quasipoisson(),data=tmp)
    #full = breakdownVOCKnown %>% ungroup() %>% select(date) %>% distinct() %>% mutate(time = as.numeric(date-lastDate))
    #full$modelled = exp(predict(fit,newdata=full))
    if(all(tmp$value == 0)) {
      
      d$value.fit = 0
      d$value.fit.se = 0
      
    } else {
      
      tryCatch({
        tmp_intercept_model = locfit::locfit(value ~ locfit::lp(time, nn=0.7, deg=1), tmp, family="poisson", link="log")
        tmp_intercept = predict(tmp_intercept_model, d$time, band="local")
        d$value.fit = ifelse(tmp_intercept$fit < 0, 0, tmp_intercept$fit) # prevent negative smoothing.
        d$value.fit.se = tmp_intercept$se.fit
      }, error=browser)
      
    }
    #browser()
    return(d)
  })

poissonVOC2 = poissonVOC %>%
  group_by(code,name,date) %>%
  mutate(mean = value.fit/sum(value.fit), binom.n=sum(value)) %>%
  mutate(
    #https://math.stackexchange.com/questions/153897/ratio-distribution-poisson-random-variables
    # P(X|X+Y=n) where X and Y are poisson.
    binom.expected = mean*binom.n,
    binom.sd = binom.n*mean*(1-mean))

ggplot(poissonVOC2 %>% filter(subgroup == "B.1.617.2" & name %in% c("Burnley")), aes(x=date, y=value.fit))+geom_line()+geom_point(aes(y=value))+facet_wrap(vars(name))

ggplot(poissonVOC2 %>% filter(subgroup == "B.1.617.2" & name %in% c("West Oxfordshire")), aes(x=date, y=mean))+geom_line()+facet_wrap(vars(name))

p1 = ggplot(breakdownVOCCount2 %>% filter(!!filterExpr), aes(x=date, y=subgroup.proportion, fill=subgroup))+
  geom_area()+
  geom_line(data=poissonVOC2 %>% filter(!!filterExpr) %>% filter(subgroup=="B.1.617.2"),mapping = aes(y=mean),colour="cyan")+
  coord_cartesian(xlim=as.Date(c("2021-03-01",NA)))+
  facet_wrap(vars(name),ncol=15)+
  theme(
    strip.text = element_text(size=unit(4,"pt")),
    axis.text.x = element_text(angle=90,vjust=0.5)
  )+
  standardPrintOutput::smallLegend()

p1 %>% standardPrintOutput::saveFigure("~/Dropbox/covid19/sa-variant/ProportionsIncludingUnknownsModelled",maxWidth = 10, maxHeight = 20) %>% invisible()


totalCount = breakdownVOC %>% 
  mutate(subgroup = case_when(
    (type == "B.1.617.2" | (is.na(type) & sGene == "positive")) ~ "B.1.617.2",
    (type == "B.1.1.7" | (is.na(type) & sGene == "negative")) ~ "B.1.1.7",
    sGene == "unknown" ~ "unknown",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(subgroup)) %>%
  group_by(code = LTLA_code, name = LTLA_name, subgroup, date = earliest_specimen_date) %>% 
  summarise(value=n()) %>%
  ungroup() %>%
  tidyr::complete(nesting(code,name),subgroup,date=dates,fill=list(value=0))

estimatedVOC = totalCount %>% left_join(
  poissonVOC %>% 
    select(code, name, inferred.subgroup = subgroup, date, binom.p = mean) %>% #subgroup.proportion, binom.n = total.roll) %>% 
    mutate(subgroup="unknown"),
  by=c("code","name","date","subgroup")) %>%
  mutate(
    binom.p = case_when(
      is.na(binom.p) ~ 1,
      TRUE ~ binom.p),
    inferred = ifelse(is.na(inferred.subgroup),subgroup,paste(subgroup,inferred.subgroup,sep=":")),
    subgroup = ifelse(!is.na(inferred.subgroup),inferred.subgroup,subgroup)
  ) %>% 
  group_by(code,name,subgroup,inferred,inferred.subgroup) %>% 
  arrange(date) %>% 
  ungroup()

estimatedVOC2 = estimatedVOC %>%
  mutate(value = value*binom.p) %>%
  group_by(code,name,subgroup,date) %>% 
  summarise(value = sum(value))

# TODO: This seems to work.
# Seems to be 
window = 14
bayes.window = 9
mu.prior = 0
var.prior = 10
# 
# 
estimatedVOC3 = estimatedVOC2 %>%
  ungroup() %>%
  filter(date<max(date)-4) %>%
  group_by(code,name,subgroup) %>%
  arrange(date) %>%
  mutate(
    subgroup.lograte = stats::filter(log(value+exp(-1)), filter=rep(1,window)/window),
    subgroup.rate = exp(subgroup.lograte)-exp(-1)
  ) %>%
  mutate(
    subgroup.growth = subgroup.lograte-lag(subgroup.lograte)
  ) %>%
  mutate(
    subgroup.growth.mu = stats::filter(x = subgroup.growth/bayes.window, filter = rep(1,bayes.window)),
    subgroup.growth.var = stats::filter(x = ((subgroup.growth-subgroup.growth.mu)^2)/(bayes.window-1), filter = rep(1,bayes.window)),
    # https://towardsdatascience.com/a-bayesian-approach-to-estimating-revenue-growth-55d029efe2dd
    # bayes normal normal - prior mu = 0, prior variance = 1.
    subgroup.growth.mu.posterior =
      (mu.prior/var.prior + bayes.window*subgroup.growth.mu/subgroup.growth.var) /
      (1/var.prior+bayes.window/subgroup.growth.var),
    subgroup.growth.var.posterior = 1 /
      (1/var.prior + bayes.window/subgroup.growth.var)
  ) %>%
  ungroup()


p2 = ggplot(estimatedVOC3, aes(x=date,colour = subgroup))+
  geom_point(aes(y=value),size=0.1)+
  geom_line(aes(y=subgroup.rate))+scale_y_continuous(trans="log1p",breaks=c(0,2,5,20,50,200,500,2000))+
  coord_cartesian(xlim=as.Date(c("2021-03-01",NA)))+
  facet_wrap(vars(name),ncol=15)+theme(strip.text = element_text(size=unit(4,"pt")))+
  scale_x_date(breaks=seq(as.Date("2021-02-04"),Sys.Date(),by = 14),date_labels="%d-%m")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  standardPrintOutput::smallLegend()

p2 %>% standardPrintOutput::saveFigure("~/Dropbox/covid19/sa-variant/CountsIncludingUnknowns",maxWidth = 10, maxHeight = 20) %>% invisible()



p3 = ggplot(estimatedVOC3, aes(x=date,colour = subgroup))+
  geom_line(aes(y=subgroup.growth.mu.posterior),alpha=1)+
  geom_ribbon(aes(
    ymin=subgroup.growth.mu.posterior-1.96*sqrt(subgroup.growth.var.posterior),
    ymax=subgroup.growth.mu.posterior+1.96*sqrt(subgroup.growth.var.posterior),
    group=subgroup
  ),alpha=0.2,colour=NA,fill="grey")+ 
  scale_y_continuous(
    sec.axis = dup_axis( 
      breaks = log(2)/c(5,10,Inf,-10,-5), 
      labels = c("5","10","\u221E","-10","-5"), 
      name="doubling time")
  )+
  facet_wrap(vars(name),ncol=15)+theme(strip.text = element_text(size=unit(4,"pt")))+
  scale_x_date(breaks=seq(as.Date("2021-02-04"),Sys.Date(),by = 14),date_labels="%d-%m")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  coord_cartesian(xlim=as.Date(c("2021-03-01",NA),ylim=c(-0.2,0.2)))

p3 %>% standardPrintOutput::saveFigure("~/Dropbox/covid19/sa-variant/GrowthIncludingUnknowns",maxWidth = 10, maxHeight = 20) %>% invisible()


