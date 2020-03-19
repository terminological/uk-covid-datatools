# uk-covid-datatools

Data tools for loading and processing covid data held on 
https://docs.google.com/spreadsheets/d/1snb-vYuH7fVpTuyoQrM8zWiABYoXbSrnn44w-zlhM90/edit#gid=1854739084

```{r}
devtools::install_github("terminological/uk-covid-datatools", force = TRUE)

library(ukcovidtools)
library(ggplot2)
library(EpiEstim)
```

## Get data and cleanse:

```{r}
UK = ukcovidtools::getUKCovidTimeseries()
cleanRegional = UK$tidyUKRegional %>% ukcovidtools::normaliseAndCleanse()
```

timeseries has UK region, England NHS region, and England Unitary Authority

## Calculate groupwise Rt (data is already grouped on uk_region)

```{r}

siConfig = EpiEstim::make_config(list(
      mean_si = 4.7, 
      std_si = 2.9
))

regionalRt = cleanRegional %>% ukcovidtools::tidyEstimateRt(siConfig)
ggplot(regionalRt %>% filter(!is.na(`Median(R)`)), aes(x=date,y=`Median(R)`,ymin=`Quantile.0.05(R)`,ymax=`Quantile.0.95(R)`,fill=uk_region,colour=uk_region))+
  geom_ribbon(alpha=0.2, colour=NA)+geom_line()+geom_hline(yintercept = 1, colour="red")+expand_limits(y=0)

```
