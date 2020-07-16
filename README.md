# uk-covid-datatools

Data tools for loading and processing covid data held on 
https://docs.google.com/spreadsheets/d/1snb-vYuH7fVpTuyoQrM8zWiABYoXbSrnn44w-zlhM90/edit#gid=1854739084

```{r}
devtools::install_github("terminological/uk-covid-datatools", force = TRUE)

library(ukcovidtools)
library(ggplot2)
library(EpiEstim)
```
## Vignettes:

sudo add-apt-repository ppa:ubuntugis/ubuntugis-unstable
sudo apt update
sudo apt install -y libudunits2-0 libudunits2-dev
sudo apt install libgeos-dev
sudo apt install libgdal-dev

## Get data and cleanse:

```{r}
UK = ukcovidtools::getUKCovidTimeseries()
cleanRegional = UK$tidyUKRegional %>% ukcovidtools::normaliseAndCleanse()


# A list of 7 data sets of similar nature and formats
glimpse(UK$UKregional)
glimpse(UK$englandNHS)
glimpse(UK$englandUnitAuth)
glimpse(UK$englandUnitAuth2NHSregion)
glimpse(UK$tidyUKRegional)
glimpse(UK$tidyEnglandNHS)
glimpse(UK$tidyEnglandUnitAuth)
```

timeseries has UK region, England NHS region, and England Unitary Authority

## Calculate groupwise Rt (data is already grouped on uk_region)

```{r}

siConfig = EpiEstim::make_config(list(
      mean_si = 4.7, 
      std_si = 2.9
))

regionalRt = cleanRegional %>% ukcovidtools::tidyEstimateRt(siConfig)
ggplot(regionalRt, aes(x=date,y=`Median(R)`,ymin=`Quantile.0.05(R)`,ymax=`Quantile.0.95(R)`,fill=uk_region,colour=uk_region))+
  geom_ribbon(alpha=0.2, colour=NA)+geom_line()+geom_hline(yintercept = 1, colour="red")+expand_limits(y=0)

```

## Supporting data sets

included in the package are a number of supporting data sets:

```{r}

# Demographic breakdown of UK in single year age bands in a tidy format
glimpse(UKDemographics2019)
glimpse(UKDemographics2019Detailed)

# UK ILI data in a tidy format going back to 2014
glimpse(UKILIdata)

# UK capacity in NHS hospitals & lists of all UK hospitals and locations
glimpse(NHSCapacity2019$hospitals)

# UK capacity in NHS hospitals - acute general medical beds and ICUs
glimpse(NHSCapacity2019$trusts)
# includes lat/long

# Guestimated UK Catchment areas for hospitals in terms of a LAD and a fraction of the LAD population that will end up in a NHS trust
# This uses population estimates in each postcode and closest hospital (straight line distance) to each postcode to calculate an overall fraction for the LAD population which will use any given hospital
glimpse(NHSCatchmentAreas$acuteBeds)

glimpse(NHSCatchmentAreas$icuBeds)

```

N.B. the ili data can also be found as a csv file in data-raw/ilidata.csv
