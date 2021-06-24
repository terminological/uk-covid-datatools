# uk-covid-datatools

Data tools for loading and processing covid data.

N.B. this is a continuous work in progress, and fairly unstable. 
I will try and make releases of it as I go but things will probably break in unpredictable ways

## TODO:

* Document options
* Switch code in geography provider for that in terminological/arear
* Look into over-dispertion in growth rate
* Expand to include international data sources


## Setup:

```R
library(data.table)
library(dtplyr)
library(tidyverse, warn.conflicts)

here::i_am("common-setup.R")

if (!"ukcovidtools" %in% c(devtools::dev_packages(),rownames(installed.packages()))) {
  if (fs::dir_exists("~/Git/uk-covid-datatools/")) {
    devtools::load_all("~/Git/uk-covid-datatools/")
  } else {
    devtools::install_github("terminological/uk-covid-datatools")
  }
}

ukcovidtools::setup(here::here("config.yml"))

```

Where `config.yml` looks something like:

```YAML
default:
  librarySource: ~/Git/uk-covid-datatools/
  outputDir: ~/covid19/output/
  cache: ~/covid19/tmp/
  secrets: ~/.covid.yml <see below>
  spimSource: s3bucket1 <must match entry in secrets file>
```

And a secrets file is provided (in example above `~/.covid.yml`) with content like:

```YAML
sftp:
  type: sftp-over-ssh
  hostName: <sftp hostname>
  ssh: <ssh login@ssh hostname>
  sshkey: <ssh key file>
  user: <sftp username>
  password: <sftp password>
s3bucket1:
  type: s3
  accesskey: <s3 access key>
  secretkey: <s3 secret key>
  region: eu-west-2
  bucket: <bucket-1 name>
s3bucket2:
  type: s3
  accesskey: <s3 access key>
  secretkey: <s3 secret key>
  region: eu-west-2
  bucket: <bucket-2 name>
localfiles:
  type: local
  root: ~/covid19/input/
```

## Meta-analysis of the SARS-CoV-2 serial interval and the impact of parameter uncertainty on the COVID-19 reproduction number 

[Code and supporting material](https://github.com/terminological/serial-interval) for our [MedRxiv paper](https://doi.org/10.1101/2020.11.17.20231548) 



## Estimates of regional infectivity of COVID-19 in the United Kingdom following imposition of social distancing measures

[Code and supporting material](https://github.com/terminological/wave-one-regional-rt) for our [MedRxiv paper](https://doi.org/10.1101/2020.04.13.20062760) has moved