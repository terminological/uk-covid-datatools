# Meta-analysis of the SARS-CoV-2 serial interval and the impact of parameter uncertainty on the COVID-19 reproduction number 

Code and supporting material for our [MedRxiv paper](https://doi.org/10.1101/2020.11.17.20231548) 

## Serial interval estimates

The truncated empirical serial interval distribution for covid-19 based on a meta-analysis of various sources is stored as a matrix
in the following file: "resampled-truncated-empirical-si-sample.txt" which has 100 samples of the probability on days 0-13 of transmission.

```R
si_sample_data = read.table("https://raw.githubusercontent.com/terminological/uk-covid-datatools/master/vignettes/serial-interval/resampled-truncated-empirical-si-sample.txt")

estimate_R(
  incid, # an incidence time series
  method = "si_from_sample",
  si_sample = si_sample_data,
  config = ... # the rest of the configuration including prior R0 etc.
)
```

## Generation interval distributions

The best fitting gamma distributions inferred from those serial intervals distributions are provided in two forms. Firstly
a full parameterisation of the gamma distributions for the generation interval presented in the paper is provided in the file "generation-interval-fitted-parameters.csv". 
Using this should be relatively self explanatory, although it is not a supported input for EpiEstim. 

Secondly a matrix containing the discrete probabilities associated with those gamma distributions in a format suitable for use in epiestim is
provided in "generation-interval-fitted-si-sample.txt"

```R
gen_int_sample_data = read.table("https://raw.githubusercontent.com/terminological/uk-covid-datatools/master/vignettes/serial-interval/generation-interval-fitted-si-sample.txt")

estimate_R(
  incid, # an incidence time series
  method = "si_from_sample",
  si_sample = gen_int_sample_data,
  config = ... # the rest of the configuration including prior R0 etc.
)
```

The parameterised gammas are provided in this form for use in EpiEstim. We don't recommend using the "uncertain_si" and the "mean_si", "std_mean_si", "min_mean_si", 
"max_mean_si", "std_si", "std_std_si", "min_std_si" and "max_std_si" options because the sampling process used to reconstruct uncertain distributions
has features that we suspect could introduce bias. This can be avoided by specifying the discrete PDFs and using the "si_from_sample" option.

Please raise an issue on github if you want other output formats and I'll see what I can do.
