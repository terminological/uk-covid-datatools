
library(readxl)
serialIntervalEstimates <- read_excel("data-raw/Serial Interval Estimates.xlsx")
usethis::use_data(serialIntervalEstimates, overwrite = TRUE)