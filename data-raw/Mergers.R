# https://github.com/publichealthengland/odsR/blob/master/R/getODS.R

areaMergers = tribble(
  ~fromCode, ~fromCodeType, ~toCode, ~toCodeType, ~toName, ~weight
  "RDZ","NHS trust","R0D","NHS trust", "University Hospitals Dorset NHS Foundation Trust", 1
  "RD3","NHS trust","R0D","NHS trust", "University Hospitals Dorset NHS Foundation Trust", 1
)

usethis::use_data(areaMergers, overwrite = TRUE)