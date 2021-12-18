library(dbplyr)
library(tidyverse)

here::i_am("data-raw/ethnicities-and-residential.R")

# con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "spim_test")

# con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "~/Data/covid/test_spim.sqlite3", cache_size = -4*1024*1024, extended_types = TRUE, synchronous="off")
# 
# db_copy_to(con, "mtcars", mtcars,temporary=FALSE)
# DBI::dbListTables(con)
# 
# dbmtcars = tbl(con,"mtcars")

devtools::load_all()
ukcovidtools::reload()
options("ukcovid.reproduce.at"=as.Date("2021-10-30"))

deaths = dpc$spim$getDeathsLineList()
genomics = dpc$spim$getVAMLineList()
ctas = dpc$spim$getCTASLineList()
immunisations = dpc$spim$getImmunizationLineList()
admissions = dpc$spim$getAdmissionLineList()
cases = dpc$spim$getLineList()
sgene = dpc$spim$getSGeneLineList()


ethnicities = bind_rows(
  deaths %>% select(ethnicity_final) %>% distinct() %>% collect(),
  genomics %>% select(ethnicity_final) %>% distinct() %>% collect(),
  ctas %>% select(ethnicity_final = ethnicity_full) %>% distinct() %>% collect(),
  ctas %>% select(ethnicity_final = ethnicity) %>% distinct() %>% collect(),
  immunisations %>% select(ethnicity_final = ethnicity_category) %>% distinct() %>% collect(),
  admissions %>% select(ethnicity_final) %>% distinct() %>% collect(),
  cases %>% select(ethnicity_final) %>% distinct() %>% collect()
) %>% distinct()

readr::write_csv(ethnicities, here::here("data-raw/ethnicities-unmatched.csv"))

ethnicityLookup = readxl::read_xlsx( here::here("data-raw/ethnicities-matched.xlsx"))
    

usethis::use_data(ethnicityLookup, overwrite = TRUE)


### residential ----

residentialLookup = bind_rows(
  deaths %>% select(residence_type, residential_category) %>% distinct() %>% collect(),
  cases %>% select(residence_type = cat, residential_category) %>% distinct() %>% collect(),
) %>% distinct()

usethis::use_data(residentialLookup, overwrite = TRUE)
