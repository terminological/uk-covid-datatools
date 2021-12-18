library(dbplyr)
library(tidyverse)

here::i_am("data-raw/variants.R")

# con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "spim_test")

# con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "~/Data/covid/test_spim.sqlite3", cache_size = -4*1024*1024, extended_types = TRUE, synchronous="off")
# 
# db_copy_to(con, "mtcars", mtcars,temporary=FALSE)
# DBI::dbListTables(con)
# 
# dbmtcars = tbl(con,"mtcars")

variantDesignation = readxl::read_excel(here::here("data-raw/Variant Designation.xlsx"))
usethis::use_data(variantDesignation, overwrite = TRUE)


### residential ----
