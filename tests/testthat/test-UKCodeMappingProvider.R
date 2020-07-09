devtools::document()
devtools::load_all()
#test_that("lookup by name works", {
  dpc = DataProviderController$setup("~/Data/maps")
  dpc$codes$findCodesByName(df = tibble(name="England"))
#})
