
devtools::load_all("~/Git/uk-covid-datatools/")
ukcovidtools::reload()

ukcovidtools::setup()

testDf = tibble::tribble(
  ~source, ~type, ~statistic, ~ageCat, ~gender, ~subgroup, ~code, ~name, ~codeType, ~date, ~value, ~population,
  "test", "incidence", "case", "<50", "male","p1","1","loc 1"
)

testDf2 = function(values) tibble::tibble(
  source = "test",
  type="incidence",
  statistic="case",
  ageCat=NA,
  gender=NA,
  subgroup=NA,
  code="test123",
  name="test place",
  codeType="TEST",
  date = as.Date(as.Date("2020-01-01"):(as.Date("2020-01-01")+length(values)-1),"1970-01-01"),
  value = values
)

test_that("ragged aggregation", {
  
  tmp = dpc$datasets$getPHEApiNations() %>% 
    filter(type=="incidence", statistic=="case", codeType=="CTRY")
  
  # cut data in wales to create ragged end of timeseries
  tmp = tmp %>% filter(!(name=="Wales" & date > "2020-12-01")) 
  
  # Aggregate to UK level. Add in a fake population.
  tmp = tmp %>% mutate(newName = "UK",newCode = "UK001", population = 1)
  
  tmp = tmp %>% covidStandardGrouping(code,name,codeType)
  
  agg = tmp %>% tsp$aggregateRagged(originalVar = name,aggregateVars = vars(newName,newCode), fn=sum, valueVars = vars(value,population))
  
  agg = agg %>% filter(!is.na(value))
  testthat::expect_condition(max(agg$date) == "2020-12-01")
  
})

test_that("age_aggregation",{
  
  tmp = dpc$datasets$getPHEApiNations() %>% 
    filter(type=="incidence", statistic=="case", codeType=="CTRY")
  
  # Aggregate to UK level. Add in a fake population.
  tmp2 = bind_rows(
    tmp %>% mutate(ageCat = "<50", population = 2),
    tmp %>% mutate(ageCat = "50+", population = 1) %>% filter(date < "2020-12-01")
  )
  
  # cut data in wales to create ragged end of timeseries
  agg = tmp2 %>% tsp$aggregateAge() #na.rm=TRUE)- adding this option in when fn=sum allows for ragged ends to be included
  
  testthat::expect_condition(all(agg$population == 3))
  
})

test_that("geog_aggregation",{
  
  tmp = dpc$datasets$getPHEApiNations() %>% 
    filter(type=="incidence", statistic=="case", codeType=="CTRY")
  
  
  # cut data in wales to create ragged end of timeseries
  # cut data in wales to create ragged end of timeseries
  tmp = tmp %>% filter(!(name=="Wales" & date > "2020-12-01"))
  
  # Aggregate to UK level. Add in a fake population.
  agg = tmp %>% mutate(population=1) %>% tsp$aggregateGeography(targetCodeTypes = "UK")
  
  testthat::expect_condition(all(agg$population == 4))
  testthat::expect_condition(max(agg$date) == "2020-12-01")
})

test_that("anomaly detection",{
  
  # sequence of 100,100,100...,2,100,100
  # if not anomaly because
  anomaly1 = testDf2(c(rep(100,10),2,rep(100,10)))
  anomaly1res = anomaly1 %>% tsp$completeAndRemoveAnomalies() %>% pull(Anomaly)
  testthat::expect_true(all(anomaly1res==FALSE))
  
  anomaly2 = testDf2(c(rep(0,5),8,rep(0,10),100))
  anomaly2res = anomaly2 %>% tsp$completeAndRemoveAnomalies() %>% pull(Anomaly)
  testthat::expect_equal(anomaly2$value %in% c(100),anomaly2res)
  
  anomaly3 = testDf2(c(0,0,0,2,4,8,16,32,64,128))
  testthat::expect_false(all(anomaly3 %>% tsp$completeAndRemoveAnomalies() %>% pull(Anomaly)))
  
  anomaly4 = testDf2(c(0,0,0,2,4,8,16,32,64,128,256,512,1024,0))
  testthat::expect_true(any(anomaly4 %>% tsp$completeAndRemoveAnomalies() %>% pull(Anomaly)))
  
})

