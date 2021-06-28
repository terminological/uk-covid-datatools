test_that("lnorm works", {

  testDfit = DistributionFit$new(distributions = "lnorm")
  testDfit$models
  
  testData = bind_rows(map2(
    list(1,2,3,4,5),
    list(2,1,2,1,2), 
    ~ tibble(
      meanlog_in = DistributionFit$conversionFrom$lnorm$meanlog(.x,.y),
      sdlog_in = DistributionFit$conversionFrom$lnorm$sdlog(.x,.y), 
      value = rlnorm(50,meanlog_in,sdlog_in))
  ))
  
  testDfit$fromUncensoredData(testData %>% group_by(meanlog_in,sdlog_in))
  
  testDfit$plot(xlim = c(0,15))
  
  testDfit2 = DistributionFit$new(distributions = "norm")
  testDfit2$models
  
  testData2 = bind_rows(map2(
    list(1,2,3,4,5),
    list(2,1,2,1,2), 
    ~ tibble(mean_in = .x,sd_in = .y, value = rnorm(50,.x,.y))
  ))
  
  testDfit2$fromUncensoredData(testData2 %>% group_by(mean_in,sd_in))
  
  testDfit2$plot(xlim = c(-5,15))

})