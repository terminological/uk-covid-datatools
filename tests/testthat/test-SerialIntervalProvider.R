test_that("multiplication works", {
  # generate sample
  df = tibble(
    exampleDist = c(rep("normal",1000),rep("log normal",100),rep("gamma",100)),
    exampleP1 = c(rep(5,1000),rep(2,100),rep(1.5,100)),
    exampleP2 = c(rep(2,1000),rep(0.5,100),rep(2,100)),
    samples = c(rnorm(1000,5,2), rlnorm(100,2,0.5), rgamma(100,1.5,2))
  )
  #dpc = DataProviderController$new("~/tmp")
  fitter = DistributionFit$new()
  df %>% group_by(exampleDist) %>% fitter$fromUncensoredData(valueExpr = samples)
  fitter$generateSamples()
  fitter$filter(aic == min(aic))
  fitter$summariseDistributions()
  fitter$printDistributionSummary()
  fitter$plot()
})
