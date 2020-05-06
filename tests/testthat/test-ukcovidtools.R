test_that("normalisation works", {
  
  cumulative_cases=c(0,NA,0,NA,NA,9,2,3,NA,6,7,8,9,14,18,25,NA,50,45,44,50,60,70,NA,70,100)
  
  ts = tibble(
    date = as.Date("2020-01-01")+0:(length(cumulative_cases)-1),
    cumulative_cases=cumulative_cases,
    group=1
  )
  
  View(ts %>% group_by(group) %>% normaliseAndCleanse())
  
})

