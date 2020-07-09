# 
# serialIntervals = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRdVV2wm6CcqqLAGymOLGrb8JXSe5muEOotE7Emq9GHUXJ1Fu2Euku9d2LhIIK5ZvrnGsinH11ejnUt/pub?gid=0&single=true&output=csv")
# 
# boot.samples = NULL
# set.seed(101)
# 
# for (iteration in 1:250) {
#   samples = NULL
#   message(".",appendLF = FALSE)
#   if (iteration %% 50==0) message(iteration)
#   for (i in 1:nrow(serialIntervals)) {
#     
#     mean = serialIntervals$mean_si_estimate[[i]]
#     mean_sd = (serialIntervals$mean_si_estimate_high_ci[[i]]-serialIntervals$mean_si_estimate_low_ci[[i]])/(1.96*2)
#     sd = serialIntervals$std_si_estimate[[i]]
#     sd_sd = (serialIntervals$std_si_estimate_high_ci[[i]]-serialIntervals$std_si_estimate_low_ci[[i]])/(1.96*2)
#     dist = serialIntervals$assumed_distribution[[i]]
#     N = serialIntervals$sample_size[[i]]*10
#     boot_mean = rnorm(1,mean,mean_sd) %>% scales::squish(range=c(serialIntervals$mean_si_estimate_low_ci[[i]],serialIntervals$mean_si_estimate_high_ci[[i]]))
#     if (is.na(sd_sd)) {
#       boot_sd = sd
#     } else {
#       boot_sd = rnorm(1,sd,sd_sd) %>% scales::squish(range=c(serialIntervals$std_si_estimate_low_ci[[i]],serialIntervals$std_si_estimate_high_ci[[i]]))
#     }
#     if (dist == "normal") {
#       samples = c(samples,rnorm(N,boot_mean,boot_sd))
#     } else if (dist == "log normal") {
#       # reparametereise
#       lmean = log(boot_mean/sqrt(1+(boot_sd^2)/(boot_mean^2)))
#       lsd = sqrt(log(1+(boot_sd^2)/(boot_mean^2)))
#       samples = c(samples,rlnorm(N,lmean,lsd))
#     } else if (dist == "gamma") {
#       scale = (boot_sd^2) / boot_mean
#       shape = (boot_mean^2) / (boot_sd^2)
#       samples = c(samples,rgamma(N,shape = shape,scale=scale))
#     }
#   }
#   #browser()
#   samples = samples[samples > 0 & !is.na(samples) & samples<21]
#   fit.gamma <- fitdistrplus::fitdist(samples, distr = "gamma", method = "mle")
#   #summary(fit.gamma)
#   fit.shape = fit.gamma$estimate[[1]]
#   fit.rate = fit.gamma$estimate[[2]]
#   fit.mean = fit.shape/fit.rate
#   fit.sd = sqrt(fit.shape/(fit.rate)^2)
#   boot.samples = boot.samples %>% bind_rows(tibble(shape = fit.shape,rate = fit.rate,mean = fit.mean,sd=fit.sd))
# }
# 
# cfg = EpiEstim::make_config(list(
#   mean_si = mean(boot.samples$mean), 
#   std_mean_si = sd(boot.samples$mean), 
#   min_mean_si = quantile(x = boot.samples$mean,0.1),
#   max_mean_si = quantile(x = boot.samples$mean,0.9),
#   std_si = mean(boot.samples$sd), 
#   std_std_si = sd(boot.samples$sd), 
#   min_std_si = quantile(x = boot.samples$sd,0.1),
#   max_std_si = quantile(x = boot.samples$sd,0.9),
#   mean_prior = 1,
#   std_prior = 0.5), method="uncertain_si")
