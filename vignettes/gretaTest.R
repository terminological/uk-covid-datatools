#reticulate::install_miniconda()
#reticulate::conda_create("r-tensorflow", packages=c("python=3.7","tensorflow=1.14","pyyaml","requests","Pillow","pip","numpy=1.16"))
#reticulate::py_install("tensorflow-probability=0.7", "r-tensorflow")
reticulate::use_condaenv(condaenv = 'r-tensorflow', required = TRUE)
#install.packages("greta")

library(greta)

# data
x <- as_data(iris$Petal.Length)
y <- as_data(iris$Sepal.Length)

# variables and priors
int <- normal(0, 1)
coef <- normal(0, 3)
sd <- student(3, 0, 1, truncation = c(0, Inf))

# operations
mean <- int + coef * x

# likelihood
distribution(y) <- normal(mean, sd)

# defining the model
m <- model(int, coef, sd)

# plotting
plot(m)

# sampling
draws <- mcmc(m, n_samples = 1000)



#### ----

library(readr)
skorea <- read_csv("https://github.com/jihoo-kim/Data-Science-for-COVID-19/raw/master/dataset/Time/Time.csv", 
                 col_types = cols(date = col_date(format = "%Y-%m-%d")))

skorea2 = skorea %>% mutate(
  released_inc = released - lag(released, default=0),
  deceased_inc = deceased - lag(deceased,default=0),
  day = date-as.Date("2020-01-20")
)

tested

#### ----

View(covidAgeData)

getTs = function(var, startDate = min(covidAgeData$date)) {
  var = ensym(var)
  tmp = covidAgeData %>% 
    filter(gender=="Both" & country=="Spain" & date>startDate) %>% 
    rename(incidence = !!var) %>%
    select(date,age,incidence) %>% group_by(age) %>% arrange(date) %>% 
    mutate(incidence = incidence-lag(incidence)) 
  #browser()
  tmp2 = tmp %>%
    mutate(incidence = ifelse(incidence<0,0,incidence)) %>% 
    filter(!is.na(incidence)) %>% 
    group_modify(function(d,g,...) {
      #browser()
      d$incidence = c(d$incidence %>% head(1),zoo::rollmean(d$incidence, 3),d$incidence %>% tail(1))
      return(d)
    }) 
  tmp3 = tmp2 %>% 
    #mutate(date = as.integer(date)-as.integer(startDate)) %>%
    pivot_wider(names_from = age,values_from = incidence) %>%
    select(-date)
  return(tmp3)
}

censoredGretaArray = function(cases) { 
  ageGroups = ncol(cases)
  timepoints = nrow(cases)
  cases = greta::as_data(cases)
  censoredCases = greta::zeros(100+timepoints,ageGroups)
  censoredCases[51:(timepoints+50),]=cases
  unknown = greta::variable(lower=0,dim=c(50,ageGroups))
  censoredCases[1:50,]=unknown
  censoredCases[(timepoints+51):(timepoints+100),]=unknown
  return(censoredCases)
}

casesByAge = getTs(cases)
cases = censoredGretaArray(casesByAge)

admissionsByAge = getTs(admitted)
admissions = censoredGretaArray(admissionsByAge)



p_admissionByAge = greta::variable(lower=0,upper=1,dim=ageGroups)
gammaShapeByAge = greta::variable(lower=0,dim=ageGroups)
gammaRateByAge = greta::variable(lower=0,dim=ageGroups)

calculate = function(shape, rate, casesMatrix) {
  tibble(time = 0:50) %>% mutate(cdf = pgamma(time,shape,rate)) %>% mutate(p = cdf-lead(cdf), time = -time)
}

# # p_admissionByDay = greta::zeros(dim=ageGroups)
# # for (i in seq_along(p_admissionByDay)) {
# #   p_admissionByDay[i,1] = greta::gamma(gammaShapeByAge[i,1], gammaRateByAge[i,1])
# # }
# 
# p_admissionByDayAndAge = p_admissionByDay * p_admissionByAge

