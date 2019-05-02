## Load the hake data
# year and age input 
load_data <- function(){
  
  
  years <- 1966:2017
  tEnd <- length(years)
  age <- 0:20

  
  nage <- length(age)
  msel <- rep(1,nage)
  # Maturity
  mat <- read.csv('maturity.csv')
  
  # weight at age 
  wage <- read.csv('waa.csv')
  wage_unfished <- read.csv('unfished_waa.csv')
  
  # Make the weight at ages the same length as the time series 
  wage_ssb = rbind(matrix(rep(as.numeric(wage_unfished[2:(nage+1)]),each = 9), nrow = 9),
                   as.matrix(wage[wage$fleet == 0,3:(nage+2)]))
  wage_catch = rbind(matrix(rep(as.numeric(wage_unfished[2:(nage+1)]),each = 9), nrow = 9),
                     as.matrix(wage[wage$fleet == 1,3:(nage+2)]))
  wage_survey = rbind(matrix(rep(as.numeric(wage_unfished[2:(nage+1)]),each = 9), nrow = 9),
                      as.matrix(wage[wage$fleet == 2,3:(nage+2)]))
  wage_mid = rbind(matrix(rep(as.numeric(wage_unfished[2:(nage+1)]),each = 9), nrow = 9),
                   as.matrix(wage[wage$fleet == -1,3:(nage+2)]))
  # names(wage)[3:23] <- 0:20
  # wage <- melt(wage, id = c("year", "fleet"), value.name = 'growth', variable.name = 'age')
  
  # Catch
  catch <- read.csv('hake_totcatch.csv')
  
  # Survey abundance
  df.survey <- read.csv('acoustic survey.csv')
  
  
  
  # Age comps
  
  age_survey.df <- read.csv('agecomps_survey.csv')
  age_survey.df$flag <- 1
  age_catch.df <- read.csv('agecomps_fishery.csv')
  age_catch.df$flag <- 1
  # Insert dummy years
  
  age_survey <- as.data.frame(matrix(-1, tEnd,dim(age_survey.df)[2]))
  names(age_survey) <- names(age_survey.df)
  age_survey$year <- years
  age_catch <- as.data.frame(matrix(-1, tEnd,dim(age_catch.df)[2]))
  names(age_catch) <- names(age_catch.df)
  age_catch$year <- years
  
  for (i in 1:dim(age_survey.df)[1]){
    idx <- which(age_survey$year == age_survey.df$year[i])
    age_survey[idx,] <-age_survey.df[i,]
    
  }
  
  for (i in 1:dim(age_catch.df)[1]){
    idx <- which(age_catch$year == age_catch.df$year[i])
    age_catch[idx,] <-age_catch.df[i,]
    
  }
  
  # Load parameters from the assessment 
  
  Rdev <- read.csv('Rdev.csv')[,2]
  initN <- read.csv('initN.csv', header = F)[,2]

  Fin <- read.csv('Fin.csv')[,1]
  PSEL <- as.matrix(read.csv('p_estimated.csv'))
  
  df <-list(      #### Parameters #####
                  wage_ssb = t(wage_ssb),
                  wage_catch = t(wage_catch),
                  wage_survey = t(wage_survey),
                  wage_mid = t(wage_mid),
                  #  Input parameters
                  Msel = msel,
                  Matsel= mat$mat,
                  nage = nage,
                  age = age,
                  year_sel = length(1991:2010), # Years to model time varying sel
                  selYear = 26,
                  tEnd = length(years), # The extra year is to initialize 
                  logQ = log(1),   # Analytical solution
                  # Selectivity 
                  Smin = 1,
                  Smin_survey = 2,
                  Smax = 6,
                  Smax_survey = 6,
                  # survey
                  survey = c(rep(1,df.survey$Year[1]-years[1]),df.survey$obs), # Make sure the survey has the same length as the catch time series
                  survey_x = c(rep(-2,df.survey$Year[1]-years[1]),df.survey$fleet), # Is there a survey in that year?
                  ss_survey = age_survey$nTrips,
                  flag_survey =age_survey$flag,
                  age_survey = t(as.matrix(age_survey[,3:17])*0.01),
                  age_maxage = 15, # Max age for age comps 
                  # Catch
  #                Catchobs = catch$Fishery, # Convert to kg
                  ss_catch = age_catch$nTrips,
                  flag_catch =age_catch$flag,
                  age_catch = t(as.matrix(age_catch[,3:17])*0.01),
                  # variance parameters
                  logSDcatch = log(0.01),
                  logSDR = log(1.4), # Fixed in stock assessment ,
                  logphi_survey = log(0.91),
                  sigma_psel = 0.04,
                  years = years,
                  # Parameters from the estimation model 
                  parms =  list( # Just start all the simluations with the same initial conditions 
                    logRinit = 14.8354,
                    logh = log(0.8122),
                    logMinit = log(0.2299),
                    logSDsurv = log(0.3048),
                    logphi_catch = log(0.3),
                    # Selectivity parameters 
                    psel_fish = c(2.8476, 0.973,0.3861,0.1775,0.5048),
                    psel_surv = c(0.5919,-0.2258,0.2876,0.3728),
                    initN = initN,
                    Rin = Rdev,
                    F0 = Fin,
                    PSEL = PSEL
                  )
  )
  
  
  return(df)
  
}
