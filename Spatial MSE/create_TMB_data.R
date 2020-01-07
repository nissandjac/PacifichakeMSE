create_TMB_data <- function(sim.data, df,
                             history = FALSE){
  # Organize a dataframe to run 'runhakeasssement.tmb'
  
  years <- df$years
  tEnd <- length(df$years)
  nyear <- tEnd
  age <- df$age
  
  
  nage <- length(age)
  msel <- rep(1,nage)
  # Maturity
  mat <- df$Matsel
  
  # weight at age 
  wage_catch <- df$wage_catch
  wage_survey <- df$wage_survey
  wage_ssb <- df$wage_ssb
  wage_mid <- df$wage_mid
  
  
  if (max(years) > 2018){
  wage_catch <- cbind(wage_catch,wage_catch[,1])  
  wage_survey <- cbind(wage_survey,wage_survey[,1])  
  wage_mid <- cbind(wage_mid,wage_mid[,1])  
  wage_ssb <- cbind(wage_ssb,wage_ssb[,1])  
  
  }
   # names(wage)[3:23] <- 0:20
  # wage <- melt(wage, id = c("year", "fleet"), value.name = 'growth', variable.name = 'age')
  
  # Catch
  catch <- sim.data$Catch
  
  # Survey abundance
  df.survey <- sim.data$survey
  Fnew <- sim.data$F0
  
  # Bias adjustment factor 
  # Parameters 
  b <- df$b
  #b <- matrix(1, tEnd)
  
  # Load parameters from the assessment 
  ### h prior distribution
  hmin <- 0.2
  hmax <- 1
  hprior <- 0.777
  hsd <- 0.117
  
  mu <- (hprior-hmin)/(hmax-hmin)
  tau <- ((hprior-hmin)*(hmax-hprior))/hsd^2-1

  
  df.new <-list(      #### Parameters #####
                  wage_catch = (wage_catch),
                  wage_survey = (wage_survey),
                  wage_ssb = wage_ssb,
                  wage_mid = wage_mid,
                  year_sel = df$year_sel,
                  #  Input parameters
                  Msel = msel,
                  Matsel= mat,
                  nage = nage,
                  age = age,
                  selYear = df$selidx,
                  years = years,
                  tEnd = length(years), # The extra year is to initialize 
                  logQ = df$logQ,   # Analytical solution
                  # Selectivity 
                  flag_sel = df$flag_sel,
                  Smin = df$Smin,
                  Smin_survey = df$Smin_survey,
                  Smax = df$Smax,
                  Smax_survey = df$Smax_survey,
                  b = b,
                  # survey
                  survey = sim.data$survey,#df.survey, # Make sure the survey has the same length as the catch time series
                  survey_x = df$survey_x, # Is there a survey in that year?
                  ss_survey = df$ss_survey,
                  flag_survey =df$flag_survey,
                  age_survey = sim.data$age_comps_surv,
                  age_maxage = df$age_maxage, # Max age for age comps 
                  # Catch
                  Catchobs = sim.data$Catch,
                  #                Catchobs = catch$Fishery, # Convert to kg
                  ss_catch = df$ss_catch,
                  flag_catch =df$flag_catch,
                  age_catch = sim.data$age_catch,
                  # variance parameters
                  logSDcatch = df$logSDcatch,
                  logSDR = df$logSDR, # Fixed in stock assessment ,
                  logphi_survey = df$logphi_survey,
                  sigma_psel = 1.4,
                  sum_zero = df$sum_zero,
                  smul = df$smul,
                  Bprior= tau*mu,
                  Aprior = tau*(1-mu),
                  survey_err = df$survey_err
                
  )
  
  
  # Add historical data if needed
  
  if(history == TRUE){
    df.new$survey <- df$survey[,1]
    
    if(length(df$survey[,1]) != length(df.new$survey)){
      stop('data not available')
    }

        df.new$age_catch <- as.matrix(df$age_catch)
        df.new$age_survey <- as.matrix(df$age_survey)
        df.new$Catchobs <- df$Catch

  }


  
  
  return(df.new)
  
}