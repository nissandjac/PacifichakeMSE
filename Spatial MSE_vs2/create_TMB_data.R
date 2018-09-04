create_TMB_data <- function(sim.data, df){
  # Organize a dataframe to run 'runhakeasssement.tmb'
  
  years <- df$years
  tEnd <- length(df$years)
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
  
  
  if (max(years) > 2017){
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
  # Calculate the bias adjustment 
  b <- matrix(0, tEnd)
  Yr <- 1946:max(years)
  # Parameters 
  yb_1 <- 1965 #_last_early_yr_nobias_adj_in_MPD
  yb_2 <- 1971 #_first_yr_fullbias_adj_in_MPD
  yb_3 <- 2016 #_last_yr_fullbias_adj_in_MPD
  yb_4 <- 2017 #_first_recent_yr_nobias_adj_in_MPD
  b_max <- 0.87 #_max_bias_adj_in_MPD
  
  b[1] <- 0
  for(j in 2:length(Yr)){
    
    if (Yr[j] <= yb_1){
      b[j] = 0}
    
    if(Yr[j] > yb_1 & Yr[j]< yb_2){
      b[j] = b_max*((Yr[j]-yb_1)/(yb_2-yb_1));
    }
    
    if(Yr[j] >= yb_2 & Yr[j] <= yb_3){
      b[j] = b_max}
    
    if(Yr[j] > yb_3 & Yr[j] < yb_4){
      b[j] = b_max*(1-(yb_3-Yr[j])/(yb_4-yb_3))
    }
    
    if(Yr[j] >= yb_4){
      b[j] = 0
    }
    # if (b[j]<b[j-1]){
    #   stop('why')
    # }
  }  
  b <- matrix(1, tEnd)
  
  # Load parameters from the assessment 
  

  
  df.new <-list(      #### Parameters #####
                  wage_catch = (wage_catch),
                  wage_survey = (wage_survey),
                  wage_ssb = wage_ssb,
                  wage_mid = wage_mid,
                  #  Input parameters
                  Msel = msel,
                  Matsel= mat,
                  nage = nage,
                  age = age,
                  year_sel = length(1991:2010), # Years to model time varying sel
                  selYear = 26,
                  years = years,
                  tEnd = length(years), # The extra year is to initialize 
                  logQ = log(1),   # Analytical solution
                  # Selectivity 
                  Smin = 1,
                  Smin_survey = 2,
                  Smax = 6,
                  Smax_survey = 6,
                  b = b,
                  # survey
                  survey = df.survey, # Make sure the survey has the same length as the catch time series
                  survey_x = df$survey_x, # Is there a survey in that year?
                  ss_survey = df$ss_survey,
                  flag_survey =df$flag_survey,
                  age_survey = sim.data$age_comps_surv,
                  age_maxage = 15, # Max age for age comps 
                  # Catch
                  Catchobs = sim.data$Catch,
                  #                Catchobs = catch$Fishery, # Convert to kg
                  ss_catch = df$ss_catch,
                  flag_catch =df$flag_catch,
                  age_catch = sim.data$age_catch,
                  # variance parameters
                  logSDcatch = log(0.01),
                  logSDR = log(1.4), # Fixed in stock assessment ,
                  logphi_survey = log(0.91),
                  sigma_psel = 0.04,
                  logh = log(0.8),
                  F0 = c(df$F0,sim.data$F0),
                  survey_err = df$survey_err
                
  )
  
  
  
  
  
  
  
  
  return(df.new)
  
}