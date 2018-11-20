## Load the hake data
# year and age input 
load_data_seasons <- function(move = TRUE){
  
  
  years <- 1966:2017
  nseason <- 4 # 4 seasons
  nyear <- length(years)
  tEnd <- length(years)*nseason
  age <- 0:20
  
  ## Age stuff
  nage <- length(age)
  msel <- rep(1,nage)
  
  # Maturity
  mat <- read.csv('maturity.csv')
  # Spatial stuff
  nspace <- 2 # number of grid cells 
  
  if(move == FALSE){
    nspace <- 1
  }
    
  recruitmat <- matrix(0, nspace) # 4 is seasonality 
  recruitmat[1] <- 1 # 10 percent change of spawning north
  recruitmat[2] <- 1 # 90 percent of spawning south

  # Movement matrix 5
  if(nseason > 1){
      movemax <- rep(0.5,nseason)
    }else{
      movemax <- rep(0.2,nseason)
      }
        

  
  
  movefifty <- 5
  moveslope <- 0.5
  
  movemat <- array(0, dim = c(nspace, nage, nseason)) # Chances of moving in to the other grid cell 
  
  if(move == TRUE){
    
    for(j in 1:nspace){
      for(i in 1:nseason){
        movemat[j,,i] <- movemax[i]/(1+exp(-moveslope*(age-movefifty)))
        
      }
    }
    
    if(nseason == 4){ # For the standard model
    movemat[,1:2,] <- 0 # Recruits and 1 year olds don't move
    
    movemat[1,3:nage,2:3] <- 0.05 # Don't move south during the year
    movemat[1,3:nage,nseason] <- 0.8
    movemat[2,3:nage,nseason] <- 0.05
}
    # movemat[1,11:nage,nseason] <- 0
    # movemat[2,11:nage,nseason] <- 0
    
    
    
    # move.init <- array(0.5, dim = c(nspace, nage))
    # 
    # move.init[1,] <- 0.3
    # move.init[2,] <- 0.7
    move.init <- c(0.3,0.7)
    
    }else{
      move.init <- 1
    }

  
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
  surveyseason <- 2
  
  if(nseason == 1){
    surveyseason = 1
  }
  # Insert dummy years
  
  age_survey <- as.data.frame(matrix(-1, nyear,dim(age_survey.df)[2]))
  names(age_survey) <- names(age_survey.df)
  age_survey$year <- years
  age_catch <- as.data.frame(matrix(-1, nyear,dim(age_catch.df)[2]))
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
  
  initN <- rev(read.csv('Ninit_MLE.csv')[,1])
  Rdev <- read.csv('Rdev_MLE.csv')[,1]
  PSEL <- as.matrix(read.csv('p_MLE.csv'))
  Fin <- assessment$F0
  
  b <- matrix(0,length(years))
  Yr <- years#1966:max(years)
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
  #b <- matrix(1, tEnd)
  
  psel<- matrix(NA,nspace, 5) 
  
  for(i in 1:nspace){
  psel[i,] <- c(2.8476, 0.973,0.3861,0.1775,0.5048) # USA selectivity 
  }
  #psel[1,] <- psel[2,]#c(1, 1, 1, 1,1)
  
  # if(move == TRUE){
     mul <- 1
  # }else{
  #  mul <- 1
#  }
  
  
     parms <- list( # Just start all the simluations with the same initial conditions 
       logRinit = 14.5614*1.015,
       logh = log(0.861909),
       logMinit = log(0.213686),
       logSDsurv = log(0.257246),
       #logSDR = log(1.4),
       logphi_catch = log(0.8276),
       logphi_survey = log(11.33),
       # logSDF = log(0.1),
       # Selectivity parameters 
       psel_fish = c(2.486490, 0.928255,0.392144,0.214365,0.475473),
       psel_surv = c(0.568618,-0.216172,0.305286 ,0.373829),
       initN = initN,
       Rin = Rdev,
       PSEL = PSEL
     )
  
     
     
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
                  nseason = nseason,
                  nyear = nyear,
                  tEnd = tEnd, # The extra year is to initialize 
                  logQ = log(1),   # Analytical solution
                  # Selectivity 
                  Smin = 1,
                  Smin_survey = 2,
                  Smax = 6,
                  Smax_survey = 6,
                  surveyseason = surveyseason,
                  # survey
                  survey = c(rep(1,df.survey$Year[1]-years[1]),df.survey$obs), # Make sure the survey has the same length as the catch time series
                  survey_x = c(rep(-2,df.survey$Year[1]-years[1]),df.survey$fleet), # Is there a survey in that year?
                  survey_err = c(rep(1,df.survey$Year[1]-years[1]),df.survey$se.log.), # Make sure the survey has the same length as the catch time series
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
                  years = years,
                  b = b,
                  #logh = log(0.8),
                  # Space parameters 
                  nspace = nspace,
                  movemat = movemat,
                  move = move,
                  recruitmat = recruitmat,
                  move.init = move.init,
                  F0 = Fin,
                  psel = psel,
                  parms = parms
                  # Parameters from the estimation model 
              
  )
  
  
  return(df)
  
}
