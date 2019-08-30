## Load the hake data
# year and age input 
load_data_seasons_future <- function(yr.future, nseason = 4, nspace = 2,
                                     movemaxinit = 0.5, movefiftyinit = 5, 
                                     nsurvey = 2, logSDR = 1.4, bfuture = 0.5,
                                     moveout = 0.6, movesouth = 0.01,
                                     moveinit = NA, moveslope = 1,
                                     yearnow = 2018){
  
  if(is.na(moveinit)){
    if(nspace == 2){
      moveinit <-  c(0.3,0.7)
    }
  }
  
  years <- 1966:(yearnow+yr.future)
  
  nyear <- length(years)
  tEnd <- length(years)*nseason
  age <- 0:20
  
  ## Age stuff
  nage <- length(age)
  msel <- rep(1,nage)
  
  # Maturity
  mat <- read.csv('data/maturity.csv')
  # Spatial stuff
  
  recruitmat <- matrix(0, nspace) # 4 is seasonality 
  recruitmat[1] <- 1 # 10 percent change of spawning north
  recruitmat[2] <- 1 # 90 percent of spawning south
  
  # Movement matrix 5
  
  movefifty <- movefiftyinit
  ## for later use
  recruitmat <- matrix(0, nspace) # 4 is seasonality 
  recruitmat[1] <- 1 # 10 percent change of spawning north
  recruitmat[2] <- 1 # 90 percent of spawning south
  
  
  # Maturity
  
  movefifty <- movefiftyinit
  
  movemax <- rep(movemaxinit,nseason)
  
  movemat <- array(0, dim = c(nspace, nage, nseason, nyear)) # Chances of moving in to the other grid cell 
  
  if(nspace == 1){
    move = FALSE
  }else{
    move = TRUE
  }
  
  if(move == TRUE){
    
    for(j in 1:nspace){
      for(i in 1:nseason){
        movemat[j,,i,] <- movemax[i]/(1+exp(-moveslope*(age-movefifty)))
        
      }
    }
    
    if(nseason == 4){ # For the standard model
      movemat[,1:2,,] <- 0 # Recruits and 1 year olds don't move
      movemat[1,3:nage,1:3,] <- movesouth # Don't move south during the year
      movemat[1,3:nage,1,] <- moveout*0.5 # continuing south movement at spawning time
      
      movemat[1,3:nage,nseason,] <- moveout
      movemat[2,3:nage,nseason,] <- movesouth
    }
    # movemat[1,11:nage,nseason] <- 0
    # movemat[2,11:nage,nseason] <- 0
    
    
    
    # move.init <- array(0.5, dim = c(nspace, nage))
    # 
    # move.init[1,] <- 0.3
    # move.init[2,] <- 0.7
    move.init <- moveinit
    
  }else{
    move.init <- 1
  }
  
  
  
  
  # weight at age # weight at age 
  wage <- read.csv('data/waa.csv')
  wage_unfished <- read.csv('data/unfished_waa.csv')
  
  
  # Make the weight at ages the same length as the time series 
  
  
  wage_ssb = rbind(matrix(rep(as.numeric(wage_unfished[2:(nage+1)]),each = 9), nrow = 9),
                   as.matrix(wage[wage$fleet == 0,3:(nage+2)]),
                   matrix(rep(as.numeric(wage_unfished[2:(nage+1)]),each = yr.future+1), nrow = yr.future+1))
  
  wage_catch = rbind(matrix(rep(as.numeric(wage_unfished[2:(nage+1)]),each = 9), nrow = 9),
                     as.matrix(wage[wage$fleet == 1,3:(nage+2)]),
                     matrix(rep(as.numeric(wage_unfished[2:(nage+1)]),each = yr.future+1), nrow = yr.future+1))
  
  wage_survey = rbind(matrix(rep(as.numeric(wage_unfished[2:(nage+1)]),each = 9), nrow = 9),
                      as.matrix(wage[wage$fleet == 2,3:(nage+2)]),
                      matrix(rep(as.numeric(wage_unfished[2:(nage+1)]),each = yr.future+1), nrow = yr.future+1))
  
  wage_mid = rbind(matrix(rep(as.numeric(wage_unfished[2:(nage+1)]),each = 9), nrow = 9),
                   as.matrix(wage[wage$fleet == -1,3:(nage+2)]),
                   matrix(rep(as.numeric(wage_unfished[2:(nage+1)]),each = yr.future+1), nrow = yr.future+1))
  # names(wage)[3:23] <- 0:20
  # wage <- melt(wage, id = c("year", "fleet"), value.name = 'growth', variable.name = 'age')
  catch <- read.csv('data/hake_totcatch.csv')
  
  # Survey abundance
  df.survey <- read.csv('data/acoustic survey.csv')
  # Maturity
  mat <- read.csv('data/maturity.csv')
  
  
  # Catch
  
  age_survey.df <- read.csv('data/agecomps_survey.csv')
  age_survey.df$flag <- 1
  age_catch.df <- read.csv('data/agecomps_fishery.csv')
  age_catch.df$flag <- 1
  
  if(nseason == 4){
    surveyseason <- 3
    
  }else{
    surveyseason <- floor(nseason/2)
  }
  
  
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
  
  initN <- rev(read.csv('data/Ninit_MLE.csv')[,1])
  Rdev <- c(read.csv('data/Rdev_MLE.csv')[,1],0)
  PSEL <- as.matrix(read.csv('data/p_MLE.csv'))
  #Fin <- assessment$F0
  
  b <- matrix(NA, nyear)
  # Parameters 
  yb_1 <- 1965 #_last_early_yr_nobias_adj_in_MPD
  yb_2 <- 1971 #_first_yr_fullbias_adj_in_MPD
  yb_3 <- 2016 #_last_yr_fullbias_adj_in_MPD
  yb_4 <- 2018 #_first_recent_yr_nobias_adj_in_MPD
  b_max <- 0.87 #_max_bias_adj_in_MPD
  
  b[1] <- 0
  Yr <- years
  
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
      b[j] = bfuture
    }
    # if (b[j]<b[j-1]){
    #   stop('why')
    # }
  }  
  
  #b <- matrix(1, tEnd)
  
  psel<- matrix(NA,nspace, 5) 
  
  for(i in 1:nspace){
    psel[i,] <- c(2.4716, 0.9084,0.399418,0.215115,0.474583) # USA selectivity 
  }
  
 # psel[1,] <- c(1,1,1,1,1)

  # Fsel <- getSelec(df$age,psel[1,], df$Smin, df$Smax)
  # plot(Fsel, col = 'red')
  # Fsel <- getSelec(df$age,psel[2,], df$Smin, df$Smax)
  # lines(Fsel)
  #
  #

  
  parms <- list( # Just start all the simluations with the same initial conditions 
    logRinit = 14.543,
    logh = log(0.8646),
    logMinit = log(0.2135),
    logSDsurv = log(0.2601),
    #logSDR = log(1.4),
    logphi_catch = log(0.8549),
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
                  year_sel = length(1991:2018), # Years to model time varying sel
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
                  nsurvey = nsurvey, # Frequency of survey years (e.g., 2 is every second year)
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
                  age_catch = t(as.matrix(age_catch[,3:17])),
                  # variance parameters
                  logSDcatch = log(0.01),
                  logSDR = log(logSDR), # Fixed in stock assessment ,
                  logphi_survey = log(10),
                  years = years,
                  b = b[Yr >= years[1]],
                  bfuture = bfuture,
                  #logh = log(0.8),
                  # Space parameters 
                  smul = 0.6, # Annual survey timing 
                  sigma_psel = 1.4,
                  nspace = nspace,
                  #TAC = TAC,
                  movemat = movemat,
                  move = move,
                  recruitmat = recruitmat,
                  move.init = move.init,
                  # F0 = Fin,
                  psel = psel,
                  parms = parms,
                  selectivity_change = 0
                  
                  # Parameters from the estimation model 
                  
  )
  Catch.obs <- read.csv('data/hake_totcatch.csv') # Total catch
  Catch.country <- read.csv('data/catch_per_country.csv')
  df$Catch <- Catch.obs$Fishery # Add the observed catch
  df$Catch.country <- as.matrix(Catch.country[,2:3])[,c(2,1)]
  # Correct for future years 
  if(nyear > length(df$Catch)){
    df$Catch <- c(df$Catch,rep(mean(df$Catch), nyear-length(Catch.obs$Fishery)))
    
  }
  
  if(nyear >nrow(df$Catch.country)){
    df$Catch.country <- rbind(df$Catch.country,t(replicate(nyear-nrow(Catch.country),colMeans(df$Catch.country))))
  }
  if(max(years) > 2018){
    
    idx.future <- length(1966:2018)+seq(2,yr.future, by = 2) # Years where survey occurs 
    
    df$survey_x <- c(df$survey_x,rep(-2, yr.future))
    df$survey_x[idx.future] <- 2
    
    df$survey_err <- c(df$survey_err,rep(1, yr.future))
    df$survey_err[idx.future] <- mean(df$survey_err[df$survey_err != 1])
    
    df$ss_survey[idx.future] <- mean(df$ss_survey[df$ss_survey != -1])
    df$flag_survey[idx.future] <- 1
    df$flag_catch[years > 2018] <- 1
    
    Rdevs <- rnorm(n = yr.future,mean = 0, sd = exp(df$logSDR))
    #Rdevs <- rep(0, yr.future)
    df$parms$Rin <- c(df$parms$Rin,Rdevs)
  }
  
  
  
  
  return(df)
  
}
