## Load the hake data
# year and age input 
load_data_seasons <- function(nseason = 4, 
                              nspace = 2, 
                              myear = 2018,
                              movemaxinit = 0.35, 
                              movefiftyinit = 6,
                              nsurvey = 2, 
                              logSDR = 1.4, 
                              bfuture = 0.5,
                              moveout = 0.85, 
                              movesouth = 0.05,
                              moveinit = NA, 
                              moveslope = 0.9,
                              selectivity_change = 0,
                              yr_future  = 0,
                              sel_hist = 1
                              ){
  
  #' @nseason = number of seasons 
  #' @nspace = Spatial areas
  #' @myear = Final year that contains empirical data
  #' @movemaxinit = max movement rate 
  #' @movefiftyinit = age at 50% movement rate
  #' @nsurvey survey frequency, i.e., 2 = every second year
  #' @logSDR Standard deviation of recruitment
  #' @bfuture recruitment bias adjustment in the future - scalar
  #' @moveout fraction of individuals that travel south in the last season
  #' @movesouth fraction of individuals that move south during the year
  #' @moveinit Initial distribution of fish
  #' @moveslope Slope of the movement function
  #' @selectivity_change flag for selectivity changing in the future 0
  #' @yr_future Create dummy data for future years
  
  
  if(is.na(moveinit)){
    if(nspace == 2){
    moveinit <-  c(0.25,0.75)
    }
  }
  
  
  years <- 1966:(myear+yr_future)
  nyear <- length(years)
  tEnd <- length(years)*nseason
  age <- 0:20
  
  ## Age stuff
  nage <- length(age)
  msel <- rep(1,nage)
  
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
    
    movemat[,1:2,,] <- 0 # Recruits and 1 year olds don't move
    
    if(nseason == 4){ # For the standard model
      
      movemat[1,3:nage,2:3,] <- movesouth # Don't move south during the year
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
  
  
  
  
  # weight at age 
  wage_ss <- read.csv('data/wage_ss.csv')
  wage_ss <- wage_ss[wage_ss$Yr %in% years,]
  wage_unfished <- read.csv('data/unfished_waa.csv')

  
  wage_ssb <- wage_ss[wage_ss$Fleet == -2,paste('X',age, sep = '')]
  wage_ssb[[1]] <- unname(wage_ssb[[1]])
  wage_catch <- wage_ss[wage_ss$Fleet == 1 ,paste('X',age, sep = '')]
  wage_survey <- wage_ss[wage_ss$Fleet == 2,paste('X',age, sep = '')]
  wage_mid <- wage_ss[wage_ss$Fleet == -1,paste('X',age, sep = '')]
  
  # if(yr_future>0){ // Namiong issues here, fix in later update
  #   tmp_ssb <- matrix(rep(wage_ssb[1,], each = yr_future), nrow = yr_future)
  #   wage_ssb <- cbind(wage_ssb,tmp_ssb)
  #    
  #   
  #   wage_catch <- wage_ss[wage_ss$Fleet == 1 ,paste('X',age, sep = '')]
  #   wage_survey <- wage_ss[wage_ss$Fleet == 2,paste('X',age, sep = '')]
  #   wage_mid <- wage_ss[wage_ss$Fleet == -1,paste('X',age, sep = '')]
  #   
  #   
  #   
  #   
  # }
  # 
  
  # Catch
  catch <- read.csv('data/hake_totcatch.csv')
  
  # Survey abundance
  df.survey <- read.csv('data/acoustic survey.csv')
  # Maturity
  mat <- read.csv('data/maturity.csv')
  mat <- wage_ssb[1,]
  
  
  # Age comps
  
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
  
  survey <- read.csv('data/survey.csv')
  # Load the age comps 
  age_survey.tmp <- read.csv('data/age_survey_ss.csv')
  age_catch.tmp <- read.csv('data/age_catch_ss.csv')
  ac.data <- read.csv('data/ac_data.csv')
  
  # age_survey <- as.data.frame(matrix(-1, nyear,dim(age_survey.df)[2]))
  # names(age_survey) <- names(age_survey.df)
  # age_survey$year <- years
  # age_catch <- as.data.frame(matrix(-1, nyear,dim(age_catch.df)[2]))
  # names(age_catch) <- names(age_catch.df)
  # age_catch$year <- years
  # 
  # for (i in 1:dim(age_survey.df)[1]){
  #   idx <- which(age_survey$year == age_survey.df$year[i])
  #   age_survey[idx,] <-age_survey.df[i,]
  #   
  # }
  # 
  # for (i in 1:dim(age_catch.df)[1]){
  #   idx <- which(age_catch$year == age_catch.df$year[i])
  #   age_catch[idx,] <-age_catch.df[i,]
  #   
  # }
  
  # Load parameters from the assessment 
  
  initN <- rev(read.csv('data/Ninit_MLE.csv')[,1])
  Rdev <- read.csv('data/Rdev_MLE.csv')[,1]
  PSEL <- as.matrix(read.csv('data/p_MLE.csv'))
  #Fin <- assessment$F0
  
  # b <- matrix(NA, nyear)
  # Yr <- 1946:max(years)
  # # Parameters 
  # yb_1 <- 1965 #_last_early_yr_nobias_adj_in_MPD
  # yb_2 <- 1971 #_first_yr_fullbias_adj_in_MPD
  # yb_3 <- 2016 #_last_yr_fullbias_adj_in_MPD
  # yb_4 <- max(years) #_first_recent_yr_nobias_adj_in_MPD
  # b_max <- 0.87 #_max_bias_adj_in_MPD
  # 
  # b[1] <- 0
  # for(j in 2:length(Yr)){
  #   
  #   if (Yr[j] <= yb_1){
  #     b[j] = 0}
  #   
  #   if(Yr[j] > yb_1 & Yr[j]< yb_2){
  #     b[j] = b_max*((Yr[j]-yb_1)/(yb_2-yb_1));
  #   }
  #   
  #   if(Yr[j] >= yb_2 & Yr[j] <= yb_3){
  #     b[j] = b_max}
  #   
  #   if(Yr[j] > yb_3 & Yr[j] < yb_4){
  #     b[j] = b_max*(1-(yb_3-Yr[j])/(yb_4-yb_3))
  #   }
  #   
  #   if(Yr[j] >= yb_4){
  #     b[j] = 0
  #   }
  #   # if (b[j]<b[j-1]){
  #   #   stop('why')
  #   # }
  # }  
  # 
  #b <- matrix(1, tEnd)
  b <- as.matrix(read.csv('data/b_input.csv'))
 
  
  # if(move == TRUE){
  #    mul <- 1.015
  # }else{
  #  mul <- 1
  #  }

  # load parameters specifically for hake 
  parms.scalar <- read.csv('data/parms_scalar.csv')
  parms.sel <- read.csv('data/selectivity.csv')
  initN <-as.matrix(read.table('data/initN.csv'))
  
  Rdev <- as.matrix(read.csv('data/Rdev.csv'))
  
  
  if(sel_hist == 1){
  PSEL <- as.matrix(read.csv('data/PSEL.csv'))
  }else{
  PSEL <- matrix(0, 5, 28)
  }
  
  
  if(nseason == 4 & nspace == 2){
  Fnseason <- matrix(NA, 2,4)
  
  #Fnseason[1,] <- c(0.0,0.4,0.50,0.1) # Must add to one 
  Fnseason[1,] <- c(0.001,0.188,0.603,0.208)
  #Fnseason[2,] <- c(0.0,0.4,0.50,0.1) # Must add to onec
  Fnseason[2,] <- c(0.000,0.317,0.382,0.302)/sum(c(0.000,0.317,0.382,0.302)) # Divide by sum to sum to 1 
    
  }else{
    Fnseason <- matrix(NA, nspace, nseason)
    Fnseason[1:nspace,] <- 1/nseason # Equally distributed catch
    
    
  }


  rmul <-1
  
  if(nspace == 2){
    rmul <- 1.1
  }
  
  
  
  

  parms <- list( # Just start all the simluations with the same initial conditions 
       logRinit = parms.scalar$logRinit+log(rmul),
       logh = parms.scalar$logh,
       logMinit = parms.scalar$logMinit,
       logSDsurv = parms.scalar$logSDsurv,
       #logSDR = log(1.4),
       logphi_catch = parms.scalar$logphi_catch,
       #logphi_survey = log(11.33),
       # logSDF = log(0.1),
       # Selectivity parameters 
       psel_fish = parms.sel$value[parms.sel$source == 'fish'],
       psel_surv = parms.sel$value[parms.sel$source == 'survey'],
       initN = initN,
       Rin = Rdev,
       PSEL = PSEL
     )
     
     psel<- matrix(NA,nspace, 5) 
     
     for(i in 1:nspace){
       #psel[i,] <- c(2.8476, 0.973,0.3861,0.1775,0.5048) # USA selectivity 
       psel[i,] <- parms$psel_fish
       
     }
    
     if(nspace == 2){
     psel[1,] <- c(1,1,1,1,1)
     
     }
       
       
# Flag if there's a selectivity change in that year     
     selYear <- 1991
     
     flag_sel <- rep(0,nyear)
     flag_sel[which(years == selYear):which(years == myear)] <- 1
     
  df <-list(      #### Parameters #####
                  wage_ssb = t(wage_ssb),
                  wage_catch = t(wage_catch),
                  wage_survey = t(wage_survey),
                  wage_mid = t(wage_mid),
                  selidx = which(years == selYear),
                  #  Input parameters
                  year_sel = length(1991:max(years)), # Years to model time varying sel
                  
                  Msel = msel,
                  Matsel= as.numeric(mat),
                  nage = nage,
                  age = age,
                  nseason = nseason,
                  nyear = nyear,
                  tEnd = tEnd, # The extra year is to initialize 
                  logQ = log(1.14135),   # Analytical solution
                  # Selectivity 
                  Smin = 1,
                  Smin_survey = 2,
                  Smax = 6,
                  Smax_survey = 6,
                  flag_sel = flag_sel,
                  surveyseason = surveyseason,
                  nsurvey = nsurvey, # Frequency of survey years (e.g., 2 is every second year)
                  # survey
                  survey = survey, # Make sure the survey has the same length as the catch time series
                  survey_x = ac.data$survey_x, # Is there a survey in that year?
                  survey_err = ac.data$ss.error, # Make sure the survey has the same length as the catch time series
                  ss_survey = ac.data$ss.survey,
                  flag_survey =ac.data$sflag,
                  age_survey = age_survey.tmp,
                  age_maxage = 15, # Max age for age comps 
                  # Catch
                  #                Catchobs = catch$Fishery, # Convert to kg
                  ss_catch = ac.data$ss.catch,
                  flag_catch =ac.data$cflag,
                  age_catch = age_catch.tmp,
                  # variance parameters
                  logSDcatch = log(0.01),
                  logSDR = log(logSDR), # Fixed in stock assessment ,
                  logphi_survey = log(11.46),
                  years = years,
                  b = b,
                  bfuture = bfuture,
                  #logh = log(0.8),
                  # Space parameters 
                  smul = 0.5, # Annual survey timing 
                  sigma_psel = 1.4,
                  sum_zero = 0,
                  nspace = nspace,
                  #TAC = TAC,
                  movemat = movemat,
                  move = move,
                  recruitmat = recruitmat,
                  move.init = move.init,
                  movefifty = movefifty,
                  movemax = movemax,
                  movesouth = movesouth,
                  moveout = moveout,
                  moveslope = moveslope,
                 # F0 = Fin,
                  psel = psel,
                  parms = parms,
                  Fnseason = Fnseason,
                  selectivity_change = selectivity_change,
                  Catch = catch
                
                  # Parameters from the estimation model 
              
  )
  
 
  
  Catch.country <- read.csv('data/catch_per_country.csv')
  df$Catch.country <- as.matrix(Catch.country[,2:3])[,c(2,1)]
  
  df$Catch <- rowSums(df$Catch.country)
  
  if(nyear > length(df$Catch)){

    
    df$Catch <- c(df$Catch,rep(mean(df$Catch), nyear-length(df$Catch)))

    
  }
  
  if(nyear >nrow(df$Catch.country)){
    df$Catch.country <- rbind(df$Catch.country,t(replicate(nyear-nrow(Catch.country),colMeans(df$Catch.country))))
  }
  
  if(yr_future > 0){
    
    idx.future <- length(1966:myear)+seq(2,yr_future, by = df$nsurvey) # Years where survey occurs 
    
    df$survey_x <- c(df$survey_x,rep(-2, yr_future))
    df$survey_x[idx.future] <- 2
    
    df$survey_err <- c(df$survey_err,rep(1, yr_future))
    df$survey_err[idx.future] <- mean(df$survey_err[df$survey_err != 1])
    
    df$ss_survey <- c(df$ss_survey, rep(0,  yr_future))
    df$ss_survey[idx.future] <- mean(df$ss_survey[df$ss_survey != -1])
    df$flag_survey <- c(df$flag_survey, rep(-1,yr_future))
    df$flag_survey[idx.future] <- 1
    df$flag_catch[years > 2018] <- 1
    
    Rdevs <- rnorm(n = yr_future,mean = 0, sd = exp(df$logSDR))
    #Rdevs <- rep(0, yr_future)
    df$parms$Rin <- c(df$parms$Rin,Rdevs)
    
    # Bias adjustment 
    df$b <- c(df$b,rep(df$bfuture, yr_future))
  }
  
  return(df)
  
}
