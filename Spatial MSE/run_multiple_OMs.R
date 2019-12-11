###### Run the HAKE MSE ####### 
run_multiple_OMs <- function(simyears = 30,seed = 12345, df, Catchin, 
                             cincrease = 0, mincrease = 0,
                             sel_change = 0){
  
  if(is.null(simyears)){
    print('Number of years to simulate not specified. Simulating 30 years into the future')
    simyears <- 30
  }
  
  
  time <- 1
  yrinit <- df$nyear
  
  year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
  N0 <- NA
  sim.data <- run.agebased.true.catch(df,seed)
  simdata0 <- sim.data # The other one is gonna get overwritten. 
  
  # 
  # save(sim.data,file = 'simulated_space_OM.Rdata')
  # save(df,file = 'sim_data_parms.Rdata')
  
  S.year.future <- seq(2019,2019+simyears, by = 2)
  # Save som OM stuff 
  # SSB.save.om <- array(NA, df$nyear+simyears)
  # R.save.om <- array(NA, df$nyear+simyears)
  # Catch.save.om <- array(NA, df$nyear+simyears)
  
  # Save the estimated parameters from the EM (exclude time varying) 
  parms.save <- array(NA, dim = c(simyears, 4))
  
  # Before the MSE starts 
  # SSB.save.om[1:df$tEnd] <- sim.data$SSB
  # R.save.om[1:df$nyear] <- sim.data$N.save[1,]
  # Catch.save.om[1:df$nyear] <- sim.data$Catch
  F0.save <- df$fmort
  years <- df$years
  model.save <- list()
  
  ## 
  if(length(Catchin) == 1){
    Catchin <- as.matrix(rep(Catchin, simyears-1))
  }
  
  for (time in 1:(simyears-1)){
    year <- yrinit+(time-1)
    #print(year.future[year])
    
    
    if (time > 1){
      
      if(sum(year.future[year] == S.year.future)>0){
        df$flag_survey <- c(df$flag_survey,1)
        df$survey_x <- c(df$survey_x,2)
        # df$ss_catch <- c(df$ss_catch,ceiling(mean(df$ss_catch[df$ss_catch > 0])))
        df$ss_survey <- c(df$ss_survey,ceiling(mean(df$ss_survey[df$ss_survey > 0])))
        df$survey_err <- c(df$survey_err,mean(df$survey_err[df$survey_err < 1]))
        
      }else{
        df$flag_survey <- c(df$flag_survey,-1)
        df$survey_x <- c(df$survey_x,-2)
        df$ss_survey <- c(df$ss_survey,-1)
        df$survey_err <- c(df$survey_err,1)
      }
      
      df$ss_catch <- c(df$ss_catch,ceiling(mean(df$ss_catch[df$ss_catch > 0])))
      df$flag_catch <- c(df$flag_catch,1)
      df$years <- year.future[1:year]
      df$nyear <- length(df$years)
      #df$tEnd <- df$tEnd+1 # Just run one more year in subsequent runs
      
      df$wage_catch <- cbind(df.new$wage_catch,df.new$wage_catch[,1])
      df$wage_survey <- cbind(df.new$wage_survey,df.new$wage_survey[,1])
      df$wage_mid <- cbind(df.new$wage_mid,df.new$wage_mid[,1])
      df$wage_ssb <- cbind(df.new$wage_ssb,df.new$wage_ssb[,1])
      
      if(ncol(Catchin) == 1){
      df$Catch <- c(df$Catch, Catchin[time])
      }else{
      df$Catch.country <- rbind(df$Catch.country, Catchin[time,])  
      df$Catch <- c(df$Catch,sum(Catchin[time,]))
      }
      ## Add a survey if catches are 0 
      if(df$Catch[time] == 0 & df$flag_survey[time] == -1){
        print('Stock in peril! Conducting emergency survey')        
        
        
        df$flag_survey[df$Catch == 0] <- 1
        df$ss_survey[df$Catch == 0] <- ceiling(mean(df$ss_survey[df$ss_survey > 0]))+200 # emergency survey adds more 200 age samples! 
        df$survey_x[df$Catch == 0] <- 2
        df$survey_err[df$Catch == 0] <- mean(df$survey_err[df$survey_err < 1])
      }
      
      df$b[length(df$b)] <- df$bfuture
      df$b <- c(df$b,df$bfuture)
      Rdevs <- rnorm(n = 1,mean = 0, sd = exp(df$logSDR))
      #Rdevs <- rep(0, yr.future)
      df$parms$Rin <- c(df$parms$Rin,Rdevs)
      
      
      ### Add movement to the new years
      move.tmp <- array(0, dim = c(df$nspace,df$nage, df$nseason, df$nyear))
      move.tmp[,,,1:df$nyear-1] <- df$movemat
      
      # Add increasing movement due to climate
      nspace <- df$nspace
      nage <- df$nage
      nseason <- df$nseason
      age <- df$age
      
      
      movenew <-array(0, dim = c(nspace, nage, nseason)) # Chances of moving in to the other grid cell 
      movemax <- df$movemax
      movefifty <- df$movefifty
      
      if(time == 2){
        movemaxtmp <- (movemax[1]+cincrease)
        df$moveout <- df$moveout-mincrease
      }else{
        movemaxtmp <- movemaxtmp+cincrease
        df$moveout <- df$moveout-mincrease
        
      }
      
      if(movemaxtmp >0.9){
        movemaxtmp <- 0.9 # Not moving more than 90% out 
        
        # (stop(paste(time,'at max movement')))
      }
      
      if(df$moveout <= 0.5){
        df$moveout <- 0.5
      }
      
      for(j in 1:nspace){
        for(i in 1:nseason){
          movenew[j,,i] <- movemaxtmp/(1+exp(-df$moveslope*(age-movefifty)))
        }
      }
      
      if(nseason == df$nseason){ # For the standard model
        movenew[,1:2,] <- 0 # Recruits and 1 year olds don't move
        
        movenew[1,4:nage,1:3] <- df$movesouth # Don't move south during the year
        movenew[1,3:nage,nseason] <- df$moveout
        movenew[2,3:nage,nseason] <- df$movesouth
      }
      
      move.tmp[,,,df$nyear] <- movenew
      df$movemat <- move.tmp
      #df$years <- c(df$years,df$years[length(df$years)]+1)
      sel_change <- df$selectivity_change
      # Fix the selectivity 
      if(sel_change == 0){
        df$flag_sel <- c(df$flag_sel,0)
      }
      if(sel_change == 1){
        flag.tmp <- c(df$flag_sel,1)
        idx <- which.min(df$flag_sel[df$flag_sel == 1])
        flag.tmp[idx] <- 0
        df$flag_sel <- flag.tmp
        df$selidx <- df$selidx+1
        
      }
      if(sel_change == 2){
        df$flag_sel <- c(df$flag_sel,1)
        df$parms$PSEL <- rbind(df$parms$PSEL,rep(0,nrow(df$parms$PSEL)))
      }
      
      sim.data <- run.agebased.true.catch(df, seed)
      
      
      
      ### Save some data 
    }
    df.new <- create_TMB_data(sim.data, df)
    
   
  }
  

  
  df.ret <- list(SSB = sim.data$SSB.all[,,3],
                 ams = sim.data$age_comps_country,
                 amc = sim.data$age_comps_catch_space
  )
  
  
  
  return(df.ret)
}