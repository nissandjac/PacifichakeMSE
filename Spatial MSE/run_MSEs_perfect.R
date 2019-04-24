###### Run the HAKE MSE ####### 
run_MSEs_perfect <- function(simyears = NULL,seed = 12345,
                              moveparms = NA, TAC = 1){
  require(ggplot2)
  
  if(is.null(simyears)){
    print('Number of years to simulate not specified. Simulating 30 years into the future')
    simyears <- 30
  }
  
  if(is.na(moveparms[1])){
    df <- load_data_seasons(move = TRUE,
                            nseason = 4, nspace = 2)
  }else{
    if(length(moveparms) != 2){
      stop('Wrong number of movement parameters')
    }
    df <- load_data_seasons(move =TRUE,
                            nseason = 4, nspace = 2,movemaxinit = moveparms[1],movefiftyinit = moveparms[2])
  }
  
  df$Catch <- Catch.obs$Fishery
  time <- 1
  yrinit <- df$nyear
  
  year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
  N0 <- NA
  sim.data <- run.agebased.true.catch(df,seed)
  simdata0 <- sim.data # The other one is gonna get overwritten. 
  
  # 
  # save(sim.data,file = 'simulated_space_OM.Rdata')
  # save(df,file = 'sim_data_parms.Rdata')
  
  F40.save<- array(NA,simyears)
  
  # Save some stuff
  SSB.save <- list()
  R.save <- list()
  Catch.save <- list()
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
  
  SSB.test.om <- list() # Test if SSB is the same in the OM
  start.time <- Sys.time()
  
  for (time in 1:simyears){
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
      df$wage_catch <- df.new$wage_catch
      df$wage_survey <- df.new$wage_survey
      df$wage_mid <- df.new$wage_mid
      df$wage_ssb <- df.new$wage_ssb
      df$Catch <- c(df$Catch, Fnew[[1]])
      df$b[length(df$b)] <- 0.870
      df$b <- c(df$b,0.87)
      Rdevs <- rnorm(n = 1,mean = 0, sd = exp(df$logSDR))
      #Rdevs <- rep(0, yr.future)
      df$parms$Rin <- c(df$parms$Rin,Rdevs)
      #df$years <- c(df$years,df$years[length(df$years)]+1)
      
      
      sim.data <- run.agebased.true.catch(df, seed)
      
    }
    df.new <- create_TMB_data(sim.data, df)
    
    Nend <- sim.data$N.save[,df$nyear]
    Fyear <- -log(1-sim.data$Catch[df$nyear]/sum(sim.data$N.save[,df$nyear]*df.new$wage_catch[,df$nyear]))
    SSB <- rowSums(sim.data$SSB)
    
    par.fixed <- df$parms
    nms <- names(df$parms)
    par.fixed <- c(df$parms$logRinit, df$parms$logh,df$parms$logMinit,df$parms$psel_fish)
    names(par.fixed) <- c(nms[1:3],rep(nms[7],5))
    
  
    
    Fnew <- getRefpoint(par.fixed, df,SSBy = SSB[length(SSB)], Fin=Fyear, Nend, TAC = TAC)
    #Fnew <- 0.3
    #print(paste('new quota = ',Fnew[[1]]))
    # Update the data data frame
    
    Ntmp <- sim.data$Nout
    
    
    # Save some EM stuff in the last year 
    SSB.save[[time]] <- SSB
    R.save[[time]] <- sim.data$N.save[1,]
    F40.save[time] <- Fnew[[2]]
    Catch.save[[time]] <- sim.data$Catch
    
    # And the fishing mortality
    F0.save <- Fnew
    
    #  print(year.future[year])
    #SSB.test.om[[time]] <- rowSums(sim.data$SSB)
    
    ### Include the parameters needed to calculate SSB0 
    
    
    
  }
  
  
  ## Calculate the average age 
  source('calcMeanAge.R')
  #dev.off()
  amc <- data.frame(year = year.future[1:year], 
                    amc.can = calcMeanAge(sim.data$age_comps_catch_space[,,1], df$age_maxage),
                    amc.US  = calcMeanAge(sim.data$age_comps_catch_space[,,2], df$age_maxage),
                    amc.tot = calcMeanAge(sim.data$age_catch, df$age_maxage))
  
  ams <- data.frame(year = year.future[1:year], 
                    ams.can = calcMeanAge(sim.data$age_comps_country[,,1], df$age_maxage),
                    ams.US  = calcMeanAge(sim.data$age_comps_country[,,2], df$age_maxage),
                    ams.tot = calcMeanAge(sim.data$age_comps_surv, df$age_maxage))
  
  df.ret <- list(Catch = sim.data$Catch,  # All output is from the OM 
                 SSB = sim.data$SSB, 
                 SSB.mid = sim.data$SSB.all[,3,],
                 Survey.om = sim.data$survey,
                 F0 = F0.save,
                 parms = parms.save,
                 ams = ams,
                 amc = amc
  )
  
  return(df.ret)
}
