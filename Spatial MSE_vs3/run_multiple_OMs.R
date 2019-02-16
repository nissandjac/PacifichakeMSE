###### Run the HAKE MSE ####### 
run_multiple_OMs <- function(simyears = 30,seed = 12345,moveparms = NA, Catchin){
  
  if(is.null(simyears)){
    print('Number of years to simulate not specified. Simulating 30 years into the future')
    simyears <- 30
  }
  
  #   if(is.na(moveparms[1])){
  # df <- load_data_seasons(move = FALSE,
  #                         nseason = 1, nspace = 1)
  #   }else{
  #     if(length(moveparms) != 2){
  #       stop('Wrong number of movement parameters')
  #     }
  # df <- load_data_seasons_move(move = TRUE,movemaxinit = moveparms[1], moveparms[2])
  #   }
  df <- load_data_seasons(move =TRUE,
                          nseason = 4, nspace = 2, movemaxinit = moveparms[1], movefiftyinit = moveparms[2])
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
      df$wage_catch <- df.new$wage_catch
      df$wage_survey <- df.new$wage_survey
      df$wage_mid <- df.new$wage_mid
      df$wage_ssb <- df.new$wage_ssb
      df$Catch <- c(df$Catch, Catchin[time])
      df$b[length(df$b)] <- 0.870
      df$b <- c(df$b,0.87)
      Rdevs <- rnorm(n = 1,mean = 0, sd = exp(df$logSDR))
      #Rdevs <- rep(0, yr.future)
      df$parms$Rin <- c(df$parms$Rin,Rdevs)
      #df$years <- c(df$years,df$years[length(df$years)]+1)
      
      
      sim.data <- run.agebased.true.catch(df, seed)
      
      
      
      ### Save some data 
    }
    df.new <- create_TMB_data(sim.data, df)
    
   
  }
  

  
  df.ret <- list(SSB = sim.data$SSB.all[,3,],
                 ams = sim.data$age_comps_country,
                 amc = sim.data$age_comps_catch_space
  )
  
  return(df.ret)
}