###### Run the HAKE MSE ####### 
run_multiple_MSEs <- function(simyears = NULL,seeds = 12345, TAC = 1, df = NA,
                                      cincrease = 0, mincrease = 0,
                              sel_change = 0){
  
  #' @simyears number of years to simulate into the future 
  #' @seeds seed for rng calculation
  #' @TAC Harvest control rule. 1=40:10, 2=JMC adjustment, 3=realized catch adjustment
  #' @df data frame of parameters for the OM
  #' @cincrease change in maximum movement rate
  #' @mincrease change in spawners returning south
  #' @sel_change calculate selectivity 0=no selectivity change, 1= calc x years of selectivity, 2= continous selectivity calculation
  
  
  require(ggplot2)
  
  if(is.null(simyears)){
    print('Number of years to simulate not specified. Simulating 30 years into the future')
    simyears <- 30
  }
  
  
  #   if(is.na(moveparms[1])){
  # df <- load_data_seasons(move = TRUE,
  #                         nseason = 4, nspace = 2)
  #   }else{
  #     if(length(moveparms) != 2){
  #       stop('Wrong number of movement parameters')
  #     }
  #     df <- load_data_seasons(move =TRUE,
  #                             nseason = 4, nspace = 2,movemaxinit = moveparms[1],movefiftyinit = moveparms[2])
  #   }
  
  time <- 1
  yrinit <- df$nyear
  
  year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
  N0 <- NA
  sim.data <- run.agebased.true.catch(df,seeds)
  simdata0 <- sim.data # The other one is gonna get overwritten. 
  
  F40.save<- array(NA,simyears)
  
  # Save some stuff
  SSB.save <- list()
  R.save <- list()
  Catch.save <- list()
  S.year.future <- seq(2019,2019+simyears, by = df$nsurvey)
  # Save som OM stuff 
  
  
  # Save the estimated parameters from the EM (exclude time varying) 
  parms.save <- array(NA, dim = c(simyears, 4))
  
  # Before the MSE starts 
  F0.save <- df$fmort
  years <- df$years
  model.save <- list()
  
  ## 
  
  SSB.test.om <- list() # Test if SSB is the same in the OM
  start.time <- Sys.time()
  df0 <- df # For debugging
  #mconverge <- rep(NA, simyears)
  df0 <- df
  
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
      
      df$wage_catch <- cbind(df.new$wage_catch,df.new$wage_catch[,1])
      df$wage_survey <- cbind(df.new$wage_survey,df.new$wage_survey[,1])
      df$wage_mid <- cbind(df.new$wage_mid,df.new$wage_mid[,1])
      df$wage_ssb <- cbind(df.new$wage_ssb,df.new$wage_ssb[,1])
      
      df$Catch <- c(df$Catch, Fnew[[1]])
      
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
      
      if(nseason == 4){ # For the standard model
        movenew[,1:2,] <- 0 # Recruits and 1 year olds don't move
        
        movenew[1,4:nage,1:3] <- df$movesouth # Don't move south during the year
        movenew[1,3:nage,nseason] <- df$moveout
        movenew[2,3:nage,nseason] <- df$movesouth
      }
      
      move.tmp[,,,df$nyear] <- movenew
      df$movemat <- move.tmp
      #df$years <- c(df$years,df$years[length(df$years)]+1)
      
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
      
      #print(df$moveout)
      sim.data <- run.agebased.true.catch(df, seeds)
      
    }
    
    # PSEL <- matrix(0,5, length(1991:years[length(years)]))
    # initN <- rep(0,df$nage-1)
    # F0 <- rep(0.01, df$nyear)
    # Rdev <- rep(0, df$nyear-1) # Can't predict last year
    # 
    # parms <- list( # Just start all the simluations with the same initial conditions
    #   logRinit = 15,
    #   logh = log(0.5),
    #   logMinit = log(0.3),
    #   logSDsurv = log(0.3),
    #   logphi_catch = log(0.8276),
    #   logphi_survey = log(11.33),
    #   # Selectivity parameters
    #   psel_fish = c(2.486490, 0.928255,0.392144,0.214365,0.475473),
    #   psel_surv = c(0.568618,-0.216172,0.305286 ,0.373829),
    #   initN = initN,
    #   Rin = Rdev,
    #   F0 = F0,
    #   PSEL = PSEL
    # )
    # 
    
    parms <- getParameters_OM(trueparms = TRUE, df = df)
    
    ##  Create a data frame to send to runHakeassessment 
    
    df.new <- create_TMB_data(sim.data, df)
    
    parms.new <- parms
    
    if(time == 1){
      F0 <- rowSums(sim.data$Fout)
      Rdev <- parms$Rin
    }else{
      F0 <- c(F0,0.2)
      Rdev <- c(Rdev, 0)
    }
    
    
    if(df$Catch[df$nyear] == 0){
      F0[length(F0)] <- 0
    }
    
    parms.new$F0 <- F0#rowSums(sim.data$Fsave, na.rm = TRUE)
    #parms.new$F0[df$Catch == 0] <- 0
    
    parms.new$Rin <- Rdev#parms.new$Rin[1:(length(parms.new$Rin)-1)]
    
    
    
    obj <-MakeADFun(df.new,parms.new,DLL="runHakeassessment", silent = TRUE) # Run the assessment 
    
    reps <- obj$report()
    
    lower <- obj$par-Inf
    upper <- obj$par+Inf
    
    #lower[names(lower) == 'logSDsurv'] <- 0.05
    
    upper <- obj$par+Inf
    #upper[names(upper) == 'psel_fish' ] <- 5
    #upper[names(upper) == 'PSEL'] <- 5
    upper[names(upper) == 'logh'] <- log(0.999)
    upper[names(upper) == 'F0'] <- 2
    #upper[names(upper) == "logphi_survey"]<- log(8)
    lower[names(lower) == 'logSDsurv'] <- log(0.01)
    lower[names(lower) == 'F0'] <- 0.01
    #lower[names(lower) == 'logMinit'] <- log(0.2)
    if(df$Catch[length(df$Catch)] == 1){
      lower[names(lower) == 'F0'] <- 1e-10
    }
    
    
    # if(df$Catch[df$nyear] == 0){
    #   lower[names(lower) =='F0'][which(df$Catch ==0)] <- 0
    #   upper[names(upper) =='F0'][which(df$Catch ==0)] <- 0
    # }
    # 
    
    system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper,
                            control = list(iter.max = 1e6, 
                                           eval.max = 1e6))) # If error one of the random effects is unused
    
    
    # if(opt$convergence != 0){
    #   print(paste('year',df$years[length(df$years)], 'has convergence issues, double checking parameters'))
    #   
    #   xx<- Check_Identifiable_vs2(obj)
    #   
    #   # if(xx[[1]] == 'model not converging'){
    #   #   mconverge[time] <- 1
    #   #   
    #   #   if(sum(mconverge, na.rm = TRUE)>1){
    #   #     print('Convergence is bad. Stopping simulation')
    #   #     return(df.ret= NA)
    #   #   }
    #   #   
    #   # }else{
    #   #   mconverge[time] <- 0
    #   # }
    # }else{
    #   mconverge[time] <- 0
    # }
    
    reps <- obj$report()
    
    SSB <- reps$SSB
    Fyear <- reps$Fyear
    N <- reps$N_beg
    Catch <- reps$Catch
    R <- reps$R
    
    # plot(sim.data$Catch)
    # lines(df$Catch)
    # 
    # plot(reps$SSB)
    # lines(rowSums(sim.data$SSB), col = 'red')
    # # # #Uncertainty
    
    
    if(time == simyears){
      rep <- sdreport(obj)
      sdrep <- summary(rep)
      rep.values<-rownames(sdrep)
      nyear <- df$tEnd
      
      # Check convergence in last year 
      source('Check_Identifiable_vs2.R')
      conv <- Check_Identifiable_vs2(obj)
      
      if(length(conv$WhichBad) == 0){
        mconverge <- 1
      }else{
        mconverge <- 0
      }
      
      
      R <- data.frame(name = sdrep[rep.values == 'R',1])
      
      SSB <- data.frame(name = sdrep[rep.values == 'SSB',1])
      SSB$SE <- sdrep[rep.values == 'SSB',2]
      SSB$min <- SSB$name-2*SSB$SE
      SSB$max <- SSB$name+2*SSB$SE
      SSB$year <- df$years
      
      if(is.na(df$move)){
        p1 <- plotValues(SSB, data.frame(x=df$years,y = sim.data$SSB), 'SSB')  
      }else{
        p1 <- plotValues(SSB, data.frame(x=df$years,y = rowSums(sim.data$SSB)), 'SSB')
      }
      
      print(p1)
      SSB.hes <- SSB
      SSB <- SSB$name
      
    }
    
    
    Vreal <- sum(sim.data$N.save.age[,df$nyear,,df$nseason]*
                   matrix(rep(df$wage_catch[,df$nyear-1],df$nspace), ncol = df$nspace)*(sim.data$Fsel[,df$nyear,]))
    
    Nend <- N[,dim(N)[2]]
    Fnew <- getRefpoint(opt$par, df,SSBy = SSB[length(SSB)], Fin=Fyear[length(Fyear)], Nend, TAC = TAC,
                        Vreal)
    #Fnew <- 0.3
    # Update the data data frame
    # if(Fnew[[1]] == 1){
    #   stop('fishery closed')
    # }
    # 
    Ntmp <- sim.data$Nout
    
    
    # Save some EM stuff in the last year 
    SSB.save[[time]] <- SSB
    R.save[[time]] <- N[1,]
    F40.save[time] <- Fnew[[2]]
    Catch.save[[time]] <- Catch
    
    # And the fishing mortality
    #F0.save <- Fnew
    
    #  print(year.future[year])
    #SSB.test.om[[time]] <- rowSums(sim.data$SSB)
    
    ### Include the parameters needed to calculate SSB0 
    parms.save[time, ] <- exp(opt$par)[1:4]
    
    # year = df$years[time]
    
    
  }
  
  nms <- unlist(strsplit(names(opt$par), split = 'log')[1:4])[c(2,4,6,8)]
  names(parms.save) <- nms
  
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  #  Catch per country per year 
  #Catch.year <- apply(sim.data$Catch.save.age, FUN = sum, MARGIN = c(2,3))
  
  Catch.year <- sim.data$Catch.save.age
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
  
  df.ret <- list(Catch = Catch.year, 
                 Catch.quota = sim.data$Catch.quota,# All output is from the OM 
                 SSB = sim.data$SSB, 
                 SSB.mid = sim.data$SSB.all[,,3],
                 SSB.hes = SSB.hes,
                 Survey.om = sim.data$survey,
                 F0 = apply(sim.data$Fout,c(1,3),sum),
                 parms = parms.save,
                 N = sim.data$N.save.age,
                 converge = mconverge,
                 ams = ams,
                 amc = amc,
                 V = sim.data$V.save
  )
  
  return(df.ret)
}