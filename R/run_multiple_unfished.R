###### Run the HAKE MSE #######
#' Iterate the pacific hake MSE
#'
#' @param simyears number of years to simulate
#' @param seeds set the seed
#' @param TAC Which harvest control rule should the model use
#' @param df data frame of parameters
#' @param cincrease increase in max movement
#' @param mincrease decrease of spawners returning south
#' @param sel_change time varying selectivity
#'
#' @return
#' @export
#'
#' @examples
run_multiple_unfished <- function(simyears = NULL,seeds = 12345, TAC = 0, df = NA,
                              cincrease = 0, mincrease = 0,
                              sel_change = 0, runOM = TRUE){
  
  
  
  if(is.null(simyears)){
    print('Number of years to simulate not specified. Simulating 30 years into the future')
    simyears <- 30
  }

  
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
      
      df$Catch <- c(df$Catch, 0)
      
      ## Add a survey if catches are 0
   
      
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

    
    #parms <- getParameters(trueparms = TRUE, mod,df = df)
    parms <- df$parms
    ##  Create a data frame to send to runHakeassessment
    
    df.new <- create_TMB_data(sim.data, df)
    
    parms.new <- parms
    
    if(time == 1){
      F0 <- rowSums(sim.data$Fout)
      Rdev <- parms$Rin[1:(length(parms$Rin)-1)]
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
    
    
    Vreal <- sum(sim.data$N.save.age[,df$nyear,,df$nseason]*
                   matrix(rep(df$wage_catch[,df$nyear-1],df$nspace), ncol = df$nspace)*(sim.data$Fsel[,df$nyear,]))
    
    
    Nend <- sim.data$N.save[,ncol(sim.data$N.save)]
    Nend[1] <- 0 # Recruitment in current year is unknown
    
    opt.OM <- unlist(df$parms)
    Fin <- rowSums(sim.data$Fout)[df$nyear]
    SSBy <- sum(sim.data$SSB[df$nyear])
    names(opt.OM)[grep('psel_fish', names(opt.OM))] <- 'psel_fish'
    
    
    Fnew <- getRefpoint(opt.OM, df,SSBy = SSBy, Fin=Fin, Nend, TAC = TAC,
                        Vreal)
    Ntmp <- sim.data$Nout
    
    
    # Save some EM stuff in the last year
    SSB.save[[time]] <- rowSums(sim.data$SSB)
    R.save[[time]] <- rowSums(sim.data$R.save)
    F40.save[time] <- Fnew[[2]]
    Catch.save[[time]] <- sim.data$Catch
    
    ### Include the parameters needed to calculate SSB0
    parms.save[time, ] <- exp(opt.OM)[1:4]
    SSB.hes <- NA
    mconverge <- 1
    
      
    
    # year = df$years[time]
  }
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  #  Catch per country per year
  #Catch.year <- apply(sim.data$Catch.save.age, FUN = sum, MARGIN = c(2,3))
  
  Catch.year <- sim.data$Catch.save.age
  ## Calculate the average age
  source('R/calcMeanAge.R')
  
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
