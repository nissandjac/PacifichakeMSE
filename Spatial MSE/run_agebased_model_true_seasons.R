# Run an agebased model based on the parameters from the estimation model

run.agebased.true_seasons <- function(df, seed = 100){
  # Constant parameters
  # Initialize the stock by age and sex
  
  # rSums <- function(x){
  #   if(df$>)
  #   
  # }
  # 
  
  
  set.seed(seed)
  nseason <- df$nseason
  
  df$tEnd <- length(df$years)*nseason
  nyear <-df$tEnd/df$nseason # 
  year <- df$years
  tEnd <- nyear*nseason
  # True values 
  M0 <- exp(df$parms$logMinit) # no difference between males and females
  F0 <- df$F0
  
  # Set up the data spatially 
  nspace <- df$nspace
  recruitmat <- df$recruitmat
  movemat <- df$movemat
  move.init <- df$move.init
  # M selectivity 
  Msel <- df$Msel # no difference between males and females
  M0 <- exp(df$parms$logMinit)
  M <- M0*Msel # Naural mortality at age
  SDR <- 1.4
  b <- rep(1, nyear)
  # Survey selectivity 
  surv.sel <- getSelec(df$age,df$parms$psel_surv, df$Smin_survey, df$Smax_survey) # Constant over time
  
  # Catchability 
  q <- exp(df$logQ) # Constant over time
  surv.sd <- exp(df$parms$logSDsurv) # Survey error
  
  # Maturity 
  Mat.sel <- df$Matsel # Fecundity
  h <- exp(df$parms$logh)
  
  # Age 
  nage <- df$nage
  age <- df$age
  
  R0 <- exp(df$parms$logRinit)
  
  Mage <- cumsum(M)
  
  # Calculate N0 based on R0
  mage <- max(df$age) # Max age
  agetmp <- 0:(mage*3)
  nagetmp <- length(agetmp)
  
  N0tmp <- rep(NA,nagetmp)
  
  N0tmp[1:(nagetmp-1)] = R0*exp(-agetmp[1:(nagetmp-1)]*M0)
  N0tmp[nagetmp] =  N0tmp[nagetmp-1]*exp(-agetmp[nagetmp-1]*M0)/(1-exp(-M0))
  
  N0 <- matrix(NA,nage)
  N0[1:(nage-1)] <- N0tmp[1:(nage-1)]
  N0[nage] <- sum(N0tmp[nage:nagetmp])
  
  SSB_0 <- rowSums(matrix(rep(N0,each =nspace),nrow = nspace)*matrix(rep(df$Matsel,each =nspace),nrow = nspace)*move.init)*0.5
  R_0 <- R0*move.init[,1]
  # Used the inital recruitment devs to get a start
  
  
  Ninit <- rep(NA,nage)
  Ninit_dev <- rev(df$parms$initN)
  
  Ninit[2:(nage-1)] <-R0 * exp(-Mage[1:(nage-2)])*exp(-0.5*SDR^2*1+Ninit_dev[1:(nage-2)])
  Ninit[nage] <- R0*exp(-(Mage[nage-1]))/(1-exp(-M[nage]))*exp(-0.5*SDR^2*1+Ninit_dev[nage-1])# Plus group (ignore recruitment dev's in first year )
  
  # Create containers to save the data
  SSB_init <- rowSums(matrix(rep(Ninit,each =nspace),nrow = nspace)*matrix(rep(df$Matsel,each =nspace),nrow = nspace)*move.init, na.rm = T)*0.5
  Ninit[1] <- sum((4*h*R_0*SSB_init/(SSB_0*(1-h)+ SSB_init*(5*h-1)))*exp(-0.5*0*SDR^2+df$parms$Rin[1]))
  
  
  SSB <- matrix(NA,nyear+1, nspace)
  Biomass.save <- matrix(NA,nyear+1, nspace)
  Catch <- matrix(NA,nyear+1)
  Catch.age <- matrix(NA,nage,nyear+1)
  R.save <- matrix(NA,nyear+1, nspace)
  
  N.save.age <- array(NA,dim = c(nage,nyear+1, nspace, nseason))
  
  survey <- array(NA,dim = c(nyear))
  
  age_comps_surv <- array(NA, dim = c(df$age_maxage,nyear)) # Fix the max ages later
  age_comps_catch <- array(NA, dim = c(df$age_maxage,nyear))
  
  
  # Distribute over space 
  for (space in 1:nspace){
    for (season in 1:nseason){
      if (season == 1){
      N.save.age[,1,space,season] <- Ninit*move.init[space,] # Just to initialize 
      }else{
      N.save.age[,1,space,season] <- N.save.age[,1,space,season-1]*exp(-M/nseason)
      }
    }
    SSB[1,space] <- sum(N.save.age[,1,space,4]*Mat.sel, na.rm = T)*0.5
    
  }
  
  
  Catch.age[,1] <- 0 # Assumed no fishing before data started 
  Catch[1]<- 0
  
  survey[1] <- 1 # Surveys start later
  
  idx.save <- seq(1,tEnd, by = nseason)
  
  for (yr in 1:(nyear)){ # Loop over years add one year for initial distribution
    
    if(year[yr] < year[df$selYear] | year[yr] > 2017){
      psel <- df$parms$psel_fish
    }else{
      psel <- df$parms$psel_fish+df$parms$PSEL[,yr-df$selYear+1]
    }
    
    Fsel <- getSelec(age,psel,df$Smin,df$Smax) # Constant over space right now 
    
    if(year[yr] < 2018){
      w_catch <- df$wage_catch[,yr]
      w_surv <- df$wage_survey[,yr]
    }else{
      w_catch <- df$wage_catch[,1]
      w_surv <- df$wage_survey[,1]
    }
    if (year[yr] < 2018){
      Ry <- df$parms$Rin[yr]
    }else{
      Ry <- rnorm(1,mean = 0, sd = SDR)
    }
    
    
    # for(ssn in 1:nseason){
    
    idx <- yr+1 # Use this index to save beyond the initial condition
    #idx.run <- yr+season
    # print(idx)
    # Calculate the mortality at length
    Fyear <- F0[yr]*Fsel
    Myear <- M # Natural mortality 
    
    for (season in 1:nseason){

      
      Z <- (Fyear+Myear)/nseason
      # Assume that recruitment is in the beginning of the year (bc why not)
      # Recruitment
      
      
      for (space in 1:nspace){
        
        # Get the indices for the surrounding spaces
        if(((space-1) == 0)){
          spaceidx <- 2
        }
        if(space == nspace){
          spaceidx <- nspace-1
        }
        if(space > 1 & space < nspace){
          spaceidx <- c(space-1,space+1)
        }
        
        if (season == 1){
        
        # Recruitment only in season 1  
        R <- ((4*h*R_0[space]*SSB[idx-1,space])/
                (SSB_0[space]*(1-h)+ SSB[idx-1,space]*(5*h-1)))*exp(-0.5*1*SDR^2+Ry)*recruitmat[space]
        N.save.age[1,idx,space,season] <- R
        
        
        N.save.age[2:(nage-1),idx,space,season] <- N.save.age[1:(nage-2), idx-1,space, nseason]*exp(-Z[1:(nage-2)])*(1-movemat[space,1:(nage-2),season])-
          N.save.age[1:(nage-2), idx-1,space,nseason]*exp(-Z[1:(nage-2)])*(movemat[space,1:(nage-2),season])+ # Remove the ones that leave``
          rowSums(N.save.age[1:(nage-2), idx-1,spaceidx,nseason]*exp(-Z[1:(nage-2)])*t(movemat[spaceidx,1:(nage-2),season]))# add the ones come to the surrounding areas
        
        N.save.age[nage, idx,space,season] <-  (N.save.age[nage-1, idx-1,space, nseason]*exp(-Z[nage-1])+N.save.age[nage, idx-1,space, nseason]*exp(-Z[nage]))*(1-movemat[space,nage, season])-
          (N.save.age[nage-1, idx-1,space,nseason]*exp(-Z[nage-1])+N.save.age[nage, idx-1,space, nseason]*exp(-Z[nage]))*(movemat[spaceidx,nage, season])+# Plus group
          sum((N.save.age[nage-1, idx-1,spaceidx,nseason]*exp(-Z[nage-1])+N.save.age[nage, idx-1,spaceidx,nseason]*exp(-Z[nage]))*(movemat[spaceidx,nage, season]))# Plus group
        
        }else{
        N.save.age[,idx,space,season] <- N.save.age[,idx,space,season-1]*exp(-Z)-
          N.save.age[, idx,space,season-1]*exp(-Z)*(movemat[space,,season])+ # Remove the ones that leave
          N.save.age[, idx,spaceidx,season-1]*exp(-Z)*(movemat[spaceidx,,season])# add the ones come to the surrounding areas
        }
        
        if (season==nseason){
        SSB[idx,space] <- sum(N.save.age[,idx,space, nseason]*Mat.sel)*0.5
        if(is.na(SSB[idx,space])){
          stop('SSB is NA')
        }
      }
        
      }
      
    }
    

  
    
    Catch.age[,idx]  <- (Fyear/(Fyear+Myear))*(1-exp(-(Fyear+Myear)))*rowSums(N.save.age[,idx,,1])*w_catch # Calculate the catch in kg 
    Catch[idx] <- sum(Catch.age[,idx]) 
      #  }
      # Save the survey 
      # Survey is conducted in the start of the year
      if (df$flag_survey[yr] == 1){
        err <- exp(rnorm(n = 1,mean = 0, sd = surv.sd))
        surv <- sum(rowSums(N.save.age[,idx,,1])*surv.sel*q*w_surv)*err # If the xtra factor is not included the mean is > 1
        survey[yr] <- surv
      }else{
        survey[yr] <- 1
      }
      Ntot.year <- rowSums(N.save.age[,idx,,1])
      
      surv.tot <- sum(Ntot.year*surv.sel*q)
      
      if(df$flag_survey[yr] == 1){
        age_comps_surv[1,yr] <- 0 # No year 1 recorded 
        
        age_comps_surv[1:(df$age_maxage-1),yr] <-  (Ntot.year[2:(df$age_maxage)]*surv.sel[2:(df$age_maxage)]*q)/surv.tot
        age_comps_surv[df$age_maxage,yr] <- sum(Ntot.year[(df$age_maxage+1):nage]*surv.sel[(df$age_maxage+1):nage]*q)/surv.tot
      }else{
        age_comps_surv[,yr] <- NA
      }
      #   
      if(df$flag_catch[yr] == 1){
        age_comps_catch[1:(df$age_maxage-1),yr] <-  Catch.age[2:(df$age_maxage),yr]/Catch[yr]
        age_comps_catch[df$age_maxage,yr] <- sum(Catch.age[(df$age_maxage+1):nage,yr]/Catch[yr])
      }else{
        age_comps_catch[,yr] <- NA
      }
      

    
  }
  
    Nsave <- apply(N.save.age[,,,1],2,rowSums)
    SSB.save <- rowSums(SSB)
    
    
    df.out   <- list(N.save = Nsave[,2:(nyear+1)], SSB = SSB[2:(nyear+1),], 
                     Catch = Catch[2:(nyear+1)], Catch.age = Catch.age[,2:(nyear+1)], 
                     survey = survey, age_comps_catch = age_comps_catch, age_comps_surv= age_comps_surv, Nout = N.save.age[,nyear+1,,1],
                     SSB_0 = SSB_0, Nsave.all = N.save.age)
    
  return(df.out)
}

