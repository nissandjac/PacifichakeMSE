# Run an agebased model based on the parameters from the estimation model

run.agebased.true <- function(df, N0 = NA){
  # Constant parameters
  # Initialize the stock by age and sex
  
  nyear <-df$tEnd # Add one year for the initial conditions
  year <- df$years
  # True values 
  M0 <- exp(df$parms$logMinit) # no difference between males and females
  F0 <- c(0,df$parms$F0)
  # M selectivity 
  Msel <- df$Msel # no difference between males and females
  
  M <- M0*Msel # Naural mortality at age
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
  if (all(is.na(N0))){ # Only do this if not extending a current run 
    mage <- max(df$age) # Max age
    agetmp <- 0:(mage*3)
    nagetmp <- length(agetmp)
    
    N0tmp <- rep(NA,nagetmp)
    
    N0tmp[1:(nagetmp-1)] = R0*exp(-agetmp[1:(nagetmp-1)]*M0)
    N0tmp[nagetmp] =  N0tmp[nagetmp-1]*exp(-agetmp[nagetmp-1]*M0)/(1-exp(-M0))
    
    N0 <- matrix(NA,nage)
    N0[1:(nage-1)] <- N0tmp[1:(nage-1)]
    N0[nage] <- sum(N0tmp[nage:nagetmp])
  }  
  SSB_0 <- sum(N0*df$Matsel)*0.5
    
  print((SSB_0*1e-3)/2032) # Only off by 1% - not sure why though. 
  
  # Used the inital recruitment devs to get a start
  Ninit <- rep(NA,nage)
  Ninit_dev <- rev(df$parms$initN)
  
  Ninit[2:(nage-1)] <-R0 * exp(-Mage[1:(nage-2)])*exp(Ninit_dev[1:(nage-2)])
  Ninit[nage] <- R0*exp(-(Mage[nage-1]))/(1-exp(-M[nage]))*exp(Ninit_dev[nage-1])# Plus group (ignore recruitment dev's in first year )
  
  SSB_init <- sum(Ninit*Mat.sel, na.rm = T)*0.5
  Ninit[1] <- (4*h*R0*SSB_init/(SSB_0*(1-h)+ SSB_init*(5*h-1)))*exp(df$parms$Rin[1])
  
  Ntest <- matrix(NA,nage,nage+1)
  SSBtest <- matrix(NA, nage+1)
  SSBtest[1] <- SSB_0
  initN <- rev(Ninit_dev)
  Ntest[,1] <- N0
  
  for (i in 2:(nage)){
    
    Ntest[1,i] = ((4*h*R0*SSBtest[i-1])/
      (SSB_0*(1-h)+ SSBtest[i-1]*(5*h-1)))*exp(initN[i-1])
    
    Ntest[2:(nage-1),i] = Ntest[1:(nage-2),i-1]*exp(-M0)
    Ntest[nage,i] =  Ntest[nage-1,i-1]*exp(-M0)+Ntest[nage,i-1]*exp(-M0)
    SSBtest[i] <- sum(Ntest[,i]*df$Matsel)*0.5
    
  }
  
  Ninit <- Ntest[,nage]
  # beta.R <- df$beta.R
  sd.rec <- exp(df$logSDR) # Recruitment deviation
  
  
  # Create containers to save the data
  SSB <- matrix(NA,nyear+1)
  Biomass.save <- matrix(NA,nyear+1)
  Catch <- matrix(NA,nyear+1)
  Catch.age <- matrix(NA,nage,nyear+1)
  R.save <- matrix(NA,nyear+1)
  
  N.save.age <- matrix(NA,nage,nyear+1)
  
  survey <- array(NA,dim = c(nyear+1))
  
  age_comps_surv <- array(NA, dim = c(df$age_maxage,nyear)) # Fix the max ages later
  age_comps_catch <- array(NA, dim = c(df$age_maxage,nyear))
  
  N.save.age[,1] <- Ninit # Just to initialize 
  SSB[1] <- SSB_init
  Catch.age[,1] <- 0 # Assumed no fishing before data started 
  Catch[1]<- 0
  
  survey[1] <- 1 # Surveys start later
  
  for (yr in 2:(nyear+1)){ # Loop over years add one year for initial distribution
    
    if(year[yr-1] < year[df$selYear] | year[yr-1] > 2017){
      psel <- df$parms$psel_fish
    }else{
      psel <- df$parms$psel_fish+df$parms$PSEL[,yr-df$selYear]
    }
    
    Fsel <- getSelec(age,psel,df$Smin,df$Smax)
    
    # Calculate the mortality at length
    Fyear <- F0[yr]*Fsel
    Myear <- M # Natural mortality 
    Z <- Fyear+Myear
    # Assume that recruitment is in the beginning of the year (bc why not)
    # Recruitment
    
    if (year[yr-1] < 2018){
      Ry <- df$parms$Rin[yr-1]
      }else{
      Ry <- rnorm(1,mean = 0, sd = sd.rec)
      }
    # 
    b <- 0 # Bias adjustment factor
    R <- ((4*h*R0*SSB[yr-1])/
            (SSB_0*(1-h)+ SSB[yr-1]*(5*h-1)))*exp(-0.5*b*sd.rec^2+Ry)
    # 
    N.save.age[1,yr] <- R
    N.save.age[2:(nage-1),yr] <- N.save.age[1:(nage-2), yr-1]*exp(-Z[1:(nage-2)])
    N.save.age[nage, yr] <-  N.save.age[nage-1, yr-1]*exp(-Z[nage-1])+N.save.age[nage, yr-1]*exp(-Z[nage]) # Plus group
    
    
    SSB[yr] <- sum(N.save.age[,yr]*Mat.sel)*0.5
    
    
    if(year[yr-1] < 2018){
        w_catch <- df$wage_catch[,yr-1]
        w_surv <- df$wage_survey[,yr-1]
      }else{
        w_catch <- df$wage_catch[,1]
        w_surv <- df$wage_survey[,1]
      }
    
    Catch.age[,yr]  <- (Fyear/(Fyear+Myear))*(1-exp(-(Fyear+Myear)))*N.save.age[,yr]*w_catch # Calculate the catch in kg 
    Catch[yr] <- sum(Catch.age[,yr]) 
    
    # Save the survey 
    # Survey is conducted in the start of the year 
    if (df$flag_survey[yr-1] == 1){
      err <- exp(rnorm(n = 1,mean = 0, sd = surv.sd))
      surv <- sum(N.save.age[,yr]*surv.sel*q*w_surv)*err # If the xtra factor is not included the mean is > 1
      survey[yr] <- surv
    }else{
      survey[yr] <- 1
    }
    surv.tot <- sum(N.save.age[,yr]*surv.sel*q)
    
    if(df$flag_survey[yr-1] == 1){
    age_comps_surv[,yr-1] <-  (N.save.age[2:(df$age_maxage+1),yr]*surv.sel[2:(df$age_maxage+1)]*q)/surv.tot
    age_comps_surv[df$age_maxage,yr-1] <-age_comps_surv[df$age_maxage,yr-1]+ sum(N.save.age[(df$age_maxage+2):nage,yr]*surv.sel[(df$age_maxage+2):nage]*q)/surv.tot
    }else{
      age_comps_surv[,yr-1] <- NA
    }
    
    if(df$flag_catch[yr-1] == 1){
      age_comps_catch[,yr-1] <-  Catch.age[2:(df$age_maxage+1),yr]/Catch[yr]
      age_comps_catch[df$age_maxage,yr-1] <-age_comps_catch[df$age_maxage,yr-1]+ 
                                              sum(Catch.age[(df$age_maxage+2):nage,yr]/Catch[yr])
    }else{
      age_comps_catch[,yr-1] <- NA
    }
    
    
  }

  df.out   <- list(N.save = N.save.age[,2:(nyear+1)], SSB = SSB[2:(nyear+1)], 
                   Catch = Catch[2:(nyear+1)], Catch.age = Catch.age[,2:(nyear+1)], 
                   survey = survey[2:(nyear+1)], age_comps_catch = age_comps_catch, age_comps_surv= age_comps_surv)
  
  return(df.out)
}

