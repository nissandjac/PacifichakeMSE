# Run an agebased model based on the parameters from the estimation model

run.agebased.true.catch <- function(df, seeds = 100){
  # Constant parameters
  # Initialize the stock by age and sex
  
  # rSums <- function(x){
  #   if(df$>)
  #   
    

  set.seed(seeds)
  
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
  if(df$move == FALSE){
    recruitmat <- 1
  }
  
  movemat <- df$movemat
  move.init <- df$move.init
  # M selectivity 
  Msel <- df$Msel # no difference between males and females
  M0 <- exp(df$parms$logMinit)
  M <- M0*Msel # Naural mortality at age
  SDR <- exp(df$logSDR)
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
  
  Mage <- c(0,cumsum(M[1:(nage-1)]))
  
  # Calculate N0 based on R0
  mage <- max(df$age) # Max age
  agetmp <- 0:(mage*3)
  nagetmp <- length(agetmp)
  
  N0tmp <- rep(NA,nagetmp)
  
  N0tmp[1:(nagetmp-1)] = R0*exp(-agetmp[1:(nagetmp-1)]*M0)
  N0tmp[nagetmp] =  R0*exp(-M0*agetmp[nagetmp])/(1-exp(-M0))
  
  N0 <- matrix(NA,nage)
  N0[1:(nage-1)] <- N0tmp[1:(nage-1)]
  N0[nage] <- sum(N0tmp[nage:nagetmp])
  
  SSB_0 <- NA
  
  for(i in 1:nspace){
    SSB_0[i] <- sum(df$Matsel*N0*move.init[i])*0.5
  }
  
  R_0 <- R0*move.init
  # Used the inital recruitment devs to get a start
  
  
  # Ninit <- rep(NA,nage)
  # Ninit_dev <- (df$parms$initN)
  # Ninit[1] <- R0
  # Ninit[2:(nage-1)] <-R0 * exp(-M[2:(nage-1)]*age[2:(nage-1)])*exp(-0.5*SDR^2*0+Ninit_dev[1:(nage-2)])
  # Ninit[nage] <- R0*exp(-(M[nage-1]*age[nage-1]))/(1-exp(-M[nage]))*exp(-0.5*SDR^2*0+Ninit_dev[nage-1])# Plus group (ignore recruitment dev's in first year )
  # 
  # Create containers to save the data
  # SSB_init <- NA
  # 
  # for(i in 1:nspace){
  #   SSB_init[i] <- sum(df$Matsel*Ninit*move.init[i], na.rm =T)*0.5
  # }
  # 
  
  #Ninit[1] <- sum((4*h*R_0*SSB_init/(SSB_0*(1-h)+ SSB_init*(5*h-1)))*exp(-0.5*1*SDR^2+df$parms$Rin[1]), na.rm = T)
  

  SSB <- matrix(NA,nyear, nspace)
  SSB.all <- array(NA, dim = c(nyear, nseason , nspace))
  Biomass.save <- matrix(NA,nyear, nspace)
  Catch <- matrix(NA,nyear)
  Catch.age <- matrix(NA,nage,nyear)
  CatchN <- matrix(NA,nyear)
  CatchN.age <- matrix(NA,nage,nyear)
  
  R.save <- matrix(NA,nyear, nspace)
  Fsel.save <- array(NA,dim = c(nyear,nspace, nage))
  Fseason.save <- array(NA,dim = c(nage, nyear, nseason,nspace))
  
  N.save.age <- array(NA,dim = c(nage,nyear+1, nspace, nseason))
  N.save.age.mid <- array(NA,dim = c(nage,nyear+1, nspace, nseason))
  
  Catch.save.age <- array(NA,dim = c(nage,nyear, nspace, nseason))
  CatchN.save.age <- array(NA,dim = c(nage,nyear, nspace, nseason))
  Catch.quota <- array(NA, dim = c(nyear, nspace, nseason))
  
  survey <- array(NA,dim = c(nyear))
  survey.true <- array(NA, dim = c(nspace, nyear))
  surv.tot <- matrix(NA, nyear,nspace)

  age_comps_surv <- array(NA, dim = c(df$age_maxage,nyear)) # Fix the max ages later
  age_comps_surv_space <- array(NA, dim = c(df$age_maxage,nyear,nspace)) # Fix the max ages later
  N.survey <- matrix(NA,df$age_maxage ,nyear)

  age_comps_catch <- array(NA, dim = c(df$age_maxage,nyear))
  age_comps_catch_space <- array(NA, dim = c(df$age_maxage,nyear,nspace))
  age_comps_OM <- array(NA, dim = c(df$nage,nyear, nspace,nseason))
  
  Z.save <- array(NA, dim = c(df$nage, nyear,nspace,nseason))

  Z.save[,1,1,1] <- M
  Catch.age[,1] <- 0 # Assumed no fishing before data started 
  Catch[1]<- 0
  
  CatchN[1] <- 0
  CatchN.age[,1] <- 0
  
  survey[1] <- 1 # Surveys start later
  
  for (space in 1:nspace){
  survey.true[space,1] <- sum(N.save.age[,1,space,df$surveyseason]*surv.sel*q*df$wage_survey[,1])
  }

  idx.save <- seq(1,tEnd, by = nseason)
  
  # Distribute over space 
  Ninit <- rep(NA,nage)
  Ninit_dev <- (df$parms$initN)
  
  Ninit[2:(nage-1)] <-R0 * exp(-Mage[2:(nage-1)])*exp(-0.5*SDR^2*0+Ninit_dev[1:(nage-2)])
  Ninit[nage] <- R0*exp(-(M[nage]*age[nage]))/(1-exp(-M[nage]))*exp(-0.5*SDR^2*0+Ninit_dev[nage-1])# Plus group (ignore recruitment dev's in first year )
  
  
  
  for (space in 1:nspace){
      # if (season == 1){
      N.save.age[,1,space,1] <- Ninit*move.init[space] # Just to initialize 
      N.save.age.mid[,1,space,1] <- N.save.age[,1,space,1]*exp(-0.5*(M/nseason))
      # }else{
      #   N.save.age[,1,space,season] <- N.save.age[,1,space,season-1]*exp(-M/nseason)
      #   N.save.age.mid[,1,space,season] <- N.save.age[,1,space,season]*exp(-0.5*(M/nseason))
      # }
      # }
  }
  
  Fnseason <- c(0.0,0.3,0.50,0.2) # Must add to one
  Fspace <- c(0.24,0.76) # Contribution of Total catch (add to one)    #Z <- (Fyear+Myear)
  pope.mul <- nseason/1*0.5
  pope.mul <- 0.50
  
  
  for (yr in 1:(nyear)){ # Loop over years add one year for initial distribution
    
    #if(year[yr] < year[df$selYear] | year[yr] > 2017){
   # }else{
   #   psel <- df$parms$psel_fish+df$parms$PSEL[,yr-df$selYear+1]
   # }
    if(year[yr] < 2018){
      w_catch <- df$wage_catch[,yr]
      w_surv <- df$wage_survey[,yr]
      w_mid <- df$wage_mid[,yr]
    }else{
      w_catch <- df$wage_catch[,1]
      w_surv <- df$wage_survey[,1]
      w_mid <- df$wage_mid[,1]
    }
    
    
    if (year[yr] < 2018){
      Ry <- df$parms$Rin[yr]
    }else{
      Ry <- df$parms$Rin[yr]
      #    Ry <- 0
    }
    

    # Fyear <- F0[yr]*Fsel
    Myear <- M # Natural mortality 
    
    ## add these to load data seasons 
    # Fnseason <- matrix(1, nseason)
    # Fnseason <- Fnseason/sum(Fnseason)
    # Fnseason <- c(0,0.5,0.5,0)
  
    
    
    if(nseason == 1){
      Fnseason = 1
    }
    
    if(df$move == FALSE){
      Fspace <- 1 # All catches in the south
    }
    
    Mseason <- Myear/nseason # M is distributed throughout the year
    
    
    # fix Ssb and recruitment in all areas 
    for(space in 1:nspace){
      SSB[yr,space] <-sum(N.save.age[,yr,space,1]*Mat.sel, na.rm = T)
      SSB.all[1,1,space]<- sum(N.save.age[,1,space,1]*Mat.sel, na.rm = T)
    
    # Recruitment only in season 1  
      R <- (4*h*R_0[space]*SSB[yr,space]/
            (SSB_0[space]*(1-h)+ SSB[yr,space]*(5*h-1)))*exp(-0.5*df$b[yr]*SDR^2+Ry)#*recruitmat[space]
    
      N.save.age[1,yr,space,1] <- R
    }
    
    
    for (season in 1:nseason){
      for (space in 1:nspace){
        
        # Get the selectivity of the season and area 
        psel <- df$psel[space,] 
        
        if(year[yr] > year[df$selYear-1] & year[yr]<2018){
          psel <- psel+df$parms$PSEL[,yr-df$selYear+1]
        }
        # 
        Fsel <- getSelec(age,psel,df$Smin,df$Smax) # Constant over space right now 
        Fsel.save[yr,space,] <- Fsel
        
        E.temp <- df$Catch[yr]*Fnseason[season]*Fspace[space] # Catch distribution in the year
        
        
        B.tmp <-  sum(N.save.age[,yr,space,season]*exp(-Mseason*pope.mul)*w_catch*Fsel) # Get biomass from previous year
        N.tmp <- N.save.age[,yr,space,season]#
        
        Fout <- getF(E.temp,B.tmp,year = df$years[yr], season, space)
        
        Catch.quota[yr,space,season] <- E.temp
        Fout <- Fout[1]
        
        if(E.temp>0){
          Fseason <- Fout*Fsel
          Fnew <- Fout  
          Z <- Fnew*Fsel+Mseason
          Fseason <- Fnew*Fsel
        }else{
          Fseason <- 0
        }
        
     
      
        Fseason.save[,yr,season,space] <- Fseason
      
        Z <- Mseason+Fseason
        Z.save[,yr,space,season]<- Z
        
        # Iterate to exactly match catch 
        
       
        #Fseason <- Fyear*Fnseason[season]*Fspace[space]
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
        
        if(df$move == FALSE){
          spaceidx <- 1
        }
        
        if(season <nseason){
        N.save.age[,yr,space,season+1] <- N.save.age[,yr,space,season]*exp(-Z)-
          N.save.age[, yr,space,season]*exp(-Z)*(movemat[space,,season,yr])+ # Remove the ones that leave
          N.save.age[, yr,spaceidx,season]*exp(-Z)*(movemat[spaceidx,,season,yr])# add the ones come to the surrounding areas
        
        age_comps_OM[,yr,space,season] <- N.save.age[, yr,space,season]/sum(N.save.age[, yr,space,season])
        
        SSB.all[yr,season,space]<- sum(N.save.age[,yr,space,season]*Mat.sel, na.rm = T)
        Catch.save.age[, yr,space, season] <- (Fseason/(Z))*(1-exp(-(Z)))*N.save.age[,yr,space,season]*w_catch
        CatchN.save.age[, yr,space, season] <- (Fseason/(Z))*(1-exp(-(Z)))*N.save.age[,yr,space,season]
        
        
        }else{
          N.save.age[2:(nage-1),yr+1,space,1] <- N.save.age[1:(nage-2),yr,space,season]*exp(-Z[1:(nage-2)])-
            N.save.age[1:(nage-2), yr,space,season]*exp(-Z[1:(nage-2)])*(movemat[space,1:(nage-2),season,yr])+ # Remove the ones that leave
            N.save.age[1:(nage-2), yr,spaceidx,season]*exp(-Z[1:(nage-2)])*(movemat[spaceidx,1:(nage-2),season,yr])# add the ones come to the surrounding areas
          
          # Plus group 
          Nsurvive.plus <- (N.save.age[nage-1, yr,space, nseason]*exp(-Z[nage-1])+
                              N.save.age[nage, yr,space, nseason]*exp(-Z[nage]))
          
          Nout.plus <- Nsurvive.plus*(movemat[space,nage, season,yr]) # Leaving

          
          Nin.plus <- (N.save.age[nage-1, yr,spaceidx,nseason]*exp(-Z[nage-1])+
                         N.save.age[nage, yr,spaceidx,nseason]*exp(-Z[nage]))*
                          (movemat[spaceidx,nage, season,yr]) # Incoming

          N.save.age[nage,yr+1,space,1] <- Nsurvive.plus- Nout.plus + Nin.plus
          
          
          age_comps_OM[,yr,space,season] <- N.save.age[, yr,space,season]/sum(N.save.age[, yr,space,season])
          
          SSB.all[yr,season,space]<- sum(N.save.age[,yr,space,season]*Mat.sel, na.rm = T)
          Catch.save.age[, yr,space, season] <- (Fseason/(Z))*(1-exp(-(Z)))*N.save.age[,yr,space,season]*w_catch
          CatchN.save.age[, yr,space, season] <- (Fseason/(Z))*(1-exp(-(Z)))*N.save.age[,yr,space,season]
         
          
        }
   
        
        if(is.na(SSB[yr,space])){
          stop('SSB is NA')
        }
      }
        
      }
      
  
    
    #Catch.age[,idx]  <- (Fyear/(Fyear+Myear))*(1-exp(-(Fyear+Myear)))*rowSums(N.save.age[,idx,,1])*w_catch # Calculate the catch in kg 
    
    if(nseason>1){
    Catch.age[,yr] <- apply(Catch.save.age[,yr,,],MARGIN = 1,FUN = sum)
    Catch[yr] <- sum(Catch.save.age[,yr,,])  
  
    CatchN.age[,yr] <- apply(CatchN.save.age[,yr,,],MARGIN = 1,FUN = sum)
    CatchN[yr] <- sum(CatchN.save.age[,yr,,])  
    }else{
      
      if(nspace == 1){
      Catch.age[,yr] <- Catch.save.age[,yr,,]
      Catch[yr] <- sum(Catch.save.age[,yr,,])
      
      CatchN.age[,yr] <- CatchN.save.age[,yr,,]
      CatchN[yr] <- sum(CatchN.save.age[,yr,,])
      }else{
        Catch.age[,yr] <- rowSums(Catch.save.age[,yr,,])
        Catch[yr] <- sum(Catch.save.age[,yr,,])
        
        CatchN.age[,yr] <- rowSums(CatchN.save.age[,yr,,])
        CatchN[yr] <- sum(CatchN.save.age[,yr,,])
      }
    }  
    
    Msurveymul <- 0
    
    for (space in 1:nspace){
    survey.true[space,yr] <- sum(N.save.age[,yr,space,df$surveyseason]*
                                    exp(-Msurveymul*Z.save[,yr,space,df$surveyseason])*surv.sel*q*w_surv)
      
    }
    
    
    #  }
      # Save the survey 
      # Survey is conducted in the start of the year
    # }else{
    #   Msurveymul <- 0.5
    # }
    
    if(df$move == FALSE){
      Nsurv <- N.save.age[,yr,,df$surveyseason]*
        exp(-Msurveymul*Z.save[,yr,space,df$surveyseason])
    }else{
      Nsurv <- rowSums(N.save.age[,yr,,df$surveyseason]*
                         exp(-Msurveymul*Z.save[,yr,space,df$surveyseason]))
    }
    
    if (df$flag_survey[yr] == 1){
        
        
        if(year[yr] > 1990){
        err <- rnorm(n = 1,mean = 0, sd = surv.sd)
        surv <- exp(log(sum(Nsurv*surv.sel*q*w_surv))+err) # If the xtra factor is not included the mean is > 1
        }else{
         surv <- sum(Nsurv*surv.sel*q*w_surv)
       }
        survey[yr] <- surv
      }else{
        survey[yr] <- 1
      }
    
      Ntot.year <- Nsurv
      
      surv.tmp <- sum(Ntot.year*surv.sel*q)
      
     if(df$flag_survey[yr] == 1){
        age_comps_surv[1,yr] <- 0 # No year 1 recorded

        age_comps_surv[1:(df$age_maxage-1),yr] <-  (Ntot.year[2:(df$age_maxage)]*surv.sel[2:(df$age_maxage)]*q)/surv.tmp
        age_comps_surv[df$age_maxage,yr] <- sum(Ntot.year[(df$age_maxage+1):nage]*surv.sel[(df$age_maxage+1):nage]*q)/surv.tmp
      }else{
        age_comps_surv[,yr] <- NA
      }
      
      for(space in 1:nspace){
      Ntot.year <- N.save.age[,yr,space,df$surveyseason]
      surv.tot[yr,space]  <- sum(Ntot.year*surv.sel*q)
        
      age_comps_surv_space[1,yr,space] <- 0 # No year 1 recorded
        
      age_comps_surv_space[1:(df$age_maxage-1),yr,space] <-  
        (Ntot.year[2:(df$age_maxage)]*surv.sel[2:(df$age_maxage)]*q)/surv.tot[yr,space]
      age_comps_surv_space[df$age_maxage,yr,space] <- 
        sum(Ntot.year[(df$age_maxage+1):nage]*surv.sel[(df$age_maxage+1):nage]*q)/surv.tot[yr,space]
      
      if(nseason>1){
      Catch.tmp <- rowSums(CatchN.save.age[, yr,space,])
      }else{
      Catch.tmp <- CatchN.save.age[, yr,space,]
      }
      
      Catch.tot <- sum(CatchN.save.age[,yr,space,])
      
      age_comps_catch_space[1:(df$age_maxage-1),yr,space] <- Catch.tmp[2:(df$age_maxage)]/Catch.tot
      age_comps_catch_space[df$age_maxage,yr,space] <- sum(Catch.tmp[(df$age_maxage+1):nage])/Catch.tot
      
      
      
      }
      
      #
      if(df$flag_catch[yr] == 1){
        age_comps_catch[1:(df$age_maxage-1),yr] <-  CatchN.age[2:(df$age_maxage),yr]/CatchN[yr]
        age_comps_catch[df$age_maxage,yr] <- sum(CatchN.age[(df$age_maxage+1):nage,yr])/CatchN[yr]
        
      }else{
        age_comps_catch[,yr] <- NA
      }
      

    }# End of year loop
  #}
  
    if(df$move == FALSE){
      Nsave <- N.save.age[,,,nspace]
      SSB.save <- SSB
    }else{  
    Nsave <- apply(N.save.age[,,,1],2,rowSums)
    SSB.save <- rowSums(SSB)
    }
    
    df.out   <- list(N.save = Nsave, SSB = SSB, 
                     N.save.age = N.save.age,
                     SSB.all = SSB.all,
                     Catch.save.age = Catch.save.age,
                     CatchN.save.age = CatchN.save.age,
                     Catch = Catch, 
                     Catch.age = Catch.age, 
                     Catch.quota = Catch.quota,
                     Nout = N.save.age, 
                     age_comps_OM = age_comps_OM,
                     age_catch = age_comps_catch,
                     SSB_0 = SSB_0, Nsave.all = N.save.age,
                     survey.true = survey.true,
                     surv.tot = surv.tot,
                     Z = Z.save,
                     survey = survey,
                     age_comps_surv = age_comps_surv,
                     age_comps_country = age_comps_surv_space,
                     age_comps_catch_space = age_comps_catch_space,
                     Fseason = Fseason.save,
                     Fsel = Fsel.save, 
                     N0 = N0,
                     #Fsave = Fseason.save[2:(nyear+1),,],
                     Ninit = Ninit,
                     SSB0 = SSB_0)
    
  return(df.out)
}

