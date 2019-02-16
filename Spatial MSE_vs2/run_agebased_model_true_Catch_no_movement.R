# Run an agebased model based on the parameters from the estimation model

run.agebased.true.catch <- function(df, seed = 100){
  # Constant parameters
  # Initialize the stock by age and sex
  
  # rSums <- function(x){
  #   if(df$>)
  #   
  
  
  set.seed(seed)
  
  
  df$tEnd <- length(df$years)
  nyear <-df$tEnd/df$nseason # 
  year <- df$years
  tEnd <- nyear
  # True values 
  M0 <- exp(df$parms$logMinit) # no difference between males and females
  F0 <- df$F0
  
  # Set up the data spatially 
  recruitmat <- df$recruitmat
  if(df$move == FALSE){
    recruitmat <- 1
  }
  
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
  # mage <- max(df$age) # Max age
  # agetmp <- 0:(mage*3)
  # nagetmp <- length(agetmp)
  # 
  # N0tmp <- rep(NA,nagetmp)
  # 
  # N0tmp[1:(nagetmp-1)] = R0*exp(-agetmp[1:(nagetmp-1)]*M0)
  # N0tmp[nagetmp] =  N0tmp[nagetmp-1]*exp(-M0*agetmp[nagetmp-1])/(1-exp(-M0))
  # 
  # N0 <- matrix(NA,nage)
  # N0[1:(nage-1)] <- N0tmp[1:(nage-1)]
  # N0[nage] <- sum(N0tmp[nage:nagetmp])
  
  N0 <- rep(NA,nage)
  N0[1] <- R0
  N0[2:(nage-1)] = R0*exp(-age[2:(nage-1)]*M0)
  N0[nage] = R0*exp(-M0*age[nage-1])/(1-exp(-M0))
  # 
  # plot(N0test/N0)
  # 
  
  #SSB_0 <- rowSums(matrix(rep(N0,each =nspace),nrow = nspace)*matrix(rep(df$Matsel,each =nspace),nrow = nspace)*move.init)*0.5
  SSB_0 <- NA
  
  SSB_0<- sum(df$Matsel*N0)*0.5

  
  R_0 <- R0
  # Used the inital recruitment devs to get a start
  Ninit <- rep(NA,nage)
  Ninit_dev <- (df$parms$initN)
  Ninit[1] <- R0
  Ninit[2:(nage-1)] <-R0 * exp(-M[2:(nage-1)]*age[2:(nage-1)])*exp(-0.5*SDR^2*0+Ninit_dev[1:(nage-2)])
  Ninit[nage] <- R0*exp(-(M[nage-1]*age[nage-1]))/(1-exp(-M[nage]))*exp(-0.5*SDR^2*0+Ninit_dev[nage-1])# Plus group (ignore recruitment dev's in first year )
  
  # Create containers to save the data
  SSB_init <- NA
  
  SSB_init <- sum(df$Matsel*Ninit, na.rm =T)*0.5

  #Ninit[1] <- sum((4*h*R_0*SSB_init/(SSB_0*(1-h)+ SSB_init*(5*h-1)))*exp(-0.5*1*SDR^2+df$parms$Rin[1]), na.rm = T)
  SSB <- matrix(NA,nyear+1)
  SSB.all <- array(NA, dim = c(nyear+1))
  Biomass.save <- matrix(NA,nyear+1)
  Catch <- matrix(NA,nyear+1)
  Catch.age <- matrix(NA,nage,nyear+1)
  CatchN <- matrix(NA,nyear+1)
  CatchN.age <- matrix(NA,nage,nyear+1)
  
  R.save <- matrix(NA,nyear+1)
  Fsel.save <- array(NA,dim = c(nyear+1, nage))
  Fseason.save <- array(NA,dim = c( nyear+1))
  
  N.save.age <- array(NA,dim = c(nage,nyear+1))
  Catch.save.age <- array(NA,dim = c(nage,nyear+1))
  CatchN.save.age <- array(NA,dim = c(nage,nyear+1))
  
  survey <- array(NA,dim = c(nyear+1))
  survey.true <- array(NA, dim = c(nyear+1))
  surv.tot <- matrix(NA, nyear+1)
  
  age_comps_surv <- array(NA, dim = c(df$age_maxage,nyear+1)) # Fix the max ages later
  N.survey <- matrix(NA,df$age_maxage ,nyear+1)
  
  age_comps_catch <- array(NA, dim = c(df$age_maxage,nyear+1))
  age_comps_OM <- array(NA, dim = c(df$nage,nyear+1))
  
  Z.save <- array(NA, dim = c(df$nage, nyear+1))
  
  # Distribute over space 

  N.save.age[,1] <- Ninit # Just to initialize 
     
  SSB[1] <-sum(N.save.age[,1]*Mat.sel, na.rm = T)*0.5
  
  Z.save <- array(NA, dim = c(df$nage, nyear+1))
  Z.save[,1] <- M
  Catch.age[,1] <- 0 # Assumed no fishing before data started 
  Catch[1]<- 0
  
  CatchN[1] <- 0
  CatchN.age[,1] <- 0
  
  survey[1] <- 1 # Surveys start later
  idx.save <- seq(1,tEnd)
  
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
    
    
    # for(ssn in 1:nseason){
    
    idx <- yr+1 # Use this index to save beyond the initial condition
    
    # Fyear <- F0[yr]*Fsel
    Myear <- M # Natural mortality 
    
    ## add these to load data seasons 
    pope.mul <- 0.5
    # Get the selectivity of the season and area 
    psel <- df$psel # Find out this comes from 
    
    if(year[yr] > year[df$selYear-1] & year[yr]<2018){
      psel <- psel+df$parms$PSEL[,idx-df$selYear]
    }
    # 
    
    Fsel <- getSelec(age,psel,df$Smin,df$Smax) # Constant over space right now 
    Fsel.save[yr,] <- Fsel
    
    E.temp <- df$Catch[yr]
    B.tmp <-  sum(N.save.age[,idx-1]*exp(-Myear*pope.mul)*w_catch*Fsel) # Get biomass from previous year
    N.tmp <- N.save.age[,idx-1]*exp(-Myear*pope.mul)#
    
    if(E.temp > 0){
      
      if(E.temp/B.tmp > 1){
        stop(paste('Catch exceeds available biomass in year:',df$years[yr],' and season', season, 'area', space))
      }
      
      temp <- E.temp/(B.tmp + 0.1*E.temp)
      join <- (1+exp(30*(temp-0.95)))^-1
      temp2 <- join*temp+0.95*(1-join)
      Fout <- -log(1-temp)
      #Fout <- df$F0[yr]
      Fseason <- Fout*Fsel
      
      Fseason.save[idx] <- Fout
      
      Fnew <- Fout
      for(i in 1:4){
        Z <- Myear+Fnew*Fsel
        Alpha <- (1-exp(-Z))
        Ctmp <- sum((Fnew/Z)*(N.tmp*w_catch*Fsel)*Alpha)
        C.N.tmp <- (Fnew/Z)*(N.tmp*Fsel)*Alpha
        Zadj <- E.temp/(Ctmp+0.0001)
        
        Zprime <- Myear+Zadj*(Z-Myear)
        Alpha <- (1-exp(-Zprime))/(Zprime)
        
        temp <- sum(N.tmp*w_catch*Fsel*Alpha)
        Ftmp <-  E.temp/(temp+0.0001)
        #print(Ftmp)
        j2 <- 1/(1+exp(30*(Ftmp-0.95*1)))
        
        Fnew <- j2*Ftmp+(1-j2)
        #N.tmp <- N.pop*exp(-(Fnew*Fsel+Mseason))
        
      }
      
      Z <- Fnew*Fsel+Myear
      Fseason <- Fnew*Fsel
    }else{
      Fseason <- 0
    }
        
        Ndot <- rep(0,nage)
        Ndot[2:(nage-1)] <- N.save.age[1:(nage-2),idx-1]
        Ndot[nage] <- N.save.age[nage,idx-1]+N.save.age[nage-1,idx-1]
        #Ndot <- N.save.age[,idx-1]
        
        SSB[idx] <- sum(Ndot*Mat.sel)*0.5
    
        # Recruitment only in season 1  
        R <- (4*h*R_0*SSB[idx]/
                (SSB_0*(1-h)+ SSB[idx]*(5*h-1)))*exp(-0.5*df$b[yr]*SDR^2+Ry)#*recruitmat[space]
        N.save.age[1,idx] <- R
        
        Nsurvive <- N.save.age[1:(nage-2), idx-1]*exp(-Myear[2:(nage-1)])
        
        Nsurvive.plus <- (N.save.age[nage-1, idx-1]*exp(-Myear[nage-1])+
                            N.save.age[nage, idx-1]*exp(-Myear[nage]))
          
        # Save for the season and year
        N.save.age[2:nage,idx] <-c(Nsurvive,Nsurvive.plus)-C.N.tmp[2:nage]
        
        Catch.save.age[, idx] <- C.N.tmp*w_catch
        CatchN.save.age[, idx] <- C.N.tmp
        Catch[idx] <- sum(Catch.save.age[,idx])
        
        R.save[idx] <- R
        
          
      }
      
# 
#     
#     #Catch.age[,idx]  <- (Fyear/(Fyear+Myear))*(1-exp(-(Fyear+Myear)))*rowSums(N.save.age[,idx,,1])*w_catch # Calculate the catch in kg 
#   
#     
#     #  }
#     # Save the survey 
#     # Survey is conducted in the start of the year
#     if(df$move == FALSE){
#       Nsurv <- N.survey
#     }else{
#       Nsurv <- rowSums(N.save.age[,idx,,df$surveyseason]*
#                          exp(-0*Mseason)) # Fix error with Nsurvey later 
#     }
#     
#     if (df$flag_survey[yr] == 1){
#       
#       
#       if(year[yr] > 2018){
#         err <- rnorm(n = 1,mean = 0, sd = surv.sd)
#         surv <- exp(log(sum(Nsurv*surv.sel*q*w_surv))+err) # If the xtra factor is not included the mean is > 1
#       }else{
#         err <- rnorm(n = 1,mean = 0, sd = surv.sd)
#         surv <- exp(log(sum(Nsurv*surv.sel*q*w_surv))+err) # If the xtra factor is not included the mean is > 1
#       }
#       survey[idx] <- surv
#     }else{
#       survey[idx] <- 1
#     }
#     
#     Ntot.year <- Nsurv
#     
#     surv.tmp <- sum(Ntot.year*surv.sel*q)
#     
#     if(df$flag_survey[yr] == 1){
#       age_comps_surv[1,idx] <- 0 # No year 1 recorded
#       
#       age_comps_surv[1:(df$age_maxage-1),idx] <-  (Ntot.year[2:(df$age_maxage)]*surv.sel[2:(df$age_maxage)]*q)/surv.tmp
#       age_comps_surv[df$age_maxage,idx] <- sum(Ntot.year[(df$age_maxage+1):nage]*surv.sel[(df$age_maxage+1):nage]*q)/surv.tmp
#     }else{
#       age_comps_surv[,idx] <- NA
#     }
#     
#     for(space in 1:nspace){
#       Ntot.year <- N.save.age[,idx,space,df$surveyseason]
#       surv.tot[idx,space]  <- sum(Ntot.year*surv.sel*q)
#       
#       age_comps_surv_space[1,idx,space] <- 0 # No year 1 recorded
#       
#       age_comps_surv_space[1:(df$age_maxage-1),idx,space] <-  
#         (Ntot.year[2:(df$age_maxage)]*surv.sel[2:(df$age_maxage)]*q)/surv.tot[idx,space]
#       age_comps_surv_space[df$age_maxage,idx,space] <- 
#         sum(Ntot.year[(df$age_maxage+1):nage]*surv.sel[(df$age_maxage+1):nage]*q)/surv.tot[idx,space]
#       
#       if(nseason>1){
#         Catch.tmp <- rowSums(CatchN.save.age[, idx,space,])
#       }else{
#         Catch.tmp <- CatchN.save.age[, idx,space,]
#       }
#       
#       Catch.tot <- sum(CatchN.save.age[,idx,space,])
#       
#       age_comps_catch_space[1:(df$age_maxage-1),idx,space] <- Catch.tmp[2:(df$age_maxage)]/Catch.tot
#       age_comps_catch_space[df$age_maxage,idx,space] <- sum(Catch.tmp[(df$age_maxage+1):nage])/Catch.tot
#       
#       
#       
#     }
#     
#     #
#     if(df$flag_catch[yr] == 1){
#       age_comps_catch[1:(df$age_maxage-1),idx] <-  CatchN.age[2:(df$age_maxage),idx]/CatchN[idx]
#       age_comps_catch[df$age_maxage,idx] <- sum(CatchN.age[(df$age_maxage+1):nage,idx])/CatchN[idx]
#       
#     }else{
#       age_comps_catch[,idx] <- NA
#     }
#     
#     
#     
#   
#   if(df$move == FALSE){
#     Nsave <- N.save.age[,,,nspace]
#     SSB.save <- SSB
#   }else{  
#     Nsave <- apply(N.save.age[,,,1],2,rowSums)
#     SSB.save <- rowSums(SSB)
#   }
#   
  df.out   <- list(SSB = SSB[1:(nyear+1),], 
                   N.save.age = N.save.age[,2:(nyear+1)],
                   Catch.save.age = Catch.save.age[,2:(nyear+1)],
                   CatchN.save.age = CatchN.save.age[,2:(nyear+1)],
                   Catch = Catch[2:(nyear+1)], 
                   R.save = R.save[2:(nyear+1)]
                   #age_catch = age_comps_catch[,2:(nyear+1)],
#                   age_comps_surv = age_comps_surv[,2:(nyear+1)],
)
  
  return(df.out)
}

