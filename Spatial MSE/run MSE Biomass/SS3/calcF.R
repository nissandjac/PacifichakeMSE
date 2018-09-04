# Calculate F 

F0 <- matrix(NA, df$tEnd)
nage <- df$nage
F_tune <- 4

for (i in 1:df$tEnd){
 
  Ntmp <- reps$N[,i]
  Myear <- rep(exp(parms$logMinit),nage)
  Catchobs <- df$Catchobs[i]
  wage_mid <- df$wage_mid[,i]
  wage_catch <- df$wage_catch[,i]
  catchselec <- reps$catchselec
  
  Bret <- 0
  Nmid <- matrix(NA, nage)
  
  for(j in 1:nage){ ## Total biomass available to the fishery
    Nmid[j] <- Ntmp[j] *exp(-Myear[j]*0.5)
    Bret <- Bret +Nmid[j]*catchselec[j]*wage_mid[j] 
  }
  
  temp <- Catchobs[i]/(Bret+0.1*Catchobs[i])
  joiner <- 1/(1+exp(30*(temp-0.95)))
  temp_s <- joiner*temp+0.95*(1-joiner)
  
  Fin <- -log(1-temp_s)
  
  for (tune_F in 1:4){
    Z_rate=Myear+Fin*catchselec # I think deadfish is the discard mortality 
    Zrate2=(1.-exp(-Z_rate))/Z_rate
    
    if(tune_F<F_Tune){
      #  now calc adjustment to Z based on changes to be made to Hrate
    
    interim_tot_catch=sum(Fin*Ntmp*catchselec*wage_catch*Zrate2)  # biomass basis
    Z_adjuster = Catchobs[i]/(interim_tot_catch+0.0001)
    Z_rate=Myear + Z_adjuster*(Z_rate-Myear);  # need to modify to only do the exact catches
    Zrate2=(1.-exp(-1*Z_rate))/Z_rate

    vbio=sum(Ntmp*catchselec*Zrate2)
    
    temp=Catchobs/(vbio+0.0001)  #  prototype new F
    join1= 1./(1.+exp(30.*(temp-0.95*1)));
    Fin=join1*temp + (1.-join1)*1  #  new F value for this fleet
  }else #  SS_Label_Info_24.3.3.3.7 #Final tuning loop; loop over fleets to apply the iterated F
    catage[i]=Fin*Ntmp*Zrate2
  }
}

