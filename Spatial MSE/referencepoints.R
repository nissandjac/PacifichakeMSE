<<<<<<< HEAD
SSB0calc <- function(Fsel){
  
  ## Make a forecast based on estimated quantitities
  h <- as.numeric(exp(rep$par.fixed)['logh'])
  R0 <- as.numeric(exp(rep$par.fixed)['logRinit'])  
  Mest <- as.numeric(exp(rep$par.fixed)['logMinit'])
  M <- rep(Mest, df$nage)
  nage <- df$nage
  age <- 1:nage
  
  Mage <- cumsum(M)
  #Fage <- cumsum(Finit)
  
  N0 <- NA
  N0[1] <- R0
  N0[2:(nage-1)] <-R0 * exp(-Mage[1:(nage-2)])
  N0[nage] <- R0*exp(-(Mage[nage-1]))/(1-exp(-M[nage]))# Plus group (ignore recruitment dev's in first year )
  
  
  SSB.age <- df$Matsel*N0 # In G
  SSB_0 <- sum(SSB.age)
  Hl <- 0.2
  HT <- 0.4
  Hs <- 1 # Scale of the harvest?

  j1 <- (1-exp(10*(SSBin-HT*SSB_0)))^-1
  j2<- (1-exp(10*(SSBin-Hl*SSB_0)))^-1

 left <- (0.0001*SSBin/(Hl*SSB_0))*j1
 curve <- 0.0001+(1-0.0001)*((HT*SSB_0)/SSBin)*((SSBin-Hl*SSB_0)/(HT*SSB_0-Hl*SSB_0))*(1-j1)
 right <- 1-j2
 ABC <- Hs*(j2*(left+curve)+Hs*right)


 
 SPR_temp<-SPB_equil/equ_Recr  #  spawners per recruit at initial F
 
 alpha <- 4.0 * steepness*Recr_virgin / (5.*steepness-1.)
 beta <- (SSB_0*(1.-h)) / (5.*h-1.)
 B_equil<-alpha * SPR_temp - beta # Has to be positive
 R_equil<-(4.*h*R0*B_equil) / (SSB_0*(1.-h)+(5.*h-1.)*B_equil)
 
 
 if(B_equil <0 ){
   stop('Error in calculation')
 }
 
 
 
 
=======
SSB0calc <- function(Fsel){
  
  ## Make a forecast based on estimated quantitities
  h <- as.numeric(exp(rep$par.fixed)['logh'])
  R0 <- as.numeric(exp(rep$par.fixed)['logRinit'])  
  Mest <- as.numeric(exp(rep$par.fixed)['logMinit'])
  M <- rep(Mest, df$nage)
  nage <- df$nage
  age <- 1:nage
  
  Mage <- cumsum(M)
  #Fage <- cumsum(Finit)
  
  N0 <- NA
  N0[1] <- R0
  N0[2:(nage-1)] <-R0 * exp(-Mage[1:(nage-2)])
  N0[nage] <- R0*exp(-(Mage[nage-1]))/(1-exp(-M[nage]))# Plus group (ignore recruitment dev's in first year )
  
  
  SSB.age <- df$Matsel*N0 # In G
  SSB_0 <- sum(SSB.age)
  Hl <- 0.2
  HT <- 0.4
  Hs <- 1 # Scale of the harvest?

  j1 <- (1-exp(10*(SSBin-HT*SSB_0)))^-1
  j2<- (1-exp(10*(SSBin-Hl*SSB_0)))^-1

 left <- (0.0001*SSBin/(Hl*SSB_0))*j1
 curve <- 0.0001+(1-0.0001)*((HT*SSB_0)/SSBin)*((SSBin-Hl*SSB_0)/(HT*SSB_0-Hl*SSB_0))*(1-j1)
 right <- 1-j2
 ABC <- Hs*(j2*(left+curve)+Hs*right)


 
 SPR_temp<-SPB_equil/equ_Recr  #  spawners per recruit at initial F
 
 alpha <- 4.0 * steepness*Recr_virgin / (5.*steepness-1.)
 beta <- (SSB_0*(1.-h)) / (5.*h-1.)
 B_equil<-alpha * SPR_temp - beta # Has to be positive
 R_equil<-(4.*h*R0*B_equil) / (SSB_0*(1.-h)+(5.*h-1.)*B_equil)
 
 
 if(B_equil <0 ){
   stop('Error in calculation')
 }
 
 
 
 
>>>>>>> baeb649a73da2a87886096ac53b590dddb82de24
 }