getSSB0 <- function(df){
  
  # Age 
  nage <- df$nage
  age <- df$age
  
  R0 <- exp(df$parms$logRinit)
  Msel <- df$Msel # no difference between males and females
  M0 <- exp(df$parms$logMinit)
  M <- M0*Msel # Naural mortality at age
  move.init <- df$move.init
  
  Mage <- c(0,cumsum(M[1:(nage-1)]))
  nspace <- df$nspace
  
  # Calculate N0 based on R0
  mage <- max(df$age) # Max age
  agetmp <- 0:(mage*3)
  nagetmp <- length(agetmp)
  
  N0tmp <- rep(NA,nagetmp)
  
  N0tmp[1:(nagetmp-1)] = R0*exp(-agetmp[1:(nagetmp-1)]*M0)
  N0tmp[nagetmp] =  N0tmp[nagetmp-1]*exp(-M0)/(1-exp(-M0))
  
  N0 <- matrix(NA,nage)
  N0[1:(nage-1)] <- N0tmp[1:(nage-1)]
  N0[nage] <- sum(N0tmp[nage:nagetmp])
  
  
  #SSB_0 <- rowSums(matrix(rep(N0,each =nspace),nrow = nspace)*matrix(rep(df$Matsel,each =nspace),nrow = nspace)*move.init)*0.5
  SSB_0 <- NA
  
  for(i in 1:nspace){
    SSB_0[i] <- sum(df$Matsel*N0*move.init[i])*0.5
  }
  
  return(SSB_0)
  
}
