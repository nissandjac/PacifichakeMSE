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
  

    minF <- function(data,par){   # Find the fishing mortality that gives F40 

      Feq <- (par*Fsel)
      Fage <- cumsum(Feq)  
      Mage <- cumsum(M)
      Zage <- Fage+Mage
      #Fage <- cumsum(Finit)
      Neq <- NA
      Neq[1] <- R0
      Neq[2:(nage-1)] <-R0 * exp(-Zage[1:(nage-2)])
      Neq[nage] <- R0*exp(-(Zage[nage-1]))/(1-exp(-(M[nage]+Feq[nage])))# Plus group (ignore recruitment dev's in first year )
      SSB.eq <- df$Matsel*Neq # In G
      SSB.eq <- sum(SSB.eq)
      
      ans = (SSB.eq/data$SSB_0-0.4)^2
    #  print(ans)
      return(log(ans))
    }
 
    
  x.optim <- optim(par = 0.3, fn = minF, data = data.frame(SSB_0 = SSB_0), method = 'Brent', lower = 0, upper = 100)
    
return(list(Fnext = x.optim$par, SSB0 = SSB_0))
     
}