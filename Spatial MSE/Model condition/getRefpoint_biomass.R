getRefpoint.Biomass <- function(par.fixed, df, SSB, Fin){
  
  R0 <- as.numeric(exp(par.fixed)['logRinit'])  
  Mest <- as.numeric(exp(par.fixed)['logMinit'])
  h <- as.numeric(exp(par.fixed)['logh'])
  psel <- as.numeric(par.fixed[7:11])
  sel <- getSelec(df$age,psel,df$Smin,df$Smax)
  
  
  M <- rep(Mest, df$nage)
  nage <- df$nage
  age <- 1:nage
  
  Mage <- cumsum(M)
  N0 <- NA
  N0[1] <- R0
  N0[2:(nage-1)] <-R0 * exp(-Mage[1:(nage-2)])
  N0[nage] <- R0*exp(-(Mage[nage-1]))/(1-exp(-M[nage]))# Plus group (ignore recruitment dev's in first year )
  
  
  SSB.age <- df$Matsel*N0 # In G
  SSB_0 <- sum(SSB.age)*0.5
  
  SSB_pr<- SSB_0/R0
  
  SBeq <- 4*h*R0*0.4*SSB_0-SSB_0*(1-h)/(5*h-1)
  # 
  # 
  Z <- M+Fin*sel
  Zage <- cumsum(M)+cumsum(Fin*sel)
  N1 <- NA
  N1[1] <- R0
  N1[2:(nage-1)] <-R0 * exp(-Zage[1:(nage-2)])
  N1[nage] <- R0*exp(-(Zage[nage-1]))/(1-exp(-M[nage]))# Plus group (ignore recruitment dev's in first year )
  
  SSB_eq <- sum(df$Matsel*N1)*0.5
  
  ## Calculate the F40 reference point
  getF <- function(par){
    Z <- M+par[1]*sel
    Zage <- cumsum(M)+cumsum(par[1]*sel)
    N1 <- NA
    N1[1] <- R0
    N1[2:(nage-1)] <-R0 * exp(-Zage[1:(nage-2)])
    N1[nage] <- R0*exp(-(Zage[nage-1]))/(1-exp(-M[nage]))# Plus group (ignore recruitment dev's in first year )
    
    SSB_eq <- sum(df$Matsel*N1)*0.5
    
    ans <- (SSB_eq/SSB_0-0.4)^2
    return(ans)
  }
  
  
  FMSY <- optim(par = 0.1, fn =getF, method = 'Brent', lower = 0, upper = 4)
  
  
  if((SSB_eq/SSB_0) <= 0.1){
    Fnew <- 0.0001
  }
  
  if((SSB_eq/SSB_0) >= 0.4){
    
    if(Fin < FMSY$par){
      Fnew <- FMSY$par
    }else{
      Fnew <-df$F0[length(df$F0)]
    }
  }
  
  if((SSB_eq/SSB_0) < 0.4){
    Fnew <- df$F0[length(df$F0)]-0.05
    #  print('test')
  }
  
  if(Fnew < 0){
    Fnew <- 0.0001
  }
  # 
  # print(SSB_eq/SSB_0)
  # print(Fnew)
  # 
  # Calculate the TAC 
  
  Z <- M+Fnew*sel
  Zage <- cumsum(M)+cumsum(Fnew*sel)
  N1 <- NA
  N1[1] <- R0
  N1[2:(nage-1)] <-R0 * exp(-Zage[1:(nage-2)])
  N1[nage] <- R0*exp(-(Zage[nage-1]))/(1-exp(-M[nage]))# Plus group (ignore recruitment dev's in first year )
  
  
  C.TAC <- sum((Fnew/(Z))*(1-exp(-(Z)))*N1*df$wage_catch[,1])
  SSB_eq <- sum(df$Matsel*N1)
  
  
  
  return(list(F0 = Fnew, TAC = C.TAC, SSB = SSB_eq))
}