calcSPR <- function(par.fixed,df, SSBy, Fin =NA, Nend)
 
R0 <- as.numeric(exp(par.fixed)['logRinit'])
M <- as.numeric(exp(par.fixed)['logMinit'])
h <- as.numeric(exp(par.fixed)['logh'])
psel <- as.numeric(par.fixed[grep('psel_fish',names(par.fixed))])
sel <- getSelec(df$age,psel,df$Smin,df$Smax)

Cw <- as.numeric(df$wage_catch[,dim(df$wage_catch)[2]])

M <- rep(Mest, df$nage)
nage <- df$nage
age <- 0:(nage-1)

Mage <- M #cumsum(M)
# N0 <- NA
# N0[1] <- R0
# N0[2:(nage-1)] <-R0 * exp(-Mage[1:(nage-2)]*age[2:(nage-1)])
# N0[nage] <- R0*exp(-(Mage[nage-1]*age[nage-1]))/(1-exp(-M[nage]))# Plus group

N0 <- NA
N0[1] <- R0
for(a in 1:(nage-1)){
  N0[a+1] <- N0[a]*exp(-Mage[a])
}
N0[nage] <- N0[nage]/(1-Mage[nage])

SSB.age <- df$Matsel*N0*0.5 # In G
SSB_0 <- sum(SSB.age)

SSB_pr<- SSB_0/R0

SBeq <- 4*h*R0*0.4*SSB_0-SSB_0*(1-h)/(5*h-1)

getF <- function(par){
  
  Z <- M+par[1]*sel
  # Zage <- Z
  # N1 <- NA
  # N1[1] <- R0
  # N1[2:(nage-1)] <-R0 * exp(-Zage[2:(nage-1)]*age[2:(nage-1)])
  # N1[nage] <- R0*exp(-(Zage[nage-1]*age[nage-1]))/(1-exp(-M[nage]))# Plus group (ignore recruitment dev's in first year )
  
  Z <- M+par[1]*sel
  Zage <- Z
  N1 <- NA
  N1[1] <- R0
  for(a in 1:(nage-1)){
    N1[a+1] <- N1[a]*exp(-Zage[a])
  }
  #adjust plus group sum of geometric series as a/(1-r)
  N1[nage] <- N1[nage]/(1-Zage[nage])
  
  SSB_eq <- sum(df$Matsel*N1)*0.5
  
  #print(SSB_eq/SSB_0)
  
  ans <- (SSB_eq/SSB_0-0.4)^2
  return(ans)
}


F40 <- optim(par = 0.1, fn =getF, method = 'Brent', lower = 0, upper = 4)

Z <- M+F40$par*sel
Zage <- Z

Neq <- NA
Neq[1] <- R0
for(a in 1:(nage-1)){
  Neq[a+1] <- Neq[a]*exp(-Zage[a])
}
# adjust plus group sum of geometric series as a/(1-r)
Neq[nage] <- Neq[nage]/(1-Zage[nage])

SSB_new <- sum(df$Matsel*Neq)*0.5

SPR_new <- SSB_new/SSB_0


  return(SPR_new)

}