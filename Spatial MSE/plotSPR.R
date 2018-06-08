plotSPR <- function(df, sim.data){
  
R0 <- exp(df$parms$logRinit)
Mest <- exp(df$parms$logMinit)
h <- exp(df$parms$logh)
psel <- exp(df$parms$psel_fish)
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

# 
SSB_eq <- matrix(NA, df$nyear)

for(i in 1:df$nyear){
  Z <- M+df$F0[i]*sel
  Zage <- cumsum(M)+cumsum(df$F0[i]*sel)
  N1 <- NA
  N1[1] <- R0
  N1[2:(nage-1)] <-R0 * exp(-Zage[1:(nage-2)])
  N1[nage] <- R0*exp(-(Zage[nage-1]))/(1-exp(-M[nage]))# Plus group (ignore recruitment dev's in first year )

SSB_eq[i] <- sum(df$Matsel*N1)*0.5

}

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

Z <- M+FMSY$par*sel
Zage <- cumsum(M)+cumsum(FMSY$par*sel)
N1 <- NA
N1[1] <- R0
N1[2:(nage-1)] <-R0 * exp(-Zage[1:(nage-2)])
N1[nage] <- R0*exp(-(Zage[nage-1]))/(1-exp(-M[nage]))# Plus group (ignore recruitment dev's in first year )

SSB_40 <- sum(df$Matsel*N1)*0.5



plot((1-SSB_eq)/(1-SSB_40))

}
