getRefpoint <- function(par.fixed, df, SSB){

R0 <- as.numeric(exp(par.fixed)['logRinit'])  
Mest <- as.numeric(exp(par.fixed)['logMinit'])
M <- rep(Mest, df$nage)
nage <- df$nage
age <- 1:nage

Mage <- cumsum(M)
N0 <- NA
N0[1] <- R0
N0[2:(nage-1)] <-R0 * exp(-Mage[1:(nage-2)])
N0[nage] <- R0*exp(-(Mage[nage-1]))/(1-exp(-M[nage]))# Plus group (ignore recruitment dev's in first year )


SSB.age <- df$Matsel*N0 # In G
SSB_0 <- sum(SSB.age)


if(SSB$name[length(SSB$name)]/SSB_0 < 0.4){
  Fnew <- df$F0[length(df$F0)]-0.05
}else{
  Fnew <-df$F0[length(df$F0)]+0.05
}

if(Fnew < 0){
  Fnew <- 0.0001
}

return(Fnew)
}