getRefpoint <- function(par.fixed, df, SSBy, Fin =NA, Nend,
                        TAC = 1, Vreal = NA){

R0 <- as.numeric(exp(par.fixed)['logRinit'])
Mest <- as.numeric(exp(par.fixed)['logMinit'])
h <- as.numeric(exp(par.fixed)['logh'])
psel <- as.numeric(par.fixed[which(names(par.fixed) == 'psel_fish')])
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
# 
# 
# Z <- M+Fin*sel
# Zage <- Z#cumsum(M)+cumsum(Fin*sel)
# N1 <- NA
# N1[1] <- R0
# N1[2:(nage-1)] <-R0 * exp(-Zage[2:(nage-1)]*age[2:(nage-1)])
# N1[nage] <- R0*exp(-(Zage[nage-1]*age[nage-1]))/(1-exp(-Z[nage]))# Plus group (ignore recruitment dev's in first year )

Z <- M+Fin*sel
Zage <- Z
N1 <- NA
N1[1] <- R0
for(a in 1:(nage-1)){
  N1[a+1] <- N1[a]*exp(-Zage[a])
}
# adjust plus group sum of geometric series as a/(1-r)
N1[nage] <- N1[nage]/(1-Zage[nage])

SSB_eq <- sum(df$Matsel*N1)*0.5

SPR <- SSB_eq/SSB_0
#SPR <- SSBy/SSB_0
#print(SPR)
# ## Calculate the F40 reference point

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

Z <- M+Fin*sel
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

Fnew <- F40$par

V <- sum(Nend*Cw*sel)

Fx <- 1-exp(-Fnew) # Convert to harvest rate 

#print(Fx/0.18)
#Fx <- 0.18

if((SSBy/SSB_0) < 0.1){
  Cnew <- 0.05*Vreal # add a very low catch (fix later)
}

if((SSBy/SSB_0) > 0.4){
  
 Cnew <- Fx*V

  
}

if(((SSBy/SSB_0) <= 0.4) & ((SSBy/SSB_0) >= 0.1)){
 
  Cnew <- Fx*V*((SSBy-0.1*SSB_0)*((0.4*SSB_0/SSBy)/(0.4*SSB_0-0.1*SSB_0)))
  
}
# 
# if (Cnew > 500000){
#   Cnew <- 500000
# }
# Adjust TAC by JMC/Utilization 
TAC.obs <- read.csv('data/adjusted_tac_fn.csv')


if(TAC == 1){
  Cexp <- Cnew
}else if(TAC == 2){
  Cexp <- TAC.obs$incpt[1]+TAC.obs$slp[1]*Cnew
}else if(TAC == 3){
  Cexp <- TAC.obs$incpt[2]+TAC.obs$slp[2]*Cnew
}else if(TAC == 4){ # Half the treaty specified and with a lower floor
  Cexp <- Cnew*0.5
  
  if(Cexp < 180000){
    Cexp <- 180000 
  }
}

# Do a test run 
# print(paste('JTC TAC = ', Cnew))
# print(paste('JMC  TAC = ', Cexp))

if(Cexp > Cnew){ # Never go over the JTC recommendation 
  Cexp <- Cnew
}


return(list(Cnew = Cexp, Fnew = F40$par))
}