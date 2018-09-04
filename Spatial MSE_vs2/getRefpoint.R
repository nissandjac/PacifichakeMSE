getRefpoint <- function(par.fixed, df, SSBy, Fin, Nend){

R0 <- as.numeric(exp(par.fixed)['logRinit'])
Mest <- as.numeric(exp(par.fixed)['logMinit'])
h <- exp(df$logh)
psel <- as.numeric(par.fixed[7:11])
sel <- getSelec(df$age,psel,df$Smin,df$Smax)

Cw <- as.numeric(df$wage_catch[,dim(df$wage_catch)[2]])

M <- rep(Mest, df$nage)
nage <- df$nage
age <- 1:nage

Mage <- M #cumsum(M)
N0 <- NA
N0[1] <- R0
N0[2:(nage-1)] <-R0 * exp(-Mage[1:(nage-2)]*age[1:(nage-2)])
N0[nage] <- R0*exp(-(Mage[nage-1]*age[nage-1]))/(1-exp(-M[nage]))# Plus group (ignore recruitment dev's in first year )


SSB.age <- df$Matsel*N0 # In G
SSB_0 <- sum(SSB.age)*0.5

SSB_pr<- SSB_0/R0

SBeq <- 4*h*R0*0.4*SSB_0-SSB_0*(1-h)/(5*h-1)
# 
# 
Z <- M+Fin*sel
Zage <- Z#cumsum(M)+cumsum(Fin*sel)
N1 <- NA
N1[1] <- R0
N1[2:(nage-1)] <-R0 * exp(-Zage[1:(nage-2)]*age[1:(nage-2)])
N1[nage] <- R0*exp(-(Zage[nage-1]*age[nage-1]))/(1-exp(-M[nage]))# Plus group (ignore recruitment dev's in first year )

SSB_eq <- sum(df$Matsel*N1)*0.5

SPR <- SSB_eq/SSB_0
# ## Calculate the F40 reference point

getF <- function(par){
  Z <- M+par[1]*sel
  Zage <- Z
  N1 <- NA
  N1[1] <- R0
  N1[2:(nage-1)] <-R0 * exp(-Zage[1:(nage-2)]*age[1:(nage-2)])
  N1[nage] <- R0*exp(-(Zage[nage-1]*age[nage-1]))/(1-exp(-M[nage]))# Plus group (ignore recruitment dev's in first year )

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
Neq[2:(nage-1)] <-R0 * exp(-Zage[1:(nage-2)]*age[1:(nage-2)])
Neq[nage] <- R0*exp(-(Zage[nage-1]*age[nage-1]))/(1-exp(-M[nage]))# Plus group (ignore recruitment dev's in first year )

SSB_new <- sum(df$Matsel*Neq)*0.5

SPR_new <- SSB_new/SSB_0

Fnew <- F40$par

V <- sum(Nend*Cw*sel)

Fx <- 0.4

if((SSBy/SSB_0) < 0.1){
  Cnew <- 0.01 # add a very low catch (fix later)
}

if((SSBy/SSB_0) > 0.4){
  
 Cnew <- Fx*V

  
}

if(((SSBy/SSB_0) <= 0.4) & ((SSBy/SSB_0) >= 0.1)){
 
  Cnew <- Fx*V*((SSBy-0.1*SSB_0)*((0.4*SSB_0/SSB_eq)/(0.4*SSB_0-0.1*SSB_0)))
  
}



return(list(Cnew = Cnew, Fnew = Fnew))
}