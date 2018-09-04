## Test the basemodel with the input parameters from the hake thingie 
setwd("C:/Users/Nis/Dropbox/NOAA/Hake MSE/Hake SS3 version")
assessment <- read.csv('assesssment.csv')
assessment <- assessment[assessment$Year < 2018,]
#assessment<-assessment[-(52:53),]
catches.obs <- read.csv('hake_totcatch.csv')

load('reps_base.Rdata')
names(reps)
source('load_data.R')

df <- load_data()
years <- df$years

U <- matrix(0, 2 , df$tEnd) # Add a third one for selectivity later
Fapprox <- assessment$E


PSEL <- matrix(0,5, length(1991:years[length(years)]))
PSEL <- as.matrix(read.csv('p_estimated.csv'))
# # Give the true parameters to U to test the TMB model 
Rdev <- read.csv('Rdev.csv')
initN <- rep(0,df$nage-1)
initN <- read.csv('initN.csv', header = F)
initN <- (initN$V2)
U[1,] <- Rdev$Rdev[1:df$tEnd]
U[2,] <- Fapprox
parms <- list( # Just start all the simluations with the same initial conditions 
  logRinit = log(2772888*1e-3),
  logh = log(0.812),
  logMinit = log(0.230),
  logSDsurv = log(0.3048),
  logphi_catch = log(0.3),
  logSDF = log(0.1),
  # Selectivity parameters 
  psel_fish = c(2.8476, 0.973,0.3861,0.1775,0.5048),
  psel_surv = c(0.5919,-0.2258,0.2876,0.3728),
  initN = initN,
  Rin = U[1,],
  F0 = U[2,],
  PSEL = PSEL
)

# What does the biomass look like 
# 
plot(years,reps$R, ylim = c(min(reps$R),max(assessment$R*1e4)), log = 'y')
lines(assessment$Year, assessment$R*1e4)

plot(years,reps$R/(assessment$R*1e4))

plot(years, reps$SSB*1e-6, ylim = c(1,5))
lines(assessment$Year,assessment$SSB*1e-3)

plot((reps$SSB*1e-6)/(assessment$SSB*1e-3))

dev.off()
# Plot all the selectivities 
# par(mfrow = c(6,6), mar = c(0.5,0.5,0.5,0.5))
# plot(df$age,reps$selectivity_save[,2], xlab ='', ylab = '', axes = F, ylim = c(0,1.2), type='l')
# for(i in 3:df$tEnd){
#   if (years[i] < years[df$selYear]){
#     lines(df$age,reps$selectivity_save[,i])
#     lines(df$age, rep(1, df$nage), lty= 2)
#   }else{
#     plot(df$age,reps$selectivity_save[,i], type = 'l', xlab ='', ylab = '', axes = F, ylim = c(0,1.2), main = years[i])
#     lines(df$age, rep(1, df$nage), lty= 2)  
#     }
# }



# What would SSB be in the first year given the Rinit 
nage <- df$nage
Mage <- cumsum(rep(exp(parms$logMinit), nage))
M <- exp(parms$logMinit)
Z <- reps$catchselec*parms$F0[1]+M

Ninit <- rep(NA,nage)
R0 <- exp(parms$logRinit)
age <- 0:20
  
Ninit[1] <- 0.5*R0*exp(parms$Rin[1])
Ninit[2:(nage-1)] <-0.5* R0 * exp(-Mage[1:(nage-2)])*exp(parms$initN[1:(nage-2)])
Ninit[nage] <- 0.5*R0*exp(-(Mage[nage-1]))/(1-exp(-M))*exp(parms$initN[(nage-1)])# Plus group (ignore recruitment dev's in first year )


Mage <- cumsum(rep(exp(parms$logMinit), nage*3))

mage <- 20 # Max age
agetmp <- 0:(mage*3)
nagetmp <- length(agetmp)

N0tmp <- rep(NA,nagetmp)

N0tmp[1:(nagetmp-1)] = R0*exp(-agetmp[1:(nagetmp-1)]*M)
N0tmp[nagetmp] =  N0tmp[nagetmp-1]*exp(-agetmp[nagetmp-1]*M)/(1-exp(-M))

N0 <- matrix(NA,nage)
N0[1:(nage-1)] <- N0tmp[1:(nage-1)]
N0[nage] <- sum(N0tmp[nage:nagetmp])
SSB0 <- sum(N0*df$Matsel)*0.5

plot(N0)
SSBinit <- sum(Ninit*df$Matsel)
SSBinit/SSB0

# Other N0 
Ntmp <- matrix(NA,nage)

Ntmp[1] <- R0
Ntmp[2:(nage-1)] <-R0 * exp(-Mage[1:(nage-2)])
Ntmp[nage] <- R0*exp(-(Mage[nage-1]))/(1-exp(-M))

SSB0_true = 2032

SSB0_true/(SSB0)

# Run the model forward in time from 20 years befpre
SSBinit <- SSB0

Ntest <- matrix(NA,nage,nage+1)
SSBtest <- matrix(NA, nage+1)
SSBtest[1] <- SSB0
initN <- rev(parms$initN)
Ntest[,1] <- N0

for (i in 2:(nage)){
  
  Ntest[1,i] = 0.5*((4*exp(parms$logh)*R0*SSBtest[i-1])/(SSB0*(1-exp(parms$logh))+ SSBtest[i-1]*(5*exp(parms$logh)-1)))*exp(initN[i-1])
  Ntest[2:(nage-1),i] = Ntest[1:(nage-2),i-1]*exp(-M)
  Ntest[nage,i] =  Ntest[nage-1,i-1]*exp(-M)+Ntest[nage,i-1]*exp(-M)
  SSBtest[i] <- sum(Ntest[,i]*df$Matsel)*0.5

}
Fone <- reps$catchselec*Fapprox[1]

Ntest[1,nage+1] = ((4*exp(parms$logh)*R0*SSBtest[i-1])/(SSB0*(1-exp(parms$logh))+ SSBtest[i-1]*(5*exp(parms$logh)-1)))*exp(0.5261)
Ntest[2:(nage-1),nage+1] = Ntest[1:(nage-2),i]*exp(-(M+Fone[1:(nage-2)]))
Ntest[nage,nage+1] =  Ntest[nage-1,i]*exp(-(M+Fone[nage-1]))+Ntest[nage,i]*exp(-(Fone[(nage)]+M))
SSBtest[nage+1] <- sum(Ntest[,nage+1]*df$Matsel)*0.5

# Ntest[1,nage+1] = 0.5*((4*exp(parms$logh)*R0*SSBinit)/(SSB0*(1-exp(parms$logh))+ SSBinit*(5*exp(parms$logh)-1)))#*exp(0.5261)
# Ntest[2:(nage-1),nage+1] = Ninit[1:(nage-2)]*exp(-(M))
# Ntest[nage,nage+1] =  Ninit[nage-1]*exp(-(M))+Ninit[nage]*exp(-(M))
# SSBtest[nage+1] <- sum(Ninit*df$Matsel)



yrprev <- (years[1]-nage):years[1]
plot(assessment$Year,assessment$SSB, type = 'l', xlim = c(1964-20,2018))
lines(yrprev,SSBtest, lty = 2)

# Do one more year
N0.est <- as.matrix(read.csv('N_age_estimated.csv'))
B0.est <- as.matrix(read.csv('B_age_estimated.csv'))
N0.est <- B0.est[1:52,]/t(df$wage_ssb)

plot(0:20,as.numeric(N0.est[1,]), log = 'y', ylim = c(15,3000))
points(0:20,Ntest[,nage+1], col = 'red')

SSB.est <- matrix(NA, df$tEnd, nage)
for (i in 1:dim(N0.est)[1]){
  SSB.est[i,] <- N0.est[i,]*df$Matsel
}
SSB.MLE <- rowSums(SSB.est)*0.5
plot(SSB.MLE*0.5*1e-3, type = 'l', col = 'red',ylim = c(0,4))
lines(assessment$SSB*1e-3)

N <- matrix(NA, nage, df$tEnd)
N[,1] <- N0.est[1,]
SSB <- matrix(NA,df$tEnd)
SSB[1] <- SSB.MLE[1]

for (i in 2:df$tEnd){

  Z <- Fapprox[i]*reps$catchselec+0.214
  
  N[1,i] = ((4*exp(parms$logh)*R0*SSB[i-1])/(SSB0*(1-exp(parms$logh))+ SSB[i-1]*(5*exp(parms$logh)-1)))*exp(parms$Rin[i])
  N[2:(nage-1),i] = Ninit[1:(nage-2)]*exp(-Z[1:(nage-2)])
  N[nage,i] =  Ninit[nage-1]*exp(-Z[nage-1])+Ninit[nage]*exp(-Z[nage])
  SSB[i] <- sum(N[,i]*df$Matsel)*0.5
  
}

dev.off()

plot(SSB)
