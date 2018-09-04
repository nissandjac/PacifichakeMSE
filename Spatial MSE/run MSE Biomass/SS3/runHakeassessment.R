## Run the SAM assessment model for hake 
setwd("C:/Users/Nis/Dropbox/NOAA/Hake MSE/Hake SS3 version")# Load the data 
library(r4ss)
library(TMB)
library(reshape2)
source('plotUncertainty.R')
source('getUncertainty.R')
source('load_data.R')
source('ylimits.R')
# Read the assessment data 
assessment <- read.csv('assesssment.csv')
assessment <- assessment[assessment$Year < 2018,]

catches.obs <- read.csv('catches.csv')

df <- load_data()
years <- df$years

U <- matrix(0, 2 , df$tEnd) # Add a third one for selectivity later
Fapprox <- assessment$F/100
Fapprox[length(Fapprox)] <- Fapprox[length(Fapprox)-1]

PSEL <- matrix(0,5, length(1991:years[length(years)]))
PSEL <- as.matrix(read.csv('p_estimated.csv'))
# # Give the true parameters to U to test the TMB model 
Rdev <- read.csv('Rdev.csv')
initN <- rep(0,df$nage-1)
initN <- read.csv('initN.csv', header = F)
initN <- rev(initN$V2)
U[1,] <- Rdev$Rdev
U[2,] <- Fapprox
#U[2,] <- 0.01
parms <- list( # Just start all the simluations with the same initial conditions 
    logRinit = 14.8353,
    logh = log(0.8122),
    logMinit = log(0.2229),
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


if(sum(parms$F0) == 0){
  parms$F0 <- parms$F0+0.001 # Make sure it's over 1
}

compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))

obj <-MakeADFun(df,parms,DLL="runHakeassessment", random = 'Rin')#, )

# Test the input things 
reps <- obj$report()

# Save the inital run 
save(file = 'reps_base.Rdata', reps)


plot(reps$SSB, type = 'l')
plot(reps$Fyear)
plot(reps$R)

lower <- obj$par-Inf
upper <- obj$par+Inf

lower[which(names(lower)== 'F0')] <- 0.0001
#lower['logh'] <- log(0.7)
#upper[which(names(upper) == 'logMinit')] <- log(0.5)
upper[which(names(upper) == 'logh')] <- log(1)


# upper['logSDsurv'] = log(1)
# lower['logSDsurv'] = log(0.1)
#upper['logQ'] = log(1e-3)

system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper)) # If error one of the random effects is unused

rep<-sdreport(obj)

rep
sdrep <- summary(rep)
rep.values<-rownames(sdrep)


unique(rep.values)

source('getUncertainty.R')
df$nyear <- length(years)
df$year <- years

SSB <- getUncertainty('SSB',df)
F0 <- getUncertainty('Fyear',df)
Catch <- getUncertainty('Catch',df)
N <- getUncertainty('N',df)
Biomass <- getUncertainty('Biomass',df)
R <- getUncertainty('R',df)
surveyselec.est <- getUncertainty('surveyselc', df)
catchselec.est <- getUncertainty('catchselec', df)


par(mfrow = c(2,1), mar = c(4,4,1,1))
plot(SSB*1e-3 ~ Year, data = assessment, type = 'l', ylim = c(0,5), col ='green')
lines(df$years,reps$SSB*1e-6, type = 'l')
lines(years,SSB$name*1e-6, type ='l', lty= 3)


plot(years, Catch$name, type = 'l')
points(catches.obs$year,catches.obs$Fishery)

#
plot(years[df$survey>1],Biomass$name[df$survey>1]*1e-9, xlim = c(1990,2019))
points(years[df$survey > 1],df$survey[df$survey > 1]*1e-9, col = 'green')


dev.off()
plot(F0$name, type = 'l')
plot(years,R$name*1e3, type = 'l', log = 'y')
lines(assessment$Year,assessment$R*1e6, lty= 2)
### Plot the estimated parms in comparison with the ones from the assessment
par.fixed <- exp(rep$par.fixed)[1:6] # Log transformed scalar parameters 
par.fixed
plotUncertainty(SSB,assessment$SSB*2*1e3)


