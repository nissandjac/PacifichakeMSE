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
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

catches.obs <- read.csv('catches.csv')

df <- load_data()
years <- df$years

U <- matrix(0, 2 , df$tEnd) # Add a third one for selectivity later
PSEL <- matrix(0,5, length(1991:years[length(years)]))

initN <- rep(0,df$nage-1)
initN <- read.csv('Ninit_MLE.csv')[,1]
Rdev <- read.csv('Rdev_MLE.csv')[,1]
PSEL <- as.matrix(read.csv('p_MLE.csv'))
F0 <- assessment$F0
#U[2,] <- 0.01
parms <- list( # Just start all the simluations with the same initial conditions 
  logRinit = 14.5614,
  logh = log(0.861909),
  logMinit = log(0.213686),
  logSDsurv = log(0.257246),
  logphi_catch = log(0.8276),
  logphi_survey = log(11.33),
  # logSDF = log(0.1),
  # Selectivity parameters 
  psel_fish = c(2.486490, 0.928255,0.392144,0.214365,0.475473),
  psel_surv = c(0.568618,-0.216172,0.305286 ,0.373829),
  initN = initN,
  Rin = Rdev,
  F0 = F0,
  PSEL = PSEL
)


if(sum(parms$F0) == 0){
  parms$F0 <- parms$F0+0.001 # Make sure it's over 1
}

compile("runHakeassessment2.cpp")
dyn.load(dynlib("runHakeassessment2"))

obj <-MakeADFun(df,parms,DLL="runHakeassessment2")#, )

# Test the input things 
reps <- obj$report()

plot(df$years,reps$SSB, type = 'l')
lines(assessment$year, assessment$SSB, col = 'red')

lower <- obj$par-Inf
upper <- obj$par+Inf

system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper)) # If error one of the random effects is unused

rep<-sdreport(obj)

rep
# Get the likelihoods 
ll <- read.csv('likelihood.csv')

# Compare with model 

ll$values[1]
reps$ans_catch

tt <- -sum(dnorm(log(df$Catchobs+1),log(df$Catchobs+1),sd = 0.01, log = TRUE))
print(tt)
reps$ans_catch


reps$ans_survey
ll$values[4]

reps$ans_psel
ll$values[9]

reps$ans_SDR
ll$values[6]

reps$ans_catchcomp

plot(reps$age_catch[,11], log = 'y')
lines(reps$age_catch_est[,11])

reps$ans_survcomp
