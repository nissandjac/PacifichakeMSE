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
PSEL <- matrix(0,5, length(1991:years[length(years)]))

initN <- rep(0,df$nage-1)

#U[2,] <- 0.01
parms <- list( # Just start all the simluations with the same initial conditions 
  logRinit = 14,
  logh = log(0.8),
  logMinit = log(0.2),
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

compile("runHakeassessment_test.cpp")
dyn.load(dynlib("runHakeassessment_test"))

obj <-MakeADFun(df,parms,DLL="runHakeassessment_test", random = 'Rin')#, )

# Test the input things 
reps <- obj$report()

Ninit <- reps$Ninit

