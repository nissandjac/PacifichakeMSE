# Run the hake assessment 
setwd("~/GitHub/PacifichakeMSE/Hake SS3 version")

library(TMB)
#library(TMBdebug)
library(ggplot2)
source('plotValues.R')
source('getUncertainty.R')
source('load_data.R')
source('ylimits.R')
source('parameters_TRUE.R')
source('Check_Identifiable_vs2.R')
source('getParms_noAge.R')

# Read the assessment data 
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]
load('hake2018_no_ageerror_fixtheta.RData')
catches.obs <- read.csv('catches.csv')

df <- load_data()
years <- df$years
df$logphi_survey <- log(10)
parms <- getParms_noage(hake2018b)

compile("runHakeassessment_3.cpp")
dyn.load(dynlib("runHakeassessment_3"))
obj <-MakeADFun(df,parms,DLL="runHakeassessment_3")#, )

# compile("hake_old.cpp")
# dyn.load(dynlib("hake_old"))
# obj <-MakeADFun(df,parms,DLL="hake_old")#, )
vals <- obj$report()

Ninit <- vals$Ninit
Nass <- vals$N
SSBass <- vals$SSB
Rass <- vals$R

age_survey  <- vals$age_survey_est
age_catch <- vals$age_catch

natage <- hake2018b$natage[2,13:33]
print(hake2018b$SBzero/vals$SSBzero)
# Compare the comps with ss3
SSB <- hake2018b$timeseries[,c("Yr","SpawnBio","Era")]
# divide by 2
SSB$SpawnBio <- SSB$SpawnBio*0.5


plot(SSB$SpawnBio[SSB$Yr>1965])
lines(SSBass)

plot(df$Catchobs)
lines(vals$Catch)

plot(df$years,SSBass)
lines(assessment$year,assessment$SSB)
lines(SSB$Yr[SSB$Yr>1965],SSB$SpawnBio[SSB$Yr>1965], col ='red')

plot(df$years,df$Catchobs)
lines(df$years,obj$report()$Catch)
