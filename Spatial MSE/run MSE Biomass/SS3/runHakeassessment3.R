## Run the SAM assessment model for hake 
setwd("C:/Users/Nis/Dropbox/NOAA/Hake MSE/Hake SS3 version")# Load the data 
library(r4ss)
library(TMB)
library(reshape2)
source('plotUncertainty.R')
source('getUncertainty.R')
source('load_data.R')
source('ylimits.R')
source('parameters_TRUE.R')
# Read the assessment data 
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

catches.obs <- read.csv('catches.csv')

df <- load_data()
years <- df$years


#U[2,] <- 0.01
parms <- getParameters(TRUE)

# if(sum(parms$F0) == 0){
#   parms$F0 <- parms$F0+0.001 # Make sure it's over 1
# }

compile("runHakeassessment3.cpp")
dyn.load(dynlib("runHakeassessment3"))

obj <-MakeADFun(df,parms,DLL="runHakeassessment3")#, )

# Test the input things 
reps <- obj$report()

# Does the model and data look correct
plot(reps$SSB)
lines(assessment$SSB)

plot(reps$Fyear)
lines(assessment$F0)

plot(reps$Catch)
lines(catches.obs$Total)


plot(df$years[df$years > 1994],reps$Biomass[df$years > 1994]*1e-6, type = 'l', ylab = 'survey', xlab = 'years')
points(df$years[df$flag_survey == 1], df$survey[df$flag_survey == 1]*1e-6)

lower <- obj$par-Inf
upper <- obj$par+Inf
# 
lower[which(names(lower)== 'F0')] <- 0.0001
# #lower['logh'] <- log(0.7)
# #upper[which(names(upper) == 'logMinit')] <- log(0.5)
upper[which(names(upper) == 'logh')] <- log(1)
# upper[which(names(upper) == 'logRinit')] <- log(20)


# upper['logSDsurv'] = log(1)
# lower['logSDsurv'] = log(0.1)
#upper['logQ'] = log(1e-3)

system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper)) # If error one of the random effects is unused

rep<-sdreport(obj)

#rep
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


plotUncertainty(SSB, assessment$SSB)
lines(reps$SSB)

plotUncertainty(Catch, df$Catchobs)

yl <- ylimits(Biomass$name*1e-9,df$survey[df$survey > 1]*1e-9)
plot(years[df$survey>1],Biomass$name[df$survey>1]*1e-9, xlim = c(1990,2019), ylim = yl, xlab= 'survey')
points(years[df$survey > 1],df$survey[df$survey > 1]*1e-9, col = 'green')

plotUncertainty(F0, assessment$F0)

plotUncertainty(R, assessment$R)


df$survey[df$survey == 1] <- NA

plot(df$year, df$survey*1e-6, xlim = c(1994,2017), ylim = c(0,5.5))
lines(Biomass$year,Biomass$name*1e-6)
lines(Biomass$year,Biomass$min*1e-6, col = 'red')
lines(Biomass$year,Biomass$max*1e-6, col = 'red')

