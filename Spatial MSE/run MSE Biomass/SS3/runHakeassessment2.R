## Run the SAM assessment model for hake 
setwd("C:/Users/Nis/Dropbox/NOAA/Hake MSE/Hake SS3 version/runHakeassessment vs2/")# Load the data 
library(r4ss)
library(TMB)
library(reshape2)
library(ggplot2)
library(ggridges)
source('plotUncertainty.R')
source('getUncertainty.R')
source('load_data.R')
source('ylimits.R')
source('parameters_TRUE.R')
source('Check_Identifiable_vs2.R')
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

compile("runHakeassessment2.cpp")
dyn.load(dynlib("runHakeassessment2"))

obj <-MakeADFun(df,parms,DLL="runHakeassessment2")#, )

# Test the input things 
reps <- obj$report()

# Does the model and data look correct
plot(reps$SSB)
lines(assessment$SSB)

plot(reps$Fyear)

plot(reps$Catch, ylim = c(50000,450000))
lines(catches.obs$Total, col = 'red')
points(df$Catchobs, col = 'red')


plot(reps$Fyear)
lines(assessment$F0)


plot(df$years[df$years > 1994],reps$Biomass[df$years > 1994]*1e-6, type = 'l', ylab = 'survey', xlab = 'years')
points(df$years[df$flag_survey == 1], df$survey[df$flag_survey == 1]*1e-6)

am.est <- reps$age_catch_est

print(colSums(am.est))
# Calculate the average age in the catch 
am.mean <- data.frame(years = df$years, am = NA)

for(i in 1:length(df$years)){
  
  am.mean$am[i] <- sum(am.est[,i]*df$age[2:16])
  
}

par(mfrow = c(3,1), mar = c(4,4,1,1))
plot(am.mean$years,am.mean$am)
plot(reps$Catch)
plot(reps$CatchN)
# 
# 
lower <- obj$par-Inf
# lower[names(lower) == 'PSEL'] <- -1
#lower[names(lower) == 'logh'] <- 0
lower[names(lower) == 'F0'] <- 0
upper <- obj$par+Inf
# upper[names(upper) == 'PSEL' ] <- 1
# upper[names(upper) == 'Rin'] <- 3
#upper[names(upper) == 'logh'] <- log(1)
#
#lower[which(names(lower)== 'F0')] <- 0.0001
# #lower['logh'] <- log(0.7)
# #upper[which(names(upper) == 'logMinit')] <- log(0.5)
#pper[which(names(upper) == 'logh')] <- log(0.9)
# upper[which(names(upper) == 'logRinit')] <- log(20)


# upper['logSDsurv'] = log(1)
# lower['logSDsurv'] = log(0.1)
#upper['logQ'] = log(1e-3)

system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper)) # If error one of the random effects is unused

rep<-sdreport(obj)
#xx<- Check_Identifiable_vs2(obj)

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

age_comps <- getUncertainty('age_catch_est')

plotUncertainty(SSB, assessment$SSB)
lines(reps$SSB)

plotUncertainty(Catch, df$Catchobs)

yl <- ylimits(Biomass$name*1e-9,df$survey[df$survey > 1]*1e-9)
plot(years[df$survey>1],Biomass$name[df$survey>1]*1e-9, xlim = c(1990,2019), ylim = yl, xlab= 'survey')
points(years[df$survey > 1],df$survey[df$survey > 1]*1e-9, col = 'green')

plotUncertainty(F0, assessment$F0)

plotUncertainty(R, assessment$R)


df$survey[df$survey == 1] <- NA

plot(df$year, df$survey*1e-6, xlim = c(1994,2017), ylim = c(0,5))
lines(Biomass$year,Biomass$name*1e-6)
lines(Biomass$year,Biomass$min*1e-6, col = 'red')
lines(Biomass$year,Biomass$max*1e-6, col = 'red')

# Plot the selectvitiy
source('getSelec.R')
source('plotSelectivity.R')
plotSelectivity(rep$par.fixed, df)
# 
# 
# 
# 
# 
# 
# 
