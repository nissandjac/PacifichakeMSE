## Run the SAM assessment model for hake 

library(TMB)
library(ggplot2)
source('plotValues.R')
source('getUncertainty.R')
source('load_data_SS3.R')
source('ylimits.R')
source('parameters_TRUE.R')
source('parameters_SS3.R')
source('getParameters.R')
source('Check_Identifiable_vs2.R')
source('load_data_seasons.R')
source('run_agebased_model_true_Catch.R')
source('getSelec.R')
source('create_TMB_data.R')

# Read the assessment data 
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

catches.obs <- read.csv('catches.csv')

df <- load_data_SS3()
years <- df$years


#U[2,] <- 0.01
parms <- getParameters(FALSE)

compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))

obj <-MakeADFun(df,parms,DLL="runHakeassessment")#, )

lower <- obj$par-Inf

lower[names(lower) == 'F0'] <- 0.0001
upper <- obj$par+Inf
upper[names(upper) == 'psel_fish' ] <- 5
upper[names(upper) == 'PSEL'] <- 9
upper[names(upper) == 'logh'] <- log(0.999)
#upper[names(upper) == 'F0'] <- 0.5


system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper, 
                        control = list(iter.max = 2000,
                                       eval.max = 2000))) #

system.time(rep<-sdreport(obj))
#rep

#xx<- Check_Identifiable_vs2(obj)

#rep
sdrep <- summary(rep)
rep.values<-rownames(sdrep)


df$nyear <- length(years)
df$year <- years

SSB <- getUncertainty('SSB',df)

plotValues(SSB, data.frame(x= assessment$year, y= assessment$SSB),'SSB')



### WHY 

assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

Catch.obs <- read.csv('hake_totcatch.csv')

df.seasons <- load_data_seasons()

df.seasons$Catch <- Catch.obs$Fishery
sim.data <- run.agebased.true.catch(df.seasons)


##  Create a data frame to send to runHakeassessment 
df.new <- create_TMB_data(sim.data, df.seasons)
years <- df$years

# PSEL <- matrix(0,5, length(1991:years[length(years)]))
# initN <- rep(0,df$nage-1)
# F0 <- rep(0.01, df$nyear)
# Rdev <- rep(0, df$nyear)
# 
# parms2 <- list( # Just start all the simluations with the same initial conditions 
#   logRinit = 15,
#   logh = log(0.5),
#   logMinit = log(0.3),
#   logSDsurv = log(0.3),
#   logphi_catch = log(0.8276),
#   logphi_survey = log(11.33),
#   # Selectivity parameters 
#   psel_fish = c(2.486490, 0.928255,0.392144,0.214365,0.475473),
#   psel_surv = c(0.568618,-0.216172,0.305286 ,0.373829),
#   initN = initN,
#   Rin = Rdev,
#   F0 = F0,
#   PSEL = PSEL
# )
# 



obj2 <-MakeADFun(df.new,parms,DLL="runHakeassessment", silent = TRUE) # Run the assessment 

reps <- obj2$report()

lower <- obj2$par-Inf
upper <- obj2$par+Inf

lower[names(lower) == 'F0'] <- 1e-10
upper <- obj2$par+Inf
upper[names(upper) == 'psel_fish' ] <- 5
upper[names(upper) == 'PSEL'] <- 5
# upper[names(upper) == 'logh'] <- log(0.999)
# upper[names(upper) == 'F0'] <- 1.2


system.time(opt<-nlminb(obj2$par,obj2$fn,obj2$gr,lower=lower,upper=upper,
                        control = list(iter.max = 5000, 
                                       eval.max = 5000))) # If error one of the random effects is unused


system.time(rep<-sdreport(obj2))
#rep

#xx<- Check_Identifiable_vs2(obj)

#rep
sdrep <- summary(rep)
rep.values<-rownames(sdrep)
reps <- obj$report()
SSB2 <- getUncertainty('SSB',df)

plotValues(SSB2,data.frame(x= assessment$year, y= assessment$SSB),name = 'SSB')



plot(SSB$name)
lines(SSB2$name, col = 'red')


# 







