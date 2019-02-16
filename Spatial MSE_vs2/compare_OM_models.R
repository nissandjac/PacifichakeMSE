### Compare time scale and seasons 


setwd("~/GitHub/PacifichakeMSE/Spatial MSE_vs2")

###### Initialize the operating model ###### 
library(TMB)
compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))

# Run the simulation model
source('run_agebased_model_true_Catch_vs2.R')

####  Plotting and uncertainty calculation functions #### 
source('ylimits.R')
source('plotUncertainty.R')
source('getUncertainty.R')
source('plotValues.R')

source('getSelec.R') # Calculate hake selectivity

source('load_data_seasons.R') # Loads data for Operating model
source('create_TMB_data.R') # Compiles operating model data to tmb data

source('getRefpoint.R') # Calculate refrence points 
source('Check_Identifiable_vs2.R') # see if hessian is positive definite 

source('getParameters.R')
source('calcSSB0.R')
source('run_multiple_MSEs.R')
source('load_data_seasons_move.R')
source('calcMeanAge.R')


#set.seed(123)
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

#parms.true <- getParameters(TRUE)
Catch.obs <- read.csv('hake_totcatch.csv')

df1 <- load_data_seasons(move = FALSE,nseason = 1,nspace = 1)
df1$Catch <- Catch.obs$Fishery
time <- 1

df2 <- load_data_seasons(move = TRUE, nseason = 4, nspace = 2)
df2$Catch <- Catch.obs$Fishery

df3 <- load_data_seasons(move = TRUE, nseason = 2, nspace = 2)
df3$Catch <- Catch.obs$Fishery

sim1 <- run.agebased.true.catch(df1)
sim2 <- run.agebased.true.catch(df2)
sim3 <- run.agebased.true.catch(df3)


plot(sim1$SSB)
lines(rowSums(sim2$SSB), col = 'red')
lines(rowSums(sim3$SSB), col = 'green')

# Compare age comps in catcgh

age1 <- calcMeanAge(sim1$age_catch,df1$age_maxage)
age2 <- calcMeanAge(sim2$age_catch,df2$age_maxage)
age3 <- calcMeanAge(sim3$age_catch,df3$age_maxage)
ageobs <- calcMeanAge(df1$age_catch,df1$age_maxage)

plot(age1[2:length(age1)], ylim = c(3,8))
lines(age2)
#lines(age3)
lines(ageobs, lty = 2, col ='red')

