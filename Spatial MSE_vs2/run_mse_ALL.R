### Run a bunch of MSE's for the future - uncertainty in Recruitment and survey 

setwd("~/GitHub/PacifichakeMSE/Spatial MSE_vs2")

###### Initialize the operating model ###### 
library(TMB)
compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))

# Run the simulation model
source('run_agebased_model_true_Catch_vs3.R')

####  Plotting and uncertainty calculation functions #### 
source('ylimits.R')
source('plotUncertainty.R')
source('getUncertainty.R')
source('plotValues.R')
source('calcF.R')
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

nruns <- 10

#set.seed(123)
seeds <- ceiling(runif(n = nruns, min = 1, 1e6))
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

#parms.true <- getParameters(TRUE)
Catch.obs <- read.csv('hake_totcatch.csv')

df <- load_data_seasons(move = FALSE,nseason = 1,nspace = 1)
df$Catch <- Catch.obs$Fishery
time <- 1
yrinit <- df$nyear
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 
#
# df$parms$Rin <- df$parms$Rin*0
# df$F0 <- 0*df$F0

simyears <- 30# Project 30 years into the future (2048 that year)
moveparms <- NA
seed <- 123

year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
year.future <- c(df$years,df)
N0 <- NA
sim.data <- run.agebased.true.catch(df)
# 
# ### Loop MSE's with different errors in future survey and recruitment
ls.save <- list()
ls.converge <- matrix(0, nruns)
#
for (i in 1:nruns){
  tmp <- try(run_multiple_MSEs(simyears = 30, seeds[i],moveparms = NA),silent = FALSE)
  #tmp <- run_multiple_MSEs(simyears = 30, seeds[i])
  print(i)

  if(is.list(tmp)){
  ls.save[[i]] <-tmp
  ls.converge[i] <- 1
  }


}
# # # # 
save(ls.save,file = 'MSErun_nomove3.Rdata')
###  Plot the true SSB ###
#load('MSErun_nomove.Rdata')
yr <- 1966:(2017+simyears-1)
SSB.plot <- data.frame(SSB = (ls.save[[1]]$SSB)/sim.data$SSB_0, year = yr, run = paste('run',1, sep=''))
Catch.plot <- data.frame(Catch = ls.save[[1]]$Catch, year = yr, run = paste('run',1, sep=''))
# AAV.plot  <- data.frame(Catch = ls.save[[1]]$Catch[2:ls.save[[1]]$df$tEnd]-ls.save[[1]]$Catch[1:(ls.save[[1]]$df$tEnd-1])], 
#                         year = ls.save[[1]]$df$years, run = paste('run',1, sep=''))

for(i in 2:nruns){
  ls.tmp <- ls.save[[i]]  
  
  if(is.list(ls.tmp)){
  SSB.tmp <- data.frame(SSB = (ls.tmp$SSB)/(sim.data$SSB_0), year = yr, run =  paste('run',i, sep=''))
  SSB.plot <- rbind(SSB.plot,SSB.tmp)
  
  Catch.tmp <- data.frame(Catch = ls.tmp$Catch, year = yr, run =  paste('run',i, sep=''))
  Catch.plot <- rbind(Catch.plot,Catch.tmp)

  
    
  }
}


source('hake_objectives.R')
ll <- hake_objectives(ls.save,sim.data)

ll
# png('MSE_results.png', width = 16, height = 12, res = 400, unit = 'cm')
do.call(grid.arrange, ll[[1]])
# dev.off()

# write.table(ll[[2]], file = 'performancemetrics.csv', row.names = F)

# Spagehtti plots 


length(ls.save)

plot(ls.save[[i]]$SSB/sim.data$SSB0, type ='l', ylim= c(0,2))

cols <- 1:nruns 
for(i in 2:nruns){
  
  lines(ls.save[[i]]$SSB/sim.data$SSB0, col = cols[i])
  
}
