### Calc MSY and Fmsy in the OM model 

direc <- "~/GitHub/PacifichakeMSE/Simple MSE"
setwd(direc)
###### Initialize the operating model ###### 
library(TMB)
compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))
# Run the simulation model
source('run_agebased_model_simple.R')
source('ylimits.R')
source('plotUncertainty.R')
source('getUncertainty.R')
source('SSB0calc.R')
source('getSelec.R')
source('load_data.R')
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

df <- load_data()
time <- 1
yrinit <- df$tEnd
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 

simyears <- 10 # Project 30 years into the future  
N0 <- NA
sim.data <- run.agebased.simple(df, seed = 100)

plot(sim.data$SSB/sim.data$SSB_0, type= 'l', ylim = c(0,1.5), lwd = 3)
lines(rep(0.1, length.out = 60), lty = 2, col = 'red')
lines(rep(0.4, length.out = 60), lty = 2, col = 'blue')

# calculate Fmsy 


yea




