direc <- "~/GitHub/PacifichakeMSE/Spatial MSE_vs2/Model condition/"
setwd(direc)
###### Initialize the operating model ###### 
library(dplyr)
library(reshape2)
library(ggplot2)
library(TMB)
seedz <- 125
set.seed(seedz)

plot.figures = FALSE # Set true for printing to file 
# Run the simulation model
source('run_agebased_model_true_Catch.R')
source('ylimits.R')
source('getSelec.R')
source('load_data_seasons.R')
source('getParameters.R')
source('parameters_TRUE.R')
source('getSSB0.R')
source('runAssessment.R')

source('load_data.R')
source('getParameters.R')
source('runAssessment.R')

# if(sum(parms$F0) == 0){
#   parms$F0 <- parms$F0+0.001 # Make sure it's over 1
# }
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]
Catch.obs <- read.csv('hake_totcatch.csv')
df.new <- load_data()
ass.tmb <- runAssessment(df.new)

df <- load_data_seasons(move = TRUE)

df$Catch <- Catch.obs$Fishery
time <- 1
yrinit <- df$nyear
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 
# df$parms$initN <- df$parms$initN*0
# df$parms$Rin <- df$parms$Rin*0
# df$F0 <- 0*df$F0

sim.data <- run.agebased.true.catch(df)

plot(df$years,rowSums(sim.data$SSB))
lines(assessment$year,assessment$SSB, col = 'red')
lines(df.new$years,ass.tmb$SSB, col = 'green')
# 
# Z.sim <- apply(sim.data$Z,c(1,2),FUN = sum)
# plot(df$years,ass.tmb$Z[21,])
# lines(df$years,Z.sim[21,])
# 
# plot(df$years,sim.data$N.save[1,], log = 'y')
#lines(df$years,ass.tmb$R)
# lines(df$years,assessment$R,col = 'red')
#lines(ass.tmb$Ninit)
SSB0_ass  <- 

plot(df$years,rowSums(sim.data$SSB)/sum(sim.data$SSB0))
lines(assessment$year,assessment$SSB/ass.tmb$SSBzero)