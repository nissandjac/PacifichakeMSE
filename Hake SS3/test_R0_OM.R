direc <- "~/GitHub/PacifichakeMSE/Spatial MSE_vs2/Model condition/"
setwd(direc)
###### Initialize the operating model ###### 
library(dplyr)
library(reshape2)
library(ggplot2)
seedz <- 125
set.seed(seedz)

plot.figures = FALSE # Set true for printing to file 
# Run the simulation model
source('run_agebased_model_true_Catch.R')
source('ylimits.R')
source('getSelec.R')
source('load_data_seasons.R')
source('getParameters.R')
source('getSSB0.R')
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]
Catch.obs <- read.csv('hake_totcatch.csv')

df <- load_data_seasons(move = FALSE)
df$Catch <- Catch.obs$Fishery
time <- 1
yrinit <- df$nyear
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 
# df$parms$initN <- df$parms$initN*0
# df$parms$Rin <- df$parms$Rin*0
# df$F0 <- 0*df$F0

sim.data <- run.agebased.true.catch(df)

plot(sim.data$SSB)
lines(assessment$SSB)

parms.true <- getParameters(TRUE)

df.new <- df
df.new$parms <- parms.true 

SSB0.assessment <- getSSB0(df.new)

SSB0.assessment/sim.data$SSB0
