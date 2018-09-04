### Run the OM in X amount of years 

direc <- "~/GitHub/PacifichakeMSE/Spatial MSE/Model condition/"
setwd(direc)
###### Initialize the operating model ###### 
library(TMB)
library(dplyr)
library(reshape2)
library(ggplot2)

plot.figures = FALSE # Set true for printing to file 
# Run the simulation model
source('run_agebased_model_true_Catch.R')
source('ylimits.R')
source('plotUncertainty.R')
source('getUncertainty.R')
source('getSelec.R')
source('load_data_seasons_future.R')
source('create_TMB_data.R')
source('getRefpoint_biomass.R')
source('Check_Identifiable_vs2.R')
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]
Catch.obs <- read.csv('hake_totcatch.csv')


seedz <- floor(runif(1, 1,1e6))  # Random of a random 
yr.future <- 50

df <- load_data_seasons_future(yr.future)
df$Catch <- Catch.obs$Fishery
time <- 1
yrinit <- df$nyear

df$Catch <- c(df$Catch, rep(mean(df$Catch), yr.future))

sim.data <- run.agebased.true.catch(df,seed =  seedz)

plot(df$years,rowSums(sim.data$SSB))


