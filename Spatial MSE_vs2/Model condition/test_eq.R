### Run the OM in X amount of years 

direc <- "~/GitHub/PacifichakeMSE/Spatial MSE_vs2/Model condition/"
setwd(direc)
###### Initialize the operating model ###### 
library(TMB)
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(gridExtra)

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

nruns <- 200

seedz <- floor(runif(n = nruns,min = 1, max = 1e6))  # Random of a random 
yr.future <- 200
df <- load_data_seasons_future(yr.future)

df$Catch <- Catch.obs$Fishery
Catch.future <- c(df$Catch, rep(0.25*1e6, yr.future))
df$Catch <- Catch.future
df$parms$Rin[df$years>2017] <- 0

sim.data <- run.agebased.true.catch(df,seed = 150)

plot(rowSums(sim.data$SSB)/sum(sim.data$SSB0))
