direc <- "~/GitHub/PacifichakeMSE/Spatial MSE_vs3/Model condition/"
setwd(direc)
###### Initialize the operating model ###### 
library(TMB)
library(dplyr)
library(reshape2)
library(ggplot2)
seedz <- 125
set.seed(seedz)

plot.figures = FALSE # Set true for printing to file 
# Run the simulation model
source('run_agebased_model_true_Catch.R')
source('ylimits.R')
source('plotUncertainty.R')
source('getUncertainty.R')
source('getSelec.R')
source('load_data_seasons.R')
source('create_TMB_data.R')
source('getRefpoint_biomass.R')
source('Check_Identifiable_vs2.R')
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

simyears <- 25 # Project 30 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df)


plot(df$years,sim.data$SSB, type='l')
lines(assessment$year,assessment$SSB, col = 'red')


nreps <- 100
# 
# Lsave <- data.frame(year = df$years, SSB = NA, )
# 
# for(i in 1:nreps){
#   
#   df <- load_data_seasons(move = TRUE)
#   df$Catch <- Catch.obs$Fishery
#   
#   
#   
# }


