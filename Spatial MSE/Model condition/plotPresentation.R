######  Plots for presentations ########

library(ggplot2)
library(dplyr)
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

assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]
Catch.obs <- read.csv('hake_totcatch.csv')


png('stockrecruitment.png', width = 16, height = 10, res = 400, unit = 'cm')
ggplot(assessment, aes(x = SSB/1e6, y = R/1e6))+geom_point()+scale_y_continuous('Recruits (millions)')+
  scale_x_continuous('SSB (million tonnes)')+theme_classic()
dev.off()



### Run the operating model 
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
lines(assessment$year,assessment$SSB)

plot(sim.data$SSB.all[,2,1]*1e-6, ylim = c(0,2), type = 'l', col = 'red')
lines(sim.data$SSB.all[,2,2]*1e-6, col = 'blue')



