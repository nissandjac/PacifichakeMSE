
###### Initialize the operating model ###### 
library(TMB)
library(ggplot2)
library(dplyr)
library(gridExtra)

compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))

# Run the simulation model
source('run_agebased_model_true_Catch.R')


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

assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

parms.true <- getParameters(TRUE)
Catch.obs <- read.csv('hake_totcatch.csv')