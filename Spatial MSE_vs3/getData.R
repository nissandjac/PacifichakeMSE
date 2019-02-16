###### Initialize the operating model ###### 
# Run the simulation model
source('run_agebased_model_true_Catch.R')

####  Plotting and uncertainty calculation functions #### 
source('getSelec.R') # Calculate hake selectivity

source('load_data_seasons.R') # Loads data for Operating model
source('getRefpoint.R') # Calculate refrence points 
source('getParameters.R')
source('calcSSB0.R')
source('create_TMB_data.R')
source('getUncertainty.R')

#parms.true <- getParameters(TRUE)
Catch.obs <- read.csv('hake_totcatch.csv')
TAC.obs <- read.csv('TAC.csv')

assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]