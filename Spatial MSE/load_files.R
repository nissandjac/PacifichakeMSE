### Load files and etc 

# Run the simulation model
source('run_agebased_model_true_Catch.R')


####  Plotting and uncertainty calculation functions #### 
source('ylimits.R')
source('plotUncertainty.R')
source('getUncertainty.R')
source('plotValues.R')
source('run_multiple_MSEs.R')
source('getSelec.R') # Calculate hake selectivity

source('load_data_seasons.R') # Loads data for Operating model
source('create_TMB_data.R') # Compiles operating model data to tmb data

source('getRefpoint.R') # Calculate refrence points 
source('Check_Identifiable_vs2.R') # see if hessian is positive definite 

source('getParameters.R')
source('calcSSB0.R')
source('hake_objectives.R')
source('df_lists.R')


assessment <- read.csv('data/asssessment_MLE.csv') ## Read the data from the assesment for comparison 
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]
