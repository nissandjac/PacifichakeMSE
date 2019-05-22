## Load the required scripts and packages 
require(TMB)
#library(TMBdebug)
require(ggplot2)


source('plotValues.R')
source('getUncertainty.R')
source('ylimits.R')
source('getParameters.R')
source('Check_Identifiable_vs2.R')
source('getParms_noAge.R')
source('load_data_seasons.R')
source('run_agebased_model_true_Catch.R')
# Run the simulation model
source('plotUncertainty.R')
source('getSelec.R')
source('create_TMB_data.R')
source('getRefpoint_biomass.R')
source('Check_Identifiable_vs2.R')
source('getF.R')
source('load_data.R')