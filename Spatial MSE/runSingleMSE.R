##### Run the hake MSE with different options 
# Load required packages and files 
require(TMB)
require(ggplot2)
require(dplyr)
require(gridExtra)
require(cowplot)
require(scales)
require(RColorBrewer)

compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))
source('load_files.R')
source('load_files_OM.R')
source('fnMSE.R')


###### Load the data to run the MSE ######
df <- load_data_seasons(nseason = 4, nspace = 2,
                           nsurvey= 2, movemaxinit = 0.5, movefiftyinit = 5) # Prepare data for operating model 


###### Run the MSE using the data above #####
ls <- fnMSE(df, simyears = 50, TAC = 1, nruns = 1, seeds = 12345) # TAC 1) HCR, 2) JTC, 3) Realized catch


# Realized Catch in Canada and the US vs TAC catch