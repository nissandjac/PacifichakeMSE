### Run MSE_all_OM ## 

### Re-run the operating models for the 100 simulations #### 

library(TMB)
compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))

seedz <- 12345
set.seed(seedz)

source('load_files.R')
source('load_files_OM.R')
source('run_multiple_OMs.R')
# 
df <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.5, movemaxinit = 0.5, movefiftyinit =8) # Prepare data for operating model

time <- 1
yrinit <- df$nyear
nruns <- 100

simyears <- 30 # Project 25 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df)
simdata0 <- sim.data # The other one is gonna get overwritten. 

folders <- 'results/Selectivity/'
