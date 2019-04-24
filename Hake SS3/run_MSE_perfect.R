###### Initialize the operating model ###### 
seedz <- 12345
set.seed(seedz)
# Run the simulation model
source('run_agebased_model_true_Catch.R')


####  Plotting and uncertainty calculation functions #### 
source('ylimits.R')
source('plotUncertainty.R')
source('getUncertainty.R')
source('plotValues.R')
source('run_MSEs_perfect.R')
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

df <- load_data_seasons(move = TRUE, nseason = 4, nspace = 2)

df$Catch <- Catch.obs$Fishery
time <- 1
yrinit <- df$nyear
nruns <- 100
seeds <- floor(runif(n = nruns, min = 1, max = 1e6))
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 
#
# df$parms$Rin <- df$parms$Rin*0
# df$F0 <- 0*df$F0

simyears <- 30 # Project 25 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df)
simdata0 <- sim.data # The other one is gonna get overwritten. 

# ### Loop MSE's with different errors in future survey and recruitment
ls.save <- list()
ls.converge <- matrix(0, nruns)
#
for (i in 1:nruns){
  tmp <- try(run_multiple_MSEs_perfect(simyears = 30,
                               seed = seeds[i],
                               moveparms = NA,
                               TAC = 1,
                               survone = 0.5))
  #tmp <- run_multiple_MSEs(simyears = 30, seeds[i])
  print(i)
  
  if(is.list(tmp)){
    ls.save[[i]] <-tmp
    ls.converge[i] <- 1
  }else{
    ls.save[[i]] <- NA
    ls.converge[i] <- 0
  }
  
  
}
# # # #
save(ls.save,file = 'MSErun_move_JTC_05.Rdata')

# ### Loop MSE's with different errors in future survey and recruitment
ls.save <- list()
ls.converge <- matrix(0, nruns)
#
for (i in 1:nruns){
  tmp <- try(run_multiple_MSEs(simyears = 30,
                               seed = seeds[i],
                               moveparms = NA,
                               TAC = 1,
                               survone = 0.2))
  #tmp <- run_multiple_MSEs(simyears = 30, seeds[i])
  print(i)
  
  if(is.list(tmp)){
    ls.save[[i]] <-tmp
    ls.converge[i] <- 1
  }else{
    ls.save[[i]] <- NA
    ls.converge[i] <- 0
  }
  
  
}

save(ls.save,file = 'MSErun_move_JTC_02.Rdata')


