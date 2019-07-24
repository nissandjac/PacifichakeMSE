library(TMB)
compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))
library(r4ss)
mod <- SS_output(paste(getwd(),'/data/', sep =''), printstats=FALSE, verbose = FALSE) # Read the true selectivity 

# Set the seed
seedz <- 12345
set.seed(seedz)

source('load_files.R')
source('load_files_OM.R')
source('run_multiple_MSEs_climate.R')
df <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.5, movemaxinit = 0.5, movefiftyinit =8) # Prepare data for operating model

parms.true <- getParameters_OM(TRUE,df) # Load parameters from assessment

time <- 1
yrinit <- df$nyear
nruns <- 100
seeds <- floor(runif(n = nruns, min = 1, max = 1e6))
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 
#

simyears <- 50 # Project 30 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df) # Run the operating model until 2018

simdata0 <- sim.data # The other one is gonna get overwritten. 

# ### Loop MSE's with different errors in future survey and recruitment
ls.save <- list()
ls.converge <- matrix(0, nruns)


for (i in 1:nruns){
  tmp <- run_multiple_MSEs_climate(simyears = simyears,
                           seeds = seeds[i],
                           TAC = 2, df = df, cincrease = 0.0)
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
save(ls.save,file = 'results/Climate/MSErun_move_JMC_climate_0.Rdata')

# ### Loop MSE's with different errors in future survey and recruitment
ls.save <- list()
ls.converge <- matrix(0, nruns)


for (i in 1:nruns){
  tmp <- run_multiple_MSEs_climate(simyears = simyears,
                                   seeds = seeds[i],
                                   TAC = 2, df = df, cincrease = 0.02)
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
save(ls.save,file = 'results/Climate/MSErun_move_JMC_climate_0_02.Rdata')

# ### Loop MSE's with different errors in future survey and recruitment
ls.save <- list()
ls.converge <- matrix(0, nruns)


for (i in 1:nruns){
  tmp <- run_multiple_MSEs_climate(simyears = simyears,
                                   seeds = seeds[i],
                                   TAC = 2, df = df, cincrease = 0.04)
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
save(ls.save,file = 'results/Climate/MSErun_move_JMC_climate_0_04.Rdata')

