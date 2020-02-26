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
source('run_agebased_model_true_catch_move.R')

df <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.5) # Prepare data for operating model

parms.true <- getParameters_OM(TRUE,df) # Load parameters from assessment

time <- 1
yrinit <- df$nyear
nruns <- 100
seeds <- floor(runif(n = nruns, min = 1, max = 1e6))
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 
#

simyears <- 30 # Project 30 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df) # Run the operating model until 2018

simdata0 <- sim.data # The other one is gonna get overwritten. 

# ### Loop MSE's with different errors in future survey and recruitment
ls.save <- list()
ls.converge <- matrix(0, nruns)


ls.save <- list()
ls.converge <- matrix(0, nruns)
df <- load_data_seasons(nseason = 4, nspace = 2,movemaxinit = 0.15, movefiftyinit = 5) # Prepare data for operating model

for (i in 1:nruns){
  tmp <- try(run_multiple_MSEs(simyears = simyears, seeds[i],
                               TAC = 2, df =df),silent = FALSE)
  #tmp <- run_multiple_MSEs(simyears = 30, seeds[i])
  print(i)
  
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
save(ls.save,file = 'results/Move/MSErun_move_realized_move1.Rdata')

ls.save <- list()
ls.converge <- matrix(0, nruns)

df <- load_data_seasons(nseason = 4, nspace = 2,movemaxinit = 0.4, movefiftyinit = 8) # Prepare data for operating model
test <- run.agebased.true.catch(df)


for (i in 1:nruns){
  tmp <- try(run_multiple_MSEs(simyears = simyears, seeds[i],
                               TAC = 2, df =df),silent = FALSE)
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
save(ls.save,file = 'results/Move/MSErun_move_realized_move2.Rdata')

ls.save <- list()
ls.converge <- matrix(0, nruns)
df <- load_data_seasons(nseason = 4, nspace = 2,movemaxinit = 0.5, movefiftyinit = 4) # Prepare data for operating model
test <- run.agebased.true.catch(df)

for (i in 1:nruns){
  tmp <- try(run_multiple_MSEs(simyears = simyears, seeds[i], TAC = 2, df =df),silent = FALSE)
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
save(ls.save,file = 'results/Move/MSErun_move_realized_move3.Rdata')