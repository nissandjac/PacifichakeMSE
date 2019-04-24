###### Initialize the operating model ###### 
library(TMB)
compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))

# Set the seed
seedz <- 12345
set.seed(seedz)

source('load_files.R')

parms.true <- getParameters(TRUE) # Load parameters from assessment

df <- load_data_seasons(move = TRUE, nseason = 4, nspace = 2) # Prepare data for operating model

df$Catch <- Catch.obs$Fishery # Add the observed catch

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
#
for (i in 1:nruns){
  tmp <- try(run_multiple_MSEs(simyears = 30,
                           seed = seeds[i],
                           moveparms = NA,
                           TAC = 1))
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
save(ls.save,file = 'results/MSErun_move_JTC.Rdata')

# ### Loop MSE's with different errors in future survey and recruitment
ls.save <- list()
ls.converge <- matrix(0, nruns)
#
for (i in 1:nruns){
  tmp <- try(run_multiple_MSEs(simyears = 30, seeds[i],moveparms = NA,
                               TAC = 2),silent = FALSE)
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
save(ls.save,file = 'results/MSErun_move_JMC.Rdata')

ls.save <- list()
ls.converge <- matrix(0, nruns)

for (i in 1:nruns){
  tmp <- try(run_multiple_MSEs(simyears = 30, seeds[i],moveparms = NA,
                               TAC = 3),silent = FALSE)
  #tmp <- run_multiple_MSEs(simyears = 30, seeds[i])
  print(i)
  #tmp <- run_multiple_MSEs(simyears = 30, seeds[i])
  if(is.list(tmp)){
    ls.save[[i]] <-tmp
    ls.converge[i] <- 1
  }else{
    ls.save[[i]] <- NA
    ls.converge[i] <- 0
  }
  
  
  
}
# # # # 
save(ls.save,file = 'results/MSErun_move_realized.Rdata')

ls.save <- list()
ls.converge <- matrix(0, nruns)
for (i in 1:nruns){
  tmp <- try(run_multiple_MSEs(simyears = 30, seeds[i],moveparms = c(0.1,5),
                               TAC = 2),silent = FALSE)
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
save(ls.save,file = 'results/MSErun_move_realized_move1.Rdata')

for (i in 1:nruns){
  tmp <- try(run_multiple_MSEs(simyears = 30, seeds[i],moveparms = c(0.75,5),
                               TAC = 2),silent = FALSE)
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
save(ls.save,file = 'results/MSErun_move_realized_move2.Rdata')


for (i in 1:nruns){
  tmp <- try(run_multiple_MSEs(simyears = 30, seeds[i],moveparms = c(0.5,2), TAC = 2),silent = FALSE)
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
save(ls.save,file = 'results/MSErun_move_realized_move3.Rdata')


