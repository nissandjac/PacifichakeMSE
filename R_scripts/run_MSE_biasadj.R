library(TMB)
compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))
library(r4ss)
mod <- SS_output(paste(getwd(),'/data/SS32018', sep =''), printstats=FALSE, verbose = FALSE) # Read the true selectivity 

# Set the seed
seedz <- 12345
set.seed(seedz)

source('load_files.R')
source('load_files_OM.R')
source('run_multiple_OMs.R')
nruns <- 1000
seeds <- floor(runif(n = nruns, min = 1, max = 1e6))

ls.save <- list()
ls.converge <- matrix(0, nruns)
df <- load_data_seasons(nseason = 4, nspace = 2) # Prepare data for operating model
df$bfuture <- 0


for (i in 1:nruns){
  tmp <- run_multiple_OMs(simyears = 50, seeds[i], df =df, Catchin =0)
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
save(ls.save,file = 'results/bias adjustment/MSErun_move_nofishing_nobiasadj.Rdata')

ls.save <- list()
ls.converge <- matrix(0, nruns)
df <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.87) # Prepare data for operating model
source('run_multiple_OMs.R')


for (i in 1:nruns){
  tmp <- run_multiple_OMs(simyears = 50, seeds[i], df =df, Catchin =0)
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
save(ls.save,file = 'results/bias adjustment/MSErun_move_nofishing_biasadj.Rdata')

ls.save <- list()
ls.converge <- matrix(0, nruns)
df <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.5) # Prepare data for operating model


for (i in 1:nruns){
  tmp <- run_multiple_OMs(simyears = 50, seeds[i], df =df, Catchin =0)
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
save(ls.save,file = 'results/bias adjustment/MSErun_move_nofishing_biasadj_med.Rdata')

