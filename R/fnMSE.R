###### Initialize the operating model ###### 
fnMSE <- function(df, simyears = 30, TAC = 1, seeds = NA, nruns = 100){


# Set the seed
if(is.na(seeds)){
  seeds <- 12345
}else{
  set.seed(seeds)
}

time <- 1
yrinit <- df$nyear
seeds <- floor(runif(n = nruns, min = 1, max = 1e6))
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 
#

year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df) # Run the operating model until 2018

simdata0 <- sim.data # The other one is gonna get overwritten. 


# ### Loop MSE's with different errors in future survey and recruitment
ls.save <- list()
ls.converge <- matrix(0, nruns)
#
for (i in 1:nruns){
  tmp <- try(run_multiple_MSEs(simyears = simyears,
                               seed = seeds[i],
                               TAC = TAC,
                               df))
  #tmp <- run_multiple_MSEs(simyears = 30, seeds[i])
  print(i)
  
  if(is.list(tmp)){
    ls.save[[i]] <-tmp
    ls.converge[i] <- 1
  }else{
    ls.save[[i]] <- NA
    ls.converge[i] <- 0
    print('model not converged')
  }
  
  
}

return(list(MSE = ls.save,OM = sim.data))

}