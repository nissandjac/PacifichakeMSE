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
# assessment <- read.csv('asssessment_MLE.csv')
# assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

#parms.true <- getParameters(TRUE)
#Catch.obs <- read.csv('hake_totcatch.csv')

df <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.5, movemaxinit = 0.5, movefiftyinit =8) # Prepare data for operating model

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

#######################################
 
ls.real.s <- list()
for (i in 1:nruns){
  if(is.na(ls.Realized[[i]])){
    ls.real.s[[i]] <- NULL
  }else{
    tmp <- run_multiple_OMs(simyears = 30, seeds[i],df, Catchin = ls.Realized[[i]]$Catch[(df$nyear+1):(length(year.future)-1)])
    ls.real.s[[i]] <- tmp
  }
}
save(ls.real.s,file = 'results/Operating models/realized_OM.RData')

ls.move1.s <- list()
df <- load_data_seasons(nseason = 4, nspace = 2,movemaxinit = 0.15, movefiftyinit = 5) # Prepare data for operating model

for (i in 1:length(ls.move1)){
  if(is.na(ls.move1[[i]])){
    ls.move1.s[[i]] <- NULL}
  else{
    tmp <- run_multiple_OMs(simyears = 30, seeds[i],df, Catchin = ls.move1[[i]]$Catch[(df$nyear+1):(length(year.future)-1)])
    ls.move1.s[[i]] <- tmp
  }
}
save(ls.move1.s,file = 'results/Operating models/move1_OM.RData')
# 
ls.move2.s <- list()
df <- load_data_seasons(nseason = 4, nspace = 2,movemaxinit = 0.3, movefiftyinit = 5) # Prepare data for operating model

for (i in 1:length(ls.move2)){
  if(is.na(ls.move2[[i]])){
    ls.move2.s[[i]] <- NULL}
  else{
    tmp <- run_multiple_OMs(simyears = 30,
                            seed = seeds[i],df,
                            Catchin = ls.move2[[i]]$Catch[(df$nyear+1):(length(year.future)-1)])
    ls.move2.s[[i]] <- tmp
  }
}
save(ls.move2.s,file = 'results/Operating models/move2_OM.RData')
# 

ls.move3.s <- list()
df <- load_data_seasons(nseason = 4, nspace = 2,movemaxinit = 0.05, movefiftyinit = 2) # Prepare data for operating model

for (i in 1:length(ls.move3)){
  if(is.na(ls.move3[[i]])){
    ls.move3.s[[i]] <- NULL}
  else{
    tmp <- run_multiple_OMs(simyears = 30, seeds[i],df, Catchin = ls.move3[[i]]$Catch[(df$nyear+1):(length(year.future)-1)])
    ls.move3.s[[i]] <- tmp
  }
}

save(ls.move3.s,file = 'results/Operating models/move3_OM.RData')
