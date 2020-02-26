### Run MSE_all_OM ## 

### Re-run the operating model for the selectivity scenario

library(TMB)
compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))

seedz <- 12345
set.seed(seedz)

source('load_files.R')
source('load_files_OM.R')
source('run_multiple_OMs.R')
# 
df <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.5, selectivity_change = 2) # Prepare data for operating model

time <- 1
yrinit <- df$nyear
nruns <- 100
seeds <- floor(runif(n = nruns, min = 1, max = 1e6))

simyears <- 30 # Project 25 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df)
simdata0 <- sim.data # The other one is gonna get overwritten. 

folders <- 'results/Selectivity/'

dts <- dir(folders)[grep('.Rdata',x = dir(folders))]


load(paste(folders,dts[1], sep = ''))
df.1 <- ls.save

OM.1 <- list()


for(i in 1:nruns){
OM.1[[i]] <- run_multiple_OMs(simyears,seed = seeds[i],df = df,Catchin = df.1[[i]]$Catch )

}
