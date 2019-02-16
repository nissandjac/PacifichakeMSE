###### Initialize the operating model ###### 
library(TMB)
compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))

seedz <- 12345
set.seed(seedz)
# Run the simulation model
source('run_agebased_model_true_Catch.R')


####  Plotting and uncertainty calculation functions #### 
source('ylimits.R')
source('plotUncertainty.R')
source('getUncertainty.R')
source('plotValues.R')
source('run_multiple_MSEs.R')
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
# ls.save <- list()
# ls.converge <- matrix(0, nruns)
# #
# for (i in 61:nruns){
#   tmp <- try(run_multiple_MSEs(simyears = 30, 
#                            seed = seeds[i],
#                            moveparms = NA,
#                            TAC = 1))
#   #tmp <- run_multiple_MSEs(simyears = 30, seeds[i])
#   print(i)
# 
#   if(is.list(tmp)){
#   ls.save[[i]] <-tmp
#   ls.converge[i] <- 1
#   }else{
#     ls.save[[i]] <- NA
#     ls.converge[i] <- 0
#   }
# 
# 
# }
# # # # # 
# save(ls.save,file = 'MSErun_move_JTC.Rdata')

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
save(ls.save,file = 'MSErun_move_JMC.Rdata')

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
save(ls.save,file = 'MSErun_move_realized.Rdata')

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
save(ls.save,file = 'MSErun_move_realized_move1.Rdata')

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
save(ls.save,file = 'MSErun_move_realized_move2.Rdata')


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
save(ls.save,file = 'MSErun_move_realized_move3.Rdata')
###  Plot the true SSB ###
#load('MSErun_nomove.Rdata')
# yr <- 1966:(2017+simyears-1)
# SSB.plot <- data.frame(SSB = rowSums(ls.save[[1]]$SSB)/sum(sim.data$SSB_0), year = yr, run = paste('run',1, sep=''))
# Catch.plot <- data.frame(Catch = ls.save[[1]]$Catch, year = yr, run = paste('run',1, sep=''))
# # AAV.plot  <- data.frame(Catch = ls.save[[1]]$Catch[2:ls.save[[1]]$df$tEnd]-ls.save[[1]]$Catch[1:(ls.save[[1]]$df$tEnd-1])], 
# #                         year = ls.save[[1]]$df$years, run = paste('run',1, sep=''))
# 
# for(i in 2:length(ls.save)){
#   ls.tmp <- ls.save[[i]]  
#   
#   if(is.list(ls.tmp)){
#   SSB.tmp <- data.frame(SSB = rowSums(ls.tmp$SSB)/sum(sim.data$SSB_0), year = yr, run =  paste('run',i, sep=''))
#   SSB.plot <- rbind(SSB.plot,SSB.tmp)
#   
#   Catch.tmp <- data.frame(Catch = ls.tmp$Catch, year = yr, run =  paste('run',i, sep=''))
#   Catch.plot <- rbind(Catch.plot,Catch.tmp)
# 
#   
#     
#   }
# }
# 
# 
# source('hake_objectives.R')
# ll <- hake_objectives(ls.save,sim.data, move = 1)
# 
# ll
# # png('MSE_results.png', width = 16, height = 12, res = 400, unit = 'cm')
# do.call(grid.arrange, ll[[1]])
# # dev.off()
# 
# # write.table(ll[[2]], file = 'performancemetrics.csv', row.names = F)
# 
# # Spagehtti plots 
# 
# 
# length(ls.save)
# 
# plot(rowSums(ls.save[[1]]$SSB)/sum(sim.data$SSB0), type ='l', ylim= c(0,2))
# 
# cols <- 1:length(ls.save)
# for(i in 2:length(ls.save)){
#   
#   lines(rowSums(ls.save[[i]]$SSB)/sum(sim.data$SSB0), col = cols[i])
#   
# }
