library(TMB)
library(r4ss)
library(devtools)
library(PacifichakeMSE)

mod <- SS_output('inst/extdata/SS32018', printstats=FALSE, verbose = FALSE) # Read the true selectivity

# Set the seed
seedz <- 12345
set.seed(seedz)

df <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.5) # Prepare data for operating model

parms.true <- getParameters_OM(TRUE,mod, df) # Load parameters from assessment

time <- 1
yrinit <- df$nyear
nruns <- 500

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
  tmp <- run_multiple_MSEs(simyears = simyears,
                           seeds = seeds[i],
                           TAC = 1, df = df, cincrease = 0, mincrease = 0)
  print(i)

  if(is.list(tmp)){
    ls.save[[i]] <-tmp
    ls.converge[i] <- 1
  }else{
    ls.save[[i]] <- NA
    ls.converge[i] <- 0
  }


}

# # # # #
# save(ls.save,file = 'results/Climate/MSErun_move_JMC_climate_0_HYBR_TAC1.Rdata')
#
# # ### Loop MSE's with different errors in future survey and recruitment
# ls.save <- list()
# ls.converge <- matrix(0, nruns)
#
#
# for (i in 1:nruns){
#   tmp <- run_multiple_MSEs(simyears = simyears,
#                                    seeds = seeds[i],
#                                    TAC = 1, df = df, cincrease = 0.02, mincrease = 0.005)
#   #tmp <- run_multiple_MSEs(simyears = 30, seeds[i])
#   print(i)
#
#   if(is.list(tmp)){
#     ls.save[[i]] <-tmp
#     ls.converge[i] <- 1
#   }else{
#     ls.save[[i]] <- NA
#     ls.converge[i] <- 0
#   }
#
#
# }
# # # # #
# save(ls.save,file = 'results/Climate/MSErun_move_JMC_climate_0_02_HYBR_TAC1.Rdata')

# # ### Loop ls.save <- list()
# ls.save <- list()
# ls.converge <- matrix(0, nruns)
#
# for (i in 156:nruns){
#   tmp <- run_multiple_MSEs(simyears = simyears,
#                                    seeds = seeds[i],
#                                    TAC = 1, df = df, cincrease = 0.04, mincrease = 0.02)
#   #tmp <- run_multiple_MSEs(simyears = 30, seeds[i])
#   print(i)
#
#   if(is.list(tmp)){
#     ls.save[[i]] <-tmp
#     ls.converge[i] <- 1
#   }else{
#     ls.save[[i]] <- NA
#     ls.converge[i] <- 0
#   }
#
#
# }
# # # # #
# save(ls.save,file = 'results/Climate/MSErun_move_JMC_climate_0_04_HYBR_TAC1_ix_156_500.Rdata')
#
#
ls.save <- list()
ls.converge <- matrix(0, nruns)


for (i in 203:473){
  tmp <- run_multiple_MSEs(simyears = simyears,
                           seeds = seeds[i],
                           TAC = 2, df = df, cincrease = 0, mincrease = 0)
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
save(ls.save,file = 'results/Climate/MSErun_move_JMC_climate_0_HYBR_TAC2_474_1.Rdata')
# #
# ### Loop MSE's with different errors in future survey and recruitment
# ls.save <- list()
# ls.converge <- matrix(0, nruns)
#
#
# for (i in 1:130){
# tmp <- run_multiple_MSEs(simyears = simyears,
#                          seeds = seeds[i],
#                          TAC = 2, df = df, cincrease = 0.02, mincrease = 0.005)
# #tmp <- run_multiple_MSEs(simyears = 30, seeds[i])
# print(i)
#
# if(is.list(tmp)){
#   ls.save[[i]] <-tmp
#   ls.converge[i] <- 1
# }else{
#   ls.save[[i]] <- NA
#   ls.converge[i] <- 0
# }
#
#
# }
# # # # #
# save(ls.save,file = 'results/Climate/MSErun_move_JMC_climate_0_02_HYBR_TAC2_130.Rdata')
#
#
# #
# # ### Loop ls.save <- list()
# ls.save <- list()
# ls.converge <- matrix(0, nruns)
#
# for (i in 285:nruns){
#   tmp <- run_multiple_MSEs(simyears = simyears,
#                                    seeds = seeds[i],
#                                    TAC = 2, df = df, cincrease = 0.04, mincrease = 0.02)
#   #tmp <- run_multiple_MSEs(simyears = 30, seeds[i])
#   print(i)
#
#   if(is.list(tmp)){
#     ls.save[[i]] <-tmp
#     ls.converge[i] <- 1
#   }else{
#     ls.save[[i]] <- NA
#     ls.converge[i] <- 0
#   }
#
#
# }
# # # # #
# save(ls.save,file = 'results/Climate/MSErun_move_JMC_climate_0_04_HYBR_TAC2_285-500.Rdata')


### Loop MSE's with different errors in future survey and recruitment
ls.save <- list()
ls.converge <- matrix(0, nruns)


for (i in 292:396){
  tmp <- run_multiple_MSEs(simyears = simyears,
                           seeds = seeds[i],
                           TAC = 3, df = df, cincrease = 0, mincrease = 0)
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
save(ls.save,file = 'results/Climate/MSErun_move_JMC_climate_0_HYBR_TAC3_292-396.Rdata')

rm(ls.save)
# # ### Loop MSE's with different errors in future survey and recruitment
# ls.save <- list()
# ls.converge <- matrix(0, nruns)
#
#
# for (i in 283:nruns){
#   tmp <- run_multiple_MSEs(simyears = simyears,
#                                    seeds = seeds[i],
#                                    TAC = 3, df = df, cincrease = 0.02, mincrease = 0.005)
#   #tmp <- run_multiple_MSEs(simyears = 30, seeds[i])
#   print(i)
#
#   if(is.list(tmp)){
#     ls.save[[i]] <-tmp
#     ls.converge[i] <- 1
#   }else{
#     ls.save[[i]] <- NA
#     ls.converge[i] <- 0
#   }
#
#
# }
# # # # #
# save(ls.save,file = 'results/Climate/MSErun_move_JMC_climate_0_02_HYBR_TAC3_284-500.Rdata')

# ### Loop ls.save <- list()
ls.save <- list()
ls.converge <- matrix(0, nruns)

for (i in 271:nruns){
  tmp <- run_multiple_unfished(simyears = simyears,
                                   seeds = seeds[i],
                                   TAC = 3, df = df, cincrease = 0.04, mincrease = 0.02)
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
save(ls.save,file = 'results/Climate/MSErun_move_JMC_climate_0_04_HYBR_TAC3_271-500.Rdata')


