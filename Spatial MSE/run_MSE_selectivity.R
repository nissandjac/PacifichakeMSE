###### Initialize the operating model ###### 
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
#source('run_agebased_model_true_catch_move.R')
source('load_data_seasons_future.R')

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
df_future <- load_data_seasons(movemaxinit = 0.35, movefiftyinit = 6,yr_future = 5)

df_future$selectivity_change <- 0
sim.data_1 <- run.agebased.true.catch(df_future) # Run the operating model until 2018
df_future$selectivity_change <- 1
sim.data_2 <- run.agebased.true.catch(df_future) # Run the operating model until 2018
df_future$selectivity_change <- 2
sim.data_3 <- run.agebased.true.catch(df_future) # Run the operating model until 2018

# Plot the scenarios 
df.plot <- data.frame(selectivity = c(sim.data_1$Fsel[,58,1],sim.data_2$Fsel[,58,1],sim.data_3$Fsel[,58,1],
                                      sim.data_1$Fsel[,58,2],sim.data_2$Fsel[,58,2],sim.data_3$Fsel[,58,2]),
                      country = rep(c('CAN','USA'), each = 3*df$nage),
                      age = rep(df$age, 6),
                      run = rep(rep(c('Conditioned OM','Low US selectivity','2018 selectivity'),each =df$nage),2)
                      )

df.plot$run <- factor(df.plot$run, levels = c('Conditioned OM','Low US selectivity','2018 selectivity'))


p1 <- ggplot(data = df.plot, aes(x = age, y = selectivity, color = country))+theme_classic()+geom_line(size = 1.2)+
  facet_wrap(~run)+scale_x_continuous(limit = c(0,df$age_maxage))+
  scale_color_manual(values = c('darkred','blue4'))
p1


png('results/Selectivity/Selectivity_difference.png',width = 16, height =8, res = 400, unit = 'cm')
p1
dev.off()

# ### Loop MSE's with different errors in future survey and recruitment
ls.save <- list()
ls.converge <- matrix(0, nruns)
#
df <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.5, selectivity_change = 0) # Prepare data for operating model

for (i in 1:nruns){
  tmp <- run_multiple_MSEs(simyears = 30,
                           seeds = seeds[i],
                           TAC = 2, df = df)
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
save(ls.save,file = 'results/Selectivity/MSE_sel1.Rdata')

# ### Loop MSE's with different errors in future survey and recruitment
ls.save <- list()
ls.converge <- matrix(0, nruns)
#
df <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.5, selectivity_change = 1) # Prepare data for operating model

for (i in 1:nruns){
  tmp <- run_multiple_MSEs(simyears = simyears, seeds[i],
                           TAC = 2, df =df)
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
save(ls.save,file = 'results/Selectivity/MSE_sel2.Rdata')

ls.save <- list()
ls.converge <- matrix(0, nruns)
df <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.5, selectivity_change = 2) # Prepare data for operating model

for (i in 1:nruns){
  tmp <- run_multiple_MSEs(simyears = simyears, seeds[i],
                           TAC = 2, df =df)
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
save(ls.save,file = 'results/Selectivity/MSE_sel3.Rdata')

write.csv(seeds,file = 'results/Selectivity/seeds.csv', row.names = FALSE) # Save the seeds
