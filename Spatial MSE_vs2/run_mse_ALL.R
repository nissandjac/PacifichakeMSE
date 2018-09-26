### Run a bunch of MSE's for the future - uncertainty in Recruitment and survey 

setwd("~/GitHub/PacifichakeMSE/Spatial MSE_vs2")

###### Initialize the operating model ###### 
library(TMB)
compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))

# Run the simulation model
source('run_agebased_model_true_Catch.R')


####  Plotting and uncertainty calculation functions #### 
source('ylimits.R')
source('plotUncertainty.R')
source('getUncertainty.R')
source('plotValues.R')

source('getSelec.R') # Calculate hake selectivity

source('load_data_seasons.R') # Loads data for Operating model
source('create_TMB_data.R') # Compiles operating model data to tmb data

source('getRefpoint.R') # Calculate refrence points 
source('Check_Identifiable_vs2.R') # see if hessian is positive definite 

source('getParameters.R')
source('calcSSB0.R')
source('run_multiple_MSEs.R')


nruns <- 100

seeds <- ceiling(runif(n = nruns, min = 1, 1e6))
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

#parms.true <- getParameters(TRUE)
Catch.obs <- read.csv('hake_totcatch.csv')


### Loop MSE's with different errors in future survey and recruitment 
ls.save <- list()
ls.converge <- matrix(0, nruns)

for (i in 1:nruns){ 
  tmp <- try(run_multiple_MSEs(simyears = 30, seeds[i]),silent = TRUE)
  print(i)
  
  if(is.list(tmp)){
  ls.save[[i]] <-tmp
  ls.converge[i] <- 1
  }
  
  
}

save(ls.save,file = 'MSErun.Rdata')
###  Plot the true SSB ###
SSB.plot <- data.frame(SSB = rowSums(ls.save[[1]]$SSB), year = ls.save[[1]]$df$years, run = paste('run',1, sep=''))
Catch.plot <- data.frame(Catch = ls.save[[1]]$Catch, year = ls.save[[1]]$df$years, run = paste('run',1, sep=''))

for(i in 2:nruns){
  ls.tmp <- ls.save[[i]]  
  
  if(is.list(ls.tmp)){
  SSB.tmp <- data.frame(SSB = rowSums(ls.tmp$SSB), year = ls.tmp$df$years, run =  paste('run',i, sep=''))
  SSB.plot <- rbind(SSB.plot,SSB.tmp)
  
  Catch.tmp <- data.frame(Catch = ls.tmp$Catch, year = ls.tmp$df$years, run =  paste('run',i, sep=''))
  Catch.plot <- rbind(Catch.plot,Catch.tmp)
  
  }
}

library(ggplot2)
library(dplyr)
library(gridExtra)

p1 <- SSB.plot %>% 
  group_by(year) %>% 
  summarise(med = median(SSB), 
            p95 = quantile(SSB, 0.95),
            p25 = quantile(SSB, 0.25),
            p75 = quantile(SSB, 0.75), 
            p5 = quantile(SSB,0.05)) %>% 
  ggplot(aes(x= year,y = med)) +
  geom_ribbon(aes(ymin = p5, ymax = p95), fill = alpha('gray', alpha =0.5))+
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = alpha('gray', alpha =0.8))+
  theme_classic()+scale_y_continuous(name = 'SSB')+
  geom_line(color="black", size = 1.5)#+geom_line(data = SSB.plot, aes(y = SSB,group = run), color = alpha('black', alpha = 0.2))


p2 <- Catch.plot %>% 
  group_by(year) %>% 
  summarise(med = median(Catch), 
            p95 = quantile(Catch, 0.95),
            p25 = quantile(Catch, 0.25),
            p75 = quantile(Catch, 0.75), 
            p5 = quantile(Catch,0.05)) %>% 
  ggplot(aes(x= year,y = med)) +
  geom_ribbon(aes(ymin = p5, ymax = p95), fill = alpha('gray', alpha =0.5))+
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = alpha('gray', alpha =0.8))+
  theme_classic()+scale_y_continuous(name = 'Catch')+
  geom_line(color="black", size = 1.5)#+geom_line(data = Catch.plot, aes(y = Catch,group = run), color = alpha('black', alpha = 0.2))

grid.arrange(p1,p2)


