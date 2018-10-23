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

df <- load_data_seasons()
df$Catch <- Catch.obs$Fishery
time <- 1
yrinit <- df$nyear
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 
#
# df$parms$Rin <- df$parms$Rin*0
# df$F0 <- 0*df$F0

simyears <- 30 # Project 25 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df)
# 
# ### Loop MSE's with different errors in future survey and recruitment 
# ls.save <- list()
# ls.converge <- matrix(0, nruns)
# 
# for (i in 2:nruns){ 
#   tmp <- try(run_multiple_MSEs(simyears = 30, seeds[i]),silent = TRUE)
#   print(i)
#   
#   if(is.list(tmp)){
#   ls.save[[i]] <-tmp
#   ls.converge[i] <- 1
#   }
#   
#   
# }
# 
# save(ls.save,file = 'MSErun.Rdata')
###  Plot the true SSB ###
load('MSErun.Rdata')
SSB.plot <- data.frame(SSB = rowSums(ls.save[[1]]$SSB)/sum(sim.data$SSB_0), year = ls.save[[1]]$df$years, run = paste('run',1, sep=''))
Catch.plot <- data.frame(Catch = ls.save[[1]]$Catch, year = ls.save[[1]]$df$years, run = paste('run',1, sep=''))
# AAV.plot  <- data.frame(Catch = ls.save[[1]]$Catch[2:ls.save[[1]]$df$tEnd]-ls.save[[1]]$Catch[1:(ls.save[[1]]$df$tEnd-1])], 
#                         year = ls.save[[1]]$df$years, run = paste('run',1, sep=''))

for(i in 2:nruns){
  ls.tmp <- ls.save[[i]]  
  
  if(is.list(ls.tmp)){
  SSB.tmp <- data.frame(SSB = rowSums(ls.tmp$SSB)/sum(sim.data$SSB_0), year = ls.tmp$df$years, run =  paste('run',i, sep=''))
  SSB.plot <- rbind(SSB.plot,SSB.tmp)
  
  Catch.tmp <- data.frame(Catch = ls.tmp$Catch, year = ls.tmp$df$years, run =  paste('run',i, sep=''))
  Catch.plot <- rbind(Catch.plot,Catch.tmp)

  
    
  }
}

library(ggplot2)
library(dplyr)
library(gridExtra)

SSB.plotquant <- SSB.plot %>% 
  group_by(year) %>% 
  summarise(med = median(SSB), 
            p95 = quantile(SSB, 0.95),
            p25 = quantile(SSB, 0.25),
            p75 = quantile(SSB, 0.75), 
            p5 = quantile(SSB,0.05)) 
  
p1 <- ggplot(data=SSB.plotquant, aes(x= year,y = med)) +
  geom_ribbon(aes(ymin = p5, ymax = p95), fill = alpha('gray', alpha =0.5))+
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = alpha('gray', alpha =0.8))+
  theme_classic()+scale_y_continuous(name = 'SSB')+
  geom_line(color="black", size = 1.5)#+geom_line(data = SSB.plot, aes(y = SSB,group = run), color = alpha('black', alpha = 0.2))


Catch.plotquant <- Catch.plot %>% 
  group_by(year) %>% 
  summarise(med = median(Catch), 
            p95 = quantile(Catch, 0.95),
            p25 = quantile(Catch, 0.25),
            p75 = quantile(Catch, 0.75), 
            p5 = quantile(Catch,0.05))

p2 <-  ggplot(Catch.plotquant,aes(x= year,y = med)) +
  geom_ribbon(aes(ymin = p5, ymax = p95), fill = alpha('gray', alpha =0.5))+
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = alpha('gray', alpha =0.8))+
  theme_classic()+scale_y_continuous(name = 'Catch')+
  geom_line(color="black", size = 1.5)#+geom_line(data = Catch.plot, aes(y = Catch,group = run), color = alpha('black', alpha = 0.2))

cairo_pdf(filename = 'MSE_run.pdf')
grid.arrange(p1,p2)
dev.off()
###  Plot the performance metrics from Kristins spreadsheet 

## Probability of S < S10
SSB.future <- SSB.plot[SSB.plot$year > 2018,]

print(paste('percentage of years where SSB < 0.1SSB0= ',
            round(length(which(SSB.future$SSB<0.1))/length(SSB.future$SSB)*100, digits = 2),'%', sep = ''))
print(paste('percentage of years where SSB > 0.1SSB0 & SSB < 0.4SSB0 = ',
            round(length(which(SSB.future$SSB>0.1 & SSB.future$SSB<0.4))/length(SSB.future$SSB)*100, digits = 2),'%', sep = ''))
print(paste('percentage of years where SSB > 0.4SSB0 = ',
            round(length(which(SSB.future$SSB>0.4))/length(SSB.future$SSB)*100, digits = 2),'%', sep = ''))


Catch.future <- Catch.plot[Catch.plot$year > 2018,]

print(paste('percentage of Catch  < 180000= ',
            round(length(which(Catch.future$Catch < 180000))/length(Catch.future$Catch)*100, digits = 2),'%', sep = ''))

print(paste('percentage of Catch  > 180k and < 350k= ',
            round(length(which(Catch.future$Catch > 180000 & Catch.future$Catch < 350000))/length(Catch.future$Catch)*100, digits = 2),'%', sep = ''))

print(paste('percentage of Catch > 350k= ',
            round(length(which(Catch.future$Catch > 350000))/length(Catch.future$Catch)*100, digits = 2),'%', sep = ''))

# Catch variability 
