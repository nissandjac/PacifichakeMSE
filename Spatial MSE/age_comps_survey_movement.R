direc <- "~/GitHub/PacifichakeMSE/Spatial MSE_vs3/Model condition/"
setwd(direc)
###### Initialize the operating model ###### 
library(TMB)
library(dplyr)
library(reshape2)
library(ggplot2)
seedz <- 125
set.seed(seedz)

plot.figures = FALSE # Set true for printing to file 
# Run the simulation model
source('run_agebased_model_true_Catch.R')
source('ylimits.R')
source('plotUncertainty.R')
source('getUncertainty.R')
source('getSelec.R')
source('load_data_movement.R')
source('create_TMB_data.R')
source('getRefpoint_biomass.R')
source('Check_Identifiable_vs2.R')
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]
Catch.obs <- read.csv('hake_totcatch.csv')
## Load the single species model to check the age comp fits 
source('load_data.R')
source('getParameters.R')
source('runAssessment.R')

df.new <- load_data()
# if(sum(parms$F0) == 0){
#   parms$F0 <- parms$F0+0.001 # Make sure it's over 1
# }
reps <- runAssessment(df.new)

maxmove <- seq(0.1,0.7, by = 0.1)


for(k in 1:length(maxmove)){
  df <- load_data_movement(TRUE, maxmove[k])
  df$Catch <- Catch.obs$Fishery
  df$surveyseason <- 2
  time <- 1
  yrinit <- df$nyear
  ### Run the OM and the EM for x number of years in the MSE 
  ### Set targets for harvesting etc 
  
  N0 <- NA
  sim.data <- run.agebased.true.catch(df)
  df.survey <- read.csv('survey_country.csv')
  
  
  # Calculate the am per country in the OM 
  am.can.survey <- sim.data$age_comps_country[,,1]
  am.us.survey <- sim.data$age_comps_country[,,2]
  
  am.mean.surv <- data.frame(year = rep(df.new$years,2), am = NA, Country = rep(c('CAN','USA'), each = length(df.new$years)), move = k)
  
  for(i in 1:length(df.new$years)){
    
    # US first 
    am.tmp <- sim.data$age_comps_country[,i,2]
    
    am.mean.surv[am.mean.surv$year == df.new$years[i] & am.mean.surv$Country == 'USA',]$am <- sum(am.tmp*df.new$age[2:16])
    
    am.tmp <- sim.data$age_comps_country[,i,1]
    am.mean.surv[am.mean.surv$year == df.new$years[i] & am.mean.surv$Country == 'CAN',]$am <- sum(am.tmp*df.new$age[2:16])
  }

  if(k == 1){
    am.save <- am.mean.surv
  }else{
    am.save <- rbind(am.save,am.mean.surv)
  }
  
  }


am.plot <- am.save %>% 
group_by(year,Country) %>% 
  summarise(am.median = mean(am), am.min = min(am), am.max = max(am))
(color = c('red','blue'))

dist.survey <- ggplot(df.survey, aes(x= year, y = am, color = Country))+
  geom_line()+geom_point()+theme_classic()+
  scale_y_continuous(name = 'Average age in survey')+
  geom_line(data = am.plot ,aes(y =am.median ),linetype = 2)+
  geom_ribbon(data = am.plot, aes(y = am.median,ymin = am.min, ymax = am.max, fill = Country), linetype = 0)+
  scale_color_manual(values=c("red", "blue"))+scale_fill_manual(values=c(alpha('red',0.2), alpha('blue',0.2)))

#png('survey_comps.png', width = 12, height = 6, res = 400,units = 'cm')
dist.survey
#dev.off()
