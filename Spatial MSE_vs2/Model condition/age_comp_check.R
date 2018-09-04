direc <- "~/GitHub/PacifichakeMSE/Spatial MSE/Check age comps"
setwd(direc)
###### Initialize the operating model ###### 
library(TMB)
library(dplyr)
library(reshape2)
library(ggplot2)
seedz <- 125
set.seed(seedz)
# Run the simulation model
source('run_agebased_model_true_Catch.R')
source('ylimits.R')
source('plotUncertainty.R')
source('getUncertainty.R')
source('getSelec.R')
source('load_data_seasons.R')
source('create_TMB_data.R')
source('getRefpoint_biomass.R')
source('Check_Identifiable_vs2.R')
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]
Catch.obs <- read.csv('hake_totcatch.csv')

df <- load_data_seasons()
df$Catch <- Catch.obs$Fishery
time <- 1
yrinit <- df$nyear

N0 <- NA
sim.data <- run.agebased.true.catch(df)

# Compare with all the data 
par(mfrow = c(2,2), mar = c(4,4,1,1))
plot(df$years,rowSums(sim.data$SSB), type = 'l', lwd = 2, ylab ='SSB')
lines(assessment$year,assessment$SSB, lwd = 2, col = 'red')

# How do we fit to the data 
yl <- ylimits(df$survey[df$survey > 1],sim.data$survey[sim.data$survey > 1])

plot(df$years[df$survey > 1],df$survey[df$survey > 1], lwd = 2, col = 'red', ylim = yl, ylab= 'survey')
points(df$years[df$survey > 1],sim.data$survey[sim.data$survey > 1], col = 'black',lwd = 2)
## Add the error bars 
# arrows(df$years[df$survey > 1],df$survey[df$survey > 1]+exp(exp(df$parms$logSDsurv)+exp(df$survey_err[df$survey > 1])), 
#        df$years[df$survey > 1], df$survey[df$survey > 1]-exp(exp(df$parms$logSDsurv)+exp(df$survey_err[df$survey > 1])), 
#        length=0.05, angle=90, code=3)


# plot(df$years,df$Catch, type = 'l', lwd = 2, col = 'red')
# # Plot the overall average age 

age.comps <- sim.data$age_comps_OM[,2:df$nyear,,2]
age.comps <- apply(age.comps,c(1,2),sum)/2

am <- matrix(NA, df$nyear)
for(i in 1:(df$nyear-1)){
  am[i] <- sum(df$age*age.comps[,i])
  
}

age.comps.can <- sim.data$age_comps_OM[,2:df$nyear,1,3]
am.can <- matrix(NA, df$nyear)
for(i in 1:(df$nyear-1)){
  am.can[i] <- sum(df$age*age.comps.can[,i])
  
}

age.comps.US <- sim.data$age_comps_OM[,2:df$nyear,2,3]
am.US <- matrix(NA, df$nyear)
for(i in 1:(df$nyear-1)){
  am.US[i] <- sum(df$age*age.comps.US[,i])
  
}


plot(df$years,am, type ='l', lwd = 2, ylab = 'average age')
lines(df$years,am.can, type ='l', col = 'red', lwd = 2)
lines(df$years,am.US, type = 'l', col = 'blue', lwd = 2)
# Put it in a data frame for ggplot 
df.am <- data.frame(year = rep(df$years,3), 
                    am = c(am,am.can,am.US), 
                    Country = rep(c('All','Can','US'), each = length(df$years)))

age.catch <- sim.data$Catch.save.age
age.catch.all <- apply(age.catch,c(1,2),mean)

am.catch <- matrix(NA, df$nyear)

for(i in 1:(df$nyear-1)){
  ac.tmp <- rep(NA,15)
  ac.tmp[1:14] <- age.catch.all[2:15,i]/sum(age.catch.all[,i])
  ac.tmp[15] <- sum(age.catch.all[16:df$nage,i])/sum(age.catch.all[,i])
  
  am.catch[i] <- sum(df$age[2:16]*ac.tmp)
  
}

age.catch.can <- apply(sim.data$Catch.save.age[,,1,],c(1,2),mean)

am.catch.can <- matrix(NA, df$nyear)

for(i in 1:(df$nyear-1)){
  ac.tmp <- rep(NA,15)
  ac.tmp[1:14] <- age.catch.can[2:15,i]/sum(age.catch.can[,i])
  ac.tmp[15] <- sum(age.catch.can[16:df$nage,i])/sum(age.catch.can[,i])
  
  am.catch.can[i] <- sum(df$age[2:16]*ac.tmp)
  
}

age.catch.US <- apply(sim.data$Catch.save.age[,,2,],c(1,2),mean)

am.catch.US <- matrix(NA, df$nyear)

for(i in 1:(df$nyear-1)){
  ac.tmp <- rep(NA,15)
  ac.tmp[1:14] <- age.catch.US[2:15,i]/sum(age.catch.US[,i])
  ac.tmp[15] <- sum(age.catch.US[16:df$nage,i])/sum(age.catch.US[,i])
  
  am.catch.US[i] <- sum(df$age[2:16]*ac.tmp)
  
}

df.am.catch <- data.frame(year = rep(df$years,3), 
                          am = c(am.catch,am.catch.can,am.catch.US), 
                          Country = rep(c('All','Can','US'), each = length(df$years)))


## See thcoe catch per country
c.country <- read.csv('catch_per_country.csv')

yl <- ylimits(c.country$Canperc,c.country$Total)
plot(c.country$year,c.country$Total, ylim = yl, type = 'l', lwd = 2)
lines(c.country$year,c.country$Can, col = 'red')
lines(c.country$year,c.country$US, col = 'blue')

# Save the Catch by country
Catch.sim <- apply(sim.data$Catch.save.age, MARGIN = c(2,3),FUN = sum)
lines(df$years,Catch.sim[,1], col = 'red', lty = 2)
lines(df$years,Catch.sim[,2], col = 'blue', lty = 2)
lines(df$years,sim.data$Catch, lwd = 2)


# Age distribution in the catch

cps <- read.csv('catch_per_sector.csv')

## Calculate 

cps.s <- melt(cps, id.vars = c('year','nfish','nhauls','Country','Catch'),measure.vars = rep(paste('X',1:15, sep =''))) # Omit fleet from this df

cps.s <- cps.s %>% 
  group_by(year,Country, variable) %>% 
  summarise(agecomp =weighted.mean(value,Catch))

# Make ages numbers rather than factors
ages <- as.numeric(unlist(strsplit(as.character(cps.s$variable), split = "X")))
ages <- ages[is.na(ages) == 0]

cps.s$age <- ages

cps.am <- cps.s %>% 
  group_by(year,Country) %>% 
  summarise(am = sum((agecomp/100)*age))

cps.am.all <- matrix(NA, df$nyear)

for(i in 1:df$nyear){
  cps.am.all[i] <- sum(df$age[2:16]*df$age_catch[,i])
}

df.am.all <- data.frame(year = df$years, Country = 'All',am = cps.am.all)


ggplot(cps.am, aes(x= year, y= am, color = Country, group = Country))+geom_line()+geom_point()+theme_classic()+
  geom_line(data = df.am.catch, linetype = 2)+scale_x_continuous(limit = c(2000,2017))+scale_y_continuous(name = 'average age in catch')+
  geom_line(data = df.am.all, col = 'red')

