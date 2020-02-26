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
source('load_data_seasons.R')
source('create_TMB_data.R')
source('Check_Identifiable_vs2.R')
source('getF.R')
assessment <- read.csv('data/assessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]
Catch.obs <- read.csv('data/hake_totcatch.csv')

df <- load_data_seasons()
df$Catch <- Catch.obs$Fishery
time <- 1
yrinit <- df$nyear
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 
# df$parms$initN <- df$parms$initN*0
# df$parms$Rin <- df$parms$Rin*0
# df$F0 <- 0*df$F0

simyears <- 25 # Project 30 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df)

simdata0 <- sim.data # The other one is gonna get overwritten. 

# Plot the biomass in ggplot 
df.plot <- data.frame(years = c(df$years,assessment$year), 
                      SSB = c(rowSums(sim.data$SSB),assessment$SSB), 
                      source = c(rep('SSB OM',length(df$years)),rep('SSB assessment', length(assessment$year))))


p1 <- ggplot(data = df.plot, aes(x = years, y = SSB, color = source))+geom_line(size = 2)+theme_classic()

if(plot.figures == TRUE){
  png('Spawningbiomass.png', width = 16, height = 12, res = 400,units = 'cm')

}
  p1
  if(plot.figures == TRUE){  
dev.off()
  }
# Compare with all the data 
par(mfrow = c(2,2), mar = c(4,4,1,1))
plot(df$years,rowSums(sim.data$SSB)*0.5, type = 'l', lwd = 2, xlab ='Year', ylab = 'Spawning biomass')
lines(assessment$year,assessment$SSB, lwd = 2, col = 'red')

# How do we fit to the data 
yl <- ylimits(df$survey[df$survey > 1],sim.data$survey[sim.data$survey > 1])

plot(df$years[df$survey > 1],df$survey[df$survey > 1], lwd = 2, col = 'red', ylim = yl, ylab= 'survey')
points(df$years[df$survey > 1],sim.data$survey[sim.data$survey > 1], col = 'black',lwd = 2)


df.plot <- data.frame(years = rep(df$years[df$survey > 1],2),
                      survey = c(df$survey[df$survey > 1],sim.data$survey[sim.data$survey > 1]),
                      source = rep(c('Survey data','OM output'), each = length(df$years[df$survey > 1])),
                      survsd= c(df$survey_err[df$flag_survey ==1], rep(NA,length(df$years[df$survey > 1]))),
                      kriegsd = c(rep(exp(df$parms$logSDsurv),length(df$years[df$survey > 1])), rep(NA,length(df$years[df$survey > 1])))
                      )
df.plot$survsd <- sqrt(df.plot$survey^2*exp(df.plot$survsd+df.plot$kriegsd-1))

p2 <- ggplot(data = df.plot, aes(x = years, y = survey/1e6, color = source))+
  geom_point(data = df.plot[df.plot$source == 'Survey data',],size = 3)+
  geom_line(data = df.plot[df.plot$source == 'OM output',], size =2)+
  theme_classic()+
  geom_errorbar(aes(ymin=(survey-survsd)/1e6, ymax=(survey+survsd)/1e6))+
  scale_y_continuous(limit = c(0,5), name = 'survey biomass (million t)')+
  scale_x_continuous(name = 'year')

if(plot.figures == TRUE){
png('survey.png', width = 16, height = 12, res = 400,units = 'cm')}
p2

 if(plot.figures == TRUE){
dev.off()}
## Add the error bars 
# arrows(df$years[df$survey > 1],df$survey[df$survey > 1]+exp(exp(df$parms$logSDsurv)+exp(df$survey_err[df$survey > 1])), 
#        df$years[df$survey > 1], df$survey[df$survey > 1]-exp(exp(df$parms$logSDsurv)+exp(df$survey_err[df$survey > 1])), 
#        length=0.05, angle=90, code=3)


# plot(df$years,df$Catch, type = 'l', lwd = 2, col = 'red')
# # Plot the overall average age 

age.comps <- sim.data$age_comps_OM[,2:df$nyear,,3]
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

age.catch <- sim.data$CatchN.save.age
age.catch.all <- apply(age.catch,c(1,2),mean)

am.catch <- matrix(NA, df$nyear)

for(i in 1:(df$nyear-1)){
  ac.tmp <- rep(NA,15)
  ac.tmp[1:14] <- age.catch.all[2:15,i]/sum(age.catch.all[,i])
  ac.tmp[15] <- sum(age.catch.all[16:df$nage,i])/sum(age.catch.all[,i])
  
  am.catch[i] <- sum(df$age[2:16]*ac.tmp)
  
}

age.catch.can <- apply(sim.data$CatchN.save.age[,,1,],c(1,2),mean)

am.catch.can <- matrix(NA, df$nyear)

for(i in 1:(df$nyear-1)){
  ac.tmp <- rep(NA,15)
  ac.tmp[1:14] <- age.catch.can[2:15,i]/sum(age.catch.can[,i])
  ac.tmp[15] <- sum(age.catch.can[16:df$nage,i])/sum(age.catch.can[,i])
  
  am.catch.can[i] <- sum(df$age[2:16]*ac.tmp)
  
}

age.catch.US <- apply(sim.data$CatchN.save.age[,,2,],c(1,2),mean)

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


p3 <- ggplot(df.am.catch, aes(x = year, y = am, color = Country))+geom_line(size = 1)+
  theme_classic()+
  scale_y_continuous(name = 'Average age in catch', limit = c(2,10))+scale_x_continuous()

p3

# png('Age_comps_catch.png', width = 16, height = 12, res = 400,units = 'cm')
# p3
# dev.off()

## See thcoe catch per country
c.country <- read.csv('data/catch_per_country.csv')

cps <- read.csv('data/catch_per_sector.csv')

## Calculate 

cps.s <- melt(cps, id.vars = c('year','nfish','nhauls','Country','Catch'),
              measure.vars = rep(paste('X',1:15, sep =''))) # Omit fleet from this df

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


p1 <- ggplot(cps.am, aes(x= year, y= am, color = Country, group = Country))+geom_line()+geom_point()+theme_classic()+
  geom_line(data = df.am.catch, linetype = 2)+
  scale_x_continuous(limit = c(1990,2017))+scale_y_continuous(limit = c(2.5,10),name = 'average age in catch')+
  geom_line(data = df.am.all, col = 'red')+geom_point(data=df.am.all, col ='red')

#png('age_comps.png', width = 16, height = 12, res = 400,units = 'cm')
p1
#dev.off()

# Is my weighted calculation correct? 
cps.all<- melt(cps, id.vars = c('year','nfish','nhauls','Country','Catch'),measure.vars = rep(paste('X',1:15, sep =''))) # Omit fleet from this df
cps.all <- cps.all %>% 
  group_by(year, variable) %>% 
  summarise(agecomp =weighted.mean(value,Catch))
# Make ages numbers rather than factors
ages <- as.numeric(unlist(strsplit(as.character(cps.all$variable), split = "X")))
ages <- ages[is.na(ages) == 0]

cps.all$age <- ages

cps.all.s <- cps.all %>% 
  group_by(year) %>% 
  summarise(am = sum((agecomp/100)*age))


## Load the single species model to check the age comp fits 
source('load_data.R')
source('parameters_TRUE.R')
source('runAssessment.R')
source('getParameters_old.R')

df.new <- load_data()
# if(sum(parms$F0) == 0){
#   parms$F0 <- parms$F0+0.001 # Make sure it's over 1
# }
reps <- runAssessment(df.new)

am.est <- reps$age_catch_est


age.comps <- sim.data$age_comps_catch


# Calculate the average age in the catch 
am.mean <- data.frame(years = df.new$years, am = NA, am.sim = NA)

for(i in 1:length(df.new$years)){
  
  am.mean$am[i] <- sum(am.est[,i]*df.new$age[2:16])
  am.mean$am.sim[i] <- sum(age.comps[,i]*df.new$age[2:16])
}

# Age comps from the OM 
df.plot <- rbind(df.am.all, data.frame(year = df.new$years, am = am.mean$am.sim, Country = 'sim'),
                 data.frame(year = df.new$years, am = am.mean$am, Country = 'assessment'))

# ggplot(df.am.all, aes(x = year, y = am))+geom_line(size = 1)+scale_x_continuous(limit = c(1975,2017))+
#   #geom_line(data = cps.all.s, col = 'red', size = 2)+
#   geom_line(data = am.mean, aes(x = years), col = 'green', size = 1)+
#   geom_line(data = am.mean,aes(x = years,y = am.sim), col = 'blue', size =1 )+theme_classic()
df.plot$Country <- mapvalues(df.plot$Country,from='All',to = 'Observed')
df.plot$am[df.plot$am<3] <- NA # Weird bug

p2 <- ggplot(df.plot, aes(x = year, y= am, color = Country))+geom_line()+theme_classic()+scale_y_continuous(name = 'Average age in catch')


## Plot the average survey age comp 

am.survey.sim <- sim.data$age_comps_surv

am.survey <- data.frame(year = rep(df$years,3), am = NA, category = rep(c('simulated','assessment','observation'),each = df$nyear))


for(i in 1:df$nyear){
  am.survey[am.survey$category == 'simulated',]$am[i] <- sum(df$age[2:16]*sim.data$age_comps_surv[,i])
  am.survey[am.survey$category == 'assessment',]$am[i] <- sum(df$age[2:16]*reps$age_survey_est[,i])
  am.survey[am.survey$category == 'observation',]$am[i] <- sum(df$age[2:16]*df$age_survey[,i])
  
}

am.survey$am[am.survey$am <0] <- NA


ix <- is.na(am.survey$am)
am.psurvey <- am.survey[ix == 0,]

p2 <- ggplot(am.psurvey, aes(x = year, y = am, color = category))+geom_point()+theme_classic()+geom_line()+
  scale_y_continuous(limit = c(2,10), name = 'Average age in survey')


#png('survey_comps.png', width = 16, height = 10, res= 400, unit = 'cm')
p2
#dev.off()


df.movement <- data.frame(age = rep(df$age, 8), movement = NA, country = rep(c('CAN','USA'), each = df$nage),
                          season = rep(1:4, each = df$nage*2))
df.movement$age[df.movement$country == 'CAN'] <-  df.movement$age[df.movement$country == 'CAN']+0.3 # For plotting
for(i in 1:4){
  mm.tmp <- df$movemat[,,i,1]
  
  df.movement[df.movement$season == i & df.movement$country == 'USA',]$movement <- mm.tmp[2,] 
  df.movement[df.movement$season == i & df.movement$country == 'CAN',]$movement <- mm.tmp[1,]
}
p.movement <- list()
for(i in 1:4){
p.movement[[i]] <- ggplot(df.movement[df.movement$season == i,], aes(x = age, y = movement, color = country))+
  geom_line()+theme_classic()+scale_y_continuous(limit = c(0,1.1))+geom_point()+ggtitle(paste('season',i))
}
library(gridExtra)
#png('survey_comps.png', width = 16, height = 10, res= 400, unit = 'cm')

do.call("grid.arrange", c(p.movement, ncol=2))

#dev.off()


# Biomass distribution in the surveys 

df.survey <- read.csv('data/survey_country_2.csv')

p1 <- ggplot(df.survey, aes(x = year, y = Bio/1e6, color = Country))+geom_line()+geom_point()+theme_classic()
p1


## Total survey
survey.tot <- df.survey %>% 
  group_by(year) %>% 
  summarise(Bio = sum(Bio),
            am = mean(am))

# Something is wrong with the survey. Just use the fraction for now 

survey.frac.us <- df.survey$Bio[df.survey$Country == 'USA']/survey.tot$Bio

survey.frac.can <-   df.survey$Bio[df.survey$Country == 'CAN']/survey.tot$Bio

idx <- which(df$flag_survey == 1)
ny <- length(idx)

sf.us <- rep(NA, df$nyear)
sf.can <- rep(NA, df$nyear)

sf.us[idx] <- survey.frac.us
sf.can[idx] <- survey.frac.can

# Spread it out on the survey
survey.plot <- data.frame(year = rep(df$years, 4), 
                          country = c(rep(c('USA','CAN'),  each = df$nyear),rep(c('USA','CAN'),  each = df$nyear)), 
                          survey = c(df$survey*sf.us,df$survey*sf.can, sim.data$survey.true[2,],
                                     sim.data$survey.true[1,]),
                          source = c(rep('observation',df$nyear*2),rep('simulation', df$nyear*2))
                          )
                            
p.survey <- ggplot(data = survey.plot, aes(x = year, y = survey*1e-6, color = country))+
  geom_line(data = survey.plot[is.na(survey.plot$survey) == 0 & survey.plot$source == 'observation',])+
  geom_point(data = survey.plot[is.na(survey.plot$survey) == 0 & survey.plot$source == 'observation',])+
  geom_line(data = survey.plot[survey.plot$source == 'simulation',], linetype = 2)+
  theme_classic()+scale_y_continuous(name = 'survey biomass (millions tons)')+
  scale_x_continuous(limit = c(1994, 2018))

#png('survey_distribution.png', width = 16, height = 12, res = 400,units = 'cm')
p.survey                            
#dev.off()                            


# Calculate the am per country in the OM 
am.can.survey <- sim.data$age_comps_country[,,1]
am.us.survey <- sim.data$age_comps_country[,,2]

am.mean.surv <- data.frame(year = rep(df.new$years,2), am = NA, Country = rep(c('CAN','USA'), each = length(df.new$years)))

for(i in 1:length(df.new$years)){
  
  # US first 
  am.tmp <- sim.data$age_comps_country[,i,2]
  
  am.mean.surv[am.mean.surv$year == df.new$years[i] & am.mean.surv$Country == 'USA',]$am <- sum(am.tmp*df.new$age[2:16])
  
  am.tmp <- sim.data$age_comps_country[,i,1]
  am.mean.surv[am.mean.surv$year == df.new$years[i] & am.mean.surv$Country == 'CAN',]$am <- sum(am.tmp*df.new$age[2:16])
}



dist.survey <- ggplot(df.survey, aes(x= year, y = am, color = Country))+geom_line()+geom_point()+theme_classic()+
  scale_y_continuous(name = 'Average age in survey', limit = c(2,10))+
  geom_line(data = am.mean.surv, linetype = 2)

#png('survey_comps.png', width = 16, height = 12, res = 400,units = 'cm')
dist.survey
#dev.off()



