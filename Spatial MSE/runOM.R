###### Initialize the operating model ###### 
library(TMB)
library(dplyr)
library(reshape2)
library(ggplot2)
library(r4ss)
source('load_files_OM.R')
seedz <- 125
set.seed(seedz)
assessment <- read.csv('data/assessment_MLE.csv')
assessment <- assessment[assessment$year > 1965,]
# Get the stock assessment output from SS3 
mod <- SS_output(paste(getwd(),'/data/SS32018', sep =''), printstats=FALSE, verbose = FALSE)


plot.figures = FALSE # Set true for printing to file 


df <- load_data_seasons(nseason = 1, nspace = 1, bfuture = 0.5, movemaxinit = 0.5, movefiftyinit =8) # Prepare data for operating model

simyears <- 25 # Project 30 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df)



# 
M <- exp(df$parms$logMinit)
R0 <- exp(df$parms$logRinit)
wtatage <- mod$wtatage


N_at_age.beg <- rep(NA, df$nage)
N_at_age.beg <- R0*exp(-M*df$age)
# replace plus-group value with sum of geometric series
N_at_age.beg[df$nage] <- R0*exp(-M*max(df$age))/(1 - exp(-M))
mod$SBzero/sum(N_at_age.beg*wtatage[wtatage$Yr == 1966 & wtatage$Fleet == -2, paste(df$age)] )


SSB.obs <- mod$derived_quants$Value[grep('SSB_1966',mod$derived_quants$Label):grep('SSB_2018',mod$derived_quants$Label)]
SSB.OM <- rowSums(sim.data$SSB.weight)

# Plot the biomass in ggplot 
df.plot <- data.frame(years = c(df$years,df$years), 
                      SSB = c(rowSums(sim.data$SSB.weight),SSB.obs), source = c(rep('SSB OM', df$nyear),
                                                                                       rep('SSB assessment', df$nyear)))

p1 <- ggplot(data = df.plot, aes(x = years, y = SSB, color = source))+geom_line(size = 2)+theme_classic()
p1

survey.ss <- data.frame(years = mod$cpue$Yr,
                        survey =mod$cpue$Exp,
                        source = 'SS',
                        survsd = NA,
                        kriegsd = NA)

df.plot <- data.frame(years = rep(df$years[df$survey > 1],2),
                      survey = c(df$survey[df$survey > 1],sim.data$survey[sim.data$survey > 1]),
                      source = rep(c('Survey data','OM output'), each = length(df$years[df$survey > 1])),
                      survsd= c(df$survey_err[df$flag_survey ==1], rep(NA,length(df$years[df$survey > 1]))),
                      kriegsd = c(rep(exp(df$parms$logSDsurv),length(df$years[df$survey > 1])), rep(NA,length(df$years[df$survey > 1])))
)

df.plot <- rbind(df.plot,survey.ss)

df.plot$survsd <- sqrt(df.plot$survey^2*exp(df.plot$survsd+df.plot$kriegsd-1))

p2 <- ggplot(data = df.plot, aes(x = years, y = survey/1e6, color = source))+
  geom_point(data = df.plot[df.plot$source == 'Survey data',],size = 3)+
  geom_line(data = df.plot[df.plot$source == 'OM output',], size =2)+
  geom_line(data = df.plot[df.plot$source == 'SS',], size = 2)+
  theme_classic()+
  geom_errorbar(aes(ymin=(survey-survsd)/1e6, ymax=(survey+survsd)/1e6))+
  scale_y_continuous(limit = c(0,5), name = 'survey biomass (million t)')+
  scale_x_continuous(name = 'year')

p2


source('calcMeanAge.R')

age.comps <- sim.data$age_comps_OM[,1:df$nyear,,df$surveyseason]
age.comps <- apply(age.comps,c(1,2),sum)/2

am <- calcMeanAge(age.comps,df$nage)


age.comps.can <- sim.data$age_comps_OM[,1:df$nyear,1,df$surveyseason]
am.can <-calcMeanAge(age.comps.can, df$nage)

age.comps.US <- sim.data$age_comps_OM[,1:df$nyear,2,df$surveyseason]
am.US <- calcMeanAge(age.comps.US, df$nage)


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



