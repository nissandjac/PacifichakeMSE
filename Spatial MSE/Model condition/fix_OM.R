### Fix the OM model #### 

library(TMB)
library(dplyr)
library(reshape2)
library(ggplot2)
seedz <- 125
set.seed(seedz)


compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))
source('load_files.R')

assessment <- read.csv('data/asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

###### Load the data to run the MSE ######

###### Run the MSE using the data above #####
#ls <- fnMSE(parms, simyears = 50, TAC = 1, nruns = 1) # TAC 1) HCR, 2) JTC, 3) Realized catch
df <- load_data_seasons(nseason  = 1,nspace = 1)

# Operating model
sim.data <- run.agebased.true.catch(df)


# Estimation model 
df.EM <- load_data()
parms <- getParameters(TRUE)
parms$F0 <- sim.data$Fseason[21,,1,1]

obj <-MakeADFun(df.EM,parms,DLL="runHakeassessment")#, )

vars <- obj$report()

# Compare the comps with ss3
SSB <- vars$SSB

plot(df.EM$years,SSB)
lines(df$years,rowSums(sim.data$SSB))
lines(assessment$year,assessment$SSB*2, col = 'red')

# 

plot(df.EM$years, vars$Catch)
lines(df.EM$years, sim.data$Catch, col = 'green')
lines(df.EM$years, df$Catch, col = 'red')


# Compare the outputs for minimization 

# Plot the age comps in all years  
ages <- 1:15
colnames(sim.data$age_catch) <- df$years
colnames(df$age_catch) <- df$years
rownames(df$age_catch) <- ages

age_catch_est <- melt(sim.data$age_catch, varnames = c('ages','years'),value.name = 'comps')
age_catch_est$model <- 'OM'

age_catch <- melt(df$age_catch, varnames = c('ages','years'), value.name = 'comps')
age_catch$model <- 'data'

df.plot <- rbind(age_catch_est,age_catch)
df.plot <- df.plot[which(df.plot$years %in% df$years[df$flag_catch == 1]),]

#png('Figures/age_comps_catch.png', width = 16, height = 12, unit = 'cm', res =400)
ggplot(data = df.plot, aes(x = ages, y = comps, color = model))+geom_line()+facet_wrap(facets = ~years)+theme_bw()



# Plot the age comps in all years  
ages <- 1:15
colnames(sim.data$age_catch) <- df$years
colnames(df$age_catch) <- df$years
rownames(df$age_catch) <- ages

age_catch_est <- melt(sim.data$age_catch, varnames = c('ages','years'),value.name = 'comps')
age_catch_est$model <- 'OM'

age_catch <- melt(df$age_catch, varnames = c('ages','years'), value.name = 'comps')
age_catch$model <- 'data'

df.plot <- rbind(age_catch_est,age_catch)
df.plot <- df.plot[which(df.plot$years %in% df$years[df$flag_catch == 1]),]

#png('Figures/age_comps_catch.png', width = 16, height = 12, unit = 'cm', res =400)
ggplot(data = df.plot, aes(x = ages, y = comps, color = model))+geom_line()+facet_wrap(facets = ~years)+theme_bw()


# Do the same for the survey 

# Plot the age comps in all years  
ages <- 1:15
colnames(sim.data$age_comps_surv) <- df$years
colnames(df$age_survey) <- df$years
rownames(df$age_survey) <- ages

age_survey_est <- melt(sim.data$age_comps_surv, varnames = c('ages','years'),value.name = 'comps')
age_survey_est$model <- 'OM'

age_survey <- melt(df$age_survey, varnames = c('ages','years'), value.name = 'comps')
age_survey$model <- 'data'

df.plot <- rbind(age_survey_est,age_survey)
df.plot <- df.plot[which(df.plot$years %in% df$years[df$flag_survey == 1]),]

#png('Figures/age_comps_catch.png', width = 16, height = 12, unit = 'cm', res =400)
ggplot(data = df.plot, aes(x = ages, y = comps, color = model))+geom_line()+facet_wrap(facets = ~years)+theme_bw()


# Plot the survey 

df.plot <- data.frame(year = rep(df$years,2), survey = c(sim.data$survey,df$survey), model = rep(c('OM','data'), each = df$tEnd))
df.plot <- df.plot[which(df.plot$year %in% df$years[df$flag_survey == 1]),]
        
ggplot(data = df.plot, aes(x = year, y = survey, color = model))+geom_line()+theme_bw()+geom_point()

