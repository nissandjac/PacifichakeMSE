## Test the assessment against real and simulated data

# Run the hake assessment 
direc <- "~/GitHub/PacifichakeMSE/Spatial MSE_vs2/"
setwd(direc)
library(TMB)
#library(TMBdebug)
library(ggplot2)
library(dplyr)
source('plotValues.R')
source('getUncertainty.R')
source('load_data.R')
source('ylimits.R')
source('getParameters.R')
source('Check_Identifiable_vs2.R')
source('getUncertainty.R')
source('run_agebased_model_true_Catch.R')
source('load_data_seasons.R')
source('getSelec.R')
source('create_TMB_data.R')
# Read the assessment data 
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

catches.obs <- read.csv('hake_totcatch.csv')

df.ass <- load_data()
years <- df.ass$years

df.sim <- load_data_seasons(move = FALSE,nseason = 1, nspace = 1)
df.sim$Catch <- catches.obs$Fishery
df.sim$Catch[length(df.sim$Catch)] <- df.sim$Catch[length(df.sim$Catch)]*0.5

sim.data <- run.agebased.true.catch(df.sim)

hist(sim.data$Catch/df.sim$Catch)
#U[2,] <- 0.01
parms <- getParameters(TRUE)

compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))
obj <-MakeADFun(df.ass,parms,DLL="runHakeassessment")#, )
# plot(obj$report()$SSB)
# lines(assessment$SSB)

lower <- obj$par-Inf
upper <- obj$par+Inf

lower[names(lower) == 'F0'] <- 0.001
upper[names(upper) == 'psel_fish' ] <- 5
upper[names(upper) == 'PSEL'] <- 9
upper[names(upper) == 'logh'] <- log(0.999)

system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper, 
                        control = list(iter.max = 1e5,
                                       eval.max = 1e5))) #
system.time(rep<-sdreport(obj))
sdrep <- summary(rep)
rep.values<-rownames(sdrep)

# Run the space model 

df.new <- create_TMB_data(sim.data, df.sim)

obj.sim <-MakeADFun(df.new,parms,DLL="runHakeassessment", silent = TRUE) # Run the assessment 


system.time(opt.sim<-nlminb(obj.sim$par,obj.sim$fn,obj.sim$gr,lower=lower,upper=upper,
                        control = list(iter.max = 1e5, 
                                       eval.max = 1e5))) 
rep.sim<- sdreport(obj.sim)
sdrep.sim <- summary(rep.sim)
rep.values.sim <- rownames(sdrep)

### Plot all the data ###
#Check_Identifiable_vs2(obj.sim)

df.plot <- rbind(data.frame(SSB = sdrep[rep.values == 'SSB',1], model = 'TMB assessment', year = df.sim$years),
                 data.frame(SSB = sdrep.sim[rep.values == 'SSB',1], model = 'OM assessment', year = df.sim$years),
                 data.frame(SSB = assessment$SSB, model = 'SS3 assessment', year = assessment$year),
                 data.frame(SSB = sim.data$SSB, model = 'space model', year = df.sim$years))

ggplot(df.plot,aes(x = year, y = SSB*1e-6, color = model))+geom_line()+theme_classic()



### Compare the data input and output and find the error 

### Bias adjustment 
plot(df.ass$years,df.ass$b)
lines(df.sim$years,df.sim$b)
lines(df.new$years,df.new$b, col = 'red')


# Plot the survey 

survey.plot <- rbind(
  data.frame(survey = sdrep[rep.values == 'Surveyobs',1][df.ass$flag_survey ==1], model = 'tmb', year = df.ass$years[df.ass$flag_survey == 1]),
  data.frame(survey= sim.data$survey[df.sim$flag_survey == 1], model = 'sim', year = df.sim$years[df.sim$flag_survey ==1]),
  data.frame(survey =sdrep.sim[rep.values == 'Surveyobs',1][df.sim$flag_survey == 1],model = 'EM', year = df.sim$years[df.sim$flag_survey == 1]),
  data.frame(survey = df.ass$survey[df.ass$flag_survey == 1],model = 'data', year = df.ass$years[df.ass$flag_survey == 1])
)

ggplot(survey.plot, aes(x = year, y = survey*1e-6, color = model))+theme_classic()+geom_point()+geom_line()


# Plot the catch(?)

catch.plot <- rbind(
  data.frame(catch = catches.obs$Fishery, year = catches.obs$year, model = 'obs'),
  data.frame(catch = sim.data$Catch, year = df.sim$years, model = 'OM'),
  data.frame(catch = sdrep[rep.values == 'Catch',1], year = df.ass$years, model = 'TMB'),
  data.frame(catch = sdrep.sim[rep.values.sim == 'Catch',1], year = df.sim$years, model = 'EM')
)

ggplot(catch.plot, aes(x = year, y = catch*1e-6, color = model))+theme_classic()+geom_point()+geom_line()+
  geom_jitter()


### Plot the agecomps in the catch ###
cage <- 1:df.sim$age_maxage
age.matrix <- matrix(cage,ncol=length(df.sim$years[df.sim$flag_catch == 1]),nrow=length(cage),byrow=FALSE)

age.tmb <- data.frame(value = sdrep[rep.values == 'age_catch_est',1],
                      age = rep(cage,df.ass$nyear),
                      years = rep(df.ass$years, each = length(cage)))

age.tmb <- age.tmb[age.tmb$years %in% df.ass$years[df.ass$flag_catch ==1],]
am.tmb <- age.tmb %>% 
  group_by(years) %>% 
  summarise(am= sum(value*age))
am.tmb$model <- 'TMB'

age.em <- data.frame(value = sdrep.sim[rep.values == 'age_catch_est',1],
                      age = rep(cage,df.sim$nyear),
                      years = rep(df.sim$years, each = length(cage)))

age.em <- age.em[age.em$years %in% df.sim$years[df.sim$flag_catch ==1],]
am.em <- age.em %>% 
  group_by(years) %>% 
  summarise(am= sum(value*age))
am.em$model <- 'em'



am.catch <- rbind(
  data.frame(am = colSums(df.sim$age_catch[,df.sim$flag_catch == 1]*age.matrix), 
             model = 'data', years = df.sim$years[df.sim$flag_catch == 1]),
  data.frame(am = colSums(sim.data$age_catch[,df.sim$flag_catch == 1]*age.matrix), 
             model = 'OM', years = df.sim$years[df.sim$flag_catch == 1]),
  am.tmb[,c(2,3,1)],
  am.em[,c(2,3,1)]
)


ggplot(am.catch, aes(x = years, y = am, color = model))+geom_line()+theme_classic()



### Plot the agecomps in the catch ###
cage <- 1:df.sim$age_maxage
age.matrix <- matrix(cage,ncol=length(df.sim$years[df.sim$flag_survey == 1]),nrow=length(cage),byrow=FALSE)

age.tmb.survey <- data.frame(value = sdrep[rep.values == 'age_survey_est',1],
                      age = rep(cage,df.ass$nyear),
                      years = rep(df.ass$years, each = length(cage)))

age.tmb.survey <- age.tmb.survey[age.tmb.survey$years %in% df.ass$years[df.ass$flag_survey ==1],]
am.tmb.survey <- age.tmb.survey %>% 
  group_by(years) %>% 
  summarise(am= sum(value*age))
am.tmb.survey$model <- 'TMB'

age.em.survey <- data.frame(value = sdrep.sim[rep.values == 'age_survey_est',1],
                     age = rep(cage,df.sim$nyear),
                     years = rep(df.sim$years, each = length(cage)))

age.em.survey <- age.em.survey[age.em.survey$years %in% df.sim$years[df.sim$flag_survey ==1],]
am.em.survey <- age.em.survey %>% 
  group_by(years) %>% 
  summarise(am= sum(value*age))
am.em.survey$model <- 'em'


am.catch <- rbind(
  data.frame(am = colSums(df.sim$age_survey[,df.sim$flag_survey == 1]*age.matrix), 
             model = 'data', years = df.sim$years[df.sim$flag_survey == 1]),
  data.frame(am = colSums(sim.data$age_comps_surv[,df.sim$flag_survey == 1]*age.matrix), 
             model = 'OM', years = df.sim$years[df.sim$flag_survey == 1]),
  am.tmb[,c(2,3,1)],
  am.em[,c(2,3,1)]
)


ggplot(am.catch, aes(x = years, y = am, color = model))+geom_line()+theme_classic()+geom_point()

# Fishing mortality 

df.F <- rbind(
  data.frame(F0 = sim.data$Fsave, year = df.sim$years, model = 'OM'),
  data.frame(F0 = sdrep[rep.values == 'F0',1], year = df.ass$years, model = 'TMB'),
  data.frame(F0 = assessment$F0, year = assessment$year, model = 'SS3'),
  data.frame(F0 = sdrep.sim[rep.values.sim == 'F0',1], year = df.sim$years, model = 'EM')
  )

ggplot(df.F, aes(x = year, y = F0, color = model))+geom_line()+theme_classic()+geom_point()



### Plot the estimated parameters 

df.parms <- data.frame(parms[1:7])

       