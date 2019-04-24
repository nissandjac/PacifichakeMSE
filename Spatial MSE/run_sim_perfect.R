## Test the assessment against perfect data 

# Run the hake assessment 
direc <- "~/GitHub/PacifichakeMSE/Spatial MSE_vs2/"
setwd(direc)
library(TMB)
compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))
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

df <- load_data_seasons(move = FALSE,nseason = 1, nspace = 1)

df$Catch <- catches.obs$Fishery
# df$flag_catch[df$flag_catch != 1] <- 1
# df$flag_survey[df$flag_survey != 1] <- 1
# df$parms$logSDsurv <- -Inf # No error on the survey
# df$ss_catch[df$ss_catch <0] <- 400
# df$ss_survey[df$ss_survey<0] <- 60
# df$surveyseason <- 1
# df$b <- rep(0.87, length(df$b))
df$Catch <- df$Catch
sim.data <- run.agebased.true.catch(df)

df.new <- create_TMB_data(sim.data, df)
parms <- getParameters(TRUE,df)

obj <-MakeADFun(df.new,parms,DLL="runHakeassessment", silent = TRUE) # Run the assessment 

lower <- obj$par-Inf
upper <- obj$par+Inf

lower[names(lower) == 'F0'] <- 0.001
upper[names(upper) == 'psel_fish' ] <- 5
upper[names(upper) == 'logh'] <- log(0.999)
# lower[names(lower) == 'logRinit'] <- df$parms$logRinit
# upper[names(upper) == 'logRinit'] <- df$parms$logRinit
# upper[names(upper) == 'PSEL'] <- 1
# lower[names(lower) == 'PSEL'] <- 1
upper[names(upper) == 'logphi_survey'] <- log(10)
TT <- obj$report()
plot(TT$Catch)
lines(sim.data$Catch)
lines(df$Catch)


system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper,
                            control = list(iter.max = 1e5, 
                                           eval.max = 1e5))) 
rep<- sdreport(obj)
sdrep <- summary(rep)
rep.values <- rownames(sdrep)

Catch.est <- sdrep[rep.values == 'Catch',1]
### Plot all the data ###
Check_Identifiable_vs2(obj)
#
df.plot <- rbind( data.frame(SSB = sim.data$SSB, model = 'space model', year = df$years),
                 data.frame(SSB = sdrep[rep.values == 'SSB',1], model = 'EM', year = df$years),
                 data.frame(SSB = assessment$SSB, model = 'SS3 assessment', year = assessment$year),
                 data.frame(SSB = TT$SSB, model = 'parm input', year = df$years))

ggplot(df.plot,aes(x = year, y = SSB*1e-6, color = model))+geom_line()+theme_classic()+geom_point()#geom_jitter()

splot(df$years,sim.data$Catch, type='l')
lines(df$years,df$Catch, col = 'red')
lines(df$years,Catch.est, col = 'green')

# Relative to unfished
SSB0_est <- sdrep[rep.values == 'SSBzero',]
df.plot <- rbind(data.frame(SSB = sdrep[rep.values == 'SSB',1]/SSB0_est[1], model = 'EM', year = df$years),
                 data.frame(SSB = sim.data$SSB/sim.data$SSB0, model = 'space model', year = df$years),
                 data.frame(SSB = TT$SSB/TT$SSBzero, model = 'parm input', year = df$years))
ggplot(df.plot,aes(x = year, y = SSB, color = model))+geom_line()+theme_classic()+geom_point()


# Total number of individuals 
df.F0 <- rbind(data.frame(F0 = sdrep[rep.values == 'Fyear',1], model = 'EM', year = df$years),
                          data.frame(F0 = assessment$F0, model = 'SS3 assessment', year = assessment$year),
                          data.frame(F0 = sim.data$Fsave, model = 'space model', year = df$years),
                          data.frame(F0 = TT$Fyear, model = 'parm input', year = df$years)
               )

ggplot(df.F0,aes(x = year, y = F0, color = model))+geom_line()+theme_classic()

Ninit = sdrep[rep.values == 'Ninit',1]


N <- sdrep[rep.values == 'N',]
## Recruitment 
R <- sdrep[rep.values == 'R',]

plot(df$years,R[,1], log = 'y')
lines(df$years,sim.data$N.save.age[1,])

initN <- sdrep[rep.values == 'initN',]

plot(initN[,1], type = 'o', col ='red')
lines(df$parms$initN)


plot(sim.data$Fsel[49,,])
lines(TT$selectivity_save[,49])
lines(TT$selectivity_save[,42])
lines(sim.data$Fsel[42,,], type = 'o', col = 'red')

names(TT)

TT$ans_tot
sdrep[rep.values == 'ans_tot',1]

hist(sdrep[rep.values == 'PSEL',1])

## Test the likelihood functions 
ans_psel <- 0
sigma_psel <- df.new$sigma_psel

# Check the survey 
# 
ix <- df$flag_survey
df.survey <- data.frame(survey = TT$Surveyobs[ix == 1], year = df$years[ix == 1], model = 'parms')

df.survey <- rbind(df.survey,
                   data.frame(survey = sdrep[rep.values == 'Surveyobs',1][ix == 1], year = df$years[ix == 1], model= 'est'),
                   data.frame(survey = sim.data$survey[ix == 1], year = df$years[ix == 1], model= 'sim'),
                   data.frame(survey = df$survey[ix == 1], year = df$years[ix == 1], model= 'obs')
)
ggplot(df.survey, aes(x = year, y = survey, color = model))+geom_line()+geom_point()

age_survey_est <- getUncertainty('age_survey_est', df.new, sdrep)
age_survey <- getUncertainty('age_survey', df.new, sdrep)
age_survey_est$year <- rep(df.new$years, each =length(1:15))
age_survey_est$age <- rep(1:15, df.new$tEnd)

df.plot <- data.frame(comps = c(age_survey_est$name,age_survey$name), 
                      year = rep(age_survey_est$year,2), age = rep(age_survey_est$age,2), 
                      model = rep(c('estimated','data'), each = 780))

df.plot <- df.plot[which(df.plot$year %in% df.new$years[df.new$flag_survey == 1]),]

ggplot(data = df.plot[df.plot$model == 'data',], aes(x = age, y = comps, color = model))+
  geom_line()+facet_wrap(facets = ~year)+theme_bw()


