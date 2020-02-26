## Run the SAM assessment model for hake 

library(TMB)
library(ggplot2)
source('plotValues.R')
source('getUncertainty.R')
source('load_data.R')
source('ylimits.R')
source('getParameters.R')
source('Check_Identifiable_vs2.R')

# Read the assessment data 
assessment <- read.csv('data/asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

catches.obs <- read.csv('data/catches.csv')

df <- load_data()
years <- df$years


#U[2,] <- 0.01
parms <- getParameters(TRUE)

compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))

obj <-MakeADFun(df,parms,DLL="runHakeassessment_SAM", random = c('Rin'))#, )

lower <- obj$par-Inf

lower[names(lower) == 'F0'] <- 0.0001
upper <- obj$par+Inf
upper[names(upper) == 'psel_fish' ] <- 5
upper[names(upper) == 'PSEL'] <- 5
upper[names(upper) == 'logh'] <- log(0.999)
upper[names(upper) == 'F0'] <- 0.5


system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper, 
                        control = list(iter.max = 2000,
                                       eval.max = 2000))) #

rep<-sdreport(obj)
rep

# xx<- Check_Identifiable_vs2(obj)

#rep
sdrep <- summary(rep)
rep.values<-rownames(sdrep)


source('getUncertainty.R')
df$nyear <- length(years)
df$year <- years

SSB <- getUncertainty('SSB',df)
F0 <- getUncertainty('Fyear',df)
Catch <- getUncertainty('Catch',df)
Surveyobs <- getUncertainty('Surveyobs',df)
R <- getUncertainty('R',df)
surveyselec.est <- getUncertainty('surveyselc', df)
catchselec.est <- getUncertainty('catchselec', df)

plotValues(SSB, data.frame(x= assessment$year, y= assessment$SSB),'SSB')
plotValues(Catch, data.frame(x = df$years,y =df$Catchobs), 'Catch')

# yl <- ylimits(Biomass$name*1e-9,df$survey[df$survey > 1]*1e-9)
# plot(years[df$survey>1],Biomass$name[df$survey>1]*1e-9, xlim = c(1990,2019), ylim = yl, xlab= 'survey')
# points(years[df$survey > 1],df$survey[df$survey > 1]*1e-9, col = 'green')
plotValues(Surveyobs, data.frame(y = df$survey[df$flag_survey == 1], x = df$years[df$flag_survey == 1]), 'survey biomass')
plotValues(F0, data.frame(y =assessment$F0, x= assessment$year), 'Fishing mortality')

# Likelihood contributions 

nms <- c('SDR','Selectivity','Catch','survey','survey comps','Catch comps','Priors')

LogLik<- getUncertainty('ans_tot', df)
LogLik$lik <- nms

ggplot(LogLik, aes(y = -name, x= lik))+geom_point()+theme_classic()+scale_y_continuous('LogLikelihood')+scale_x_discrete(name = '')+
  geom_errorbar(aes(x = lik, ymin = -min, ymax = -max), col = 'black')


# Plot the age comps in all years  
ages <- 1:15
comp.year <- length(df$flag_catch[df$flag_catch == 1])

age_catch_est <- getUncertainty('age_catch_est', df)
age_catch <- getUncertainty('age_catch', df)

age_catch_est$age <- rep(ages, df$nyear)
age_catch_est$year <- rep(df$year, each = 15)

head(age_catch_est)

df.plot <- data.frame(comps = c(age_catch_est$name,age_catch$name), 
                      year = rep(age_catch_est$year,2), age = rep(age_catch_est$age,2), model = rep(c('estimated','data'), each = 780))

df.plot <- df.plot[which(df.plot$year %in% df$year[df$flag_catch == 1]),]

ggplot(data = df.plot, aes(x = age, y = comps, color = model))+geom_line()+facet_wrap(facets = ~year)+theme_bw()


ages <- 1:15
comp.year <- length(df$flag_survey[df$flag_survey == 1])

age_survey_est <- getUncertainty('age_survey_est', df)
age_survey <- getUncertainty('age_survey', df)

age_survey_est$age <- rep(ages, df$nyear)
age_survey_est$year <- rep(df$year, each = 15)

head(age_survey_est)

df.plot <- data.frame(comps = c(age_survey_est$name,age_survey$name), 
                      year = rep(age_survey_est$year,2), age = rep(age_survey_est$age,2), model = rep(c('estimated','data'), each = 780))

df.plot <- df.plot[which(df.plot$year %in% df$year[df$flag_survey == 1]),]

ggplot(data = df.plot, aes(x = age, y = comps, color = model))+geom_line()+facet_wrap(facets = ~year)+theme_bw()
