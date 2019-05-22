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
source('load_files_OM.R')

assessment <- read.csv('data/asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

###### Load the data to run the MSE ######

###### Run the MSE using the data above #####
#ls <- fnMSE(parms, simyears = 50, TAC = 1, nruns = 1) # TAC 1) HCR, 2) JTC, 3) Realized catch
df <- load_data_seasons(nseason  = 4,nspace = 2)
# Operating model
sim.data <- run.agebased.true.catch(df)


# Estimation model 
df.EM <- load_data()
parms <- getParameters(TRUE)
parms$F0 <- sim.data$Fseason[21,,1,1]

# Run the operating model assessment 
df.new <- create_TMB_data(sim.data, df)

parms.new <- df$parms
parms.new$F0 <- sim.data$Fseason[21,,1,1]
parms.new$Rin <- parms$Rin#parms.new$Rin[1:(length(parms.new$Rin)-1)]
# df.new$survey <- df$survey
# df.new$age_survey <- df$age_survey
# df.new$age_catch <- df$age_catch


obj <-MakeADFun(df.new,parms.new,DLL="runHakeassessment", silent = TRUE) # Run the assessment 

reps <- obj$report()

lower <- obj$par-Inf
upper <- obj$par+Inf

#lower[names(lower) == 'logSDsurv'] <- 0.05

upper <- obj$par+Inf
upper[names(upper) == 'psel_fish' ] <- 5
upper[names(upper) == 'PSEL'] <- 5
upper[names(upper) == 'logh'] <- log(0.999)
upper[names(upper) == 'F0'] <- 2
#upper[names(upper) == "logphi_survey"]<- log(8)
lower[names(lower) == 'logSDsurv'] <- log(0.01)
lower[names(lower) == 'F0'] <- 0.01
#lower[names(lower) == 'logMinit'] <- log(0.2)

system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper,
                        control = list(iter.max = 1e6, 
                                       eval.max = 1e6))) # If error one of the random effects is unused
rep <- sdreport(obj)
sdrep <- summary(rep)
rep.values<-rownames(sdrep)
nyear <- df$tEnd


sdrep <- summary(rep)
rep.values<-rownames(sdrep)

years <- df$years
source('getUncertainty.R')
df$nyear <- length(years)
df$year <- years

SSB <- getUncertainty('SSB',df.new,sdrep)
F0 <- getUncertainty('Fyear',df.new,sdrep)
Catch <- getUncertainty('Catch',df.new,sdrep)
Surveyobs <- getUncertainty('Surveyobs',df.new,sdrep)
R <- getUncertainty('R',df.new,sdrep)
surveyselec.est <- getUncertainty('surveyselc', df.new,sdrep)
catchselec.est <- getUncertainty('catchselec', df.new,sdrep)

loglik <- data.frame(name = sdrep[rep.values == 'ans_tot',1])
plotValues(Catch, data.frame(x = df$years,y =df$Catch), 'Catch')

SSB$name <- SSB$name*1e-6
SSB$min <- SSB$min*1e-6
SSB$max <- SSB$max*1e-6
#png('Figures/F0.png', width = 16, height = 12, unit = 'cm', res =400)
plotValues(F0, data.frame(x = df.new$years,y =assessment$F0), 'F0')
plotValues(SSB, data.frame(x= assessment$year, y= rowSums(sim.data$SSB)*1e-6),'SSB')
plotValues(Catch, data.frame(x = df.new$years,y =df.new$Catch), 'Catch')

Surveyobs[Surveyobs$year < 1995,] <- NA

plotValues(Surveyobs, data.frame(y = df.new$survey[df$flag_survey == 1], x = df.new$years[df$flag_survey == 1]), 'survey biomass')

#png('estimated_F0.png', width = 16, height = 12, unit = 'cm', res =400)
plotValues(F0, data.frame(y =sim.data$Fseason[21,,1,1], x= assessment$year), 'Fishing mortality')
dev.off()
# Likelihood contributions 

nms <- c('SDR','Selectivity','Catch','survey','survey comps','Catch comps','Priors')

LogLik<- getUncertainty('ans_tot', df.new, sdrep)
LogLik$lik <- nms

#png('Figures/LogLikelihoods.png', width = 16, height = 12, unit = 'cm', res =400)
ggplot(LogLik, aes(y = -name, x= lik))+geom_point()+theme_classic()+scale_y_continuous('LogLikelihood')+scale_x_discrete(name = '')+
  geom_errorbar(aes(x = lik, ymin = -min, ymax = -max), col = 'black')
#dev.off()

# Plot the age comps in all years  
ages <- 1:15
comp.year <- length(df.new$flag_catch[df.new$flag_catch == 1])

age_catch_est <- getUncertainty('age_catch_est', df.new, sdrep)
age_catch <- getUncertainty('age_catch', df.new, sdrep)

age_catch_est$age <- rep(ages, df.new$tEnd)
age_catch_est$year <- rep(df.new$years, each = 15)

head(age_catch_est)

df.plot <- data.frame(comps = c(age_catch_est$name,age_catch$name), 
                      year = rep(age_catch_est$year,2), age = rep(age_catch_est$age,2), model = rep(c('estimated','data'), each = 780))

df.plot <- df.plot[which(df.plot$year %in% df$year[df$flag_catch == 1]),]

#png('Figures/age_comps_catch.png', width = 16, height = 12, unit = 'cm', res =400)
ggplot(data = df.plot, aes(x = age, y = comps, color = model))+geom_line()+facet_wrap(facets = ~year)+theme_bw()
dev.off()

ages <- 1:15
comp.year <- length(df$flag_survey[df$flag_survey == 1])

age_survey_est <- getUncertainty('age_survey_est', df.new, sdrep)
age_survey <- getUncertainty('age_survey', df.new, sdrep)

age_survey_est$age <- rep(ages, df.new$tEnd)
age_survey_est$year <- rep(df.new$years, each = 15)

head(age_survey_est)

df.plot <- data.frame(comps = c(age_survey_est$name,age_survey$name), 
                      year = rep(age_survey_est$year,2), age = rep(age_survey_est$age,2), model = rep(c('estimated','data'), each = 780))

df.plot <- df.plot[which(df.plot$year %in% df$year[df$flag_survey == 1]),]

#png('Figures/age_comps_survey.png', width = 16, height = 12, unit = 'cm', res =400)
ggplot(data = df.plot, aes(x = age, y = comps, color = model))+geom_line()+facet_wrap(facets = ~year)+theme_bw()
#dev.off()

## Compare parameter estimations with the ones from the SS3 assessment 
parms.true <- getParameters(TRUE) # Parameters estimated in the SS3 model 
# Non related parameters
nparms <- 5

nms <- unlist(strsplit(names(rep$par.fixed)[1:nparms],split ='log'))[2*(1:nparms)]
vals <- exp(rep$par.fixed)[1:nparms]
err <- sdrep[,2][1:nparms]

SE <- vals*err

df.plot.parms <- data.frame(value = vals, min = vals-2*SE, max = vals+2*SE, name =nms, model = 'TMB')

df.assessment <- data.frame(value = exp(as.numeric(parms.true[1:5])), min = NA, max = NA, name = nms, model = 'SS3')

df.plot.parms <- rbind(df.plot.parms,df.assessment)

#png('Figures/parameters_estimated.png', width = 16, height = 12, unit = 'cm', res =400)
ggplot(df.plot.parms, aes(x = name, y = value, colour = model))+
  geom_point(size = 2)+geom_linerange(aes(ymin = min, ymax = max))+theme_classic()+facet_wrap(~name, scale = 'free')+
  theme(strip.text.x = element_blank())+scale_x_discrete('')
#dev.off()
png('Figures/parameters_estimated.png', width = 16, height = 12, unit = 'cm', res =400)
ggplot(df.plot.parms[df.plot.parms$name %in% c('Rinit', 'h', 'Minit', 'SDsurv'),], aes(x = name, y = value, colour = model))+
  geom_point(size = 2)+geom_linerange(aes(ymin = min, ymax = max))+theme_classic()+facet_wrap(~name, scale = 'free')+
  theme(strip.text.x = element_blank())+scale_x_discrete('')
dev.off()
# plot the base and survey selectivity 
source('getSelec.R')

sel.ss3 <- getSelec(df$age,psel =parms.true['psel_fish'][[1]],Smin = df$Smin,df$Smax)
sel.tmb <- getSelec(df$age, psel = rep$par.fixed[names(rep$par.fixed) == 'psel_fish'], Smin = df$Smin, Smax = df$Smax)

sel.survey.ss3 <- getSelec(df$age,psel =parms.true['psel_surv'][[1]],Smin = df$Smin_survey,df$Smax_survey)
sel.survey.tmb <- getSelec(df$age,psel = rep$par.fixed[names(rep$par.fixed) == 'psel_surv'],Smin = df$Smin_survey,df$Smax_survey)

df.plot <- data.frame(age = rep(df$age, 4), sel = c(sel.ss3,sel.tmb,sel.survey.ss3,sel.survey.tmb), 
                      fleet = rep(c('fishery','survey'), each = length(df$age)*2), 
                      model = rep(c('ss3','TMB'), each = length(df$age)))

ggplot(df.plot, aes(x = age, y = sel, color = model))+geom_line()+facet_wrap(~fleet)
