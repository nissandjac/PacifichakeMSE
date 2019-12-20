# Run the hake assessment 
source('load_files.R')
source('getParameters_ss.R')
source('load_data_ss.R')
library(r4ss)
library(dplyr)
library(reshape2)
# Read the assessment data 
mod <- SS_output(paste(getwd(),'/data/SS32018/', sep =''), printstats=FALSE, verbose = FALSE)

df <- load_data_ss(mod)
df$smul <- 0.5

years <- df$years

#U[2,] <- 0.01
parms.ss <- getParameters_ss(TRUE, mod)


compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))
obj <-MakeADFun(df,parms.ss,DLL="runHakeassessment")#, )

vars <- obj$report()

age_survey  <- obj$report()$age_survey_est
age_catch <- obj$report()$age_catch
# Compare the two models with the same parameters 

SSBass <- vars$SSB
SSB.ss3 <- mod$derived_quants$Value[grep('SSB_1966', mod$derived_quants$Label):grep('SSB_2018', mod$derived_quants$Label)]
R.ss <- mod$derived_quants$Value[grep('Recr_1966', mod$derived_quants$Label):grep('Recr_2018', mod$derived_quants$Label)]

plot(df$years,SSBass)
lines(df$years,SSB.ss3)

plot(df$years, R.ss, log = 'y')
lines(df$years,vars$R)

# Compare selectivity in year 1993
selyear <- 2010
plot(df$age,vars$selectivity_save[,which(df$years == selyear)])
idx <- which(mod$ageselex$Yr == selyear & mod$ageselex$Factor == 'Asel')
lines(df$age,as.numeric(mod$ageselex[idx,8:28]), col = 'red')


plot(df$years,df$Catchobs/obj$report()$Catch)



# Compare surveys 
plot(df$years,vars$Surveyobs, xlim = c(1990, 2020))
lines(mod$cpue$Yr,mod$cpue$Exp, type = 'l')
points(mod$cpue$Yr, mod$cpue$Obs, col = 'red')

# See N at age 
Ninit <- mod$natage[mod$natage$Era == 'INIT' & mod$natage$`Beg/Mid` == 'B',]
N.ss <- mod$natage[mod$natage$Era == 'TIME' & mod$natage$`Beg/Mid` == 'B',]
ix <- 13:33 # ages in the df above 
# First year 
yr <- 2010

df.N <- data.frame(N = c(as.numeric(N.ss[N.ss$Yr == yr,ix]), vars$N_beg[,which(df$years == yr)]),
                   model = c(rep('ss', df$nage),rep('tmb', df$nage)), age = rep(df$age,2))

ggplot(df.N, aes(x= age, y = N, color = model))+geom_line()

# Initial distribution 
df.N <- data.frame(N = c(as.numeric(Ninit[,ix]), vars$Nzero),
                   model = c(rep('ss', df$nage),rep('tmb', df$nage)), age = rep(df$age,2))

ggplot(df.N, aes(x= age, y = N, color = model))+geom_line()

# Compare age distribution in catch 
ac.tmb <- as.data.frame(t(vars$age_catch_est))
colnames(ac.tmb) <-paste(1:15)
ac.tmb$year <- df$years
ac.tmb.plot <- melt(ac.tmb, id.vars = 'year', variable.name = 'age')
ac.tmb.plot <- ac.tmb.plot[ac.tmb.plot$year %in% df$years[df$flag_catch == 1],]


age.ss <- mod$agedbase[mod$agedbase$Fleet == 1,]
age.ss.plot <- data.frame(year = age.ss$Yr, value = age.ss$Exp, age = age.ss$Bin)

ac.ss3 <- as.data.frame(t(df$age_catch))
colnames(ac.ss3) <-paste(1:15)
ac.ss3$year <- df$years
ac.ss3.plot <- melt(ac.ss3, id.vars = 'year', variable.name = 'age')
ac.ss3.plot <- ac.ss3.plot[ac.ss3.plot$year %in% df$years[df$flag_catch == 1],]

yr <- 1970

df.N <- data.frame(N = c(as.numeric(N.ss[N.ss$Yr == yr,ix]), vars$N_beg[,which(df$years == yr)]),
                   model = c(rep('ss', df$nage),rep('tmb', df$nage)), age = rep(df$age,2))

ggplot(df.N, aes(x= age, y = N, color = model))+geom_line()+scale_y_log10()


ggplot(ac.tmb.plot[ac.tmb.plot$year == yr,],aes(x= as.numeric(age), y = value))+
  geom_line(col = 'red')+
  geom_point(data =age.ss.plot[age.ss.plot$year == yr,])#+
  

# Compare the total catch at age 
caa <- mod$catage[mod$catage$Era == 'TIME',]
yr <- 1966
ix <- 11:31


df.c<- data.frame(N = c(as.numeric(caa[caa$Yr == yr,ix]), vars$CatchNAge[,which(df$years == yr)]),
                   model = c(rep('ss', df$nage),rep('tmb', df$nage)), age = rep(df$age,2))


ggplot(df.c, aes(x= age, y = N, color = model))+geom_line()


ggplot(ac.tmb.plot[ac.tmb.plot$year == yr,],aes(x= as.numeric(age), y = value))+
  geom_line(col = 'red')+
  geom_point(data =age.ss.plot[age.ss.plot$year == yr,])#+
# Compare age distribution in survey
ac.tmb <- as.data.frame(t(vars$age_survey_est))
colnames(ac.tmb) <-paste(1:15)
ac.tmb$year <- df$years
ac.tmb.plot <- melt(ac.tmb, id.vars = 'year', variable.name = 'age')
ac.tmb.plot <- ac.tmb.plot[ac.tmb.plot$year %in% df$years[df$flag_survey == 1],]


age.sssurvey <- mod$agedbase[mod$agedbase$Fleet == 2 & mod$agedbase$Yr %in% df$years[df$flag_survey == 1],]
age.ss_survey.plot <- data.frame(year = age.sssurvey$Yr, value = age.sssurvey$Exp, age = age.sssurvey$Bin)


ggplot(ac.tmb.plot,aes(x= as.numeric(age), y = value))+geom_line(col = 'red')+geom_point(data =age.ss_survey.plot)+
  facet_wrap(~year)

ggplot(ac.tmb.plot[ac.tmb.plot$age == 1,], aes(x= year, y = value))


lower <- obj$par-Inf

lower[names(lower) == 'F0'] <- 0.001
upper <- obj$par+Inf
#upper[names(upper) == 'psel_fish' ] <- 5
upper[names(upper) == 'PSEL'] <- 9
upper[names(upper) == 'logh'] <- log(0.999)
upper[names(upper) == 'F0'] <- 2


system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper, 
                        control = list(iter.max = 2000,
                                       eval.max = 2000))) #

system.time(rep<-sdreport(obj))
rep

xx<- Check_Identifiable_vs2(obj)
# 
# tt <- TMBhelper::Optimize(obj,fn = obj$fn,obj$gr,lower=lower,upper=upper,
#                            control = list(iter.max = 1e8, eval.max = 1e8,
#                                           rel.tol = 1e-10))
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
SSB0 <- getUncertainty('SSBzero', df)


SSB$name <- SSB$name
SSB$min <- SSB$min
SSB$max <- SSB$max


#png('Figures/SSB_survey_mid.png', width = 16, height = 12, unit = 'cm', res =400)
plotValues(SSB, data.frame(x= df$years, y= SSB.ss3),'SSB')
#dev.off()


## Do the same plot with 
df.plot <- data.frame(SSB = SSB$name/(SSB0$name*1e-6))

plotValues(Catch, data.frame(x = df$years,y =df$Catchobs), 'Catch')

#png('Figures/F0.png', width = 16, height = 12, unit = 'cm', res =400)
plotValues(F0, data.frame(x = df$years,y =df$F0), 'F0')
#dev.off()
# yl <- ylimits(Biomass$name*1e-9,df$survey[df$survey > 1]*1e-9)
# plot(years[df$survey>1],Biomass$name[df$survey>1]*1e-9, xlim = c(1990,2019), ylim = yl, xlab= 'survey')
# points(years[df$survey > 1],df$survey[df$survey > 1]*1e-9, col = 'green')
Surveyobs[Surveyobs$year < 1995,] <- NA

plotValues(Surveyobs, data.frame(y = df$survey[df$flag_survey == 1], x = df$years[df$flag_survey == 1]), 'survey biomass')

#png('estimated_F0.png', width = 16, height = 12, unit = 'cm', res =400)
plotValues(F0, data.frame(y =assessment$F0, x= assessment$year), 'Fishing mortality')
dev.off()
# Likelihood contributions 

nms <- c('SDR','Selectivity','Catch','survey','survey comps','Catch comps','Priors')

LogLik<- getUncertainty('ans_tot', df)
LogLik$lik <- nms

#png('Figures/LogLikelihoods.png', width = 16, height = 12, unit = 'cm', res =400)
ggplot(LogLik, aes(y = -name, x= lik))+geom_point()+theme_classic()+scale_y_continuous('LogLikelihood')+scale_x_discrete(name = '')+
  geom_errorbar(aes(x = lik, ymin = -min, ymax = -max), col = 'black')
#dev.off()

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

#png('Figures/age_comps_catch.png', width = 16, height = 12, unit = 'cm', res =400)
ggplot(data = df.plot, aes(x = age, y = comps, color = model))+geom_line()+facet_wrap(facets = ~year)+theme_bw()
dev.off()

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

#png('Figures/age_comps_survey.png', width = 16, height = 12, unit = 'cm', res =400)
ggplot(data = df.plot, aes(x = age, y = comps, color = model))+geom_line()+facet_wrap(facets = ~year)+theme_bw()
#dev.off()

## Compare parameter estimations with the ones from the SS3 assessment 
parms.true <- getParameters(TRUE) # Parameters estimated in the SS3 model 
# Non related parameters

nms <- unlist(strsplit(names(rep$par.fixed)[1:6],split ='log'))[2*(1:6)]
vals <- exp(rep$par.fixed)[1:6]
err <- sdrep[,2][1:6]

SE <- vals*err

df.plot.parms <- data.frame(value = vals, min = vals-2*SE, max = vals+2*SE, name =nms, model = 'TMB')

df.assessment <- data.frame(value = exp(as.numeric(parms.true[1:6])), min = NA, max = NA, name = nms, model = 'SS3')

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

png('Figures/selectivities.png', width = 16, height = 12, unit = 'cm', res =400)
ggplot(df.plot, aes(x = age, y = sel, color = model))+geom_line()+facet_wrap(~fleet)
dev.off()

# 
# fit <- tmbstan(obj = obj, chains = 1, init = unlist(parms), lower = lower, upper = upper)
# launch_shinystan(fit)


