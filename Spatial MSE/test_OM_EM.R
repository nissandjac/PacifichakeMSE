### Run a bunch of MSE's for the future - uncertainty in Recruitment and survey 
source('load_files.R')
source('load_files_OM.R')
source('getParameters_mod.R')
###### Initialize the operating model ###### 
source('calcMeanAge.R')
source('load_data_seasons_mod.R')
library(r4ss)
library(TMB)

# Read the assessment data 
# assessment <- read.csv('data/asssessment_MLE.csv')
# assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]
mod <- SS_output(paste(getwd(),'/data/', sep =''), printstats=FALSE, verbose = FALSE)

compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))


df <- load_data_seasons_mod(nseason = 4, nspace = 2,
                        movemaxinit = 0.5, movefiftyinit = 5, 
                        nsurvey = 2, mod = mod)

time <- 1
yrinit <- df$nyear

parms <- getParameters_mod(TRUE,mod = mod, df= df)

### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 
#
# df$parms$Rin <- df$parms$Rin*0
# df$F0 <- 0*df$F0

simyears <- 30# Project 30 years into the future (2048 that year)
moveparms <- NA
seed <- 123

sim.data <- run.agebased.true.catch(df)

SSB.idx <- grep('SSB_1966', rownames(mod$derived_quants)):grep('SSB_2017', rownames(mod$derived_quants))
SSB.ss3 <- mod$derived_quants$Value[SSB.idx]

parms.true <- getParameters_OM(TRUE, df)
parms.true$F0 <- rowSums(sim.data$Fout)
##  Create a data frame to send to runHakeassessment 

df.new <- create_TMB_data(sim.data, df)
#@df2 <- load_data()
# parms.true$F0 <- df$parms$F0

if(exists('obj')){
  rm(obj)
}


obj <-MakeADFun(df.new,parms.true,DLL="runHakeassessment", silent = FALSE) # Run the assessment 
#obj2 <-MakeADFun(df2,parms,DLL="runHakeassessment", silent = TRUE) # Run the assessment 

repsold <- obj$report()


plot(repsold$pmax_catch_save)
lines(sim.data$p.save)



plot(df$years,rowSums(sim.data$SSB), type ='l')
lines(df$years, SSB.ss3)
lines(df$years,repsold$SSB, col = 'red')

plot(df$years,sim.data$Catch)
lines(df$years,df$Catch)
lines(df$years,repsold$Catch, col = 'red')
# Compare selectivity 
# Compare selectivity in year 1993
selyear <- 1991
plot(df$age,repsold$selectivity_save[,which(df$years == selyear)], ylim = c(0,1.2))
lines(df$age, sim.data$Fsel[which(df$years == selyear),1,])
idx <- which(mod$ageselex$Yr == selyear & mod$ageselex$Factor == 'Asel')
lines(df$age,as.numeric(mod$ageselex[idx,8:28]), col = 'red')


plot(df$years,repsold$Catch)
lines(df$years,sim.data$Catch)
lines(df$years,df$Catch, col = 'red')

#lines(df$years, rep(1, df$nyear))


#plot(df$age,repsold$N[,1]/sim.data$N.save.age[,1])


lower <- obj$par-Inf
upper <- obj$par+Inf

lower[names(lower) == 'F0'] <- 0.01
upper <- obj$par+Inf
#upper[names(upper) == 'psel_fish' ] <- 5
upper[names(upper) == 'PSEL'] <- 5
upper[names(upper) == 'logh'] <- log(0.999)
upper[names(upper) == 'F0'] <- 2

system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper,
                        control = list(iter.max = 1e8, eval.max = 1e8,
                                       rel.tol = 1e-10))) # If error one of the random effects is unused
opt
#system.time(opt2<-nlminb(obj2$par,obj2$fn,obj2$gr,lower=lower,upper=upper,
#                       control = list(iter.max = 1e5, 
#                                     eval.max = 1e5))) # If error one of the random effects is unused
tt <- TMBhelper::Optimize(obj,fn = obj$fn,obj$gr,lower=lower,upper=upper,
                          control = list(iter.max = 1e8, eval.max = 1e8,
                                         rel.tol = 1e-10))
plot(tt$opt$diagnostics$final_gradient)

if(opt$convergence != 0){
  print(paste('year',df$years[length(df$years)], 'did not converge'))
  stop('Model not converged')
  xx<- Check_Identifiable_vs2(obj)
  
}

reps <- obj$report()

SSB <- reps$SSB
Fyear <- reps$Fyear
N <- reps$N
Catch <- reps$Catch
R <- reps$R


plot(SSB)
lines(SSB.ss3)
lines(rowSums(sim.data$SSB), col = 'red')

# Plot all the fitted data 

plot(df$years,sim.data$Fsave)
lines(df$years,Fyear)

plot(df$years,sim.data$Catch/Catch)
#lines(df$years,Catch)

age.ass <- calcMeanAge(reps$age_catch_est,df.new$age_maxage)
age.sim <- calcMeanAge(df.new$age_catch,df.new$age_maxage)

plot(df$years,reps$age_catch_est[5,])
lines(df$years,df.new$age_catch[5,])

plot(df$years,age.ass)
lines(df$years,age.sim)

AS.ass <- calcMeanAge(reps$age_survey_est,df.new$age_maxage)
AS.sim <- calcMeanAge(df.new$age_survey,df.new$age_maxage)
AS.sim[is.na(AS.sim)] <- 0

plot(df$years,AS.ass)
lines(df$years,AS.sim, col = 'red',type = )

reps$ans_tot
repsold$ans_tot

print(sum(reps$ans_tot)/sum(repsold$ans_tot))

plot(reps$Surveyobs[df$flag_survey == 1])
lines(sim.data$survey.true[df$flag_survey == 1], col = 'red')

opt$par[opt$par == 'logSDsurv']

sum(-dnorm(log(repsold$Surveyobs[df$flag_survey == 1]),
           log(df.new$survey[df$flag_survey == 1]), 
           exp(parms.true$logSDsurv)+df.new$survey_err[df$flag_survey == 1], log = TRUE))

sum(-dnorm(log(repsold$Surveyobs[df$flag_survey == 1]),
           log(df.new$survey[df$flag_survey == 1]), 
           exp(opt$par[names(opt$par) == 'logSDsurv'])+
             df.new$survey_err[df$flag_survey == 1], log = TRUE))

