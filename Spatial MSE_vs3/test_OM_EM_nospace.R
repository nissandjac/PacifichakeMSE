### Run a bunch of MSE's for the future - uncertainty in Recruitment and survey 
setwd("~/GitHub/PacifichakeMSE/Spatial MSE_vs2")

###### Initialize the operating model ###### 
library(TMB)
compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))

# Run the simulation model
source('run_agebased_model_true_Catch_no_movement.R')

####  Plotting and uncertainty calculation functions #### 
source('ylimits.R')
source('plotUncertainty.R')
source('getUncertainty.R')
source('plotValues.R')

source('getSelec.R') # Calculate hake selectivity

source('load_data_seasons.R') # Loads data for Operating model
source('create_TMB_data.R') # Compiles operating model data to tmb data

source('getRefpoint.R') # Calculate refrence points 
source('Check_Identifiable_no.R') # see if hessian is positive definite 

source('getParameters.R')
source('calcSSB0.R')
source('run_multiple_MSEs.R')
source('load_data_seasons_move.R')
source('calcMeanAge.R')

assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

#parms.true <- getParameters(TRUE)
Catch.obs <- read.csv('hake_totcatch.csv')

df <- load_data_seasons(move = FALSE,nseason = 1,nspace = 1)
df$Catch <- Catch.obs$Fishery
time <- 1
yrinit <- df$nyear
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 

moveparms <- NA
seed <- 123

sim.data <- run.agebased.true.catch(df)

plot(sim.data$SSB)
lines(assessment$SSB)


PSEL <- matrix(0,5, length(1991:df$years[length(df$years)]))
initN <- rep(0,df$nage-1)
F0 <- rep(0.001, df$nyear)
Rdev <- rep(0, df$nyear)

parms <- list( # Just start all the simluations with the same initial conditions 
  logRinit = 15,
  logh = log(0.5),
  logMinit = log(0.3),
  logSDsurv = log(0.3),
  logphi_catch = log(0.8276),
  logphi_survey = log(11.33),
  # Selectivity parameters 
  psel_fish = c(2.486490, 0.928255,0.392144,0.214365,0.475473),
  psel_surv = c(0.568618,-0.216172,0.305286 ,0.373829),
  initN = initN,
  Rin = Rdev,
  F0 = F0,
  PSEL = PSEL
)

##  Create a data frame to send to runHakeassessment 

df.new <- create_TMB_data(sim.data, df)
#@df2 <- load_data()
parms.true <- df$parms
parms.true$F0 <- sim.data$Fsave

if(exists('obj')){
  rm(obj)
}


obj <-MakeADFun(df.new,parms.true,DLL="runHakeassessment", silent = TRUE) # Run the assessment 
#obj2 <-MakeADFun(df2,parms,DLL="runHakeassessment", silent = TRUE) # Run the assessment 

repsold <- obj$report()

plot(df$years,repsold$CatchN)
lines(df$years,colSums(sim.data$CatchN.save.age))
lines(df$years,df$Catch)
#lines(df$years, rep(1, df$nyear))


plot(df$age,repsold$N[,1]/sim.data$N.save.age[,1])


age.ass <- calcMeanAge(repsold$age_catch_est,df.new$age_maxage)
age.sim <- calcMeanAge(df.new$age_catch,df.new$age_maxage)

plot(df$years,repsold$age_catch_est[5,]/df.new$age_catch[5,])

plot(df$years,age.ass/age.sim)

AS.ass <- calcMeanAge(repsold$age_survey_est,df.new$age_maxage)
AS.sim <- calcMeanAge(df.new$age_survey,df.new$age_maxage)
AS.sim[is.na(AS.sim)] <- 0

plot(df$years[AS.ass >0],AS.ass[AS.ass >0]/AS.sim[AS.sim >0])
#lines(df$years[AS.sim >0],, col = 'red',type = )

plot(df$years,sim.data$survey.true/repsold$Surveyobs)

plot(df$years,sim.data$survey)
lines(df$years,repsold$Surveyobs)

plot(df$years, repsold$Catch/sim.data$Catch)
#lines(df$years, sim.data$Catch)
#lines(df$years, df$Catch, col = 'red')

plot(df$years,repsold$Zsave[15,]/sim.data$Z[15,])

lower <- obj$par-Inf
upper <- obj$par+Inf

lower[names(lower) == 'F0'] <- 0.01
lower[names(lower) == 'logSDsurv'] <- log(0.1)
upper <- obj$par+Inf
upper[names(upper) == 'psel_fish' ] <- 5
upper[names(upper) == 'PSEL'] <- 5
upper[names(upper) == 'logh'] <- log(0.999)
upper[names(upper) == 'F0'] <- 2
upper[names(upper) == 'logSDsurv'] <- log(1)

system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper,
                        control = list(iter.max = 1e8, eval.max = 1e8))) # If error one of the random effects is unused
opt
#system.time(opt2<-nlminb(obj2$par,obj2$fn,obj2$gr,lower=lower,upper=upper,
#                       control = list(iter.max = 1e5, 
#                                     eval.max = 1e5))) # If error one of the random effects is unused


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
lines(assessment$SSB)
lines((sim.data$SSB), col = 'red')

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

plot(reps$Surveyobs[df$flag_survey == 1]/sim.data$survey.true[df$flag_survey == 1])
lines(repsold$Surveyobs[df$flag_survey == 1]/sim.data$survey.true[df$flag_survey == 1], col = 'red')

opt$par[opt$par == 'logSDsurv']

sum(-dnorm(log(repsold$Surveyobs[df$flag_survey == 1]),
           log(df.new$survey[df$flag_survey == 1]), 
           exp(parms.true$logSDsurv)+df.new$survey_err[df$flag_survey == 1], log = TRUE))

sum(-dnorm(log(repsold$Surveyobs[df$flag_survey == 1]),
           log(df.new$survey[df$flag_survey == 1]), 
           exp(opt$par[names(opt$par) == 'logSDsurv'])+
             df.new$survey_err[df$flag_survey == 1], log = TRUE))

