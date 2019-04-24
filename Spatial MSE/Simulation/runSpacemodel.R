###### Run the HAKE MSE ####### 
library(dplyr)
library(reshape2)

###### Initialize the operating model ###### 
library(TMB)
compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))

seedz <- 100
set.seed(seedz)
# Run the simulation model
source('run_agebased_model_true_Catch.R')


####  Plotting and uncertainty calculation functions #### 
source('ylimits.R')
source('plotUncertainty.R')
source('getUncertainty.R')
source('plotValues.R')

source('getSelec.R') # Calculate hake selectivity
 
source('load_data_seasons.R') # Loads data for Operating model
source('create_TMB_data.R') # Compiles operating model data to tmb data

source('getRefpoint.R') # Calculate refrence points 
source('Check_Identifiable_vs2.R') # see if hessian is positive definite 

source('getParameters.R')
source('calcSSB0.R')

assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

parms.true <- getParameters(TRUE)
Catch.obs <- read.csv('hake_totcatch.csv')

df <- load_data_seasons()

df$Catch <- Catch.obs$Fishery
time <- 1
yrinit <- df$nyear
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 
#
# df$parms$Rin <- df$parms$Rin*0
# df$F0 <- 0*df$F0

simyears <- 25 # Project 25 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df)
simdata0 <- sim.data # The other one is gonna get overwritten. 


par(mfrow = c(3,1), mar = c(4,4,1,1))
plot(df$years,rowSums(sim.data$SSB)/sum(sim.data$SSB_0[2]), type = 'l', ylab = 'SSB/SSB_0')
SSB0_ass <- calcSSB0(exp(parms.true$logRinit), exp(parms.true$logMinit), df$nage, df$Matsel)
lines(assessment$year,assessment$SSB/SSB0_ass, col = 'red')

plot(df$years,rowSums(sim.data$SSB), type = 'l', ylab = 'SSB')
lines(assessment$year, assessment$SSB, col = 'red')

plot(df$years,df$Catch)
lines(df$years, sim.data$Catch)
# 
# save(sim.data,file = 'simulated_space_OM.Rdata')
# save(df,file = 'sim_data_parms.Rdata')
years <- df$years

PSEL <- matrix(0,5, length(1991:years[length(years)]))
initN <- rep(0,df$nage-1)
F0 <- rep(0.01, df$nyear)
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
  
obj <-MakeADFun(df.new,parms,DLL="runHakeassessment", silent = TRUE) # Run the assessment 
  
reps <- obj$report()

lower <- obj$par-Inf
upper <- obj$par+Inf

lower[names(lower) == 'F0'] <- 1e-10
upper <- obj$par+Inf
upper[names(upper) == 'psel_fish' ] <- 5
upper[names(upper) == 'PSEL'] <- 5
# upper[names(upper) == 'logh'] <- log(0.999)
# upper[names(upper) == 'F0'] <- 1.2


system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper,
                        control = list(iter.max = 5000, 
                                       eval.max = 5000))) # If error one of the random effects is unused


if(opt$convergence != 0){
  print(paste('year',df$years[length(df$years)], 'did not converge'))
  stop('Model not converged')
}

system.time(rep<-sdreport(obj))
#rep

#xx<- Check_Identifiable_vs2(obj)

#rep
sdrep <- summary(rep)
rep.values<-rownames(sdrep)
reps <- obj$report()
SSB <- getUncertainty('SSB',df)

plotValues(SSB, data.frame(x= assessment$year, y= assessment$SSB),'SSB')

yl <- ylimits(SSB$name,sim.data$SSB[,1])
plot(df$years,SSB$name, ylim = yl)
lines(df$years,rowSums(sim.data$SSB), col = 'red') # One model takes SSB in the beginning of the year the other in the end
lines(df$years,assessment$SSB, col = 'green')

## Compare the data from the sim.data and the assessment model
yr <- df$years

plot(yr,df$Catch, type ='o')
lines(yr,sim.data$Catch)


## Survey 
plot(yr[df$survey > 1],df$survey[df$survey > 1])
lines(yr[df$survey > 1],sim.data$survey[df$survey > 1], col = 'red')

### Age comps survey 
yr.s <- df$years[df$flag_survey == 1]

age.df <- as.data.frame(t(df$age_survey))[df$flag_survey == 1,]
age.df$year <- yr.s

age.df <- melt(age.df, id.vars = 'year', variable.name = 'age')
age.df <- age.df[order(age.df$year),]
age.df$age <- rep(1:15, length(yr.s))
age.df$model <- 'assessment'

# And from the simulated data 

age.df.sim <- as.data.frame(t(sim.data$age_comps_surv))[df$flag_survey == 1,]
age.df.sim$year <- yr.s
age.df.sim <- melt(age.df.sim, id.vars = 'year', variable.name = 'age')
age.df.sim <- age.df.sim[order(age.df.sim$year),]
age.df.sim$age <- rep(1:15, length(yr.s))
age.df.sim$model <- 'OM'

age.df <- rbind(age.df,age.df.sim)

ggplot(age.df, aes(x = age, y= value, color = model))+geom_line()+facet_wrap(~year)

### Age comps catch
yr.s <- df$years[df$flag_catch == 1]

age.df <- as.data.frame(t(df$age_catch))[df$flag_catch == 1,]
age.df$year <- yr.s

age.df <- melt(age.df, id.vars = 'year', variable.name = 'age')
age.df <- age.df[order(age.df$year),]
age.df$age <- rep(1:15, length(yr.s))
age.df$model <- 'assessment'

# And from the simulated data 

age.df.sim <- as.data.frame(t(sim.data$age_catch))[df$flag_catch == 1,]
age.df.sim$year <- yr.s
age.df.sim <- melt(age.df.sim, id.vars = 'year', variable.name = 'age')
age.df.sim <- age.df.sim[order(age.df.sim$year),]
age.df.sim$age <- rep(1:15, length(yr.s))
age.df.sim$model <- 'OM'

age.df <- rbind(age.df,age.df.sim)

ggplot(age.df, aes(x = age, y= value, color = model))+
  geom_line(data = age.df[age.df$model == 'assessment',])+
  geom_point(data = age.df[age.df$model == 'OM',])+facet_wrap(~year)


