##### Calculate selectivity ##### 

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


df <- load_data_seasons_mod(nseason = 1, nspace = 1,
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
parms.true$F0 <- sim.data$Fout
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

plot(df$years,sim.data$SSB, type ='l')
lines(df$years, SSB.ss3)
lines(df$years,repsold$SSB, col = 'red')

#plot(df$years,repsold$CatchN)
#lines(df$years,colSums(sim.data$CatchN.save.age))
# Compare selectivity 
# Compare selectivity in year 1993
selyear <- 2010
plot(df$age,repsold$selectivity_save[,which(df$years == selyear)]/max(repsold$selectivity_save[,which(df$years == selyear)]), ylim = c(0,1.2))
lines(df$age, sim.data$Fsel[which(df$years == selyear),1,])
idx <- which(mod$ageselex$Yr == selyear & mod$ageselex$Factor == 'Asel')
lines(df$age,as.numeric(mod$ageselex[idx,8:28]), col = 'red')

