#### Run a simulation run of the OM and  EM ####

require(TMB)
require(ggplot2)
require(dplyr)
require(gridExtra)
require(cowplot)
require(scales)
require(RColorBrewer)
library(r4ss)

compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))
source('load_files.R')
source('load_files_OM.R')
source('fnMSE.R')
source('hake_objectives.R')
set.seed(12345)
assessment <- read.csv('data/assessment_MLE.csv')
mod <- SS_output(paste(getwd(),'/data/SS32018/', sep =''), printstats=FALSE, verbose = FALSE)

SSB.ss3 <- mod$derived_quants$Value[grep('SSB_1966', mod$derived_quants$Label):grep('SSB_2018', mod$derived_quants$Label)]
R.ss <- mod$derived_quants$Value[grep('Recr_1966', mod$derived_quants$Label):grep('Recr_2018', mod$derived_quants$Label)]

###### Load the data to run the MSE ######
df <- load_data_seasons(nseason = 4, nspace = 2,
                        nsurvey= 2, movemax = 0.4) # Prepare data for operating model 

simyears <- 50
TAC <- 1
nruns <- 1
sim.data <- run.agebased.true.catch(df)


# Plott stuff 
plot(df$years,rowSums(sim.data$SSB.weight))
lines(df$years,SSB.ss3)

plot(df$years,sim.data$N.save[1,1:df$nyear])
lines(df$years, R.ss, col = 'red')


plot(sim.data$Catch/rowSums(df$Catch.country))



parms <- getParameters_OM(trueparms = TRUE, df = df)

##  Create a data frame to send to runHakeassessment 

df.new <- create_TMB_data(sim.data, df)

parms.new <- parms
F0 <- rowSums(sim.data$Fout)
Rdev <- parms$Rin
parms.new$F0 <- F0
parms.new$Rin <- Rdev

obj <-MakeADFun(df.new,parms.new,DLL="runHakeassessment") # Run the assessment 

reps <- obj$report()


plot(reps$selectivity_save[,29])
lines(sim.data$Fsel[,29,2])

lower <- obj$par-Inf
upper <- obj$par+Inf
upper[names(upper) == 'logh'] <- log(0.999)
upper[names(upper) == 'F0'] <- 2
lower[names(lower) == 'logSDsurv'] <- log(0.01)
lower[names(lower) == 'F0'] <- 0.01


system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper,
                        control = list(iter.max = 1e6, 
                                       eval.max = 1e6))) 

rep <- sdreport(obj)
sdrep <- summary(rep)
rep.values<-rownames(sdrep)
nyear <- df$tEnd
  
R <- data.frame(name = sdrep[rep.values == 'R',1])
  
SSB <- data.frame(name = sdrep[rep.values == 'SSB',1])
SSB$SE <- sdrep[rep.values == 'SSB',2]
SSB$min <- SSB$name-2*SSB$SE
SSB$max <- SSB$name+2*SSB$SE
SSB$year <- df$years

Catch <- data.frame(name = sdrep[rep.values == 'Catch',1])
Catch$SE <- sdrep[rep.values == 'Catch',2]
Catch$min <- Catch$name-2*Catch$SE
Catch$max <- Catch$name+2*Catch$SE
Catch$year <- df$years

plot(df$years,Catch$name)
lines(df$years,rowSums(sim.data$Catch))

plot(SSB$name)
lines(rowSums(sim.data$SSB))
