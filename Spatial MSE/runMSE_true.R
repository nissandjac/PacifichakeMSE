## Run a simple MSE based on subfunctions ###### Run the HAKE MSE ####### 
direc <- "C:/Users/Nis/Dropbox/NOAA/Hake MSE/Hake MSE true/"
setwd(direc)
###### Initialize the operating model ###### 
library(TMB)
compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))
# Run the simulation model
source('run_agebased_model_true.R')
source('ylimits.R')
source('plotUncertainty.R')
source('getUncertainty.R')
source('SSB0calc.R')
source('getSelec.R')
source('load_data.R')

df <- load_data()
time <- 1
yrinit <- df$tEnd
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 

simyears <- 10 # Project 30 years into the future  
N0 <- NA
sim.data <- run.agebased.true(df, N0 = N0)
simdata0 <- sim.data # The other one is gonna get overwritten. 

F40.save<- array(NA,simyears)

# Save some stuff
SSB.save <- list()
R.save <- list()

# Save som OM stuff 
SSB.save.om <- array(NA, df$tEnd+simyears)
R.save.om <- array(NA, df$tEnd+simyears)
Catch.save.om <- array(NA, df$tEnd+simyears)
# Before the MSE starts 
SSB.save.om[1:df$tEnd] <- sim.data$SSB
R.save.om[1:df$tEnd] <- sim.data$N.save[1,]
Catch.save.om[1:df$tEnd] <- sim.data$Catch
F0.save <- df$fmort

for (time in 1:simyears){
  
  year <- yrinit+(time-1)
  
  if (time > 1){
    
    sim.data.tmp <- run.agebased(df, N0 = Ntmp)
    
    # Add to the original data frame 
    
    # 1 measurement per year
    sim.data$SSB <- c(sim.data$SSB, sim.data.tmp$SSB[df$tEnd])
    sim.data$Catch<- c(sim.data$Catch, sim.data.tmp$Catch[df$tEnd])
    sim.data$Catch.obs <- c(sim.data$Catch.obs, sim.data.tmp$Catch.obs[df$tEnd])
    
    # Measurement per age 
    sim.data$N.save <- cbind(sim.data$N.save,sim.data.tmp$N.save[,df$tEnd])
    sim.data$survey <- cbind(sim.data$survey,sim.data.tmp$survey[,df$tEnd])
    sim.data$Catch.age <- cbind(sim.data$Catch.age,sim.data.tmp$Catch.age[,df$tEnd])
    
  }
  U <- matrix(0, 2 , df$tEnd-1)
  PSEL <- matrix(0,5, length(1991:df$years[length(df$years)]))
  
  
  # First run the operating model with current \

  
  if (time ==1){
    parms <- list( # Just start all the simluations with the same initial conditions 
      logRinit = 14.8354,
      logh = log(0.8122),
      logMinit = log(0.2229),
      logSDsurv = log(0.3048),
      logphi_catch = log(0.3),
      logSDF = log(0.1),
      # Selectivity parameters 
      psel_fish = c(2.8476, 0.973,0.3861,0.1775,0.5048),
      psel_surv = c(0.5919,-0.2258,0.2876,0.3728),
      initN = rep(0,nage-1),
      Rin = U[1,],
      F0 = U[2,],
      PSEL = PSEL
    )}else{
      parms <- as.list(rep$par.fixed)
      parms$U <- U
    }
  
  obj <-MakeADFun(df,parms,DLL="runHakeassessment", random = 'U') # Run the assessment 
  
  lower <- obj$par-Inf
  #lower['logFinit'] <- log(0.1)
  upper <- obj$par+Inf
  #upper['logQ'] <- log(0.01) # Sometimes Q goes over 1 
  
  system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper)) # If error one of the random effects is unused
  
  rep<-sdreport(obj)
  #Uncertainty 
  sdrep <- summary(rep)
  rep.values<-rownames(sdrep)
  nyear <- df$tEnd
  
  SSB <- getUncertainty('SSB',df)
  F0 <- getUncertainty('Fyear',df)
  Catch <- getUncertainty('Catch',df)
  N <- getUncertainty('N',df)
  N$age <- rep(seq(1,df$nage), length.out = year*df$nage)
  N$year <- rep(1:year, each = df$nage)
  
  surveyest <- getUncertainty('surveyest',df)
  R <- getUncertainty('R',df)
  
  ## Plots 
  plotUncertainty(SSB, sim.data$SSB) 
  
  # Calculate the fishing mortality needed to reach F40  
  
  F40 <- SSB0calc()
  
  
  df$fmort <- c(df$fmort[df$tEnd], F40$Fnext)
  Ntmp <- N$name[N$year == year]
  
  df$tEnd <- 2
  
  
  # Save some EM stuff in the last year 
  SSB.save[[time]] <- SSB
  R.save[[time]] <- R
  F40.save[time] <- F40$Fnext
  
  # And the fishing mortality
  F0.save <- c(F0.save,F40$Fnext)
  
}

# Plot the SSB over time and see if it changed 
plot(SSB.save[[1]]$name, xlim = c(8, df$tEnd), ylim = c(2e12,3e13), type ='l')
for (i in 2:simyears){
  
  if (i == simyears){
    lines(SSB.save[[i]]$name, col = alpha('green', alpha = 0.6))
    lines(SSB.save[[i]]$min, col = alpha('green', alpha = 0.6))
    lines(SSB.save[[i]]$max, col = alpha('green', alpha = 0.6))
  }else{
    lines(SSB.save[[i]]$name, col = alpha('black', alpha = 0.3))
    
    
  }
}
lines(sim.data$SSB, col = 'red')

### Plot the estimated variance parameters vs the true ones after the last 

library(ggplot2)

se <- summary(rep, "fixed")[, "Std. Error"]

SDR <- exp(rep$par.fixed['logSDR'])
SDF <- exp(rep$par.fixed['logSDF'])
se <- rep[par, "Std. Error"]

par(mfrow = c(1, 1), mar = c(3, 3, 0, 0), oma = c(.5, .5, .5, .5),
    mgp = c(2, 0.5, 0), cex = 1, tck = -0.02)
plot(1, 1, xlim = c(0, 2), ylim = c(0, 3), type = "n",
     xlab = "Coefficient value", ylab = "", yaxt = "n")
axis(2, at = c(1,2), labels = c("SDR", "SDF"),
     las = 1)

points(SDR,1, pch = 19)
lines(seq(SDR-2*se['logSDR'],SDR+2*se['logSDR'], length.out = 10),rep(1,10))
points(df$sd.rec,1, col = 'red')

points(SDF,2, pch = 19)
lines(seq(SDF-2*se['logSDF'],SDF+2*se['logSDF'], length.out = 10),rep(2,10))
points(0.05,2, col = 'red')




