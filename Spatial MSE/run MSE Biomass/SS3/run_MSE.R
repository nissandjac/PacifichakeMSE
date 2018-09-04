## Run a simple MSE based on subfunctions ###### Run the HAKE MSE ####### 
direc <- "C:/Users/Nis/Dropbox/NOAA/Hake MSE"
setwd(direc)
###### Initialize the operating model ###### 
library(TMB)
compile("EM_hake2.cpp")
dyn.load(dynlib("EM_hake2"))
# Run the simulation model
source('run_agebased_model2.R')
source('ylimits.R')
source('plotUncertainty.R')
source('getOMparms.R')
source('getUncertainty.R')
source('SSB0calc.R')
df <- getOMparms()
time <- 1
yrinit <- df$nyear
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 

simyears <- 10 # Project 10 years into the future  
sim.data <- run.agebased(df, N0 = NA)
simdata0 <- sim.data # The other one is gonna get overwritten. 

F40.save<- array(NA,simyears)

# Save some stuff
SSB.save <- list()
R.save <- list()

# Save som OM stuff 
SSB.save.om <- array(NA, df$nyear+simyears)
R.save.om <- array(NA, df$nyear+simyears)
Catch.save.om <- array(NA, df$nyear+simyears)
# Before the MSE starts 
SSB.save.om[1:df$nyear] <- sim.data$SSB
R.save.om[1:df$nyear] <- sim.data$N.save[1,]
Catch.save.om[1:df$nyear] <- sim.data$Catch
F0.save <- df$fmort

for (time in 1:simyears){
  
    year <- yrinit+(time-1)
    
    if (time > 1){
      
      sim.data.tmp <- run.agebased(df, N0 = Ntmp)
      
      # Add to the original data frame 
      
      # 1 measurement per year
      sim.data$SSB <- c(sim.data$SSB, sim.data.tmp$SSB[df$nyear])
      sim.data$Catch<- c(sim.data$Catch, sim.data.tmp$Catch[df$nyear])
      sim.data$Catch.obs <- c(sim.data$Catch.obs, sim.data.tmp$Catch.obs[df$nyear])
      
      # Measurement per age 
      sim.data$N.save <- cbind(sim.data$N.save,sim.data.tmp$N.save[,df$nyear])
      sim.data$survey <- cbind(sim.data$survey,sim.data.tmp$survey[,df$nyear])
      sim.data$Catch.age <- cbind(sim.data$Catch.age,sim.data.tmp$Catch.age[,df$nyear])
      
    }
  # First run the operating model with current \
    data <-list(      #### Parameters
    wlen = df$wage,
  #  Minit = df$nmort,
    Msel = df$mselec,
    Matsel= df$maturefrac,
    nage = df$nage,
    age = 1:df$nage,
    tEnd = year,
    ## Data input
    survey = sim.data$survey,
    Catchobs = sim.data$Catch.obs,
    logSDcatch = log(df$SDcatch)
    
  )
  
  U <- matrix(0, 2 , data$tEnd-1)
  
  # # Give the true parameters to U to test the TMB model 
  # U[,1]
  
  if (time ==1){
  parms <- list( # Just start all the simluations with the same initial conditions 
    logRinit = log(1e10),
    logh = log(0.6),
    logMinit = log(0.4),
    logSDsurv = log(0.2),
    logSDR = log(1),
    logSDF = log(0.01),
    logQ = log(1e-5),
    logFinit = log(0.5),
    logetacatch = log(1.5),
    logselcatch = log(6),
    logselsurv = log(5),
    logetasurv = log(1),
    U = U
  )}else{
    parms <- as.list(rep$par.fixed)
    parms$U <- U
  }
  
  obj <-MakeADFun(data,parms,DLL="EM_hake2", random = 'U') # Run the assessment 

  lower <- obj$par-Inf
  #lower['logFinit'] <- log(0.1)
  upper <- obj$par+Inf
  #upper['logQ'] <- log(0.01) # Sometimes Q goes over 1 

  system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper)) # If error one of the random effects is unused
  
  rep<-sdreport(obj)
  #Uncertainty 
  sdrep <- summary(rep)
  rep.values<-rownames(sdrep)
  nyear <- df$nyear

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

  
  df$fmort <- c(df$fmort[df$nyear], F40$Fnext)
  Ntmp <- N$name[N$year == year]
  
  df$nyear <- 2
  
  
  # Save some EM stuff in the last year 
  SSB.save[[time]] <- SSB
  R.save[[time]] <- R
  F40.save[time] <- F40$Fnext
  
  # And the fishing mortality
  F0.save <- c(F0.save,F40$Fnext)

}

# Plot the SSB over time and see if it changed 
plot(SSB.save[[1]]$name, xlim = c(8, df$nyear), ylim = c(2e12,3e13), type ='l')
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




