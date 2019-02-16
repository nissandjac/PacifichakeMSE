## Run a simple MSE based on subfunctions ###### Run the HAKE MSE ####### 
direc <- "~/GitHub/PacifichakeMSE/Spatial MSE/run MSE Biomass/"
setwd(direc)
###### Initialize the operating model ###### 
library(TMB)
compile("runHakeassessment2.cpp")

seedz <- 125
set.seed(seedz)
dyn.load(dynlib("runHakeassessment2"))
# Run the simulation model
source('run_agebased_model_true_Catch.R')
source('ylimits.R')
source('plotUncertainty.R')
source('getUncertainty.R')
source('getSelec.R')
source('load_data_seasons.R')
source('create_TMB_data.R')
source('getRefpoint_biomass.R')
source('Check_Identifiable_vs2.R')
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]
Catch.obs <- read.csv('hake_totcatch.csv')

df <- load_data_seasons()
df$Catch <- Catch.obs$Fishery
time <- 1
yrinit <- df$nyear
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 
# df$parms$initN <- df$parms$initN*0
# df$parms$Rin <- df$parms$Rin*0
# df$F0 <- 0*df$F0

simyears <- 25 # Project 30 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df)
simdata0 <- sim.data # The other one is gonna get overwritten. 

plot(df$years,rowSums(sim.data$SSB), type = 'l', ylab = 'SSB/SSB_0')
lines(df$years[sim.data$survey > 1],sim.data$survey[sim.data$survey > 1])

plot(df$years,df$Catch, type = 'l')
points(df$years,sim.data$Catch)

# Plot the fishing mortality
plot(df$years,(rowSums(sim.data$Fsave[,,2])+rowSums(sim.data$Fsave[,,1]))/2)
lines(df$years,assessment$F0)
# 
# save(sim.data,file = 'simulated_space_OM.Rdata')
# save(df,file = 'sim_data_parms.Rdata')

F40.save<- array(NA,simyears)

# Save some stuff
SSB.save <- list()
R.save <- list()
Catch.save <- list()
S.year.future <- seq(2019,2019+simyears, by = 2)
# Save som OM stuff 
SSB.save.om <- array(NA, df$tEnd+simyears)
R.save.om <- array(NA, df$tEnd+simyears)
Catch.save.om <- array(NA, df$tEnd+simyears)
# Before the MSE starts 
SSB.save.om[1:df$tEnd] <- sim.data$SSB
R.save.om[1:df$tEnd] <- sim.data$N.save[1,]
Catch.save.om[1:df$tEnd] <- sim.data$Catch
F0.save <- df$fmort
years <- df$years
df$F0 <- assessment$F0
model.save <- list()

## 
#SSB.test.om <- list() # Test if SSB is the same in the OM


for (time in 1:simyears){
  
  year <- yrinit+(time-1)
  
  
  
  if (time > 1){
    
    if(sum(year.future[year] == S.year.future)>0){
      df$flag_survey <- c(df$flag_survey,1)
      df$survey_x <- c(df$survey_x,2)
      # df$ss_catch <- c(df$ss_catch,ceiling(mean(df$ss_catch[df$ss_catch > 0])))
      df$ss_survey <- c(df$ss_survey,ceiling(mean(df$ss_survey[df$ss_survey > 0])))
      df$survey_err <- c(df$survey_err,mean(df$survey_err[df$survey_err < 1]))
      
    }else{
      df$flag_survey <- c(df$flag_survey,-1)
      df$survey_x <- c(df$survey_x,-2)
      df$ss_survey <- c(df$ss_survey,-1)
      df$survey_err <- c(df$survey_err,1)
    }
    
    df$ss_catch <- c(df$ss_catch,ceiling(mean(df$ss_catch[df$ss_catch > 0])))
    df$flag_catch <- c(df$flag_catch,1)
    df$years <- year.future[1:year]
    df$nyear <- length(df$years)
    
    
    sim.data <- run.agebased.true.catch(df, seedz)
    
    # # Add to the original data frame 
    # 
    # # 1 measurement per year
    # sim.data$SSB <- c(sim.data$SSB, sim.data.tmp$SSB[df.tmp$tEnd])
    # sim.data$Catch<- c(sim.data$Catch, sim.data.tmp$Catch[df.tmp$tEnd])
    # sim.data$Catch.obs <- c(sim.data$Catch.obs, sim.data.tmp$Catch.obs[df.tmp$tEnd])
    # 
    # # Measurement per age 
    # sim.data$N.save <- cbind(sim.data$N.save,sim.data.tmp$N.save)
    # sim.data$survey <- cbind(sim.data$survey,sim.data.tmp$survey[df.tmp$tEnd])
    # sim.data$Catch.age <- cbind(sim.data$Catch.age,sim.data.tmp$Catch.age)
    # 
    
  }
  
  PSEL <- matrix(0,5, length(1991:years[length(years)]))
  initN <- rep(0,df$nage-1)
  #F0 <- rep(0.01, df$tEnd)
  Rdev <- rep(0, df$nyear)
  
  parms <- list( # Just start all the simluations with the same initial conditions 
    logRinit = 15,
    logh = log(0.5),
    logMinit = log(0.3),
    logSDsurv = log(0.3),
    #logSDR = log(1.4),
    logphi_catch = log(0.8276),
    logphi_survey = log(11.33),
    # logSDF = log(0.1),
    # Selectivity parameters 
    psel_fish = c(2.486490, 0.928255,0.392144,0.214365,0.475473),
    psel_surv = c(0.568618,-0.216172,0.305286 ,0.373829),
    initN = initN,
    Rin = Rdev,
    # F0 = F0,
    PSEL = PSEL
  )
  
  ##  Create a data frame to send to runHakeassessment 
  
  df.new <- create_TMB_data(sim.data, df)
  
  obj <-MakeADFun(df.new,parms,DLL="runHakeassessment2", silent = TRUE) # Run the assessment 
  
  reps <- obj$report()
  
  lower <- obj$par-Inf
  upper <- obj$par+Inf
  
  upper[names(upper) == 'logh']<- log(1) # h can't be over 1 
  # lower[names(lower) == 'PSEL'] <- -0.8
  # upper[names(upper) == 'PSEL'] <- +0.8
  
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
  # N$age <- rep(seq(1,df$nage), length.out = year*df$nage)
  # N$year <- rep(1:year, each = df$nage)
  
  Biomass <- getUncertainty('Biomass',df)
  R <- getUncertainty('R',df)
  
  ## Plots   
  #tt <- Check_Identifiable_vs2(obj)
  
  
  yl <- ylimits(SSB$name,sim.data$SSB)
  # plot(df.new$years,SSB$name, type = 'l', ylim = yl, xlab = 'year')
  # lines(df.new$years,rowSums(sim.data$SSB), col = 'red')
  # polygon()
  par(mfrow = c(2,1), mar = c(4,4,1,1))
  plotUncertainty(SSB,rowSums(sim.data$SSB))
  
  df.plot <- df.new
  df.plot$survey[df.plot$survey == 1] <- NA
  plotUncertainty(Biomass, df.plot$survey)
  # # Calculate the fishing mortality needed to reach F40  
  xx<- Check_Identifiable_vs2(obj)
  
  model.save[[time]] <- list(df = df.new, xx = xx, parameters = rep$par.fixed)
  
  
  # Fsel <- getSelec(df$age,rep$par.fixed[names(rep$par.fixed) == 'psel_fish'], df$Smin, df$Smax)
  # F40 <- referencepoints(SSB$name[length(SSB$name)])
  # 
  Fnew <- getRefpoint.Biomass(rep$par.fixed, df,SSB, Fin=df$F0[length(df$F0)])
  
  
  # Update the data data frame
  df$Catch <- c(df$Catch,Fnew$TAC)
  Ntmp <- sim.data$Nout
  df$tEnd <- df$tEnd+1 # Just run one more year in subsequent runs
  df$wage_catch <- df.new$wage_catch
  df$wage_survey <- df.new$wage_survey
  df$wage_mid <- df.new$wage_mid
  df$wage_ssb <- df.new$wage_ssb
  
  
  
  # Save some EM stuff in the last year 
  SSB.save[[time]] <- SSB
  R.save[[time]] <- R
  F40.save[time] <- Fnew$F0
  Catch.save[[time]] <- Catch
  
  # And the fishing mortality
  F0.save <- Fnew
  
  #  print(year.future[year])
  #SSB.test.om[[time]] <- rowSums(sim.data$SSB)
  
}
save(model.save, file = 'modelsave.Rdata')

# 
plot(SSB$name, type = 'l', ylim = yl, xlab = 'year')
lines(rowSums(sim.data$SSB), col = 'red')


png(file = 'spawningMSE.png', width = 800, height = 400)
library(scales)
# Plot the SSB over time and see if it changed 
plot(SSB.save[[1]]$name*1e-5, xlim = c(0, df$tEnd/df$nseason), type ='l', ylim =c(3,20), ylab = 'spawning biomass')
for (i in 2:simyears){
  
  if (i == simyears){
    lines(SSB.save[[i]]$name*1e-5, col = alpha('green', alpha = 0.6))
    
  }else{
    lines(SSB.save[[i]]$name*1e-5, col = alpha('black', alpha = 0.3))
    
    
  }
}
lines(rowSums(sim.data$SSB)*1e-5, col = 'red', lwd = 2)

dev.off()
# Plot the standard error
# SSB.mean <- matrix(NA,simyears)
# 
# for (i in 1:year){
#   for (j in 1:year){
#     SSB.mean[i] <- SSB.save
#   }
# }


### Plot the estimated variance parameters vs the true ones after the last 
SSB.end <- SSB.save[[simyears]]
R.end <- R.save[[simyears]]
# F0.end <- F0.save[[simyears]]
Catch.end <- Catch.save[[simyears]]

SE.SSB <- ((SSB.end$name-rowSums(sim.data$SSB))/SSB.end$name)*100
SE.R <- ((R.end$name-sim.data$N.save[1,])/R.end$name)*100
SE.Catch <- ((Catch.end$name-sim.data$Catch)/Catch.end$name)*100

png(file = 'SE estimates.png', width = 800, height = 400)

par(mfrow = c(2,2), mar = c(4,4,1,1))
plot(df$years,SE.SSB, ylim = c(-100,100), type = 'l', xlab = 'year', ylab = 'SSB SE')
lines(df$years,rep(1,length(df$years)), lty = 2)
plot(df$years, SE.R, ylim = c(-300,300), type = 'l', xlab = 'year', ylab = 'R SE')
lines(df$years,rep(1,length(df$years)), lty = 2)
plot(df$years, SE.Catch,ylim= c(-100,100) ,type = 'l', xlab = 'year', ylab = 'Catch SE')
lines(df$years,rep(1,length(df$years)), lty = 2)

plot(F40.save, type= 'l', ylab= 'Fishing mortality')
dev.off()

source('get_performance_metrics.R')
#cairo_pdf(file = 'performancemetrics.pdf', width = 12/2.3, height = 16/2.3)
png(file = 'performancemetrics.png',width = 800, height = 400)
get_performance_metrics(sim.data,df)
dev.off()
