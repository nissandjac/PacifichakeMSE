###### Run the HAKE MSE ####### 


###### Initialize the operating model ###### 
library(TMB)
compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))

seedz <- 12345
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

df <- load_data_seasons(move = TRUE, nseason = 4, nspace = 2)

df$Catch <- Catch.obs$Fishery
time <- 1
yrinit <- df$nyear
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 
#
# df$parms$Rin <- df$parms$Rin*0
# df$F0 <- 0*df$F0

simyears <- 30 # Project 25 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df)
simdata0 <- sim.data # The other one is gonna get overwritten. 


par(mfrow = c(3,1), mar = c(4,4,1,1))
plot(df$years,sim.data$SSB[,2]/sum(sim.data$SSB_0[2]), type = 'l', ylab = 'SSB/SSB_0')
SSB0_ass <- calcSSB0(exp(parms.true$logRinit), exp(parms.true$logMinit), df$nage, df$Matsel)
lines(assessment$year,assessment$SSB/SSB0_ass, col = 'red')

plot(df$years,rowSums(sim.data$SSB), type = 'l', ylab = 'SSB')
lines(assessment$year, assessment$SSB, col = 'red')

plot(df$years,df$Catch)
lines(df$years, sim.data$Catch)
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
model.save <- list()

## 

SSB.test.om <- list() # Test if SSB is the same in the OM
start.time <- Sys.time()
dev.off()

for (time in 1:simyears){
  
  year <- yrinit+(time-1)
  print(year.future[year])
  
  
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
    #df$tEnd <- df$tEnd+1 # Just run one more year in subsequent runs
    df$wage_catch <- df.new$wage_catch
    df$wage_survey <- df.new$wage_survey
    df$wage_mid <- df.new$wage_mid
    df$wage_ssb <- df.new$wage_ssb
    df$Catch <- c(df$Catch, Fnew[[1]])
    #df$years <- c(df$years,df$years[length(df$years)]+1)
    
    
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
  
  lower[names(lower) == 'F0'] <- 0.01
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
    xx<- Check_Identifiable_vs2(obj)
    
    }
  
  reps <- obj$report()
  
  SSB <- reps$SSB
  Fyear <- reps$Fyear
  N <- reps$N
  Catch <- reps$Catch
  R <- reps$R
  
  
  #par(mfrow = c(2,1), mar = c(4,4,1,1))
  yl <- ylimits(SSB,sim.data$SSB[,1])
  plot(year.future[1:year],SSB, ylim = yl)
  lines(year.future[1:(year)],rowSums(sim.data$SSB)) # One model takes SSB in the beginning of the year the other in the end
  #rep<-sdreport(obj)
  
  # #Uncertainty
  if(time == simyears){
    rep <- sdreport(obj)
    sdrep <- summary(rep)
    rep.values<-rownames(sdrep)
    nyear <- df$tEnd
    SSB <- getUncertainty('SSB',df)
    p1 <- plotValues(SSB, data.frame(x=df$years,y = rowSums(sim.data$SSB)), 'SSB')
    p1
    SSB <- SSB$name
  }
      # # F0 <- getUncertainty('Fyear',df)
  # # Catch <- getUncertainty('Catch',df)
  # # N <- getUncertainty('N',df)
  # # N$age <- rep(seq(1,df$nage), length.out = year*df$nage)
  # # N$year <- rep(df$years, each = df$nage)
  # # 
  # Surveyobs <- getUncertainty('Surveyobs',df)
  # Surveyobs <- Surveyobs[df.new$flag_survey == 1,]
  # p2 <- plotValues(Surveyobs, data.frame(x=df$years[df.new$flag_survey == 1],y = sim.data$survey[df.new$flag_survey == 1]), 'survey biomass')
  # 
  # grid.arrange(p1,p2)
  # 
  # df.plot <- Surveyobs
  # df.plot$model <- 'estimation'
  # df.plot$year <- year.future[1:year]
  # 
  # df.plot <- rbind(df.plot, data.frame(name = sim.data$survey, SE = NA, min = NA, max = NA, model = 'OM', year = df.plot$year))
  # df.plot$name[df.plot$name == 1] <- NA
  #   # R <- getUncertainty('R',df)
  # ggplot(df.plot, aes(x=  year,y= name, color = model))+geom_line(data = df.plot[df.plot$model == 'estimation',])+theme_bw()+
  #   geom_point(data = df.plot[df.plot$model == 'OM',])+
  #   geom_ribbon(data = df.plot[df.plot$model == 'estimation',],aes(ymin = min, ymax= max), fill = alpha('gray', alpha = 0.3), linetype = 0)
  # ## Plots

# 
#   yl <- ylimits(SSB$name,sim.data$SSB)
#   # plot(df.new$years,SSB$name, type = 'l', ylim = yl, xlab = 'year')
#   # lines(df.new$years,rowSums(sim.data$SSB), col = 'red')
#   # polygon()
#   par(mfrow = c(2,1), mar = c(4,4,1,1))
 # plotUncertainty(SSB,rowSums(sim.data$SSB))
 # plotUncertainty(Surveyobs,sim.data$survey)
 # points(sim.data$survey, col = 'green') 
 # # df.plot <- df.new
 #  # df.plot$survey[df.plot$survey == 1] <- NA
 #  # plotUncertainty(Surveyobs, df.plot$survey)
 #  plotUncertainty(Catch, df.new$Catchobs)
  # # # Calculate the fishing mortality needed to reach F40

  # model.save[[time]] <- list(df = df.new, xx = xx, parameters = rep$par.fixed)


    # Fsel <- getSelec(df$age,rep$par.fixed[names(rep$par.fixed) == 'psel_fish'], df$Smin, df$Smax)
  # F40 <- referencepoints(SSB$name[length(SSB$name)])
  # 
  # Nend <- N$name[N$year == df$years[length(df$years)]]
  Nend <- N[,dim(N)[2]]
  Fnew <- getRefpoint(opt$par, df,SSB[length(SSB)], Fin=Fyear[length(Fyear)], Nend)
  #Fnew <- 0.3
  print(paste('new quota = ',Fnew[[1]]))
  # Update the data data frame
  
  Ntmp <- sim.data$Nout

  
  # Save some EM stuff in the last year 
  SSB.save[[time]] <- SSB
  R.save[[time]] <- N[1,]
  F40.save[time] <- Fnew[[2]]
  Catch.save[[time]] <- Catch
  
  # And the fishing mortality
  F0.save <- Fnew
  
#  print(year.future[year])
  SSB.test.om[[time]] <- rowSums(sim.data$SSB)
  
}


end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

dev.off()
plot(1966:(1966+yrinit-1),SSB.save[[1]], type = 'l', col = alpha('black', alpha = 0.3), xlab = c(1965,df$years[year]))

for(i in 2:simyears){
  lines(1966:(1966+yrinit-2+i), SSB.save[[i]], col = alpha('black', alpha = 0.3))
  
}

lines(df$years,SSB.test.om[[time]], lwd = 2, col = 'red')
lines(assessment$year,assessment$SSB, lwd = 2, col = 'green')


#save(model.save, file = 'modelsave.Rdata')

# 
yl <- ylimits(SSB,rowSums(sim.data$SSB))
plot(SSB, type = 'l', ylim = yl, xlab = 'year')
lines(rowSums(sim.data$SSB), col = 'red')


#png(file = 'spawningMSE.png', width = 800, height = 400)
library(scales)
# Plot the SSB over time and see if it changed 
plot(SSB.save[[1]]*1e-5, xlim = c(0, df$tEnd/df$nseason), type ='l', ylim =c(3,30), ylab = 'spawning biomass')
for (i in 2:simyears){
  
  if (i == simyears){
    lines(SSB.save[[i]]*1e-5, col = alpha('green', alpha = 0.6))
    
  }else{
    lines(SSB.save[[i]]*1e-5, col = alpha('black', alpha = 0.3))
    
    
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

SE.SSB <- ((SSB.end-rowSums(sim.data$SSB))/SSB.end)*100
SE.R <- (((R.end)-(sim.data$N.save[1,]))/(R.end))*100
SE.Catch <- ((Catch.end-sim.data$Catch)/Catch.end)*100

#png(file = 'SE estimates.png', width = 800, height = 400)

par(mfrow = c(2,2), mar = c(4,4,1,1))
plot(df$years,SE.SSB, ylim = c(-100,100), type = 'l', xlab = 'year', ylab = 'SSB SE')
lines(df$years,rep(1,length(df$years)), lty = 2)
plot(df$years, SE.R, ylim = c(-50,50), type = 'l', xlab = 'year', ylab = 'log(R) SE')
lines(df$years,rep(1,length(df$years)), lty = 2)
plot(df$years, SE.Catch,ylim= c(-100,100) ,type = 'l', xlab = 'year', ylab = 'Catch SE')
lines(df$years,rep(1,length(df$years)), lty = 2)

plot(F40.save, type= 'l', ylab= 'Fishing mortality')
dev.off()

source('get_performance_metrics.R')
#cairo_pdf(file = 'performancemetrics.pdf', width = 12/2.3, height = 16/2.3)
#png(file = 'performancemetrics.png',width = 800, height = 400)
get_performance_metrics(sim.data,df)
dev.off()
