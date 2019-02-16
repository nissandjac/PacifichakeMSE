### Test ref point ### 
### See how the ref point would be in my model comapred to the historical period 

### Run a bunch of MSE's for the future - uncertainty in Recruitment and survey 

setwd("~/GitHub/PacifichakeMSE/Spatial MSE_vs2")

###### Initialize the operating model ###### 
# Run the simulation model
source('run_agebased_model_true_Catch.R')

####  Plotting and uncertainty calculation functions #### 
source('getSelec.R') # Calculate hake selectivity

source('load_data_seasons.R') # Loads data for Operating model
source('getRefpoint.R') # Calculate refrence points 
source('getParameters.R')
source('calcSSB0.R')
source('create_TMB_data.R')
source('getUncertainty.R')

#parms.true <- getParameters(TRUE)
Catch.obs <- read.csv('hake_totcatch.csv')
TAC.obs <- read.csv('TAC.csv')

assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]


df <- load_data_seasons()
df$Catch <- Catch.obs$Fishery

sim.data <- run.agebased.true.catch(df)

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

obj <-MakeADFun(df.new,parms,DLL="runHakeassessment", silent = TRUE) # Run the assessment 

reps <- obj$report()

lower <- obj$par-Inf
upper <- obj$par+Inf

lower[names(lower) == 'F0'] <- 0.01
upper <- obj$par+Inf
upper[names(upper) == 'psel_fish' ] <- 5
upper[names(upper) == 'PSEL'] <- 5
upper[names(upper) == 'logh'] <- log(0.999)
upper[names(upper) == 'F0'] <- 2


system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper,
                        control = list(iter.max = 1e5, 
                                       eval.max = 1e5))) # If error one of the random effects is unused

rep <- sdreport(obj)
sdrep <- summary(rep)
rep.values<-rownames(sdrep)


SSB <- getUncertainty('SSB',df, sdrep)
SSB$type <- 'assess'

SSB <- rbind(SSB, data.frame(name = assessment$SSB, SE = NA, min = NA, max = NA, year = assessment$year,
                             type = 'obs'),
             data.frame(name = rowSums(sim.data$SSB), SE = NA, min = NA, max = NA, year = df$years,
                        type = 'sim'))

ggplot(SSB, aes(x= year, y = name, color = type))+geom_line()



F.tac <-matrix(NA, df$nyear) 
TAC <- matrix(NA, df$nyear)

for(i in 1:df$nyear){

  Fin <- sum(sim.data$Fsave[i,2:4]
  
  Fnew <- getRefpoint(par.fixed, df,rowSums(sim.data$SSB)[i] , Fin=Fyear[length(Fyear)], Nend)
  
  F.tac[i] <- Fnew[[1]]
  TAC[i] <- Fnew[[2]]
  
}


