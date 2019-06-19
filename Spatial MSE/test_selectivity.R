### Test the different selectivity calculations ### 
### Run a bunch of MSE's for the future - uncertainty in Recruitment and survey 
source('load_files.R')
source('load_files_OM.R')
source('getParameters_mod.R')
###### Initialize the operating model ###### 
source('calcMeanAge.R')
source('load_data_seasons_mod.R')
library(r4ss)
library(TMB)

mod <- SS_output(paste(getwd(),'/data/', sep =''), printstats=FALSE, verbose = FALSE)

compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))

df <- load_data_seasons_mod(nseason = 4, nspace = 2,
                            movemaxinit = 0.5, movefiftyinit = 5, 
                            nsurvey = 2, mod = mod)
time <- 1
yrinit <- df$nyear

parms <- getParameters_mod(TRUE,mod = mod, df= df)
parms.true <- getParameters_OM(TRUE, df)
sim.data <- run.agebased.true.catch(df)
##  Create a data frame to send to runHakeassessment 
df.new <- create_TMB_data(sim.data, df)
df.new$lort <- c(1,2,3)

obj <-MakeADFun(df.new,parms.true,DLL="runHakeassessment", silent = FALSE) # Run the assessment 
#obj2 <-MakeADFun(df2,parms,DLL="runHakeassessment", silent = TRUE) # Run the assessment 

repsold <- obj$report()

# Compare selectivity estimations
selyear <- 2010

# OM selectivity calc 
source('getSelec.R')
psel <- df$psel[1,] # It can change between countries 

if(selyear> df$years[df$selYear-1] & selyear<2018){
  pseltmp <- psel+df$parms$PSEL[,which(df$years ==selyear)-df$selYear+1]*df$sigma_psel
}else{
  pseltmp <- psel
}

Fsel <- getSelec(df$age,pseltmp, df$Smin,df$Smax)

plot(df$age,repsold$selectivity_save[,which(df$years == selyear)], 
     ylim = c(0,max(repsold$selectivity_save[,which(df$years == selyear)])))
idx <- which(mod$ageselex$Yr == selyear & mod$ageselex$Factor == 'Asel')
lines(df$age,as.numeric(mod$ageselex[idx,8:28]), col = 'red', type ='o')
lines(df$age, Fsel, col = 'green')

