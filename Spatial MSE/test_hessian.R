#### Compare year one and year two - why doesn't one of them converge? 
library(TMB)
compile("runHakeassessment2.cpp")

seedz <- 125
set.seed(seedz)
dyn.load(dynlib("runHakeassessment2"))

load('dfnew.Rdata')
load('dfold.Rdata')

plot(df.new$years,df.new$F0)
lines(df.save$years,df.save$F0)


plot(df.new$years,df.new$Catchobs)
lines(df.save$years,df.save$Catchobs)

plot(df.new$years,df.new$age_survey[3,], lwd = 2)
points(df.save$years,df.save$age_survey[3,], col = 'red')

plot(df.new$years,df.new$age_catch[6,], lwd = 2)
points(df.save$years,df.save$age_catch[6,], col = 'red')

plot(df.new$years,df.new$ss_catch, lwd = 2)
points(df.save$years,df.save$ss_catch, col = 'red')



parms <- list( # Just start all the simluations with the same initial conditions 
  logRinit = 15,
  #logh = log(0.9),
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