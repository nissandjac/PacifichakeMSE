# Run the hake assessment 
source('load_files.R')

# Read the assessment data 
assessment <- read.csv('data/asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]
load('data/hake2018_no_ageerror_fixtheta.RData')
catches.obs <- read.csv('data/catches.csv')

df <- load_data()
years <- df$years
df$logphi_survey <- log(10)
parms <- getParms_noage(hake2018b)

compile("runHakeassessment_4.cpp") # Assessment where phi is constant 
dyn.load(dynlib("runHakeassessment_4"))
obj <-MakeADFun(df,parms,DLL="runHakeassessment_4")#, )
vals <- obj$report()

# compile("hake_old.cpp")
# dyn.load(dynlib("hake_old"))
# obj <-MakeADFun(df,parms,DLL="hake_old")#, )

Ninit <- vals$Ninit
Nass <- vals$N
SSBass <- vals$SSB
Rass <- vals$R
Nzero <- vals$Nzero
Catchass <- vals$Catch

Ninit.ss3 <- hake2018b$recruit

age_catch <- vals$age_catch

natage <- hake2018b$natage
ageidx <- 13:33

plot(df$age,natage[1,ageidx])
lines(df$age,Nzero)
#lines(df$age,natage[3,ageidx], col = 'red')

print(hake2018b$SBzero/vals$SSBzero)
# Compare the comps with ss3
SSB.ss3 <- hake2018b$timeseries[,c("Yr","SpawnBio","Era")]
# divide by 2
SSB.ss3$SpawnBio <- SSB.ss3$SpawnBio

SSB.ss3$SpawnBio

plot(SSB.ss3$Yr,SSB.ss3$SpawnBio)
lines(df$years,SSBass)

plot(df$Catchobs)
lines(Catchass)

deriv <- hake2018b$derived_quants
sel_idx <- 8:28

catch_selec <- hake2018b$ageselex[hake2018b$ageselex$Factor =='Asel2'& hake2018b$ageselex$Fleet ==1,]
dev.off()
source('getSelec.R')
par(mfrow = c(6,6), mar = c(0.1,0.1,0.1,0.1))
plot(df$age, catch_selec[2,sel_idx], xaxt = 'n', yaxt = 'n')
lines(df$age, vals$selectivity_save[,1])
lines(df$age,getSelec(df$age, parms$psel_fish,df$Smin,df$Smax), col = 'green', lty = 2)
text(15,0.2,labels = catch_selec$Yr[2])

for(i in 27:51){
  plot(df$age,catch_selec[i,sel_idx], xaxt = 'n', yaxt = 'n')  
  lines(df$age, vals$selectivity_save[,i-1])
  text(15,0.2,labels = catch_selec$Yr[i])
  text(15,0.4,labels = df$years[i-1])
  
  # Do the OM calc 
  psel <- parms$psel_fish+parms$PSEL[,i-df$selYear]
  
  Fsel <- getSelec(df$age,psel,df$Smin,df$Smax)
  lines(df$age,Fsel, col = 'green', lty = 2)
}


survey_selec <- hake2018b$ageselex[hake2018b$ageselex$Factor =='Asel2' & hake2018b$ageselex$Fleet ==2,]

plot(df$age,survey_selec[1,sel_idx])
for(i in 2:nrow(survey_selec)){
  lines(df$age,survey_selec[i,sel_idx])
}
lines(df$age,vals$surveyselc, col = 'red')

dev.off()
plot(df$Catchobs)
lines(vals$Catch)

plot(df$years,SSBass)
lines(SSB.ss3$Yr[SSB.ss3$Yr>1965],SSB.ss3$SpawnBio[SSB.ss3$Yr>1965], col ='red')

lines(df$years,F0$Value[3:54]*0.5)

plot(df$years,df$Catchobs)
lines(df$years,obj$report()$Catch)

lower <- obj$par-Inf

lower[names(lower) == 'F0'] <- 0.001
upper <- obj$par+Inf
#upper[names(upper) == 'psel_fish' ] <- 5
upper[names(upper) == 'PSEL'] <- 9
upper[names(upper) == 'logh'] <- log(0.999)
upper[names(upper) == 'F0'] <- 2


system.time(opt<-nlminb(obj$par,obj$fn,obj$gr,lower=lower,upper=upper, 
                        control = list(iter.max = 2000,
                                       eval.max = 2000))) #

system.time(rep<-sdreport(obj))
rep

sdrep <- summary(rep)
rep.values<-rownames(sdrep)


source('getUncertainty.R')
df$nyear <- length(years)
df$year <- years

SSB.est <- getUncertainty('SSB',df)
F0 <- getUncertainty('Fyear',df)
Catch <- getUncertainty('Catch',df)
Surveyobs <- getUncertainty('Surveyobs',df)
R <- getUncertainty('R',df)
surveyselec.est <- getUncertainty('surveyselc', df)
catchselec.est <- getUncertainty('catchselec', df)


plot(SSB.est$name)
lines(assessment$SSB)
lines(SSB.ss3$SpawnBio[SSB.ss3$Yr>1965], col = 'red')







