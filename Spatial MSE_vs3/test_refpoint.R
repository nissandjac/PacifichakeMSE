### Test ref point ### 
### See how the ref point would be in my model comapred to the historical period 

### Run a bunch of MSE's for the future - uncertainty in Recruitment and survey 
library(TMB)
library(ggplot2)
library(reshape2)
compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))

source('getData.R')

df <- load_data_seasons()
df$Catch <- Catch.obs$Fishery

sim.data <- run.agebased.true.catch(df)

Rdev <- rep(0, df$nyear-1)

##  Create a data frame to send to runHakeassessment 

df.new <- create_TMB_data(sim.data, df)
parms <- df$parms
parms$F0 <- rowSums(sim.data$Fsave, na.rm = TRUE)
parms$Rin <- Rdev

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
Fyear <- getUncertainty('Fyear', df, sdrep)
Catch <- getUncertainty('Catch',df, sdrep)

SSB.plot <- rbind(SSB, data.frame(name = assessment$SSB, SE = NA, min = NA, max = NA, year = assessment$year,
                             type = 'obs'),
             data.frame(name = rowSums(sim.data$SSB), SE = NA, min = NA, max = NA, year = df$years,
                        type = 'sim'))

ggplot(SSB.plot, aes(x= year, y = name, color = type))+geom_line()+
  geom_ribbon(aes(ymin = min, ymax = max),fill = alpha('gray', 0.3), linetype = 0)


N <- obj$report()$N
F.tac <-matrix(NA, df$nyear) 
TAC <- matrix(NA, df$nyear)
par.fixed <- rep$par.fixed

for(i in 1:df$nyear){

  Fin <- rowSums(sim.data$Fsave, na.rm = TRUE)
  
  Fnew <- getRefpoint(par.fixed, df,SSBy = SSB$name[i] , Fin=Fin[i], Nend = N[,i])
  
  F.tac[i] <- Fnew[[2]]
  TAC[i] <- Fnew[[1]]
  
}

print(F.tac) # Note that the calculated exploitation rate at F40 is about 50% of in the assessment document
TAC.obs <- read.csv('TAC.csv')
TAC.plot <- melt(TAC.obs,id.vars = 'Year', measure.vars =c(2,3,7))

plot(df$years,TAC, ylim = c(1e5, 1e6))
lines(TAC.obs$Year,TAC.obs$AssessTac)
lines(TAC.obs$Year,TAC.obs$TAC)
lines(TAC.obs$Year,TAC.obs$Realized)

ggplot(data.frame(year = df$years, TAC.MSE =TAC), aes(x = year, y = TAC*1e-5))+geom_line()+
  geom_line(data = TAC.plot, aes(x =Year, y = value*1e-5, color = variable))
