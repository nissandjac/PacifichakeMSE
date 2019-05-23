### Check the selectivity #### 
source('getParms_noAge.R')
source('getSelec.R')
source('load_data.R')
source('load_data_ss.R')

library(reshape2)

mod <- SS_output(paste(getwd(),'/data/', sep =''), printstats=FALSE, verbose = FALSE) # Read the true selectivity 

df <- load_data_ss(mod)

parms <- getParms_noage(mod)
sel.tmp <- mod$ageselex[mod$ageselex$Factor == 'Asel' & mod$ageselex$Yr > 1989 & mod$ageselex$Yr < 2018 & mod$ageselex$Fleet == 1,]
# Plot the selectivity 
sel.ss3 <- melt(sel.tmp, id.vars = 'Yr',measure.vars = paste(0:20), variable.name = 'age', value.name = 'sel')
sel.ss3$model <- 'ss3'

ggplot(sel.ss3, aes(x = as.numeric(age), y = sel)) + geom_line()+theme_classic()+facet_wrap(~Yr)+
  coord_cartesian(ylim = c(0, 1.2))

### Calculate the Nis selectivity ### 
yrs <- unique(sel.ss3$Yr)

sel.tmb <- data.frame(Yr = rep(yrs, each = df$nage),age = as.factor(rep(df$age, length(yrs))) ,sel = NA,
                      model = 'tmb')

for(i in 1:length(yrs)){
  
  if(i == 1){
  sel.tmp <- getSelec(df$age, parms$psel_fish, df$Smin, df$Smax) 
  }else{
  sel.tmp <- getSelec(df$age, parms$psel_fish+parms$PSEL[,i-1]*1.4, df$Smin, df$Smax)  
  }

  sel.tmb[sel.tmb$Yr == yrs[i],]$sel <- sel.tmp 

}

df.plot <- rbind(sel.ss3, sel.tmb)

ggplot(df.plot, aes(x = as.numeric(age), y = sel, color = model)) + geom_line()+theme_classic()+facet_wrap(~Yr)+
  coord_cartesian(ylim = c(0, 1.2))+geom_point(data = sel.ss3, size = 0.5)



# From the actual TMB model 
# Run the hake assessment 
source('load_files.R')
library(r4ss)
# Read the assessment data 
assessment <- read.csv('data/asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

catches.obs <- read.csv('data/catches.csv') # Read the historical catches
mod <- SS_output(paste(getwd(),'/data/', sep =''), printstats=FALSE, verbose = FALSE)

df <- load_data()
df$smul <- 1
years <- df$years


compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))
obj <-MakeADFun(df,parms,DLL="runHakeassessment")#, )

vars <- obj$report()

age_survey  <- obj$report()$age_survey_est
age_catch <- obj$report()$age_catch
# Compare the comps with ss3
SSBass <- vars$SSB

SSB.obs <- mod$derived_quants$Value[grep('SSB_1966',mod$derived_quants$Label):grep('SSB_2017',mod$derived_quants$Label)]

plot(df$years,SSB.obs)
lines(df$years,SSBass)

# Compare selectivity in year 1993
plot(vars$selectivity_save[,which(df$years == 1992)])
lines(mod$ageselex[mod$ageselex$Yr == 1992 & mod$ageselex$Fleet == 1])


plot(df$years,df$Catchobs)
lines(df$years,obj$report()$Catch)
