# Run the hake assessment 
source('load_files.R')
source('getParameters_ss.R')
source('load_data_ss.R')
library(r4ss)
library(dplyr)
library(reshape2)
# Read the assessment data 
mod <- SS_output(paste(getwd(),'/data/SS32018/', sep =''), printstats=FALSE, verbose = FALSE)

df <- load_data_ss(mod)
df$smul <- 0.5

years <- df$years

#U[2,] <- 0.01
parms.ss <- getParameters_ss(TRUE, mod)


compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))
obj <-MakeADFun(df,parms.ss,DLL="runHakeassessment")#, )
vars <- obj$report() # 

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

SSB <- getUncertainty('SSB',df)
F0 <- getUncertainty('Fyear',df)
Catch <- getUncertainty('Catch',df)
Surveyobs <- getUncertainty('Surveyobs',df)
R <- getUncertainty('R',df)
surveyselec.est <- getUncertainty('surveyselc', df)
catchselec.est <- getUncertainty('catchselec', df)
SSB0 <- getUncertainty('SSBzero', df)
age_survey <- getUncertainty('age_survey_est', df)

# Compare the different things that go into the likelihood functions 

# Survey biomass 
s.obs <- df$survey
s.obs[s.obs == 1] <- NA
ss.exp <- rep(NA, df$nyear)
ss.exp[df$year %in%mod$cpue$Yr] <- mod$cpue$Exp

df.ss <- data.frame(year = rep(df$year,4), 
                    survey = c(Surveyobs$name,
                               s.obs,
                               vars$Surveyobs,
                               ss.exp
                               ),
                    model = rep(c('TMB est','Obs','TMB SS3 parms','SS3'), each = df$tEnd)
                    )

ggplot(df.ss[df.ss$model != 'Obs' & df.ss$model != 'SS3',], aes(x = year, y = survey*1e-6, color = model))+geom_line()+
  geom_point(data = df.ss[df.ss$model %in% c('SS3','Obs'),])+theme_classic()

# Total catch

ss.exp <- rep(NA, df$nyear)
ss.exp[df$year %in%mod$catch$Yr] <- mod$catch$Exp[mod$catch$Yr %in% df$year]

df.ss <- data.frame(year = rep(df$year,4), 
                    survey = c(Catch$name,
                               df$Catchobs,
                               vars$Catch,
                               ss.exp
                    ),
                    model = rep(c('TMB est','Obs','TMB SS3 parms','SS3'), each = df$tEnd)
)

ggplot(df.ss[df.ss$model != 'Obs' & df.ss$model != 'SS3',], aes(x = year, y = survey*1e-6, color = model))+geom_line()+
  geom_point(data = df.ss[df.ss$model %in% c('SS3','Obs'),])+theme_classic()


# Total SSB


ss.exp <- rep(NA, df$nyear)
SSB.ss3 <- mod$derived_quants$Value[grep('SSB_1966', mod$derived_quants$Label):grep('SSB_2018', mod$derived_quants$Label)]


df.ss <- data.frame(year = rep(df$year,3), 
                    survey = c(SSB$name,
                               vars$SSB,
                               SSB.ss3
                    ),
                    model = rep(c('TMB est','TMB SS3 parms','SS3'), each = df$tEnd)
)

ggplot(df.ss[df.ss$model != 'Obs' & df.ss$model != 'SS3',], aes(x = year, y = survey*1e-6, color = model))+geom_line()+
  geom_point(data = df.ss[df.ss$model %in% c('SS3','Obs'),])+theme_classic()

# Age comps in survey 
library(reshape2)

age.ss <- melt(mod$agedbase[mod$agedbase$Fleet == 2,],id.vars = c('Yr','Bin'), 
               measure.vars = c('Exp','Obs'),
               variable.name = 'model',
               value.name = 'N')

TMB <- data.frame(
  Yr = rep(df$year[df$flag_survey == 1], each = length(df$age[2:(df$age_maxage+1)])),
  Bin = rep(df$age[2:(df$age_maxage+1)], length(df$year[df$flag_survey ==1])),
            model = 'TMB',
            N = NA
  )

age.tmb <- vars$age_survey_est[,df$flag_survey ==1]
syears <- df$year[df$flag_survey ==1]

for(i in 1:length(syears)){
  
  TMB[TMB$Yr == syears[i],]$N <- age.tmb[,i]
  
}


ggplot(TMB, aes(x = Bin, y = N))+geom_line()+geom_line(data = age.ss[age.ss$model =='Exp',], color = 'blue')+
  geom_point(data = age.ss[age.ss$model =='Obs',], color = alpha(alpha = 0.2,'red'))+
  facet_wrap(~Yr)+
  theme_classic()




