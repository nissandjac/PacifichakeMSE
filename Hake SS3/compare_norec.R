# Run the hake assessment 
source('load_files.R')
source('getParameters_ss.R')
source('load_data_ss.R')
library(r4ss)
library(dplyr)
library(reshape2)
library(scales)
# Read the assessment data 
mod <- SS_output(paste(getwd(),'/data/SS32018/', sep =''), printstats=FALSE, verbose = FALSE)
mod <- SS_output(paste(getwd(),'/data/SS32018_norec/', sep =''), printstats=TRUE, verbose = FALSE)

df <- load_data_ss(mod, sum_zero = 0)

years <- df$years

#U[2,] <- 0.01
parms.ss <- getParameters_ss(TRUE, mod)
parms.ss.true <- getParameters_ss(TRUE,mod)

compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))
obj <-MakeADFun(df,parms.ss,DLL="runHakeassessment")#, )
obj.true <- MakeADFun(df,parms.ss.true, DLL='runHakeassessment')
vars <- obj.true$report() # 

lower <- obj$par-Inf
lower[names(lower) == 'F0'] <- 0.001
upper <- obj$par+Inf
upper[names(upper) == 'PSEL'] <- 9
upper[names(upper) == 'logh'] <- log(0.999)
upper[names(upper) == 'F0'] <- 2
upper[names(upper) == 'psel_fish'] <- 3
lower[names(lower) == 'psel_fish'] <- 0.0001
# lower[names(lower) == 'logMinit'] <- parms.ss$logMinit#log(0.15)
# upper[names(upper) == 'logMinit'] <- parms.ss$logMinit#log(0.3)
# 
# lower[names(lower) == 'initN'] <- parms.ss$initN-0.01
# upper[names(upper) == 'initN'] <- parms.ss$initN+0.01
# lower[names(lower) == 'logRinit'] <- parms.ss$logRinit
# upper[names(upper) == 'logRinit'] <- parms.ss$logRinit


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

SSB <- getUncertainty('SSB',df, sdrep)
F0 <- getUncertainty('Fyear',df, sdrep)
Catch <- getUncertainty('Catch',df, sdrep)
Surveyobs <- getUncertainty('Surveyobs',df, sdrep)
R <- getUncertainty('R',df, sdrep)
surveyselec.est <- getUncertainty('surveyselc', df, sdrep)
catchselec.est <- getUncertainty('catchselec', df, sdrep)
SSB0 <- getUncertainty('SSBzero', df, sdrep)
age_survey <- getUncertainty('age_survey_est', df, sdrep)
age_catch <- getUncertainty('age_catch_est', df, sdrep)

initN <- getUncertainty('initN', df, sdrep)


# Compare the estimated parameters 
df.p <- as.data.frame(rep$par.fixed)
df.p$name <- names(rep$par.fixed)
df.p$idx <- 1:nrow(df.p)
df.p$model <- 'TMB'
names(df.p)[1] <- 'parameter'
df.p2 <- as.data.frame(unlist(parms.ss.true))
df.p2$name <- names(rep$par.fixed)

df.p2$idx <- 1:nrow(df.p2)
names(df.p2)[1] <- 'parameter'
df.p2$model <- 'SS3'
df.plot <- rbind(df.p,df.p2)

# Fix the log values 
idx <- grep('log', df.plot$name)
df.plot$parameter[idx] <- exp(df.plot$parameter[idx])

ggplot(df.plot, aes(x=  idx, y = parameter, color = model))+geom_point()+
  facet_wrap(~name,scales = 'free')+theme_classic()

# What's the sum of the recruitment deviations 
sum(df.plot$parameter[df.plot$name == 'Rin'])


# Compare the different things that go into the likelihood functions 




# Survey biomass 
s.obs <- df$survey
s.obs[s.obs == 1] <- NA
ss.exp <- rep(NA, df$nyear)
ss.exp[df$year %in%mod$cpue$Yr] <- mod$cpue$Exp

df.ss <- data.frame(year = rep(df$year,4), 
                    survey = c(Surveyobs$value,
                               s.obs,
                               vars$Surveyobs,
                               ss.exp
                    ),
                    model = rep(c('TMB est','Obs','TMB SS3 parms','SS3'), each = df$tEnd)
)

cols <- PNWColors::pnw_palette('Starfish', n = 5)

ggplot(df.ss[df.ss$model != 'Obs' & df.ss$model != 'SS3',], aes(x = year, y = survey*1e-6, color = model))+
  geom_line()+
  geom_point(data = df.ss[df.ss$model %in% c('SS3','Obs'),], show.legend = FALSE)+theme_classic()+
  scale_color_manual(values = cols)+
  scale_y_continuous('survey biomass \n(million tonnes)')+
  theme(legend.position = c(0.8,0.8),
        legend.title = element_blank())+
  geom_ribbon(data = Surveyobs, aes(ymin = min*1e-6, ymax = max*1e-6, x = year,
                                    y= value*1e-6, color = NA),fill = alpha('gray', alpha = 0.3), color = NA)

# Total catch

ss.exp <- rep(NA, df$nyear)
ss.exp[df$year %in%mod$catch$Yr] <- mod$catch$Exp[mod$catch$Yr %in% df$year]

df.ss <- data.frame(year = rep(df$year,4), 
                    survey = c(Catch$value,
                               df$Catchobs,
                               vars$Catch,
                               ss.exp
                    ),
                    model = rep(c('TMB est','Obs','TMB SS3 parms','SS3'), each = df$tEnd)
)



p.ssb <- ggplot(df.ss[df.ss$model != 'Obs' & df.ss$model != 'SS3',], aes(x = year, y = survey*1e-6, color = model))+
  geom_line()+
  scale_color_manual(values = cols)+
  geom_point(data = df.ss[df.ss$model %in% c('SS3','Obs'),])+theme_classic()

p.ssb
# Total SSB


ss.exp <- rep(NA, df$nyear)
SSB.ss3 <- mod$derived_quants$Value[grep('SSB_1966', mod$derived_quants$Label):grep('SSB_2018', mod$derived_quants$Label)]


df.ss <- data.frame(year = rep(df$year,3), 
                    SSB = c(SSB$value,
                            vars$SSB,
                            SSB.ss3
                    ),
                    model = rep(c('TMB est','TMB SS3 parms','SS3'), each = df$tEnd)
)

cols <- PNWColors::pnw_palette('Starfish', n = length(unique(df.ss$model)))

p.ssb <- ggplot(df.ss[df.ss$model != 'Obs' & df.ss$model != 'SS3',], aes(x = year, y = SSB*1e-6, color = model))+
  geom_line()+
  geom_point(data = df.ss[df.ss$model %in% c('SS3','Obs'),])+
  theme_classic()+scale_y_continuous('Spawning biomass\n (million tonnes)')+
  scale_color_manual(values = cols)+
  theme(legend.position = c(0.8,0.8),
        legend.title = element_blank())+
  geom_ribbon(data = SSB, aes(ymin = min*1e-6, ymax = max*1e-6, x = year,
                              y= value*1e-6, color = NA, group = NA),fill = alpha('gray', alpha = 0.3), color = NA)

p.ssb


png('Figures/SSB.png', width = 16, height =16, res = 400, unit = 'cm')
p.ssb
dev.off()
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

age_survey <- age_survey %>% rename(Yr = year)

p.surv <- ggplot(TMB, aes(x = Bin, y = N))+geom_line()+
  geom_point(data = age.ss[age.ss$model =='Exp',], color = 'blue')+
  geom_point(data = age.ss[age.ss$model =='Obs',], color = alpha(alpha = 0.2,'red'))+
  facet_wrap(~Yr)+
  geom_line(data = age_survey[age_survey$Yr %in% df$years[df$flag_survey == 1],], 
            aes(x = age, y = value), col = 'green')+
  theme_classic()
p.surv

# Age comps in catch
age.ss <- melt(mod$agedbase[mod$agedbase$Fleet == 1,],id.vars = c('Yr','Bin'), 
               measure.vars = c('Exp','Obs'),
               variable.name = 'model',
               value.name = 'N')

TMB <- data.frame(
  Yr = rep(df$year[df$flag_catch == 1], each = length(df$age[2:(df$age_maxage+1)])),
  Bin = rep(df$age[2:(df$age_maxage+1)], length(df$year[df$flag_catch ==1])),
  model = 'TMB',
  N = NA
)

age.tmb <- vars$age_catch_est[,df$flag_catch ==1]
syears <- df$year[df$flag_catch ==1]

for(i in 1:length(syears)){
  
  TMB[TMB$Yr == syears[i],]$N <- age.tmb[,i]
  
}
age_catch <- age_catch %>% rename(Yr = year)


p.catch <- ggplot(TMB, aes(x = Bin, y = N))+geom_line()+
  geom_point(data = age.ss[age.ss$model =='Exp',], color = 'blue')+
  geom_point(data = age.ss[age.ss$model =='Obs',], color = alpha(alpha = 0.2,'red'))+
  geom_line(data = age_catch[age_catch$Yr %in% df$years[df$flag_catch == 1],], 
            aes(x = age, y = value), col = 'green')+
  facet_wrap(~Yr)+  
  theme_classic()
p.catch

# # Check the numbers at age in 1998 
N.tmb <- as.data.frame(t(vars$N_beg))
names(N.tmb) <- df$age
N.tmb$Yr <- c(df$years,2019)
N.tmb <- melt(N.tmb, id.vars = 'Yr', measure.vars = 0:21,
              variable.name = 'age',
              value.name = 'N')
N.tmb$model <- 'TMB'

N.ss <- mod$natage[,c(8,11,13:33)]
n.ss <- melt(N.ss[N.ss$`Beg/Mid` == 'B',],id.vars = c('Yr'),
             measure.vars = paste(0:20),
             variable.name = 'age',
             value.name = 'N')
n.ss$model <- 'SS3'

n.plot <- rbind(N.tmb,n.ss)
# 
# 
# p.n <- ggplot(n.plot[n.plot$model == 'SS3',], aes(x = as.numeric(age), y = N/sum(N)))+
# theme_classic()+geom_point()+facet_wrap(~Yr)+
#   geom_line(data = n.plot[n.plot$model == 'TMB',])
# p.n  
# 
# # Redo 2001 and see what is happening 


df.c <- data.frame(C.w = NA, C.N = NA, 
                   year = rep(df$years, each = df$nage), 
                   age = rep(df$age, df$tEnd))
Ctot <- rep(NA, df$tEnd)
# And the Catch is 
for(i in 1:df$tEnd){
  
  df.tmp <- n.plot[n.plot$Yr == df$years[i] & n.plot$model == 'TMB',]
  
  Ftmp <- vars$Fyear[i]*vars$selectivity_save[,i]
  Z <- vars$Zsave[,i]
  
  df.c[df.c$year == df$years[i],]$C.N <- (Ftmp/(Z))*(1-exp(-(Z)))*df.tmp$N
  df.c[df.c$year == df$years[i],]$C.w <- (Ftmp/(Z))*(1-exp(-(Z)))*df.tmp$N*df$wage_catch[,i]
  
  Ctot[i] <- sum((Ftmp/(Z))*(1-exp(-(Z)))*df.tmp$N*df$wage_catch[,i])
  
}
df.c$model <- 'TMB'

# Get the age comps from SS3 

df.ss3 <- melt(mod$catage, id.vars = 'Yr', measure.vars = paste(0:20),variable.name = 'age',
               value.name = 'C.N')
names(df.ss3)[1] <- 'year'
df.ss3$C.w <- NA
df.ss3$model <- 'SS3'
df.plot <- rbind(df.c,df.ss3)

ggplot(df.plot[df.plot$model == 'TMB',], aes(x = as.numeric(age), y = C.N))+geom_line()+
  geom_point(data = df.plot[df.plot$model == 'SS3',])+
  facet_wrap(~year, scales = 'free_y')
# Compare likelihoods 

mod$likelihoods_by_fleet
mod$likelihoods_used

