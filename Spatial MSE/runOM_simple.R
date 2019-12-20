library(TMB)
library(dplyr)
library(reshape2)
library(ggplot2)
library(r4ss)
source('load_files_OM.R')
seedz <- 125
set.seed(seedz)
assessment <- read.csv('data/assessment_MLE.csv')
assessment <- assessment[assessment$year > 1965,]
# Get the stock assessment output from SS3 
mod <- SS_output(paste(getwd(),'/data/SS32018', sep =''), printstats=FALSE, verbose = FALSE)


plot.figures = FALSE # Set true for printing to file 



df <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.5, movemaxinit = 0.5, movefiftyinit =8) # Prepare data for operating model
#df$move.init <- rep(1, df$nspace)


simyears <- 25 # Project 30 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df)



# 
SSB.obs <- mod$derived_quants$Value[grep('SSB_1966',mod$derived_quants$Label):grep('SSB_2018',mod$derived_quants$Label)]
SSB.OM <- rowSums(sim.data$SSB.weight)

# Plot the biomass in ggplot 
df.plot <- data.frame(years = c(df$years,df$years), 
                      SSB = c(rowSums(sim.data$SSB.weight),SSB.obs), source = c(rep('SSB OM', df$nyear),
                                                                                rep('SSB assessment', df$nyear)))

p1 <- ggplot(data = df.plot, aes(x = years, y = SSB, color = source))+geom_line(size = 2)+theme_classic()+
  geom_point(data = df.plot[df.plot$source == 'SSB assessment',], size = 2)
p1

mean(df.plot$SSB[df.plot$source == 'SSB assessment']/df.plot$SSB[df.plot$source == 'SSB OM'])

survey.ss <- data.frame(years = mod$cpue$Yr,
                        survey =mod$cpue$Exp,
                        source = 'SS',
                        survsd = NA,
                        kriegsd = NA)

if(df$nspace>1){
  survey.true <- colSums(sim.data$survey.true
                         )
}

df.plot <- data.frame(years = c(df$years[df$survey > 1], df$years),
                      survey = c(df$survey[df$survey > 1],
                                 survey.true),
                      source = c(rep('Survey data',length(df$years[df$survey > 1])),
                                 rep('OM output', df$nyear)),
                      survsd= c(df$survey_err[df$flag_survey ==1], rep(NA,df$nyear)),
                      kriegsd = c(rep(exp(df$parms$logSDsurv),length(df$years[df$survey > 1])), 
                                  rep(NA,df$nyear))
)

df.plot <- rbind(df.plot,survey.ss)

df.plot$survsd <- sqrt(df.plot$survey^2*exp(df.plot$survsd+df.plot$kriegsd-1))

p2 <- ggplot(data = df.plot, aes(x = years, y = survey/1e6, color = source))+
  geom_point(data = df.plot[df.plot$source == 'Survey data',],size = 3)+
  geom_line(data = df.plot[df.plot$source == 'OM output',], size =2)+
  geom_line(data = df.plot[df.plot$source == 'SS',], size = 2)+
  theme_classic()+
  geom_errorbar(aes(ymin=(survey-survsd)/1e6, ymax=(survey+survsd)/1e6))+
  scale_y_continuous(limit = c(0,5), name = 'survey biomass (million t)')+
  scale_x_continuous(name = 'year', limit = c(1994,2019))

p2

# Compare the fisheries selectivity 
sel.obs <- melt(mod$ageselex[mod$ageselex$Factor == 'Asel' & mod$ageselex$Fleet == 1,],id.vars = 'Yr', measure.vars = paste(0:20),value.name = 'selectivity', 
                 variable.name = 'age')

ggplot(sel.obs,aes(x = as.numeric(age), y=  as.numeric(selectivity)))+
  geom_line()+facet_wrap(~Yr)+theme_classic()


#Check selec params
surv.idx <- grep('Acoustic_Survey',mod$parameters$Label)
surv.idx <- surv.idx[-grep('Q_ex', mod$parameters$Label)]
psel_surv <- mod$parameters$Value[surv.idx]

sel <- mod$parametersh

# Check the numbers at age and survey selectivity 


yrs <- 1995:2018#df$years[df$flag_survey == 1]
sel <- surv.obs$selectivity[surv.obs$Yr == 1963]
surv <- rep(0,length(yrs))
surv.om <- rep(0, length(yrs))

surv.est <- mod$cpue[mod$cpue$Yr %in% yrs,]
cpue <- mod$cpue
Z.ss <- mod$Z_at_age
Z.OM <- sim.data$Z[,,1,1]


sel.om <- getSelec(df$age,df$parms$psel_surv, df$Smin_survey, df$Smax_survey)

for(i in 1:length(yrs)){
  natage <- as.numeric(mod$natage[mod$natage$`Beg/Mid` == 'B' & mod$natage$Yr == yrs[i],][13:33]) # Do the begnning of the year
  wage <- df$wage_survey[,df$years == yrs[i]]
  Z <- as.numeric(Z.ss[Z.ss$Yr == yrs[i],][4:24])
  Z[is.na(Z)]  <- as.numeric(Z[length(Z[is.na(Z) == 0])])
  surv[i] <- sum(natage*sel*surv.est$Calc_Q[1]*exp(-0.5*Z)*wage)
  
  
  ntmp <- sim.data$N.save[,df$years == yrs[i]]
  surv.om[i] <- sum(ntmp*sel.om*exp(df$logQ)*exp(-0.5*Z.OM[,df$years ==yrs[i]])*wage)
  

   
}

plot(yrs,surv*1e-6, ylim = c(0.6,3))
lines(cpue$Yr, cpue$Exp*1e-6)
lines(yrs,surv.om*1e-6, col = 'red')
lines(df$years,sim.data$surv.tot*1e-6, col = 'green')

# Compare recruitment 

r.ss3 <- mod$recruit$pred_recr[mod$recruit$Yr > 1965 & mod$recruit$Yr < 2019]

df.r <- data.frame(r = c(r.ss3,rowSums(sim.data$R.save)),
                   source = c(rep('SS3', length(r.ss3)),rep('OM', df$nyear)),
                   year = rep(df$years,2)
                              )

ggplot(df.r, aes(x= year, y = r, color = source))+geom_line()