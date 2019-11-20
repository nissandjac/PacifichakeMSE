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


df <- load_data_seasons(nseason = 1, nspace = 1, bfuture = 0.5, movemaxinit = 0.5, movefiftyinit =8) # Prepare data for operating model

simyears <- 25 # Project 30 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df)



# 
M <- exp(df$parms$logMinit)
R0 <- exp(df$parms$logRinit)
wtatage <- mod$wtatage


N_at_age.beg <- rep(NA, df$nage)
N_at_age.beg <- R0*exp(-M*df$age)
# replace plus-group value with sum of geometric series
N_at_age.beg[df$nage] <- R0*exp(-M*max(df$age))/(1 - exp(-M))
mod$SBzero/sum(N_at_age.beg*wtatage[wtatage$Yr == 1966 & wtatage$Fleet == -2, paste(df$age)] )


SSB.obs <- mod$derived_quants$Value[grep('SSB_1966',mod$derived_quants$Label):grep('SSB_2018',mod$derived_quants$Label)]
SSB.OM <- rowSums(sim.data$SSB.weight)

# Plot the biomass in ggplot 
df.plot <- data.frame(years = c(df$years,df$years), 
                      SSB = c(rowSums(sim.data$SSB.weight),SSB.obs), source = c(rep('SSB OM', df$nyear),
                                                                                rep('SSB assessment', df$nyear)))

p1 <- ggplot(data = df.plot, aes(x = years, y = SSB, color = source))+geom_line(size = 2)+theme_classic()
p1

survey.ss <- data.frame(years = mod$cpue$Yr,
                        survey =mod$cpue$Exp,
                        source = 'SS',
                        survsd = NA,
                        kriegsd = NA)

df.plot <- data.frame(years = rep(df$years[df$survey > 1],2),
                      survey = c(df$survey[df$survey > 1],sim.data$survey[sim.data$survey > 1]),
                      source = rep(c('Survey data','OM output'), each = length(df$years[df$survey > 1])),
                      survsd= c(df$survey_err[df$flag_survey ==1], rep(NA,length(df$years[df$survey > 1]))),
                      kriegsd = c(rep(exp(df$parms$logSDsurv),length(df$years[df$survey > 1])), rep(NA,length(df$years[df$survey > 1])))
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
  scale_x_continuous(name = 'year')

p2



surv.sel <- getSelec(df$age,df$parms$psel_surv, df$Smin_survey, df$Smax_survey) # Constant over time
surv.obs <- melt(mod$ageselex[mod$ageselex$Factor == 'Asel2',],id.vars = 'Yr', measure.vars = paste(0:20),value.name = 'selectivity', 
                 variable.name = 'age')

ggplot(surv.obs[surv.obs$Yr %in% df$years[df$survey_x == 2],],aes(x = as.numeric(age), y=  as.numeric(selectivity)))+
         geom_line()+facet_wrap(~Yr)+theme_classic()


# Compare the fisheries selectivity 
sel.obs <- melt(mod$ageselex[mod$ageselex$Factor == 'Asel',],id.vars = 'Yr', measure.vars = paste(0:20),value.name = 'selectivity', 
                 variable.name = 'age')

ggplot(sel.obs,aes(x = as.numeric(age), y=  as.numeric(selectivity)))+
  geom_line()+facet_wrap(~Yr)+theme_classic()


#Check selec params
surv.idx <- grep('Acoustic_Survey',mod$parameters$Label)
surv.idx <- surv.idx[-grep('Q_ex', mod$parameters$Label)]
psel_surv <- pars$Value[surv.idx]



sel <- mod$parametersh


