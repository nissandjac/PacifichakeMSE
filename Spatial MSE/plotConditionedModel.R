###### Initialize the operating model ###### 
library(TMB)
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(gridExtra)
library(dplyr)
library(cowplot)
library(r4ss)
library(RColorBrewer)
# Survey age distribution 
source('calcMeanAge.R')
# 2018 assessment
mod <- SS_output(paste(getwd(),'/data/SS32018/', sep =''), printstats=FALSE, verbose = FALSE)


plot.figures = TRUE# Set true for printing to file 
# Run the simulation model
source('load_files_OM.R')
source('load_files.R')
source('runfuture_OM.R')
source('run_agebased_model_true_catch.R')
assessment <- read.csv('data/assessment_MLE.csv')

#assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]
survey.obs <- read.csv('data/survey_country.csv')
survey.ac <- read.csv('data/ac_survey_country.csv')

nparms <- 5
movemax.parms <- seq(0.1,0.9, length.out = nparms)
movefifty.parms <- seq(1,10, length.out = nparms)


yr.future <- 2

df <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.5, movemaxinit = 0.35, movefiftyinit =6,
                        yr_future = yr.future) # Prepare data for operating model

df.2 <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.5, movemaxinit = 0.35, movefiftyinit =6,
                          yr_future = yr.future,
                          sel_hist = 0) # Prepare data for operating model


df$surveyseason <- 3
sim.data <- run.agebased.true.catch(df)
sim.data_nosel <- run.agebased.true.catch(df.2)

Catch.future <- c(df$Catch, rep(206507.8, yr.future)) # Project MSY
df$Catch <- Catch.future
r.succes <- matrix(NA, length(movemax.parms)*length(movefifty.parms))
AC.catch.tot <- AC.survey.tot <- array(NA, dim = c(df$age_maxage,df$nyear, 
                                                   df$nspace,length(movemax.parms)*length(movefifty.parms)))
survey.ml <- array(NA,dim = c(df$nyear,df$nspace,length(movemax.parms)*length(movefifty.parms)))

catch.ac.obs <- read.csv('data/age_in_catch_obs.csv')
standard.move <- runfuture_OM(df, 1)
standard.move_sel <- runfuture_OM(df.2, 1)

ac.survey.tot <- survey.ac %>% 
  group_by(year, country) %>% 
  summarise(AC.mean = sum(survey*age))

AC.survey <- data.frame(AC.mean = c(standard.move$survey.AC[,1],standard.move$survey.AC[,2]),
                        country = rep(c('CAN','USA'), each = df$nyear), year = rep(df$years,2))


p.AC.survey <- ggplot(AC.survey, aes(x = year, y= AC.mean, color = country))+geom_line(size = 1)+theme_classic()+
  geom_line(data = ac.survey.tot, linetype = 2, size = 1.2)+
  geom_point(data = ac.survey.tot)+  scale_x_continuous(limit = c(1965,2018))+
  scale_color_manual(values = c('darkred','blue4'))+
  theme(legend.position = 'none')+scale_y_continuous('mean age')
p.AC.survey


if(plot.figures == TRUE){
  png(filename = 'Figs/AC_survey.png', width = 16, height = 12, res = 400, units = 'cm')
  print(p.AC.survey)
  dev.off()
}



# plot the biomasses in the two countries 
df.plot.sim <- data.frame(survey = c(standard.move$survey.space[1,],standard.move$survey.space[2,]),
                          source = 'OM',
                          years = rep(df$years,2),
                          country = rep(c('CAN','USA'), each = length(df$years)))
df.obs <- data.frame(survey = c(survey.obs$survey[survey.obs$country == 'CAN'],survey.obs$survey[survey.obs$country == 'USA']),
                     source = 'observation',
                     years = survey.obs$year,
                     country = rep(c('CAN','USA'), each = length(survey.obs$year)/2))

df.plot <- rbind(df.plot.sim,df.obs)

p1 <- ggplot(df.plot.sim, aes( x=  years, y = survey*1e-6, color = country))+theme_classic()+geom_line(size = 1)+
  geom_line(data = df.obs, linetype = 2, size = 1.2)+geom_point(data = df.obs)+
  scale_color_manual(values = c('darkred','blue4'))+scale_y_continuous('survey biomass \n(million tonnes)')+
  scale_x_continuous(limit = c(1965,2018))+
  theme(legend.position = c(0.1,0.8),
        legend.background = element_rect(fill=NA),
        legend.title = element_blank(), axis.title.x = element_blank())


p1

if(plot.figures == TRUE){
  png(filename = 'Figs/Biomass_survey.png', width = 16, height = 12, res = 400, units = 'cm')
  print(p1)
  dev.off()
}

#windows(width = 16/cm(1), height = 10/cm(1))

# plot_grid(p1,p.AC.survey, nrow = 2, align = 'hv', 
#           label_x = c(0.95,0.95),
#           label_y = c(0.98,0.98),
#           labels = 'auto')
# dev.off()
if(plot.figures == TRUE){
  png(filename = 'Figs/Biomass_survey.png', width = 16, height = 10, res = 400, units = 'cm')
  print(plot_grid(p1,p.AC.survey, nrow = 2, align = 'hv', 
                  label_x = c(0.95,0.95),
                  label_y = c(0.98,0.98),
                  labels = 'auto'))
  dev.off()
}
# From the asssessment 
df.ass <- data.frame(survey = mod$cpue$Exp,source = 'assessment',years = mod$cpue$Yr, country ='Both')

df.tot <- rbind(df.plot,df.ass) %>% 
  group_by(years, source) %>% 
  summarise(survey = sum(survey))
cols <- brewer.pal(6, 'Dark2')

df.survey <- data.frame(years = df$years[df$flag_survey == 1],
                        source = 'Survey data',
                        survey = df$survey$x[df$flag_survey == 1],
                        survsd=  sqrt(df$survey$x[df$flag_survey == 1]^2*exp(df$survey_err[df$flag_survey == 1]+
                                                                             exp(df$parms$logSDsurv)-1))
)
df.tot$survsd <- NA


p2 <- ggplot(data = df.survey, aes(x = years, y = survey/1e6, color = source))+
  geom_point(size = 3, col = cols[3])+
  geom_line(data = df.tot[which(df.tot$source =='OM'),], size =1.5, col = cols[1])+
  geom_line(data = df.tot[which(df.tot$source =='assessment'),], size =1.2, col = cols[2], linetype = 2)+
  theme_classic()+theme(legend.position = 'none')+
  geom_errorbar(aes(ymin=(survey-survsd)/1e6, ymax=(survey+survsd)/1e6), color = cols[3])+
  scale_y_continuous(limit = c(0,5), name = 'survey biomass \n(million t)')+
  scale_x_continuous(name = 'year', limit = c(1994,2019),
                     labels = c(2000,2010),
                     breaks = c(2000,2010))

p2

if(plot.figures == TRUE){
  
  png(file = 'Figs/survey_total.png', width = 8*2, height = 7, res =400, units = 'cm')
  print(p2)
  dev.off()
  
}

catch.ac.obs <- read.csv('data/age_in_catch_obs.csv')
catch.model <- data.frame(year = rep(df$years,2),
                          Country = rep(c('US','Can'), each = df$nyear),
                          am = c(standard.move$catch.AC[,2],standard.move$catch.AC[,1]))


p.AC.catch <- ggplot(catch.ac.obs, aes(x = year, y= am, color = Country))+geom_line(size = 1, linetype = 2)+theme_classic()+
  geom_line(data = catch.model, linetype = 1, size = 1)+geom_point()+
  scale_color_manual(values = c('darkred','blue4'))+
  theme(legend.position = 'none')+scale_y_continuous('mean age')

p.AC.catch

catch.model.2 <- data.frame(year = rep(df$years,4),
                          Country = rep(c('US','Can','US no sel','Can no sel'), each = df$nyear),
                          am = c(standard.move$catch.AC[,2],standard.move$catch.AC[,1],
                                 standard.move_sel$catch.AC[,2], standard.move_sel$catch.AC[,1]))

cols <- LaCroixColoR::lacroix_palette('PinaFraise',n = 4)

p.AC.catch <- ggplot(catch.ac.obs, aes(x = year, y= am, color = Country))+geom_line(size = 1, linetype = 2)+theme_classic()+
  geom_line(data = catch.model.2, linetype = 1, size = 1)+geom_point()+
  scale_color_manual(values = rep(c('darkred','blue4'), each = 2))+
  theme()+scale_y_continuous('mean age')

p.AC.catch



if(plot.figures == TRUE){
  png(filename = 'Figs/AC_catch.png', width = 16, height = 8, res = 400, units = 'cm')
  print(p.AC.catch)
  dev.off()
  
}


surv.tot <- survey.obs %>% 
  group_by(year) %>%   
  summarise(survey = sum(survey))

#surv.model <- data.frame(year = df$years, )



# Fishing mortality per area 

F.plot <- apply(sim.data$Fout, FUN = sum, MARGIN = c(1,3))
F0.assessment <- read.csv('data/F0.csv')

df.F <- data.frame(year = rep(df$years,2),
                   F0 = c(F.plot[,1],F.plot[,2]),
                   Country = rep(c('CAN','USA'), each = df$nyear))

p2 <- ggplot(df.F)+geom_col(aes(x = year, y = F0, fill = Country), position = position_dodge(width = 0.9))+
  scale_fill_manual(values = c('darkred','blue4'))+
  geom_line(data = data.frame(year = 1966:2018, F0 = F0.assessment$x), 
            aes(x = year, y= F0), size = 1.5, color = 'black')+
  theme(legend.position = 'none')
p2


if(plot.figures == TRUE){
  png(filename = 'Figs/F_country.png', width = 16, height = 12, res = 400, units = 'cm')
  print(p2)
  dev.off()
  
}

df.movement <- data.frame(age = rep(df$age, 8), movement = NA, country = rep(c('CAN','USA'), each = df$nage),
                          season = rep(1:4, each = df$nage*2))
df.movement$age[df.movement$country == 'CAN'] <-  df.movement$age[df.movement$country == 'CAN']+0.3 # For plotting
for(i in 1:df$nseason){
  mm.tmp <- df$movemat[,,i,1]
  
  df.movement[df.movement$season == i & df.movement$country == 'USA',]$movement <- mm.tmp[2,] 
  df.movement[df.movement$season == i & df.movement$country == 'CAN',]$movement <- mm.tmp[1,]
}

#png('survey_comps.png', width = 16, height = 10, res= 400, unit = 'cm')
p.move <-ggplot(df.movement, aes(x = age, y = movement, color = country))+facet_wrap(~season)+theme_classic()+
  geom_line(size = 1.45)+scale_y_continuous(limit = c(0,1),name = 'movement rate')+
  scale_color_manual(values = c('darkred','blue4'))+
  theme(legend.title = element_blank(),legend.position = c(0.85,0.85), legend.direction = 'horizontal',
        legend.background = element_rect(fill=NA))

p.move

if(plot.figures == TRUE){
  png(filename = 'Figs/Movement.png', width = 16, height = 12, res = 400, units = 'cm')
  print(p.move)
  dev.off()
  
}


