### Run the OM in X amount of years 

###### Initialize the operating model ###### 
library(TMB)
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(gridExtra)
library(dplyr)
library(cowplot)

plot.figures = FALSE # Set true for printing to file 
# Run the simulation model
source('load_files_OM.R')
source('load_files.R')
source('runfuture_OM.R')
source('load_data_seasons_future.R')
source('run_agebased_model_true_catch_move.R')
assessment <- read.csv('data/asssessment_MLE.csv')

assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]
survey.obs <- read.csv('data/survey_country_2.csv')


nparms <- 5
movemax.parms <- seq(0.1,0.70, length.out = nparms)
movefifty.parms <- seq(5,10, length.out = nparms)

nruns <- nparms^2

yr.future <- 2

df <- load_data_seasons_future(yr.future, movemaxinit = 0.5 , movefiftyinit = 8)
df$surveyseason <- 3

#df$parms$PSEL <- 0*df$parms$PSEL

Catch.future <- c(df$Catch, rep(206507.8, yr.future)) # Project MSY
df$Catch <- Catch.future
r.succes <- matrix(NA, length(movemax.parms)*length(movefifty.parms))
AC.catch.tot <- AC.survey.tot <- array(NA, dim = c(df$age_maxage,df$nyear, 
                                                   df$nspace,length(movemax.parms)*length(movefifty.parms)))
survey.ml <- array(NA,dim = c(df$nyear,df$nspace,length(movemax.parms)*length(movefifty.parms)))

survey.obs <- read.csv('data/df_survey.csv')
catch.ac.obs <- read.csv('data/age_in_catch_obs.csv')


standard.move <- runfuture_OM(df, 1)

plot(df$years,standard.move$survey.AC[,1], type= 'l', col = 'red', ylim = c(2,11), xlim = c(1990,2018))
lines(df$years,standard.move$survey.AC[,2], col = 'blue')
lines(survey.obs$year[survey.obs$Country == 'USA'],survey.obs$am[survey.obs$Country == 'USA'], lty = 'longdash' , col = 'blue')
lines(survey.obs$year[survey.obs$Country == 'CAN'],survey.obs$am[survey.obs$Country == 'CAN'], lty = 'longdash' , col = 'red')

plot(df$years,standard.move$survey.space[1,], col = 'red', type = 'l', ylim = c(1e5, 5e6))
lines(df$years,standard.move$survey.space[2,], col = 'blue')
lines(survey.obs$year[survey.obs$Country == 'USA'], survey.obs$Biomass[survey.obs$Country == 'USA']*1e-3, type = 'o', col = 'blue')

surv.tot <- survey.obs %>% 
  group_by(year) %>% 
  summarise(survey = sum(Biomass)*1e-2)

# Is the country split correct? 
ggplot(surv.tot, aes(x = year, y = survey))+geom_point()+geom_line(col = 'red')+
  geom_line(data=data.frame(year = df$years[df$flag_survey ==1], survey = df$survey[df$flag_survey ==1]))


save.idx <- 1
i <- 1
j <- 1

for(i in 1:length(movemax.parms)){
  for(j in 1:length(movefifty.parms)){
    
    print(paste('movemax =', movemax.parms[i]))
    print(paste('move fifty =', movefifty.parms[j]))
    
    df <- load_data_seasons_future(yr.future,
                                        movemaxinit  = movemax.parms[i] , movefiftyinit = movefifty.parms[j])
    
    Catch.future <- c(df$Catch, rep(206507.8, yr.future)) # Project MSY
    df$Catch <- Catch.future
    
    move.tmp <- runfuture_OM(df,1)
    
    if(i == 1 & j == 1){
      
      SSB <- move.tmp$SSB
      SSB$run <- paste(as.character(i),'-',as.character(j), sep ='')
    
      r.succes[save.idx] <- move.tmp$success.runs
      
      AC.catch <- data.frame(AC = c(move.tmp$catch.AC[,1],move.tmp$catch.AC[,2]),
                             Country = rep(c('CAN','USA'), each = df$nyear), year = rep(df$years,2))
      AC.survey <- data.frame(AC = c(move.tmp$survey.AC[,1],move.tmp$survey.AC[,2]),
                               Country = rep(c('CAN','USA'), each = df$nyear), year = rep(df$years,2))
      
      AC.catch$run<- paste(as.character(i),'-',as.character(j), sep ='')
      AC.survey$run <- paste(as.character(i),'-',as.character(j), sep ='')
      
      
      AC.catch.tot[,,,save.idx] <- move.tmp$catch.AC.tot
      AC.survey.tot[,,,save.idx] <- move.tmp$survey.AC.tot

     
      survey.ml[,,save.idx] <- move.tmp$survey.space
      survey.save <- data.frame(survey = c(move.tmp$survey.space[1,],move.tmp$survey.space[2,]),
                                Country = rep(c('CAN','USA'), each = df$nyear), year = rep(df$years,2),
                                run = paste(as.character(i),'-',as.character(j), sep =''))
     
                              
    }else{
      SSB.tmp <- move.tmp$SSB
      SSB.tmp$run <- paste(as.character(i),'-',as.character(j), sep ='')
      
      AC.catch.tmp <- data.frame(AC = c(move.tmp$catch.AC[,1],move.tmp$catch.AC[,2]),
                                 Country = rep(c('CAN','USA'), each = df$nyear), year = rep(df$years,2))
      AC.survey.tmp <- data.frame(AC = c(move.tmp$survey.AC[,1],move.tmp$survey.AC[,2]),
                              Country = rep(c('CAN','USA'), each = df$nyear), year = rep(df$years,2))
      AC.catch.tmp$run<- paste(as.character(i),'-',as.character(j), sep ='')
      AC.survey.tmp$run <- paste(as.character(i),'-',as.character(j), sep ='')
      
      
      SSB <- rbind(SSB, SSB.tmp)
      AC.catch <- rbind(AC.catch, AC.catch.tmp)
      AC.survey <- rbind(AC.survey, AC.survey.tmp)
      
      survey.tmp <- data.frame(survey = c(move.tmp$survey.space[1,],move.tmp$survey.space[2,]),
                                           Country = rep(c('CAN','USA'), each = df$nyear), year = rep(df$years,2),
                                           run = paste(as.character(i),'-',as.character(j), sep =''))
      
      survey.save <- rbind(survey.save,survey.tmp)
      
      
      r.succes[save.idx] <- move.tmp$success.runs
      
      AC.catch.tot[,,,save.idx] <- move.tmp$catch.AC.tot
      AC.survey.tot[,,,save.idx] <- move.tmp$survey.AC.tot
      survey.ml[,,save.idx] <- move.tmp$survey.space
      
      
    }
    
   # p <- movement_parameters(df,ac.surv = move.tmp$survey.AC.tot,ac.catch = move.tmp$catch.AC.tot,
    #                         biomass = move.tmp$survey.space)
    
    
    
    save.idx <- save.idx+1
  }
}


## Add the true SSB
SSB.true <- read.csv('data/asssessment_MLE.csv')
SSB.true <- SSB.true[SSB.true$year > 1965 & SSB.true$year < 2018 ,]
SSB.true$SSB <- SSB.true$SSB*2


p.ssb <- ggplot(SSB, aes(x = year, y= SSB*1e-6, color = run))+geom_line()+theme_classic()+
  theme(legend.position="none")+geom_line(data = SSB.true, color = 'black', linetype = 2)+
  scale_y_continuous('SSB (million tons)')

if(plot.figures == TRUE){
png(filename = 'SSB.png', width = 16, height = 12, res = 400, units = 'cm')
}
  p.ssb

dev.off()

catch.ac.obs <- read.csv('data/age_in_catch_obs.csv')

AC.catch.plot <- AC.catch %>% 
  group_by(year,Country) %>% 
  summarise(AC.mean = mean(AC), AC.sd = sd(AC)) 


p.AC.catch <- ggplot(AC.catch.plot, aes(x = year, y= AC.mean, color = Country))+geom_line()+theme_classic()+
  geom_ribbon(data = AC.catch.plot[AC.catch.plot$Country == 'CAN',],
              aes(ymin= AC.mean-2*AC.sd, ymax= AC.mean+2*AC.sd), fill = c(alpha('darkred', 0.3)), color = NA)+
  geom_ribbon(data = AC.catch.plot[AC.catch.plot$Country == 'USA',],
              aes(ymin= AC.mean-2*AC.sd, ymax= AC.mean+2*AC.sd), fill = c(alpha('blue4', 0.3)), color = NA)+
  geom_line(data = catch.ac.obs, aes(x = year, y = am, color = Country), linetype = 1, size = 0.8)+
  geom_point(data = catch.ac.obs, aes(x = year, y = am, color = Country))+
  scale_color_manual(values = rep(c('darkred','blue4'),each= 2))+
  theme(legend.position = 'none')+scale_y_continuous('average age in catch')

if(plot.figures == TRUE){
  png(filename = 'AC_catch.png', width = 16, height = 12, res = 400, units = 'cm')
}
p.AC.catch
dev.off()

survey.obs <- read.csv('data/df_survey.csv')

AC.survey.plot <- AC.survey %>% 
  group_by(year,Country) %>% 
  summarise(AC.mean = mean(AC), AC.sd = sd(AC)) 


p.AC.survey <- ggplot(AC.survey.plot, aes(x = year, y= AC.mean, color = Country))+geom_line()+theme_bw()+
  geom_ribbon(data = AC.survey.plot[AC.survey.plot$Country == 'CAN',],
              aes(ymin= AC.mean-2*AC.sd, ymax= AC.mean+2*AC.sd), fill = c(alpha('darkred', 0.3)), color = NA)+
  geom_ribbon(data = AC.survey.plot[AC.survey.plot$Country == 'USA',],
              aes(ymin= AC.mean-2*AC.sd, ymax= AC.mean+2*AC.sd), fill = c(alpha('blue4', 0.3)), color = NA)+
  geom_line(data = survey.obs, aes(x = year, y = am, color = Country), linetype = 1, size = 0.8)+
  geom_point(data = survey.obs, aes(x = year, y = am, group = Country))+
  scale_color_manual(values = c('darkred','blue4'))+
  theme(legend.position = 'none')+scale_y_continuous('average age in survey')


if(plot.figures == TRUE){
png(filename = 'AC_survey.png', width = 16, height = 12, res = 400, units = 'cm')
}
p.AC.survey
dev.off()

### Survey 


# Which set of movement parameters best fit the data 
survey.obs <- read.csv('data/survey_country_2.csv')

df.plot.survey <- survey.save %>% 
  group_by(Country,year) %>% 
  summarise(survey.m = mean(survey), surv.min = min(survey), surv.max = max(survey))
  
mul <- 1e-6


p1.survey <- ggplot(df.plot.survey, aes(x = year, y = survey.m*mul, color = Country))+theme_classic()+geom_line(size = 1.2)+ 
   geom_ribbon(data = df.plot.survey[df.plot.survey$Country == 'CAN',],
               aes(ymin= surv.min*mul, ymax= surv.max*mul), fill = c(alpha('darkred', 0.3)), color = NA)+
   geom_ribbon(data = df.plot.survey[df.plot.survey$Country == 'USA',],
               aes(ymin= surv.min*mul, ymax= surv.max*mul), fill = c(alpha('blue4', 0.3)), color = NA)+
   geom_point(data = survey.obs, aes(y = Bio*1e-3*mul), size = 2)+
  geom_line(data = survey.obs, aes(y = Bio*1e-3*mul), size = 0.9)+
  scale_y_continuous('Survey biomass \n(1000 tonnes)')

if(plot.figures == TRUE){
  png('survey_country.png', width = 16, height = 12, units = 'cm', res = 400)
}
p1.survey
dev.off()

