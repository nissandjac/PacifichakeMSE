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
library(r4ss)
library(RColorBrewer)
# Survey age distribution 
source('calcMeanAge.R')
# 2018 assessment
mod <- SS_output(paste(getwd(),'/data/SS32018/', sep =''), printstats=FALSE, verbose = FALSE)
cols <- brewer.pal(6, 'Dark2')


plot.figures = TRUE# Set true for printing to file 
# Run the simulation model
source('load_files_OM.R')
source('load_files.R')
source('runfuture_OM.R')
source('load_data_seasons.R')
source('run_agebased_model_true_catch.R')
source('getSelec.R')
source('getF.R')
assessment <- read.csv('data/assessment_MLE.csv')

#assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]
survey.obs <- read.csv('data/survey_country.csv')
survey.ac <- read.csv('data/ac_survey_country.csv')

nparms <- 5
movemax.parms <- seq(0.15,0.7, length.out = nparms)
movefifty.parms <- seq(1,10, length.out = nparms)

nruns <- nparms^2

yr.future <- 2

df <- load_data_seasons(movemaxinit = 0.4, yr_future = yr.future)

Catch.future <- c(df$Catch, rep(206507.8, yr.future)) # Project MSY
df$Catch <- Catch.future
r.succes <- matrix(NA, length(movemax.parms)*length(movefifty.parms))
AC.catch.tot <- AC.survey.tot <- array(NA, dim = c(df$age_maxage,df$nyear, 
                                                   df$nspace,length(movemax.parms)*length(movefifty.parms)))
survey.ml <- array(NA,dim = c(df$nyear,df$nspace,length(movemax.parms)*length(movefifty.parms)))

#catch.ac.obs <- read.csv('data/age_in_catch_obs.csv')
catch.ac.obs <- read.csv('data/ac_catch_new.csv')
standard.move <- runfuture_OM(df,nruns = 1)
sim.data <- run.agebased.true.catch(df)

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

p1

#windows(width = 16/cm(1), height = 10/cm(1))

# plot_grid(p1,p.AC.survey, nrow = 2, align = 'hv', 
#           label_x = c(0.95,0.95),
#           label_y = c(0.98,0.98),
#           labels = 'auto')
# dev.off()
if(plot.figures == TRUE){
  png(filename = 'Figs/survey_combined.png', width = 16, height = 10, res = 400, units = 'cm')
  print(plot_grid(p1,p.AC.survey, nrow = 2, align = 'hv', 
                  label_x = c(0.95,0.95),
                  label_y = c(0.98,0.98),
                  labels = 'auto'))
  dev.off()
}



# Total survey

# From the asssessment 
df.ass <- data.frame(survey = mod$cpue$Exp,source = 'assessment',years = mod$cpue$Yr, country ='Both')

df.tot <- rbind(df.plot,df.ass) %>% 
  group_by(years, source) %>% 
  summarise(survey = sum(survey))

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
catch.ac.obs <- read.csv('data/ac_catch_new.csv')

catch.model <- data.frame(year = rep(df$years,2),
                          Country = rep(c('USA','CAN'), each = df$nyear),
                          am = c(standard.move$catch.AC[,2],standard.move$catch.AC[,1]))


p.AC.catch <- ggplot(catch.ac.obs, aes(x = year, y= am, color = Country))+geom_line(size = 1, linetype = 2)+theme_classic()+
  geom_line(data = catch.model, linetype = 1, size = 1)+geom_point()+
  scale_color_manual(values = c('darkred','blue4','green'))+coord_cartesian(xlim = c(min(df$years),2019))+
  theme(legend.position = 'none')+scale_y_continuous('mean age')

p.AC.catch

if(plot.figures == TRUE){
  png(filename = 'Figs/AC_catch.png', width = 16, height = 8, res = 400, units = 'cm')
  print(p.AC.catch)
  dev.off()
  
  }

## Add the overall average age
ac <- apply(df$age_catch,2, function(x){sum(x*1:15)})
ac[ac<0] <- NA

p.AC.catch2 <- p.AC.catch+geom_line(data = data.frame(year = df$years, am = ac, Country = 'All'), color ='black')

p.AC.catch2


surv.tot <- survey.obs %>% 
  group_by(year) %>%   
  summarise(survey = sum(survey))

#surv.model <- data.frame(year = df$years, )
surv.tot$frac <- survey.obs$survey[survey.obs$country == 'CAN']/survey.obs$survey[survey.obs$country == 'USA']

ggplot(surv.tot, aes(x = year, y = frac))+geom_line()+geom_hline(aes(yintercept =1), lty = 2)+
  geom_hline(aes(yintercept = 0.26), col= 'red', lty = 2)+
  geom_hline(aes(yintercept = median(frac)), col= 'green')


# Fishing mortality per area 

F.plot <- apply(sim.data$Fout, FUN = sum, MARGIN = c(1,3))
F0.assessment <- read.csv('data/F0.csv')

df.F <- data.frame(year = rep(df$years,2),
                   F0 = c(F.plot[,1],F.plot[,2]),
                   Country = rep(c('CAN','USA'), each = df$nyear))

p2 <- ggplot(df.F)+geom_col(aes(x = year, y = F0, fill = Country), position = position_dodge(width = 0.9))+
  scale_fill_manual(values = c('darkred','blue4'))+
  geom_line(data = data.frame(year = 1966:2018, F0 = F0.assessment$x), aes(x = year, y= F0), size = 1.5, color = 'black')
  
p2


if(plot.figures == TRUE){
  png(filename = 'Figs/F_country.png', width = 16, height = 12, res = 400, units = 'cm')
  print(p2)
  dev.off()
  
}

## Do the comparison with Kristins data 

save.idx <- 1
i <- 1
j <- 1

succes <- matrix(NA, nruns)


for(i in 1:length(movemax.parms)){
  for(j in 1:length(movefifty.parms)){
    
    # 
    # print(paste('movemax =', movemax.parms[i]))
    # print(paste('move fifty =', movefifty.parms[j]))
    # 
    df <- load_data_seasons_future(yr.future,
                                        movemaxinit  = movemax.parms[i] , movefiftyinit = movefifty.parms[j])
    
    Catch.future <- c(df$Catch, rep(206507.8, yr.future)) # Project MSY
    df$Catch <- Catch.future
    
    move.tmp <- try(runfuture_OM(df,1), silent = TRUE)
    
    
    if(length(move.tmp) == 1){
      move.tmp <- NA
      succes[save.idx] <- 0
    }else{
      
      succes[save.idx] <- 1
      
      if(i == 1 & j == 1){
        
        
        SSB <- move.tmp$SSB
        SSB.weight <- move.tmp$SSB.weight
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
        
        SSB.w.tmp <- move.tmp$SSB.weight
        SSB.w.tmp$run <- paste(as.character(i),'-',as.character(j), sep ='')
        
        AC.catch.tmp <- data.frame(AC = c(move.tmp$catch.AC[,1],move.tmp$catch.AC[,2]),
                                   Country = rep(c('CAN','USA'), each = df$nyear), year = rep(df$years,2))
        AC.survey.tmp <- data.frame(AC = c(move.tmp$survey.AC[,1],move.tmp$survey.AC[,2]),
                                    Country = rep(c('CAN','USA'), each = df$nyear), year = rep(df$years,2))
        AC.catch.tmp$run<- paste(as.character(i),'-',as.character(j), sep ='')
        AC.survey.tmp$run <- paste(as.character(i),'-',as.character(j), sep ='')
        
        
        SSB <- rbind(SSB, SSB.tmp)
        #SSB.weight <- rbind(SSB.weight, SSB.w.tmp)
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
    
    }
    
    save.idx <- save.idx+1
  }
}

sum(succes)
## Add the true SSB
SSB.true <- assessment

SSB.2 <- mod$derived_quants$Value[grep('SSB_1966',x = mod$derived_quants$Label):grep('SSB_2018',x = mod$derived_quants$Label)]


p.ssb <- ggplot(SSB, aes(x = year, y= SSB*1e-6, color = run))+geom_line(size = 1.2)+theme_classic()+
  theme(legend.position="none")+geom_line(data = SSB.true, color = 'black', linetype = 2)+
  scale_y_continuous('SSB (million tons)')

if(plot.figures == TRUE){
png(filename = 'Figs/SSB.png', width = 16, height = 12, res = 400, units = 'cm')
}
  p.ssb

dev.off()



AC.survey.plot <- AC.survey %>% 
  group_by(year,Country) %>% 
  summarise(AC.mean = mean(AC), AC.sd = sd(AC)) 


p.AC.survey <- ggplot(AC.survey.plot, aes(x = year, y= AC.mean, color = Country))+geom_line()+theme_bw()+
  geom_ribbon(data = AC.survey.plot[AC.survey.plot$Country == 'CAN',],
              aes(ymin= AC.mean-2*AC.sd, ymax= AC.mean+2*AC.sd), fill = c(alpha('darkred', 0.3)), color = NA)+
  geom_ribbon(data = AC.survey.plot[AC.survey.plot$Country == 'USA',],
              aes(ymin= AC.mean-2*AC.sd, ymax= AC.mean+2*AC.sd), fill = c(alpha('blue4', 0.3)), color = NA)+
  geom_line(data = ac.survey.tot, aes(color = country), linetype = 1, size = 0.8)+
  geom_point(data = ac.survey.tot, aes(color = country))+
  scale_color_manual(values = c('darkred','blue4'))+
  theme(legend.position = 'none')+scale_y_continuous('average age in survey')


if(plot.figures == TRUE){
png(filename = 'AC_survey.png', width = 16, height = 12, res = 400, units = 'cm')
}
p.AC.survey
dev.off()

### Survey 
grid.arrange(p.AC.catch,p.AC.survey)

df.plot.survey <- survey.save %>% 
  group_by(Country,year) %>% 
  summarise(survey.m = median(survey), surv.min = min(survey), surv.max = max(survey))
  
mul <- 1e-3
p1.survey <- ggplot(df.plot.survey, aes(x = year, y = survey.m*mul, color = Country))+theme_classic()+geom_line(size = 1.2)+ 
   geom_ribbon(data = df.plot.survey[df.plot.survey$Country == 'CAN',],
               aes(ymin= surv.min*mul, ymax= surv.max*mul), fill = c(alpha('darkred', 0.3)), color = NA)+
   geom_ribbon(data = df.plot.survey[df.plot.survey$Country == 'USA',],
               aes(ymin= surv.min*mul, ymax= surv.max*mul), fill = c(alpha('blue4', 0.3)), color = NA)+
   geom_point(data = survey.obs, aes(color = country, y = survey*mul), size = 2)+
  geom_line(data = survey.obs, aes(color = country, y = survey*mul), size = 0.9)+
  scale_y_continuous('Survey biomass \n(1000 tonnes)')

if(plot.figures == TRUE){
  png('survey_country.png', width = 16, height = 12, units = 'cm', res = 400)
}
p1.survey
dev.off()
