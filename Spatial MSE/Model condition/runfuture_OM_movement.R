### Run the OM in X amount of years 

direc <- "~/GitHub/PacifichakeMSE/Spatial MSE_vs3/Model condition/"
setwd(direc)
###### Initialize the operating model ###### 
library(TMB)
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(gridExtra)

plot.figures = FALSE # Set true for printing to file 
# Run the simulation model
source('run_agebased_model_true_Catch.R')
source('ylimits.R')
source('plotUncertainty.R')
source('getUncertainty.R')
source('getSelec.R')
source('load_data_seasons_future_move.R')
source('create_TMB_data.R')
source('getRefpoint_biomass.R')
source('Check_Identifiable_vs2.R')
source('runfuture_OM.R')
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]
Catch.obs <- read.csv('hake_totcatch.csv')

movemax.parms <- seq(0.1,0.75, length.out = 5)
movefifty.parms <- seq(5,10, length.out = 5)

move <- c(0.5,5)
yr.future <- 50

df <- load_data_seasons_future_move(yr.future,move = TRUE, movemaxinit  = move[1] , movefiftyinit = move[2])

df$Catch <- Catch.obs$Fishery
Catch.future <- c(df$Catch, rep(206507.8, yr.future)) # Project MSY
df$Catch <- Catch.future
r.succes <- matrix(NA, length(movemax.parms)*length(movefifty.parms))

standard.move <- runfuture_OM(df,100, c(move[1],move[2]))
save.idx <- 1

plot(assessment$year, assessment$SSB)
lines(df$years,standard.move$SSB$p50)

i <- 1 
j <- 1

for(i in 1:length(movemax.parms)){
  for(j in 1:length(movefifty.parms)){
  
  move.tmp <- runfuture_OM(df, nruns = 100, move = c(movemax.parms[i],movefifty.parms[j]))
  
  if(i == 1 & j == 1){
    
    SSB <- move.tmp$SSB
    SSB$run <- paste(as.character(i),'-',as.character(j), sep ='')
    SSB$movemax <- movemax.parms[i]
    SSB$movefifty <- movefifty.parms[j]
    
    
    r.succes[save.idx] <- move.tmp$success.runs
    
    
    # Fix the SSB per country
    SSB.CAN <- as.data.frame(t(apply(move.tmp$SSB.save[,,1], MARGIN = 2, FUN = quantile, probs = c(0.5,0.25,0.75,0.95, 0.05), names = TRUE)))
    SSB.USA <- as.data.frame(t(apply(move.tmp$SSB.save[,,2], MARGIN = 2, FUN = quantile, probs = c(0.5,0.25,0.75,0.95, 0.05), names = TRUE)))
    
    names(SSB.CAN) <- c('p5','p25','p75','p95','p05')
    names(SSB.USA) <- c('p5','p25','p75','p95','p05')
    
    SSB.CAN$year <- df$years
    SSB.USA$year <- df$years
    
    catch.CAN <- as.data.frame(t(apply(move.tmp$catch.AC[,,1], MARGIN = 2, FUN = quantile, probs = c(0.5,0.25,0.75,0.95, 0.05), names = TRUE)))
    catch.USA <- as.data.frame(t(apply(move.tmp$catch.AC[,,2], MARGIN = 2, FUN = quantile, probs = c(0.5,0.25,0.75,0.95, 0.05), names = TRUE)))
    
    names(catch.CAN) <- c('p5','p25','p75','p95','p05')
    names(catch.USA) <- c('p5','p25','p75','p95','p05')
    
    catch.CAN$year <- df$years
    catch.USA$year <- df$years
    
    survey.CAN <- as.data.frame(t(apply(move.tmp$survey.AC[,,1], MARGIN = 2, FUN = quantile, probs = c(0.5,0.25,0.75,0.95, 0.05), names = TRUE)))
    survey.USA <- as.data.frame(t(apply(move.tmp$survey.AC[,,2], MARGIN = 2, FUN = quantile, probs = c(0.5,0.25,0.75,0.95, 0.05), names = TRUE)))
    
    names(survey.CAN) <- c('p5','p25','p75','p95','p05')
    names(survey.USA) <- c('p5','p25','p75','p95','p05')
    
    survey.CAN$year <- df$years
    survey.USA$year <- df$years
    
    
    survey.CAN.s <- as.data.frame(t(apply(move.tmp$survey.space[,,1], MARGIN = 2, FUN = quantile, probs = c(0.5,0.25,0.75,0.95, 0.05), names = TRUE)))
    survey.USA.s <- as.data.frame(t(apply(move.tmp$survey.space[,,2], MARGIN = 2, FUN = quantile, probs = c(0.5,0.25,0.75,0.95, 0.05), names = TRUE)))
    
    names(survey.CAN.s) <- c('p5','p25','p75','p95','p05')
    names(survey.USA.s) <- c('p5','p25','p75','p95','p05')
    
    survey.CAN.s$year <- df$years
    survey.USA.s$year <- df$years
    
    SSB.CAN$run <- paste(as.character(i),'-',as.character(j), sep ='')
    SSB.USA$run <- paste(as.character(i),'-',as.character(j), sep ='')
    catch.CAN$run <- paste(as.character(i),'-',as.character(j), sep ='')
    survey.CAN$run <- paste(as.character(i),'-',as.character(j), sep ='')
    catch.USA$run <- paste(as.character(i),'-',as.character(j), sep ='')
    survey.USA$run <- paste(as.character(i),'-',as.character(j), sep ='')
    
  }else{
    SSB.tmp <- move.tmp$SSB
    SSB.tmp$run <- paste(as.character(i),'-',as.character(j), sep ='')
    SSB.tmp$movemax <- movemax.parms[i]
    SSB.tmp$movefifty <- movefifty.parms[j]
    # Fix the SSB per country
    SSB.CAN.tmp <- as.data.frame(t(apply(move.tmp$SSB.save[,,1], MARGIN = 2, FUN = quantile, probs = c(0.5,0.25,0.75,0.95, 0.05), names = TRUE)))
    SSB.USA.tmp <- as.data.frame(t(apply(move.tmp$SSB.save[,,2], MARGIN = 2, FUN = quantile, probs = c(0.5,0.25,0.75,0.95, 0.05), names = TRUE)))
    
    names(SSB.CAN.tmp) <- c('p5','p25','p75','p95','p05')
    names(SSB.USA.tmp) <- c('p5','p25','p75','p95','p05')
    
    SSB.CAN.tmp$year <- df$years
    SSB.USA.tmp$year <- df$years
    
    SSB <- rbind(SSB, SSB.tmp)
    
    
    SSB.CAN.tmp$run <- paste(as.character(i),'-',as.character(j), sep ='')
    SSB.USA.tmp$run <- paste(as.character(i),'-',as.character(j), sep ='')
    
    SSB.CAN <- rbind(SSB.CAN,SSB.CAN.tmp)
    SSB.USA <- rbind(SSB.USA, SSB.USA.tmp)
    
    r.succes[save.idx] <- move.tmp$success.runs
    
    catch.CAN.tmp <- as.data.frame(t(apply(move.tmp$catch.AC[,,1], MARGIN = 2, FUN = quantile, probs = c(0.5,0.25,0.75,0.95, 0.05), names = TRUE)))
    catch.USA.tmp <- as.data.frame(t(apply(move.tmp$catch.AC[,,2], MARGIN = 2, FUN = quantile, probs = c(0.5,0.25,0.75,0.95, 0.05), names = TRUE)))
    
    names(catch.CAN.tmp) <- c('p5','p25','p75','p95','p05')
    names(catch.USA.tmp) <- c('p5','p25','p75','p95','p05')
    
    catch.CAN.tmp$year <- df$years
    catch.USA.tmp$year <- df$years
    
    survey.CAN.tmp <- as.data.frame(t(apply(move.tmp$survey.AC[,,1], MARGIN = 2, FUN = quantile, probs = c(0.5,0.25,0.75,0.95, 0.05), names = TRUE)))
    survey.USA.tmp <- as.data.frame(t(apply(move.tmp$survey.AC[,,2], MARGIN = 2, FUN = quantile, probs = c(0.5,0.25,0.75,0.95, 0.05), names = TRUE)))
    
    names(survey.CAN.tmp) <- c('p5','p25','p75','p95','p05')
    names(survey.USA.tmp) <- c('p5','p25','p75','p95','p05')
    
    survey.CAN.tmp$year <- df$years
    survey.USA.tmp$year <- df$years
    
    
    survey.CAN.s.tmp <- as.data.frame(t(apply(move.tmp$survey.space[,,1], MARGIN = 2, FUN = quantile, probs = c(0.5,0.25,0.75,0.95, 0.05), names = TRUE)))
    survey.USA.s.tmp <- as.data.frame(t(apply(move.tmp$survey.space[,,2], MARGIN = 2, FUN = quantile, probs = c(0.5,0.25,0.75,0.95, 0.05), names = TRUE)))
    
    names(survey.CAN.s.tmp) <- c('p5','p25','p75','p95','p05')
    names(survey.USA.s.tmp) <- c('p5','p25','p75','p95','p05')
    
    survey.CAN.s.tmp$year <- df$years
    survey.USA.s.tmp$year <- df$years
    
    
    
    catch.CAN.tmp$run <- paste(as.character(i),'-',as.character(j), sep ='')
    survey.CAN.tmp$run <- paste(as.character(i),'-',as.character(j), sep ='')
    catch.USA.tmp$run <- paste(as.character(i),'-',as.character(j), sep ='')
    survey.USA.tmp$run <- paste(as.character(i),'-',as.character(j), sep ='')
    
    
    
    catch.CAN <- rbind(catch.CAN,catch.CAN.tmp)
    catch.USA <- rbind(catch.USA,catch.USA.tmp)
    
    survey.CAN <- rbind(survey.CAN,survey.CAN.tmp)
    survey.USA <- rbind(survey.USA,survey.USA.tmp)
    
    
    
    
      }
    save.idx <- save.idx+1
  }
}



### Plot all the different values
ass.obs <- read.csv('asssessment_MLE.csv')
ass.obs <- ass.obs[ass.obs$year >1965 & ass.obs$year < 2018,]  

# Fix  the movemement rates 
idx.tmp <- t(as.data.frame(strsplit((SSB$run), split = '-')))
row.names(idx.tmp) <- NULL

## Total spawning biomass .

p1 <- ggplot(data = SSB, aes(x = year, y = p50*1e-6, color =movemax, group = run))+theme_classic()+
  geom_line(data = ass.obs, aes(x= year, y = SSB*1e-6, group = NA), color = 'black', size = 1.5)+geom_line()+
  scale_y_continuous('SSB (million  tonnes)')
png("C:/Users/Nis/Documents/GitHub/PacifichakeMSE/Spatial MSE_vs2/Model condition/Figures/SSB_move.png",
    width = 12, height = 8, res = 400, unit = 'cm')
p1
dev.off()
p2 <- ggplot(data = SSB, aes(x = year, y = p50*1e-6, color =movefifty, group = run))+theme_classic()+
  geom_line(data = ass.obs, aes(x= year, y = SSB*1e-6, group = NA), color = 'black', size = 1.5)+geom_line()+
  scale_y_continuous('SSB (million  tonnes)')
png("C:/Users/Nis/Documents/GitHub/PacifichakeMSE/Spatial MSE_vs2/Model condition/Figures/SSB_move2.png",
    width = 12, height = 8, res = 400, unit = 'cm')

p2
dev.off()

png("C:/Users/Nis/Documents/GitHub/PacifichakeMSE/Spatial MSE_vs2/Model condition/Figures/SSB_tot.png",
    width = 12, height = 8, res = 400, unit = 'cm')

p3 <- ggplot(data = SSB, aes(x = year, y = p50*1e-6, color = run))+theme_classic()+
  geom_line(data = ass.obs, aes(x= year, y = SSB*1e-6, group = NA), color = 'black', size = 1.5)+geom_line()+
  scale_y_continuous('SSB (million  tonnes)')+
  geom_ribbon(aes(ymin = p95*1e-6, ymax = p5*1e-6), fill = alpha('darkred', 0.2), color = NA)+
  geom_ribbon(aes(ymin = p75*1e-6, ymax = p25*1e-6), fill = alpha('darkred', 0.2), color = NA)

p3
dev.off()
### SSB in the two different countries 
tmp.can <- SSB.CAN %>% 
  group_by(year) %>% 
  summarise(SSB.m = median(p5), SSB.min = min(p5), SSB.max = max(p5))
tmp.can$country <- 'CAN'
tmp <- SSB.USA %>% 
  group_by(year) %>% 
  summarise(SSB.m = median(p5), SSB.min = min(p5), SSB.max = max(p5))
tmp$country <- 'USA'
df.plot <- rbind(tmp.can, tmp)

mul <- 1e-6

# Data 
survey.obs <- read.csv('survey_country_2.csv')

survey.tot <- survey.obs %>% 
  group_by(year) %>% 
  summarise(Biomass = sum(Bio))

p2 <- ggplot(df.plot, aes(x = year, y= SSB.m*mul, color = country))+geom_line(size = 1.5)+theme_classic()+
  geom_ribbon(data = df.plot[df.plot$country == 'CAN',],
              aes(ymin = SSB.min*mul, ymax = SSB.max*mul), fill = alpha('darkred',0.3), color = NA)+
  geom_ribbon(data = df.plot[df.plot$country == 'USA',],
              aes(ymin = SSB.min*mul, ymax = SSB.max*mul), fill = alpha('blue4',0.3), color = NA)+
  theme(legend.position = '')+scale_y_continuous('Spawning biomass (million tonnes)')+
  coord_cartesian(xlim = c(1965,2018))

png("C:/Users/Nis/Documents/GitHub/PacifichakeMSE/Spatial MSE_vs2/Model condition/Figures/SSB_country.png",
    width = 12, height = 8, res = 400, unit = 'cm')

p2

dev.off()
## The median age at catch
## Which run has the average min and max SSB
source('which_median.R')

minmaxSSB <- SSB.USA[SSB.USA$year>2018,] %>% 
  summarise(SSBmin = min(p5), SSBmax = max(p5), mid = median(p5), 
            runmin = run[which.min(p5)],
            runmax = run[which.max(p5)],
            runmid = run[which.median(p5)])

write.csv(x = minmaxSSB, file = 'minmaxSSB.csv', row.names = FALSE)
### SSB in the two different countries 
tmp.can <- catch.CAN %>% 
  group_by(year) %>% 
  summarise(AC.m = median(p5), AC.min = min(p5), AC.max = max(p5))
tmp.can$country <- 'CAN'

tmp <- catch.USA %>% 
  group_by(year) %>% 
  summarise(AC.m = median(p5), AC.min = min(p5), AC.max = max(p5))
tmp$country <- 'USA'
df.plot <- rbind(tmp.can, tmp)

mul <- 1
am.obs <- read.csv('age_in_catch_obs.csv')

p2 <- ggplot(df.plot, aes(x = year, y= AC.m*mul, color = country))+geom_line(size = 1.5)+theme_classic()+
  geom_ribbon(data = df.plot[df.plot$country == 'CAN',],
              aes(ymin = AC.min*mul, ymax = AC.max*mul), fill = alpha('darkred',0.3), color = NA)+
  geom_ribbon(data = df.plot[df.plot$country == 'USA',],
              aes(ymin = AC.min*mul, ymax = AC.max*mul), fill = alpha('blue4',0.3), color = NA)+
  theme(legend.position = '')+scale_y_continuous('Average age\nin catch')+
  geom_point(data = am.obs,aes(y =am, color=Country), size = 1.5)+
  geom_line(data = am.obs,aes(y =am, color=Country), size = 1, linetype = 2)+
  scale_color_manual(values = c('darkred','darkred', 'blue4', 'blue4'))+
  scale_x_continuous(limit = c(1968,2018),expand=c(0,0), name = '')

p2
#png("C:/Users/Nis/Documents/GitHub/PacifichakeMSE/Spatial MSE_vs3/Model condition/Figures/age_country_catch.png",
#    width = 12, height = 8, res = 400, unit = 'cm')

p2

dev.off()

tmp.can <- survey.CAN %>% 
  group_by(year) %>% 
  summarise(AC.m = median(p5), AC.min = min(p5), AC.max = max(p5))
tmp.can$country <- 'CAN'

tmp <- survey.USA %>% 
  group_by(year) %>% 
  summarise(AC.m = median(p5), AC.min = min(p5), AC.max = max(p5))
tmp$country <- 'USA'
df.plot <- rbind(tmp.can, tmp)

am.s <- read.csv('df_survey.csv')

mul <- 1

p3 <- ggplot(df.plot, aes(x = year, y= AC.m*mul, color = country))+geom_line(size = 1.5)+theme_classic()+
  geom_ribbon(data = df.plot[df.plot$country == 'CAN',],
              aes(ymin = AC.min*mul, ymax = AC.max*mul), fill = alpha('darkred',0.3), color = NA)+
  geom_ribbon(data = df.plot[df.plot$country == 'USA',],
              aes(ymin = AC.min*mul, ymax = AC.max*mul), fill = alpha('blue4',0.3), color = NA)+
  theme(legend.position = 'n')+scale_y_continuous('Average age\nin survey')+scale_color_manual(values = c('darkred','blue4', 'darkred', 'blue4'))+
  geom_point(data = am.s,aes(y =am, color=Country), size = 1.5)+
  geom_line(data = am.s,aes(y =am, color=Country), size = 1, linetype = 2)+
  scale_x_continuous(limit= c(1968,2018),expand=c(0,0))
#   
p3


#png("C:/Users/Nis/Documents/GitHub/PacifichakeMSE/Spatial MSE_vs3/Model condition/Figures/age_survey.png",
 #   width = 12, height = 8, res = 400, unit = 'cm')




png("C:/Users/Nis/Documents/GitHub/PacifichakeMSE/Spatial MSE_vs3/Model condition/Figures/average_ages.png",
  width = 16, height = 10, res = 400, unit = 'cm')
plot_grid(p2,p3,ncol = 1,labels = c('A','B'), label_x = 0.12)
dev.off()


survey.CAN$country <- 'CAN'

survey.USA$country <- 'USA'
df.plot2 <- rbind(survey.CAN,survey.USA)
df.plot2$movemax <- rep(SSB$movemax, 2)
df.plot2$movefifty <- rep(SSB$movefifty,2)
##  Paint the individual lines 
p4 <- ggplot(df.plot2, aes(x =year, y = p5, group = run, color = movemax))+
  geom_line(data = df.plot2[df.plot2$country == 'CAN',], linetype = 2)+
  geom_line(data = df.plot2[df.plot2$country == 'USA',])+theme_classic()+
  scale_y_continuous('Average age')+scale_color_continuous(low = 'darkred', high = 'blue4')

png("C:/Users/Nis/Documents/GitHub/PacifichakeMSE/Spatial MSE_vs2/Model condition/Figures/AM_move.png",
    width = 12, height = 8, res = 400, unit = 'cm')

p4
dev.off()

p5 <- ggplot(df.plot2, aes(x =year, y = p5, group = run, color = movefifty))+
  geom_line(data = df.plot2[df.plot2$country == 'CAN',], linetype = 2)+
  geom_line(data = df.plot2[df.plot2$country == 'USA',])+theme_classic()+
  scale_y_continuous('Average age')+scale_color_continuous(low = 'darkred', high = 'blue4')
png("C:/Users/Nis/Documents/GitHub/PacifichakeMSE/Spatial MSE_vs2/Model condition/Figures/AM_move_2.png",
    width = 12, height = 8, res = 400, unit = 'cm')

p5
dev.off()

# Plot the distribution in the survey 

### SSB in the two different countries 
tmp.can <- survey.CAN.s %>% 
  group_by(year) %>% 
  summarise(survey.m = median(p5), SSB.min = min(p5), SSB.max = max(p5))
tmp.can$country <- 'CAN'
tmp <- SSB.USA %>% 
  group_by(year) %>% 
  summarise(SSB.m = median(p5), SSB.min = min(p5), SSB.max = max(p5))
tmp$country <- 'USA'
df.plot <- rbind(tmp.can, tmp)

