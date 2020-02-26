#### Plot some data from the hake assessment 

setwd("~/GitHub/PacifichakeMSE/Hake SS3 version")

library(ggplot2)


# Read the assessment data 
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]




### Plot stock recruitment #### 
nyear <- nrow(assessment)

p1 <- ggplot(assessment, aes(x = SSB*1e-6, y = R*1e-6))+geom_point()+
  theme_classic()+scale_y_continuous(name = 'Recruits \n(1e-6 millions)')+
  scale_x_continuous('Spawning biomass')
p1


### Do the stock recruitment relationship 
SSB.pred <- seq(1, max(assessment$SSB), length.out = 100)

SSB0 <- 1750225
h <- 0.8
R0 <- 2108316

Rpred <- 4*h*SSB.pred*R0/(SSB0*(1-h)+SSB.pred*(5*h-1))
plot(SSB.pred,Rpred)

df.plot <- data.frame(SSB = SSB.pred, R = Rpred)

p1 <- ggplot(assessment, aes(x = SSB*1e-6, y = R*1e-6))+geom_point()+
  theme_classic()+scale_y_continuous(name = 'Recruits')+
  scale_x_continuous('Spawning biomass')+
  geom_line(data=df.plot, size = 2, color = 'darkred')

png('C:/Users/Nis/Dropbox/NOAA/teaching/hake MSE/SRplot.png', width = 16, height = 12, units = 'cm', 
    res = 400)
p1
dev.off()

create_pptx(p1,'C:/Users/Nis/Dropbox/NOAA/teaching/hake MSE/SR.pptx')


## Plot spawning biomass divided by SSB0

df.plot <- data.frame(year = assessment$year, SSB = assessment$SSB/SSB0)

p1 <- ggplot(df.plot, aes(x = year, y = SSB))+geom_point()+geom_line()+
  theme_classic()+scale_y_continuous(name = 'Depletion (SSB/SSB0)')+
  scale_x_continuous('Year')+
  geom_hline(aes(yintercept = 1), linetype = 2)+
  geom_hline(aes(yintercept = 0.4), linetype = 2, col = 'blue')+
  geom_hline(aes(yintercept = 0.1), linetype = 2, col = 'darkred')+
  coord_cartesian(ylim = c(0,1.3))

png('C:/Users/Nis/Dropbox/NOAA/teaching/hake MSE/SSBdepletions.png', width = 16, height = 12, units = 'cm', 
    res = 400)
p1
dev.off()
