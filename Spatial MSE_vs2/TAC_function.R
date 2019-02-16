### Calculate the estimated vs realized TAC 
library(ggplot2)
library(scales)
source('load_data_seasons.R')
source('getSelec.R')

df.tac <- read.csv('TAC.csv')

SSB_0 <- 2177473


## Calculate the theoretical TAC 
SSB <- seq(0, SSB_0*2, length.out = dim(df.tac)[1])
TAC <- matrix(NA, dim(df.tac)[1])

TAC[(SSB/SSB_0)< 0.1]<- 0 # add a very low catch (fix later)
TAC[(SSB/SSB_0)> 0.4]<- 0.4*SSB[(SSB/SSB_0)> 0.4] # For simplicity assume SSB is the same as V (it's close)

ix <- (SSB/SSB_0)<= 0.4 & (SSB/SSB_0) >= 0.1
TAC[ix]<- 0.4*SSB[ix]*((SSB[ix]-0.1*SSB_0)*((0.4*SSB_0/SSB[ix])/(0.4*SSB_0-0.1*SSB_0)))


df.plot <- data.frame(SSB = SSB, TAC = TAC)

ggplot(data = df.plot, aes(x = SSB*1e-6, y = TAC*1e-6))+geom_line()+
  scale_y_continuous('TAC (million tonnes)')+scale_x_continuous('SSB (million tonnes)')+theme_classic()

df.tac$theotac <- TAC


### Do a regression on the difference between

lm.JMC <- lm(TAC ~ AssessTac, data = df.tac)
lm.realized <- lm(Realized ~ AssessTac, data = df.tac)


#### Print the TAC calc adjustment ####

df.adjTAC <- data.frame(incpt = c(lm.JMC$coefficients[1],lm.realized$coefficients[1]),
                        slp = c(lm.JMC$coefficients[2],lm.realized$coefficients[2]),
                        adj = c('JMC','Realized'))

#write.csv(df.adjTAC, 'adjusted_tac_fn.csv', row.names = FALSE)

df.plot <- data.frame(TAC = TAC,
                      TAC.JMC = predict(lm.JMC, newdata = data.frame(AssessTac = TAC)),
                      TAC.realized = predict(lm.realized, newdata = data.frame(AssessTac = TAC)),
                      SSB = SSB)

df.plot$TAC.JMC[df.plot$TAC.JMC > df.plot$TAC] <-df.plot$TAC[df.plot$TAC.JMC >TAC] 
df.plot$TAC.realized[df.plot$TAC.realized > df.plot$TAC] <-df.plot$TAC[df.plot$TAC.realized >TAC] 


p1 <- ggplot(df.tac, aes( x= theotac*1e-3, y= theotac*1e-3))+geom_line(size = 1)+
  scale_y_continuous('TAC (thousand tonnes)')+
  scale_x_continuous('TAC (thousand tonnes)')+ coord_cartesian(ylim=c(0, 800), xlim = c(0,1000))+theme_bw()+
  geom_point(aes(x=AssessTac*1e-3, y = Realized*1e-3), color = alpha('blue',0.5))+
  geom_point(aes(x=AssessTac*1e-3,y = TAC*1e-3), color = alpha('red',0.5))+
  geom_line(data = df.plot, aes(x = TAC*1e-3, y = TAC.JMC*1e-3), color = alpha('red',0.5), linetype = 2, size = 0.8)+
  geom_line(data = df.plot, aes(x = TAC*1e-3, y = TAC.realized*1e-3), color = alpha('blue',0.5), linetype = 2, size =0.8)

p2 <- ggplot(df.plot, aes(x = SSB*1e-6, y = TAC*1e-6))+geom_line(size = 0.8)+
  geom_line(aes(y = TAC.JMC*1e-6), color = alpha('red',0.5), linetype = 2, size = 0.8)+
  geom_line(aes(y = TAC.realized*1e-6), color = alpha('blue',0.5), linetype = 2, size = 0.8)+
  scale_y_continuous('TAC (million tonnes)')+
  scale_x_continuous('SSB (million tonnes)')+theme_classic()
png('SSB_tac.png', width = 16, height = 12, unit = 'cm', res =400)
p2
dev.off()
