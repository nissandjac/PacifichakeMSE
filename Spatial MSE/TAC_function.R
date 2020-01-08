### Calculate the estimated vs realized TAC 
library(ggplot2)
library(scales)
source('load_data_seasons.R')
source('getSelec.R')
source('load_files.R')
source('load_files_OM.R')

df.tac <- read.csv('data/TAC.csv')

df <- load_data_seasons(nseason = 1, nspace = 1)
sim.data <- run.agebased.true.catch(df)
## Calculate the theoretical TAC 
SSB <- seq(0, sum(sim.data$SSB0)*2, length.out = dim(df.tac)[1])
SSB_0 <- sum(sim.data$SSB0)

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

#lm.JMC <- lm(TAC ~ AssessTac, data = df.tac[df.tac$Year >= 2012,])
#lm.STAR <- lm(TAC ~ AssessTac, data = df.tac[df.tac$Year < 2012,])
lm.historical <- lm(TAC ~ AssessTac, data = df.tac)
lm.realized <- lm(Realized ~ AssessTac, data = df.tac)


#### Print the TAC calc adjustment ####
# 
# df.adjTAC <- data.frame(incpt = c(lm.JMC$coefficients[1],lm.realized$coefficients[1], lm.STAR$coefficients[1]),
#                         slp = c(lm.JMC$coefficients[2],lm.realized$coefficients[2], lm.STAR$coefficients[2]),
#                         adj = c('JMC','Realized','STAR'))

df.adjTAC <- data.frame(incpt = c(lm.historical$coefficients[1],lm.realized$coefficients[1]),
                        slp = c(lm.historical$coefficients[2],lm.realized$coefficients[2]),
                        adj = c('historical','realized'))

#write.csv(df.adjTAC, 'adjusted_tac_fn.csv', row.names = FALSE)
# 
# df.plot <- data.frame(TAC = TAC,
#                       TAC.JMC = predict(lm.JMC, newdata = data.frame(AssessTac = TAC)),
#                       TAC.realized = predict(lm.realized, newdata = data.frame(AssessTac = TAC)),
#                       TAC.STAR = predict(lm.STAR, newdata = data.frame(AssessTac = TAC)),
#                       SSB = SSB)

df.plot <- data.frame(TAC = TAC,
                      TAC.historical = predict(lm.historical, newdata = data.frame(AssessTac = TAC)),
                      TAC.realized = predict(lm.realized, newdata = data.frame(AssessTac = TAC)),
                      SSB = SSB)



df.plot$TAC.historical[df.plot$TAC.historical > df.plot$historical] <-df.plot$TAC[df.plot$TAC.historical >TAC] 
df.plot$TAC.realized[df.plot$TAC.realized > df.plot$TAC] <-df.plot$TAC[df.plot$TAC.realized >TAC] 
#df.plot$TAC.STAR[df.plot$TAC.STAR > df.plot$TAC] <-df.plot$TAC[df.plot$TAC.STAR >TAC] 
# Floor data 
df.plot$Floor <- df.plot$TAC*0.5
df.plot$Floor[df.plot$Floor<= 180000] <- 180000
df.plot$TAC.HCR <- df.plot$TAC

df.plot.w <- melt(df.plot[,-4], id.vars = 'TAC', value.name = 'Quota', variable.name = 'HCR')
nhcr <- unique(df.plot.w$HCR)
cols <- RColorBrewer::brewer.pal(length(unique(df.plot.w$HCR)),'Dark2')


df.plot.w$HCR <- factor(df.plot.w$HCR, levels = c("TAC.HCR", "TAC.historical", "TAC.realized",
                                                  'Floor'))



p1 <- ggplot(df.plot.w, aes(x= TAC*1e-3, y = Quota*1e-3, color = HCR))+geom_line(linetype = 2, size = 0.8)+
  scale_y_continuous('Catch \n(thousand tonnes)')+scale_color_manual(values = c('black',cols[1:3]),
                                                                     labels = c('HCR','historical','realized','floor'))+
  scale_x_continuous('Harvest control rule')+ coord_cartesian(ylim=c(0, 800), xlim = c(0,1000))+
  geom_point(data = df.tac,aes(x=AssessTac*1e-3, y = Realized*1e-3), color = alpha(cols[2],0.5))+
  geom_point(data = df.tac,aes(x=AssessTac*1e-3,y = TAC*1e-3), color = alpha(cols[1],0.5))+
  #geom_point(data = df.tac[df.tac$Year >= 2012,],aes(x=AssessTac*1e-3,y = TAC*1e-3), color = alpha(cols[4],0.5))+
  theme(legend.title = element_blank(),
        legend.key.size =  unit(.2, "cm"),
        legend.text=element_text(size=7),
        legend.position = c(0.1,0.8))
p1

# p1 <- ggplot(df.tac, aes( x= theotac*1e-3, y= theotac*1e-3))+geom_line(size = 1, linetype = 2)+
#   geom_point(aes(x=AssessTac*1e-3, y = Realized*1e-3), color = alpha(cols[2],0.5))+
#   geom_point(aes(x=AssessTac*1e-3,y = TAC*1e-3), color = alpha(cols[1],0.5))+
#   geom_line(data = df.plot, aes(x = TAC*1e-3, y = TAC.JMC*1e-3),
#             color = alpha(cols[1],0.5), linetype = 2, size = 0.8)+
#   geom_line(data = df.plot, aes(x = TAC*1e-3, y = TAC.realized*1e-3),
#             color = alpha(cols[2],0.5), linetype = 2, size =0.8)+
#   geom_line(data = df.plot, aes(x = TAC*1e-3, y = Floor*1e-3),
#             color = alpha(cols[3],0.5), linetype = 2, size =0.8)+
#   scale_y_continuous('Catch \n(thousand tonnes)')+
#   scale_x_continuous('Harvest control rule')+ coord_cartesian(ylim=c(0, 800), xlim = c(0,1000))
# 
# 
# p1

# 
p2 <- ggplot(df.plot, aes(x = SSB*1e-6, y = TAC*1e-6))+geom_line(size = 0.8)+
  geom_line(aes(y = TAC.JMC*1e-6), color = alpha('red',0.5), linetype = 2, size = 0.8)+
  geom_line(aes(y = TAC.realized*1e-6), color = alpha('blue',0.5), linetype = 2, size = 0.8)+
  scale_y_continuous('TAC (million tonnes)')+
  scale_x_continuous('SSB (million tonnes)')+theme_classic()
#png('SSB_tac.png', width = 16, height = 12, unit = 'cm', res =400)
p2
# dev.off()



png('Figs/tacplot.png', width = 12, height = 8, unit = 'cm', res =400)
p1
dev.off()
