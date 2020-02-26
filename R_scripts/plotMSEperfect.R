### Plot the perfect MSE 
source('hake_objectives.R')
source('df_lists.R')
library(scales)

load('MSErun_perfect.Rdata')
load('sim_data.Rdata')
simyears <- 30
yr <- 1966:(2017+simyears-1)
nruns <- 100
## Biomass 

obj.JMC <- hake_objectives(ls.save,sim.data, move = 1)
obj.JMC[[2]]$HCR <- 'JMC'


ls.HCR.plot <- df_lists(ls.save, 'Perfect')

df.all <- ls.HCR.plot[[1]]
df.all <- df.all[df.all$year>2017,]

p1 <- ggplot(ls.HCR.plot[[3]]$SSBtot, aes(x = year, y = med/sum(sim.data$SSB0)))+geom_line(size= 1.2)+
  geom_line(aes(y = avg/sum(sim.data$SSB0)), linetype = 2, size = 1.1)+
  geom_ribbon(aes(ymin = p5/sum(sim.data$SSB0),ymax = p95/sum(sim.data$SSB0)),
              fill = alpha(colour = 'darkred', alpha = 0.2))+
  geom_line(data = df.all[1:180,], aes(x = year, y= SSBtot/sum(sim.data$SSB0), group = run), color = alpha('black', 0.5))+
  scale_y_continuous('SSB/SSB0')+coord_cartesian(ylim = c(0,1.2))
  
print(ls.HCR.plot[[3]]$SSBtot$med/sum(sim.data$SSB0))


png('SSB_perfect.png', width = 12, height =8, res = 400, unit = 'cm')
p1
dev.off()



