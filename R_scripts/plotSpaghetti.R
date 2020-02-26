## Plot a spaghetti plot ## 

library(ggplot2)
library(scales)
library(gridExtra)
library(RColorBrewer)
source('load_files.R')
source('load_files_OM.R')
source('df_lists.R')


load('results/HCR/MSE_JMC.Rdata') # ls.save 
simyears <- length(ls.save[[1]][1]$Catch)-(length(1966:2017))+1
years <- 1966:(2017+simyears-1)
lists <- df_lists(ls.save, nms = 'lol')

df.tmp <- data.frame(SSB = rowSums(ls.save[[1]]$SSB), years = years, run = 1) 
df <- load_data_seasons()
sim.data <- run.agebased.true.catch(df)

SSB.plot <- df.tmp

for(i in 1:length(ls.save)){
  tmp <- data.frame(SSB = rowSums(ls.save[[i]]$SSB), years = years, run = i) 
  SSB.plot <- rbind(SSB.plot,tmp)
}

cols <- brewer.pal(6, 'Dark2')
idx <- c(1,2,3,4)

df.all <- df

## Add the average and spread


p1 <- ggplot(SSB.plot[SSB.plot$run %in% idx,], aes(x = years, y = SSB/sum(sim.data$SSB0),color = as.factor(run)))+
  geom_line(size = 1.1)+scale_color_manual(values = cols[idx])+
  geom_line(data = SSB.plot[SSB.plot$run == 1 & SSB.plot$years<2019,], color = 'black', size = 1.2)+
  scale_y_continuous('SSB/SSB0')+coord_cartesian(ylim = c(0,2))+
  geom_hline(aes(yintercept = 1), linetype = 2)+ theme(legend.position = "none")
  
  
p2 <- ggplot(SSB.plot[SSB.plot$run %in% idx,], aes(x = years, y = SSB/sum(sim.data$SSB0),color = as.factor(run)))+
  geom_line(size = 1.1, linetype = 1)+scale_color_manual(values = cols[idx])+
  geom_line(data = SSB.plot[SSB.plot$run == 1 & SSB.plot$years<2019,], color = 'black', size = 1.2)+
  scale_y_continuous('SSB/SSB0')+coord_cartesian(ylim = c(0,3))+
  geom_hline(aes(yintercept = 1), linetype = 2)+ theme(legend.position = "none")+
  geom_line(data = lists[[3]]$SSBtot, aes(x = year,y = med/sum(sim.data$SSB0)), size = 2, color ='black')+
#  geom_ribbon(data = lists[[3]]$SSBtot,aes(ymin = p5, ymax = p5), color = alpha('gray', alpha = 0.2))+
  geom_line(data = lists[[3]]$SSBtot, aes(x = year,y = p5/sum(sim.data$SSB0)), size = 1, color ='black', linetype = 1)+
  geom_line(data = lists[[3]]$SSBtot, aes(x = year,y = p95/sum(sim.data$SSB0)), size = 1, color ='black', linetype = 1)

p2  

png('Figs/Spaghetti.png', height = 12, width = 16, units = 'cm', res = 400)
p2
dev.off()
