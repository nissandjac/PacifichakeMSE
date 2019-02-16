
source('df_lists_OM.R')
require(ggplot2)
require(dplyr)
require(scales)
require(RColorBrewer)
require(reshape2)

load('JTC_OM.RData')
load('JMC_OM.RData')
load('realized_OM.RData')
load('move1_OM.RData')
load('move2_OM.RData')
load('move3_OM.RData')


simyears <- 30
yr <- 1966:(2017+simyears-1)
nruns <- 100


ls.HCR.plot <- df_lists_OM(ls.JTC.s, 'HCR')
ls.JMC.plot <- df_lists_OM(ls.JMC.s, 'JMC')
ls.real.plot <- df_lists_OM(ls.real.s, 'Realized')
ls.move1.plot <- df_lists_OM(ls.move1.s, 'Move 1')
ls.move2.plot <- df_lists_OM(ls.move2.s, 'Move 2')
ls.move3.plot <- df_lists_OM(ls.move3.s, 'Move 3')

df.all <- rbind(ls.HCR.plot[[3]][[1]], ls.JMC.plot[[3]][[1]],ls.real.plot[[3]][[1]],
                ls.move1.plot[[3]][[1]],ls.move2.plot[[3]][[1]], ls.move3.plot[[3]][[1]]
)
df.all$run <- as.factor(df.all$run)
df.all$run <- ordered(df.all$run, levels = c("HCR", "JMC", "Realized",'Move 1','Move 2','Move 3'))

p1 <- ggplot(df.all, aes(x = year, y = med.can*1e-6))+geom_line(color = 'red')+
  geom_line(aes(y = med.US*1e-6), color = 'blue')+theme_classic()+scale_y_continuous(name ='SSB (million tonnes)')+facet_wrap(~run)+  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_ribbon(aes(ymin = p5.can*1e-6, ymax = p95.can*1e-6), fill = alpha('red', alpha = 0.2), linetype = 0)+
  geom_ribbon(aes(ymin = p5.US*1e-6, ymax = p95.US*1e-6), fill = alpha('blue', alpha = 0.2), linetype = 0)

p1


cols <- brewer.pal(6, 'Dark2')

df.ams <- rbind(ls.HCR.plot[[3]][[2]], ls.JMC.plot[[3]][[2]],ls.real.plot[[3]][[2]],
                ls.move1.plot[[3]][[2]],ls.move2.plot[[3]][[2]], ls.move3.plot[[3]][[2]])

# df.ams2 <- melt(df.ams, id.vars = c(1,8))
 df.ams$run <- ordered(df.ams$run, levels = c("HCR", "JMC", "Realized",'Move 1','Move 2','Move 3'))
# rbs <- c('p95.can','p5.can','p95.US','p5.US')

p3 <- ggplot(df.ams,aes(x = year, y = med.can))+geom_line(size = 2, color = cols[1])+
  geom_ribbon(data =df.ams, aes(ymin = p5.can, ymax = p95.can), linetype = 0, fill = alpha(cols[1], alpha = 0.2), color = cols[1])+
  geom_line(aes(y = med.US), size = 2, color = cols[2])+
  geom_ribbon(data =df.ams, aes(ymin = p5.US, ymax = p95.US), linetype = 0, fill = alpha(cols[2], alpha = 0.2), color = cols[2])+
  scale_y_continuous(name = 'Average age in survey')+
  facet_wrap(~run)+theme(legend.position = 'n')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
#  scale_fill_manual(values = alpha(cols, alpha = 0.2), name="fill")
p3

df.amc <- rbind(ls.HCR.plot[[3]][[3]], ls.JMC.plot[[3]][[3]],ls.real.plot[[3]][[3]],
                ls.move1.plot[[3]][[3]],ls.move2.plot[[3]][[3]], ls.move3.plot[[3]][[3]])

df.amc$run <- ordered(df.ams$run, levels = c("HCR", "JMC", "Realized",'Move 1','Move 2','Move 3'))

p4 <- ggplot(df.amc,aes(x = year, y = med.can))+geom_line(size = 2, color = cols[1])+
  geom_ribbon(aes(ymin = p5.can, ymax = p95.can), linetype = 0, fill = alpha(cols[1], alpha = 0.2), color = cols[1])+
  geom_line(aes(y = med.US), size = 2, color = cols[2])+
  geom_ribbon(data =df.ams, aes(ymin = p5.US, ymax = p95.US), linetype = 0, fill = alpha(cols[2], alpha = 0.2), color = cols[2])+
  scale_y_continuous(name = 'Average age in survey')+
  facet_wrap(~run)+theme(legend.position = 'n')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p4
