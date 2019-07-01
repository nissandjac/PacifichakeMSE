## Load MSE's and compare ## 

source('df_lists.R')
require(ggplot2)
require(dplyr)
require(scales)
require(RColorBrewer)
require(cowplot)
library(gridExtra)
source('load_files_OM.R')

load('results/MSErun_move_JMC.Rdata')
ls.JMC <- ls.save 
load('results/MSErun_move_JTC.Rdata')
ls.JTC <- ls.save
load('results/MSErun_move_realized.Rdata')
ls.Realized <- ls.save
#load('results/sim_data.Rdata')
load('results/MSErun_move_realized_move1.Rdata')
ls.move1 <- ls.save
load('results/MSErun_move_realized_move3.Rdata')
ls.move2 <- ls.save
#load('results/MSErun_move_realized_move3.Rdata')
ls.move3 <- ls.save
simyears <- 30
yr <- 1966:(2017+simyears-1)
nruns <- 100


df <- load_data_seasons(nseason = 4, nspace = 2) # Prepare data for operating model
sim.data <- run.agebased.true.catch(df)



source('hake_objectives.R')
obj.JMC <- hake_objectives(ls.JMC,sim.data$SSB0, move = 1, simyears = 30)
obj.JMC[[2]]$HCR <- 'JMC'

obj.JTC <- hake_objectives(ls.JTC,sim.data$SSB0, move = 1, simyears = 30)
obj.JTC[[2]]$HCR <- 'HCR'

obj.real <- hake_objectives(ls.Realized,sim.data, move = 1, simyears = 30) 
obj.real[[2]]$HCR <- 'real'

obj.move1 <- hake_objectives(ls.move1,sim.data, move = 1, simyears = 30) 
obj.move1[[2]]$HCR <- 'move1'

obj.move2 <- hake_objectives(ls.move2,sim.data, move = 1, simyears = 30) 

obj.move2[[2]]$HCR <- 'move2'

obj.move3 <- hake_objectives(ls.move3,sim.data, move = 1, simyears = 30)
obj.move3[[2]]$HCR <- 'move3'



df.obj <- data.frame(rbind(obj.JMC[[2]],obj.JTC[[2]],obj.real[[2]],
                           obj.move1[[2]],obj.move2[[2]],obj.move3[[2]]))
df.obj$HCR <- ordered(df.obj$HCR, levels = c("HCR", "JMC", "real",'move1','move2','move3'))
df.obj$indicator <- paste(rep(c('A) ','B) ','C) ','D) ','E) ','F) ','G) ','H) '),6),df.obj$indicator)

png('Figs/objective_bars_b0.png', width = 16, height =18, res = 400, unit = 'cm')
ggplot(df.obj, aes(x = HCR,y = value))+geom_bar(stat = 'identity', aes(fill = HCR))+facet_wrap(~indicator, scales = 'free', ncol = 2)+
  scale_x_discrete(name = '')+  scale_y_continuous(name = '')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = 'none')


dev.off()


### Plot the future spawning biomass 

ls.HCR.plot <- df_lists(ls.JTC[-1], 'HCR',simyears = 30)
ls.JMC.plot <- df_lists(ls.JMC, 'JMC',simyears = 30)
ls.real.plot <- df_lists(ls.Realized[-1], 'Realized',simyears = 30)
ls.move1.plot <- df_lists(ls.move1, 'Move 1',simyears = 30)
ls.move2.plot <- df_lists(ls.move2, 'Move 2',simyears = 30)
ls.move3.plot <- df_lists(ls.move3, 'Move 3',simyears = 30)

df.all <- rbind(ls.HCR.plot[[3]]$SSBplot, ls.JMC.plot[[3]]$SSBplot,ls.real.plot[[3]]$SSBplot,
                ls.move1.plot[[3]]$SSBplot,ls.move2.plot[[3]]$SSBplot, ls.move3.plot[[3]]$SSBplot
                )
df.all$run <- as.factor(df.all$run)
df.all$run <- ordered(df.all$run, levels = c("HCR", "JMC", "Realized",'Move 1','Move 2','Move 3'))

p1 <- ggplot(df.all, aes(x = year, y = med.can*1e-6))+geom_line(color = 'red')+
  geom_line(aes(y = med.US*1e-6), color = 'blue')+theme_classic()+scale_y_continuous(name ='SSB (million tonnes)')+facet_wrap(~run)+  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_ribbon(aes(ymin = p5.can*1e-6, ymax = p95.can*1e-6), fill = alpha('red', alpha = 0.2), linetype = 0)+
  geom_ribbon(aes(ymin = p5.US*1e-6, ymax = p95.US*1e-6), fill = alpha('blue', alpha = 0.2), linetype = 0)

p1

df.catch.1 <- rbind(ls.HCR.plot[[3]]$Catchplot, ls.JMC.plot[[3]]$Catchplot,ls.real.plot[[3]]$Catchplot)
df.catch.2 <- rbind(ls.move1.plot[[3]]$Catchplot,ls.move2.plot[[3]]$Catchplot, ls.move3.plot[[3]]$Catchplot)
df.catch.1$run <- ordered(df.catch.1$run, levels = c("HCR", "JMC", "Realized"))
df.catch.2$run <- ordered(df.catch.2$run, levels = c('Move 1','Move 2','Move 3'))

cols <- brewer.pal(6, 'Dark2')
p2.1 <- ggplot(df.catch.1, aes(x = year, y = med*1e-6))+geom_line(size = 2)+
  #geom_ribbon(aes(ymin = p5*1e-6, ymax = p95*1e-6), linetype = 2, fill = alpha(alpha =0.2, colour = cols))+
  scale_color_manual(values=cols[1:3])+scale_y_continuous(name = 'Catch (million tonnes)')+
  geom_line(aes(y = p5*1e-6, color = run), linetype = 2)+geom_line(aes(y = p95*1e-6, color = run), linetype = 2)+
  facet_wrap(~run)

p2.1


p2.2 <- ggplot(df.catch.2, aes(x = year, y = med*1e-6, color = run))+geom_line(size = 2)+
  #geom_ribbon(aes(ymin = p5*1e-6, ymax = p95*1e-6, color = run), linetype = 2, fill = NA)+
  scale_color_manual(values=cols[4:6])+scale_y_continuous(name = 'Catch (million tonnes)')+
  geom_line(aes(y = p5*1e-6, color = run), linetype = 2)+geom_line(aes(y = p95*1e-6, color = run), linetype = 2)

#  scale_fill_manual(values = alpha(cols, alpha = 0.2), name="fill")

png('Figs/catch_MSE_1_b0.png', width = 12, height =8, res = 400, unit = 'cm')

p2.1
dev.off()
png('Figs/catch_MSE_2_b0.png', width = 12, height =8, res = 400, unit = 'cm')

p2.2
dev.off()


df.ams.1 <- rbind(ls.HCR.plot[[3]]$amsplot, ls.JMC.plot[[3]]$amsplot,ls.real.plot[[3]]$amsplot)
df.ams.2 <- rbind(ls.move1.plot[[3]]$amsplot,ls.move2.plot[[3]]$amsplot, ls.move3.plot[[3]]$amsplot)
df.ams.1$run <- ordered(df.ams.1$run, levels = c("HCR", "JMC", "Realized"))
df.ams.2$run <- ordered(df.ams.2$run, levels = c('Move 1','Move 2','Move 3'))

rm.idx.1 <- which(is.na(df.ams.1$med))
rm.idx.2 <- which(is.na(df.ams.2$med))

p3.1 <- ggplot(df.ams.1[-rm.idx.1,], aes(x = year, y = med, color = run))+geom_line(size = 2)+
#  geom_ribbon(aes(ymin = p5, ymax = p95, color = run), linetype = 2, fill = NA)+
  scale_color_manual(values=cols)+scale_y_continuous(name = 'Average age in survey')+
  geom_line(aes(y = p5, color = run), linetype = 2)+geom_line(aes(y = p95, color = run), linetype = 2)

#  scale_fill_manual(values = alpha(cols, alpha = 0.2), name="fill")
png('Figs/age_s_move1_b0.png', width = 12, height =8, res = 400, unit = 'cm')
p3.1
dev.off()

p3.2 <- ggplot(df.ams.2[-rm.idx.2,], aes(x = year, y = med, color = run))+geom_line(size = 2)+
#  geom_ribbon(aes(ymin = p5, ymax = p95, color = run), linetype = 2, fill = NA)+
  scale_color_manual(values=cols)+scale_y_continuous(name = 'Average age in survey')+
  geom_line(aes(y = p5, color = run), linetype = 2)+geom_line(aes(y = p95, color = run), linetype = 2)
#  scale_fill_manual(values = alpha(cols, alpha = 0.2), name="fill")
png('Figs/age_s_move2_b0.png', width = 12, height =8, res = 400, unit = 'cm')
p3.2
dev.off()

df.amc.1 <- rbind(ls.HCR.plot[[3]]$amcplot, ls.JMC.plot[[3]]$amcplot,ls.real.plot[[3]]$amcplot)
df.amc.2 <-rbind(ls.move1.plot[[3]]$amcplot,ls.move2.plot[[3]]$amcplot, ls.move3.plot[[3]]$amcplot)

df.amc.1$run.1 <- ordered(df.amc.1$run, levels = c("HCR", "JMC", "Realized"))
df.amc.2$run.2 <- ordered(df.amc.1$run, levels = c('Move 1','Move 2','Move 3'))

p4.1 <- ggplot(df.amc.1, aes(x = year, y = med, color = run))+geom_line(size = 2)+
#  geom_ribbon(aes(ymin = p5, ymax = p95, color = run), linetype = 2, fill = NA)+
  scale_color_manual(values=cols)+scale_y_continuous(name = 'Average age in catch')+
  geom_line(aes(y = p5, color = run), linetype = 2)+geom_line(aes(y = p95, color = run), linetype = 2)
#  scale_fill_manual(values = alpha(cols, alpha = 0.2), name="fill")
png('Figs/age_c_move1_b0.png', width = 12, height =8, res = 400, unit = 'cm')
p4.1
dev.off()

p4.2 <- ggplot(df.amc.2, aes(x = year, y = med, color = run))+geom_line(size = 2)+
#  geom_ribbon(aes(ymin = p5, ymax = p95, color = run), linetype = 2, fill = NA)+
  scale_color_manual(values=cols)+scale_y_continuous(name = 'Average age in catch')+
  geom_line(aes(y = p5, color = run), linetype = 2)+geom_line(aes(y = p95, color = run), linetype = 2)
#  scale_fill_manual(values = alpha(cols, alpha = 0.2), name="fill")
png('Figs/age_c_move2_b0.png', width = 12, height =8, res = 400, unit = 'cm')
p4.2
dev.off()


### SSB in the middle of the year 
df.all.mid <- rbind(ls.HCR.plot[[3]]$SSBmid, ls.JMC.plot[[3]]$SSBmid,ls.real.plot[[3]]$SSBmid,
                ls.move1.plot[[3]]$SSBmid,ls.move2.plot[[3]]$SSBmid, ls.move3.plot[[3]]$SSBmid)
df.all.mid$run <- ordered(df.all.mid$run, levels = c("HCR", "JMC", "Realized",'Move 1','Move 2','Move 3'))


p5 <- ggplot(df.all.mid, aes(x = year, y = med.can*1e-6))+geom_line(color = 'red')+
  geom_line(aes(y = med.US*1e-6), color = 'blue')+
  theme_classic()+scale_y_continuous(name ='SSB (m tonnes)\nmidyear')+facet_wrap(~run, scale = 'free')+  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_ribbon(aes(ymin = p5.can*1e-6, ymax = p95.can*1e-6), fill = alpha('red', alpha = 0.2), linetype = 0)+
  geom_ribbon(aes(ymin = p5.US*1e-6, ymax = p95.US*1e-6), fill = alpha('blue', alpha = 0.2), linetype = 0)

p5


df.ams.space <- rbind(ls.HCR.plot[[3]]$ams.space, ls.JMC.plot[[3]]$ams.space,ls.real.plot[[3]]$ams.space,
                ls.move1.plot[[3]]$ams.space,ls.move2.plot[[3]]$ams.space, ls.move3.plot[[3]]$ams.space)

# df.ams2 <- melt(df.ams, id.vars = c(1,8))
df.ams.space$run <- ordered(df.ams$run, levels = c("HCR", "JMC", "Realized",'Move 1','Move 2','Move 3'))
# rbs <- c('p95.can','p5.can','p95.US','p5.US')

cols <- c('darkred', 'blue4')

p6 <- ggplot(df.ams.space,aes(x = year, y = med.can))+geom_line(size = 2, color = cols[1])+
  geom_ribbon(aes(ymin = p5.can, ymax = p95.can), linetype = 0, fill = alpha(cols[1], alpha = 0.2), color = cols[1])+
  geom_line(aes(y = med.us), size = 2, color = cols[2])+
  geom_ribbon(aes(ymin = p5.us, ymax = p95.us), linetype = 0, fill = alpha(cols[2], alpha = 0.2), color = cols[2])+
  scale_y_continuous(name = 'Average age in survey')+
  facet_wrap(~run)+theme(legend.position = 'n')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#  scale_fill_manual(values = alpha(cols, alpha = 0.2), name="fill")
png('Figs/age_s_space_b0.png', width = 12, height =8, res = 400, unit = 'cm')
p6
dev.off()

df.amc.space <- rbind(ls.HCR.plot[[3]]$amc.space, ls.JMC.plot[[3]]$amc.space,ls.real.plot[[3]]$amc.space,
                      ls.move1.plot[[3]]$amc.space,ls.move2.plot[[3]]$amc.space, ls.move3.plot[[3]]$amc.space)

# df.amc2 <- melt(df.amc, id.vars = c(1,8))
df.amc.space$run <- ordered(df.amc.space$run, levels = c("HCR", "JMC", "Realized",'Move 1','Move 2','Move 3'))
# rbs <- c('p95.can','p5.can','p95.US','p5.US')

p7 <- ggplot(df.amc.space,aes(x = year, y = med.can))+geom_line(size = 2, color = cols[1])+
  geom_ribbon(aes(ymin = p5.can, ymax = p95.can), linetype = 0, fill = alpha(cols[1], alpha = 0.2), color = cols[1])+
  geom_line(aes(y = med.us), size = 2, color = cols[2])+
  geom_ribbon(aes(ymin = p5.us, ymax = p95.us), linetype = 0, fill = alpha(cols[2], alpha = 0.2), color = cols[2])+
  scale_y_continuous(name = 'Average age in catch')+
  facet_wrap(~run)+theme(legend.position = 'n')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#  scale_fill_manual(values = alpha(cols, alpha = 0.2), name="fill")
png('Figs/age_c_space_b0.png', width = 12, height =8, res = 400, unit = 'cm')

p7
dev.off()


# How accurate is the SSB estimation 
source('calcSE.R')

# Rebuild into nice figs 
qfunc <- function(df, name){
  
  
  df.quant <- df %>% 
    group_by(year) %>% 
    summarise(E5 = median(SE.SSB, na.rm = TRUE), 
              E95 = quantile(SE.SSB,0.95, na.rm = TRUE),
              E05 = quantile(SE.SSB,0.05, na.rm = TRUE)
              )
  df.quant$name <- name
  return(df.quant)
}

HCR.E <- qfunc(calcSE(ls.JTC),'HCR')
JMC.E <- qfunc(calcSE(ls.JMC),'JMC')
real.E <- qfunc(calcSE(ls.Realized),'Real')
move1.E <- qfunc(calcSE(ls.move1),'Move 1')
move2.E <- qfunc(calcSE(ls.move2),'Move 2')
move3.E <- qfunc(calcSE(ls.move3),'Move 3')

ls.plot <- rbind(HCR.E,JMC.E,real.E,move1.E,move2.E,move3.E)
ls.plot$name <- ordered(ls.plot$name, levels = c("HCR", "JMC", "Real",'Move 1','Move 2','Move 3'))

p8  <- ggplot(ls.plot, aes(x = year, y = E5))+theme_classic()+
  geom_line()+facet_wrap(~name)+geom_hline(yintercept = 0.0, linetype = 2)+
  geom_ribbon(aes(ymin =E05, ymax = E95), fill = alpha('gray', alpha = 0.5))+
  scale_y_continuous(limit = c(-0.5,0.5), name = 'Standard error')

png('Figs/SE_SSB.png', width = 12, height =8, res = 400, unit = 'cm')

p8
dev.off()


## Plot the fishing mortality rates 


df.all <- ls.HCR.plot[[1]]
df.all <- df.all[df.all$year>2017,]

p9 <- ggplot(ls.HCR.plot[[3]]$SSBtot, aes(x = year, y = med/sum(sim.data$SSB0)))+geom_line(size= 1.2)+
  geom_line(aes(y = avg/sum(sim.data$SSB0)), linetype = 2, size = 1.1)+
  geom_ribbon(aes(ymin = p5/sum(sim.data$SSB0),ymax = p95/sum(sim.data$SSB0)),
              fill = alpha(colour = 'darkred', alpha = 0.2))+
  geom_line(data = df.all[1:180,], aes(x = year, y= SSBtot/sum(sim.data$SSB0), group = run), color = alpha('black', 0.9))+
  scale_y_continuous('SSB/SSB0')+coord_cartesian(ylim = c(0,1.2))


png('Figs/SSB.png', width = 12, height =8, res = 400, unit = 'cm')
p9
dev.off()

load('Figs/MSErun_move_JTC.Rdata')

df.plot <- df_lists(ls.save, 'HCR')

df.all <- df.plot[[1]]
df.F0 <- df.plot[[3]]$F0

p9 <- ggplot(df.F0, aes(x = year, y = med.can))+geom_line(size= 1.2, col = 'darkred')+
  geom_line(aes(y = med.us), linetype = 1, size = 1.1, col = 'blue4')+
  geom_ribbon(aes(ymax = p95.us, ymin = p5.us), fill = alpha('blue', alpha = 0.2))+
  geom_ribbon(aes(ymax = p95.can, ymin = p5.can),fill = alpha('red', alpha = 0.2))+
  scale_y_continuous('F0')+coord_cartesian(ylim = c(0,0.7), xlim = c(2017,2050))

p9

png('Figs/F0.png', width = 12, height =8, res = 400, unit = 'cm')
p9
dev.off()
# 
#   geom_line(aes(y = p95.can), col = 'darkred', linetype = 2)+  
#   geom_line(aes(y = p5.can), col = 'darkred', linetype = 2)+
#   geom_line(aes(y = p95.us), col = 'blue4', linetype = 2)+  
#   geom_line(aes(y = p5.us), col = 'blue4', linetype = 2)+