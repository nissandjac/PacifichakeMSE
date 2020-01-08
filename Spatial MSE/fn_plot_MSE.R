
fn_plot_MSE <- function(ls, sim.data, plotfolder = '/Figs/newplots/', plotexp = TRUE, pidx = NA){

  
# Do some calculations for survey and catch age comps 

  
  
nms <- names(ls)  

if(is.na(pidx)){
  pidx <- 1:length(nms)
}

obj.plot <- list()
#cols <- brewer.pal(6, 'Dark2')
cols <- PNWColors::pnw_palette('Starfish',n = length(nms), type = 'discrete')


for(i in 1:length(nms)){
  

  obj.plot[[i]] <- hake_objectives(ls[[i]],sim.data$SSB0, move = 1)
  obj.plot[[i]][[2]]$HCR <- nms[i]

}


df.obj <- data.frame(obj.plot[[1]][[2]])


for(i in 2:length(nms)){
  df.obj <- rbind(df.obj, obj.plot[[i]][[2]])
}
 
indicators <- unique(df.obj$indicator)

df.obj2 <- df.obj %>%
  filter(indicator %in% indicators[1:6])  
# df.obj2$indicator=factor(df.obj2$indicator, 
#                          levels=c('SSB <0.10 SSB0',
#                                   'S>0.10<0.4S0',
#                                   'S>0.4S0',
#                                   'AAV',
#                                   'short term catch',
#                                   'long term catch'))

df.sp <- df.obj %>%
  filter(indicator %in% indicators[7:12])

# df.sp$indicator=factor(df.sp$indicator,
#                        levels=c('Canada TAC/V spr', 
#                                 'Canada TAC/V sum', 
#                                 'Canada TAC/V fall',
#                                 'US TAC/V spr', 
#                                 'US TAC/V sum', 
#                                 'US TAC/V fall'))

df.sp$country=c(rep('Canada',3), rep('US',3))
df.sp$season=c(rep(c('Apr-Jun','July-Sept','Oct-Dec')))

# Fix the y scales 
dummy <- data.frame(indicator = 'long term catch', value = c(0.2,0.3), HCR = 'ymin')


# for(i in 1:length(indicators)){
#   df.obj2[df.obj2$HCR != 'move_0' & df.obj2$indicator == indicators[i],]$value <-
#     df.obj2[df.obj2$HCR != 'move_0' & df.obj2$indicator == indicators[i],]$value/
#     df.obj2[df.obj2$HCR == 'move_0' & df.obj2$indicator == indicators[i],]$value-1
#   
# }

#df.obj2$value[df.obj2$HCR == 'move_0'] <- 0
df.obj2$HCR <- factor(df.obj2$HCR, levels = names(ls)[pidx])

#df.obj2$ind2=factor(df.obj2$indicator, as.character(df.obj2$indicator))

p1 <- ggplot(df.obj2, aes(x = HCR,y = value))+geom_bar(stat = 'identity', aes(fill = HCR))+
  scale_x_discrete(name = '')+  
  scale_y_continuous(name = '')+
  scale_fill_manual(values = cols[1:length(unique(df.obj$HCR))])+
  facet_wrap(~indicator, scales = 'free_y', ncol = 3)+
#  geom_blank(data = dummy)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.position = 'none')
p1


p1.sp<- ggplot(df.sp, aes(x = HCR,y = value, factor=season))+
  geom_bar(stat = 'identity', aes(fill = season), position="dodge2")+
  scale_x_discrete(name = '')+  
  scale_y_continuous(name = '')+
  scale_fill_manual(values = cols[1:length(unique(df.obj$HCR))])+
  facet_wrap(~country, scales = 'fixed', ncol = 2, dir='v')+
  #  geom_blank(data = dummy)+
  #geom_hline(yintercept=c(.045, .07, 0.12))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p1.sp


if(plotexp == TRUE){
png(paste(plotfolder,'objective_bars.png', sep = ''), width = 20, height =20, res = 400, unit = 'cm')
print(p1)
dev.off()

png(paste(plotfolder,'sp_objective_bars.png'), width = 20, height =20, res = 400, unit = 'cm')
print(p1.sp)
dev.off()
}  

# Do violin plots as well
source('hake_violin.R')

obj.plot.v <- hake_violin(ls[[1]],sim.data$SSB0, move = 1)
obj.plot.v$HCR <- nms[1]

for(i in 2:length(nms)){
  df.tmp <- hake_violin(ls[[i]],sim.data$SSB0, move = 1)
  df.tmp$HCR <- nms[i]
  obj.plot.v <- rbind(obj.plot.v, df.tmp)
}

# Save the violin data to run in a seperate file 
obj.plot.v$HCR <- factor(obj.plot.v$HCR, levels = nms)

save(obj.plot.v,file = paste(results,'violindata.Rdata', sep =''))

source('plotViolin.R')
p.vio <- plotViolin(paste(results,'violindata.Rdata', sep = ''))
# cols <- PNWColors::pnw_palette('Starfish',n = length(nms), type = 'discrete')
# 
# 
# ## Do some adjustments to fix the scales 
# 
# p.v <- ggplot(obj.plot.v, aes(x = HCR, y = value, fill = HCR))+
#   geom_violin()+
#   geom_boxplot(width=0.15, col = 'black', outlier.shape = NA)+
#   scale_fill_manual(values = cols)+
#   facet_wrap(~variable, scales = 'free', ncol = 3, dir='v')+
#   theme(legend.position = 'none',
#         axis.text.x = element_text(angle = 60, vjust = 0.5))+
#   scale_x_discrete(name = '')+  
#   scale_y_continuous(name = '')#+
#   #coord_cartesian(ylim = c(0,1))

if(plotexp == TRUE){
  png(paste(plotfolder,'objective_violin.png'), width = 15, height =12, res = 400, unit = 'cm')
  print(p.vio)
  dev.off()

}

# Redo this plot without facet wrap



ls.data <- list()


for(i in 1:length(nms)){
 ls.data[[i]] <- df_lists(ls[[i]], nms[i]) 

 }

df.all <- data.frame(ls.data[[1]][[3]]$SSBplot)
df.catch <- data.frame(ls.data[[1]][[3]]$Catchplot)

for(i in 2:length(nms)){
  df.all <- rbind(df.all, ls.data[[i]][[3]]$SSBplot)
  df.catch <- rbind(df.catch, ls.data[[i]][[3]]$Catchplot)
}


df.all$run <- factor(df.all$run, levels = nms[pidx])
df.catch$run <- factor(df.catch$run, levels = nms[pidx])

p2 <- ggplot(df.all, aes(x = year, y = med.can*1e-6))+geom_line(color = 'darkred', size = 1.5)+
  geom_line(aes(y = med.US*1e-6), color = 'darkblue', size = 1.5)+theme_classic()+scale_y_continuous(name ='SSB (million tonnes)')+facet_wrap(~run)+  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_ribbon(aes(ymin = p5.can*1e-6, ymax = p95.can*1e-6), fill = alpha('red', alpha = 0.2), linetype = 0)+
  geom_ribbon(aes(ymin = p5.US*1e-6, ymax = p95.US*1e-6), fill = alpha('blue', alpha = 0.2), linetype = 0)

p2

if(plotexp == TRUE){
  png(paste(plotfolder,'SSB_country.png'), width = 16, height =18, res = 400, unit = 'cm')
  print(p2)
  dev.off()
}  


#yl <- 


p3 <- ggplot(df.catch, aes(x = year, y = med*1e-6, color = run))+geom_line(size = 1.5)+
#  geom_ribbon(aes(ymin = p5*1e-6, ymax = p95*1e-6), linetype = 2, fill = alpha(alpha =0.2, colour = cols))+
  scale_color_manual(values=cols[1:length(unique(df.catch$run))])+scale_y_continuous(name = 'Catch (million tonnes)')+
  geom_line(aes(y = p5*1e-6, color = run), linetype = 2)+geom_line(aes(y = p95*1e-6, color = run), linetype = 2)+
  coord_cartesian(ylim = c(0,1.5))

p3

if(plotexp == TRUE){
  png(paste(plotfolder,'total_catch.png'), width = 16, height =12, res = 400, unit = 'cm')
  print(p3)
  dev.off()
}  
# 
df.ams <- data.frame(ls.data[[1]][[3]]$amsplot)
df.amc <- data.frame(ls.data[[1]][[3]]$amcplot)


for(i in 2:length(nms)){
  df.ams <- rbind(df.ams, ls.data[[i]][[3]]$amsplot)
  df.amc <- rbind(df.amc, ls.data[[i]][[3]]$amcplot)
}

df.ams$run <- factor(df.ams$run, levels = nms[pidx])
df.amc$run <- factor(df.amc$run, levels = nms[pidx])

rm.idx <- which(is.na(df.ams$med)) # remove years with no measurement

p4 <- ggplot(df.ams[-rm.idx,], aes(x = year, y = med, color = run))+geom_line(size = 2)+
  #  geom_ribbon(aes(ymin = p5, ymax = p95, color = run), linetype = 2, fill = NA)+
  scale_color_manual(values=cols)+scale_y_continuous(name = 'Average age in survey')+
  geom_line(aes(y = p5, color = run), linetype = 2)+geom_line(aes(y = p95, color = run), linetype = 2)+
  theme(legend.title = element_blank())

if(plotexp == TRUE){
  png(paste(plotfolder,'total_average_age_surv.png'), width = 12, height =8, res = 400, unit = 'cm')
  print(p4)
  dev.off()
}



p5 <- ggplot(df.amc, aes(x = year, y = med, color = run))+geom_line(size = 2)+
  #  geom_ribbon(aes(ymin = p5, ymax = p95, color = run), linetype = 2, fill = NA)+
  scale_color_manual(values=cols)+scale_y_continuous(name = 'Average age in catch')+
  geom_line(aes(y = p5, color = run), linetype = 2)+geom_line(aes(y = p95, color = run), linetype = 2)
#  scale_fill_manual(values = alpha(cols, alpha = 0.2), name="fill")
if(plotexp == TRUE){
  png(paste(plotfolder,'total_average_age_catch.png'), width = 12, height =16, res = 400, unit = 'cm')
  print(p5)
  dev.off()
}

if(plotexp == TRUE){
  
  p4 <- ggplot(df.ams[-rm.idx,], aes(x = year, y = med, color = run))+geom_line(size = 2)+
    #  geom_ribbon(aes(ymin = p5, ymax = p95, color = run), linetype = 2, fill = NA)+
    scale_color_manual(values=cols)+scale_y_continuous(name = 'Average age\n in survey')+
    geom_line(aes(y = p5, color = run), linetype = 2)+geom_line(aes(y = p95, color = run), linetype = 2)+
    theme(legend.position = c(0.1,0.9),
          legend.title = element_blank())+
    coord_cartesian(ylim = c(2.5,10))
  
  
  p5 <- ggplot(df.amc, aes(x = year, y = med, color = run))+geom_line(size = 2)+
    #  geom_ribbon(aes(ymin = p5, ymax = p95, color = run), linetype = 2, fill = NA)+
    scale_color_manual(values=cols)+scale_y_continuous(name = 'Average age\n in catch')+
    geom_line(aes(y = p5, color = run), linetype = 2)+geom_line(aes(y = p95, color = run), linetype = 2)+
    theme(legend.position = 'none')+
    coord_cartesian(ylim = c(2.5,10))
  
  
  
  png(paste(plotfolder,'total_average_ags.png'), width = 16, height =10, res = 400, unit = 'cm')
  print(cowplot::plot_grid(plotlist = list(p4,p5), ncol = 2, labels = c('a','b')))
  dev.off()
}


df.SSB <- data.frame(ls.data[[1]][[3]]$SSBmid)


for(i in 2:length(nms)){
  df.SSB <- rbind(df.SSB, ls.data[[i]][[3]]$SSBmid)
}


### SSB in the middle of the year 
df.SSB$run <- factor(df.SSB$run, levels = nms[pidx])



p6 <- ggplot(df.SSB, aes(x = year, y = med.can*1e-6))+geom_line(color = 'red', size = 1.2)+
  geom_line(aes(y = med.US*1e-6), color = 'blue', size = 1.2)+
  theme_classic()+scale_y_continuous(name ='SSB (m tonnes)\nmidyear')+facet_wrap(~run, scale = 'free')+  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_ribbon(aes(ymin = p5.can*1e-6, ymax = p95.can*1e-6), fill = alpha('red', alpha = 0.2), linetype = 0)+
  geom_ribbon(aes(ymin = p5.US*1e-6, ymax = p95.US*1e-6), fill = alpha('blue', alpha = 0.2), linetype = 0)

p6


if(plotexp == TRUE){
  png(paste(plotfolder,'SSB_mid_year.png'), width = 16, height =10, res = 400, unit = 'cm')
  print(p6)
  dev.off()
}  
# 
df.ams.space <- data.frame(ls.data[[1]][[3]]$ams.space)

for(i in 2:length(nms)){
  df.ams.space <- rbind(df.ams.space, ls.data[[i]][[3]]$ams.space)
}

df.ams.space$run <- factor(df.ams.space$run, levels = nms[pidx])

cols_c <- c('darkred', 'blue4')

p7 <- ggplot(df.ams.space,aes(x = year, y = med.can))+geom_line(size = 2, color = cols_c[1])+
  geom_ribbon(aes(ymin = p5.can, ymax = p95.can), linetype = 0, fill = alpha(cols_c[1], alpha = 0.2), color = cols_c[1])+
  geom_line(aes(y = med.us), size = 2, color = cols_c[2])+
  geom_ribbon(aes(ymin = p5.us, ymax = p95.us), linetype = 0, fill = alpha(cols_c[2], alpha = 0.2), color = cols_c[2])+
  scale_y_continuous(name = 'Average age in survey')+
  facet_wrap(~run)+theme(legend.position = 'n')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#  scale_fill_manual(values = alpha(cols, alpha = 0.2), name="fill")
if(plotexp == TRUE){
  png(paste(plotfolder,'average_age_survey_country.png'), width = 12, height =16, res = 400, unit = 'cm')
  print(p7)
  dev.off()
}
# 
# # 
df.amc.space <- data.frame(ls.data[[1]][[3]]$amc.space)

for(i in 2:length(nms)){
  df.amc.space <- rbind(df.amc.space, ls.data[[i]][[3]]$amc.space)
}

df.amc.space$run <- factor(df.amc.space$run, levels = nms[pidx])

p8 <- ggplot(df.amc.space[df.amc.space$year > 2014,],aes(x = year, y = med.can))+geom_line(size = 2, color = cols_c[1])+
  geom_ribbon(aes(ymin = p5.can, ymax = p95.can), linetype = 0, fill = alpha(cols_c[1], alpha = 0.2), color = cols_c[1])+
  geom_line(aes(y = med.us), size = 2, color = cols_c[2])+
  geom_ribbon(aes(ymin = p5.us, ymax = p95.us), linetype = 0, fill = alpha(cols_c[2], alpha = 0.2), color = cols_c[2])+
  scale_y_continuous(name = 'Average age in catch')+
  facet_wrap(~run)+theme(legend.position = 'n')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        strip.background =element_rect(fill="white"))

p8
# 
if(plotexp == TRUE){
  png(paste(plotfolder,'average_age_catch_country.png'), width = 12, height =8, res = 400, unit = 'cm')
  print(p8)
  dev.off()
}

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

se.plot <- data.frame(qfunc(calcSE(ls[[1]]),nms[1]))
for(i in 1:length(nms)){
  se.plot <- rbind(se.plot, data.frame(qfunc(calcSE(ls[[i]]),nms[i])))
}

p9  <- ggplot(se.plot[se.plot$year > 2014,], aes(x = year, y = E5))+theme_classic()+
  geom_line(size = 1.5)+facet_wrap(~name)+geom_hline(yintercept = 0.0, linetype = 2)+
  geom_ribbon(aes(ymin =E05, ymax = E95), fill = alpha('gray', alpha = 0.5))+
  scale_y_continuous(name = 'Standard error')+
  coord_cartesian(ylim = c(-0.5,0.5))

if(plotexp == TRUE){
  png(paste(plotfolder,'SE_SSB.png'), width = 12, height =8, res = 400, unit = 'cm')
  print(p9)
  dev.off()
}  

if(plotexp == FALSE){
  #grid.arrange(p3,p6,p7,p9)
  print(p1)
}


# Plot Fishing mortality 
df.F0 <- data.frame(ls.data[[1]][[3]]$F0)

  # if(all(is.na(df.F0))) == 0){
  #   for(i in 2:length(nms)){
  #     df.F0 <- rbind(df.F0, ls.data[[i]][[3]]$F0)
  #   }
  #   
    
    ### SSB in the middle of the year 
df.F0$run <- factor(df.F0$run, levels = nms[pidx])    

    p10 <- ggplot(df.F0, aes(x = year, y = med.can))+geom_line(color = 'red')+
      geom_line(aes(y = med.us), color = 'blue')+coord_cartesian(ylim = c(0,1))+
      theme_classic()+scale_y_continuous(name ='Exploitation rate')+facet_wrap(~run)+  
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
      geom_ribbon(aes(ymin = p5.can, ymax = p95.can), fill = alpha('red', alpha = 0.2), linetype = 0)+
      geom_ribbon(aes(ymin = p5.us, ymax = p95.us), fill = alpha('blue', alpha = 0.2), linetype = 0)
    
    p10
    
    if(plotexp == TRUE){
      png(paste(plotfolder,'F0.png'), width = 12, height =16, res = 400, unit = 'cm')
      print(p10)
      dev.off()
    }  
 # }
# Plot realized catch
df.catchq<- data.frame(ls.data[[1]][[3]]$Catch.q)


for(i in 2:length(nms)){
  df.catchq <- rbind(df.catchq, ls.data[[i]][[3]]$Catch.q)
}
df.catchq$run <- factor(df.catchq$run, levels = nms[pidx])

p11 <- ggplot(df.catchq, aes(x = year, y = med.can))+geom_line(color = 'red')+
  geom_line(aes(y = med.us), color = 'blue')+
  theme_classic()+scale_y_continuous(name ='Catch/quota')+facet_wrap(~run)+  
  coord_cartesian(ylim = c(0.6,1.1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_ribbon(aes(ymin = p5.can, ymax = p95.can), fill = alpha('red', alpha = 0.2), linetype = 0)+
  geom_ribbon(aes(ymin = p5.us, ymax = p95.us), fill = alpha('blue', alpha = 0.2), linetype = 0)+
  geom_hline(aes(yintercept = 1), color = 'black', linetype = 2)

p11

if(plotexp == TRUE){
  png(paste(plotfolder,'Realized_catch.png'), width = 12, height =16, res = 400, unit = 'cm')
  print(p11)
  dev.off()
}  



df.SSB <- data.frame(ls.data[[1]][[3]]$SSBtot)


for(i in 2:length(nms)){
  df.SSB <- rbind(df.SSB, ls.data[[i]][[3]]$SSBtot)
}


### SSB in the middle of the year 
df.SSB$med <- df.SSB$med/sum(sim.data$SSB0)
df.SSB$p5 <- df.SSB$p5/sum(sim.data$SSB0)
df.SSB$p95 <- df.SSB$p95/sum(sim.data$SSB0)
df.SSB$run <-  factor(df.SSB$run, levels = nms[pidx])

p12<- ggplot(df.SSB, aes(x = year, y = med, color = run, fill = run))+
  geom_line(size = 1.5)+
  geom_line(aes(y = p95), linetype =2, size = 1.2)+
  geom_line(aes(y = p5), linetype =2, size = 1.2)+
  scale_color_manual(values= cols[1:length(nms)])+
  theme_classic()+scale_y_continuous(name ='Total SSB/SSB0')+
  coord_cartesian(ylim  = c(0,2.5))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  #geom_ribbon(aes(ymin = p5, ymax = p95), linetype = 0)+
  scale_fill_manual(values= alpha(cols[1:length(nms)], alpha = 0.2))+
  geom_hline(aes(yintercept = 1), color = 'black', linetype = 2)

p12


if(plotexp == TRUE){
  png(paste(plotfolder,'SSB_total.png'), width = 16, height =12, res = 400, unit = 'cm')
  print(p12)
  dev.off()
}  


p13 <- ggplot(df.catchq, aes(x = year, y = med.tot, color = run))+geom_line(size = 1.4)+
  scale_color_manual(values = cols[1:length(nms)])+
  geom_line(aes(y = p5.tot), linetype =2,size = 0.9)+
  geom_line(aes(y = p95.tot), linetype =2,size = 0.9)+
  theme_classic()+scale_y_continuous(name ='Catch/quota')+
  geom_hline(aes(yintercept = 1), color = 'black', linetype = 2)+coord_cartesian(ylim = c(0.4,1.05))+
  theme(legend.position = c(0.2,0.4),
        legend.title = element_blank())

p13

if(plotexp == TRUE){
  png(paste(plotfolder,'Catch_quota_tot.png'), width = 16, height =8, res = 400, unit = 'cm')
  print(p13)
  dev.off()
}  
print(p13)


}
