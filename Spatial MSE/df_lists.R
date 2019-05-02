df_lists <- function(ls.save, nms, simyears){
source('calcMeanAge.R')
yr <- 1966:(2017+simyears-1)
  
nruns <- length(ls.save)
nfailed <- rep(1, nruns)

for(i in 1:nruns){
  if(length(ls.save[[i]]) > 1){
    
  if(i == 1){
    ls.df <- data.frame(year = yr, SSB.can = ls.save[[i]]$SSB[,1], SSB.US = ls.save[[i]]$SSB[,2],
                         SSBtot =  ls.save[[i]]$SSB[,1]+ls.save[[i]]$SSB[,2],
                         # F0.can = rowSums(ls.save[[i]]$F0[,,1], na.rm = TRUE),
                         # F0.us = rowSums(ls.save[[i]]$F0[,,2], na.rm = TRUE),
                         amc = ls.save[[i]]$amc$amc.tot, 
                         ams = ls.save[[i]]$ams$ams.tot, 
                         amc.can = ls.save[[i]]$amc$amc.can,
                         amc.us = ls.save[[i]]$amc$amc.US,
                         ams.can = ls.save[[i]]$ams$ams.can,
                         ams.us = ls.save[[i]]$ams$ams.US,
                         SSB.mid.can = ls.save[[i]]$SSB.mid[,1],
                         SSB.mid.us = ls.save[[i]]$SSB.mid[,2],
                         run = paste('run',i, sep = '-'),
                         Catch = ls.save[[i]]$Catch)
  }else{
    
      ls.tmp <- data.frame(year = yr, SSB.can = ls.save[[i]]$SSB[,1], SSB.US = ls.save[[i]]$SSB[,2],
                           SSBtot =  ls.save[[i]]$SSB[,1]+ls.save[[i]]$SSB[,2],
                           # F0.can = rowSums(ls.save[[i]]$F0[,,1], na.rm = TRUE),
                           # F0.us = rowSums(ls.save[[i]]$F0[,,2], na.rm = TRUE),
                           amc = ls.save[[i]]$amc$amc.tot, 
                           ams = ls.save[[i]]$ams$ams.tot, 
                           amc.can = ls.save[[i]]$amc$amc.can,
                           amc.us = ls.save[[i]]$amc$amc.US,
                           ams.can = ls.save[[i]]$ams$ams.can,
                           ams.us = ls.save[[i]]$ams$ams.US,
                           SSB.mid.can = ls.save[[i]]$SSB.mid[,1],
                           SSB.mid.us = ls.save[[i]]$SSB.mid[,2],
                           run = paste('run',i, sep = '-'),
                           Catch = ls.save[[i]]$Catch)
      ls.df <- rbind(ls.df, ls.tmp)}
    
  }
  else{
    nfailed[i] <- 0
  }
}

SSB.plotquant <- ls.df[ls.df$year > 2010,] %>% 
  group_by(year) %>% 
  summarise(med.can = median(SSB.can), 
            p95.can = quantile(SSB.can, 0.95),
            p5.can = quantile(SSB.can,0.05),
            med.US = median(SSB.US), 
            p95.US = quantile(SSB.US, 0.95),
            p5.US = quantile(SSB.US,0.05)) 
SSB.plotquant$run <- nms

SSB.plottot <- ls.df[ls.df$year > 2010,] %>% 
  group_by(year) %>% 
  summarise(med = median(SSBtot),
            avg = mean(SSBtot),
            p95 = quantile(SSBtot, 0.95),
            p5 = quantile(SSBtot,0.05)) 
SSB.plottot$run <- nms



SSB.plotmid <- ls.df[ls.df$year > 2010,] %>% 
  group_by(year) %>% 
  summarise(med.can = median(SSB.mid.can), 
            p95.can = quantile(SSB.mid.can, 0.95),
            p5.can = quantile(SSB.mid.can,0.05),
            med.US = median(SSB.mid.us), 
            p95.US = quantile(SSB.mid.us, 0.95),
            p5.US = quantile(SSB.mid.us,0.05)) 
SSB.plotmid$run <- nms


Catch.plotquant <- ls.df[ls.df$year > 2010,] %>% 
  group_by(year) %>% 
  summarise(med = median(Catch), 
            p95 = quantile(Catch, 0.95),
            p5 = quantile(Catch,0.05)
            ) 
Catch.plotquant$run <- nms

ams.plotquant <- ls.df[ls.df$year > 2010,] %>% 
  group_by(year) %>% 
  summarise(med = median(ams,na.rm = TRUE), 
            p95 = quantile(ams, 0.95,na.rm = TRUE),
            p5 = quantile(ams,0.05,na.rm = TRUE)
  ) 
ams.plotquant$run <- nms

amc.plotquant <- ls.df[ls.df$year > 2010,] %>% 
  group_by(year) %>% 
  summarise(med= median(amc,na.rm = TRUE), 
            p95 = quantile(amc, 0.95,na.rm = TRUE),
            p5 = quantile(amc,0.05,na.rm = TRUE)
  ) 
amc.plotquant$run <- nms

ams.space <- ls.df[ls.df$year > 2010,] %>% 
  group_by(year) %>% 
  summarise(med.can = median(ams.can,na.rm = TRUE), 
            p95.can = quantile(ams.can, 0.95,na.rm = TRUE),
            p5.can = quantile(ams.can,0.05,na.rm = TRUE),
            med.us = median(ams.us,na.rm = TRUE), 
            p95.us = quantile(ams.us, 0.95,na.rm = TRUE),
            p5.us = quantile(ams.us,0.05,na.rm = TRUE)
                              
  ) 
ams.space$run <- nms

amc.space <- ls.df[ls.df$year > 2010,] %>% 
  group_by(year) %>% 
  summarise(med.can = median(amc.can,na.rm = TRUE), 
            p95.can = quantile(amc.can, 0.95,na.rm = TRUE),
            p5.can = quantile(amc.can,0.05,na.rm = TRUE),
            med.us = median(amc.us,na.rm = TRUE), 
            p95.us = quantile(amc.us, 0.95,na.rm = TRUE),
            p5.us = quantile(amc.us,0.05,na.rm = TRUE)
            
  ) 
amc.space$run <- nms

# F0.space <- ls.df[ls.df$year > 2010,] %>% 
#   group_by(year) %>% 
#   summarise(med.can = median(F0.can,na.rm = TRUE), 
#             p95.can = quantile(F0.can, 0.95,na.rm = TRUE),
#             p5.can = quantile(F0.can,0.05,na.rm = TRUE),
#             med.us = median(F0.us,na.rm = TRUE), 
#             p95.us = quantile(F0.us, 0.95,na.rm = TRUE),
#             p5.us = quantile(F0.us,0.05,na.rm = TRUE)
#             
#   ) 
# amc.space$run <- nms
# 



return(list(ls.df, nfailed, 
            list(
              SSBplot = SSB.plotquant, 
              SSBmid = SSB.plotmid,
              SSBtot = SSB.plottot,
              Catchplot = Catch.plotquant,
              amcplot = amc.plotquant,
              amsplot = ams.plotquant,
              amc.space = amc.space,
              ams.space = ams.space#,
              #F0 =F0.space)
              ))
)
}
