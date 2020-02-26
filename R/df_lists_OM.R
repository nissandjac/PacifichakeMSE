df_lists_OM <- function(ls.save, nms){
  source('calcMeanAge.R')
  
  nruns <- length(ls.save)
  nfailed <- rep(1, nruns)

  
  source('calcMeanAge.R')
  
  if(dim(ls.save[[1]]$SSB)[2] == 1){
    
    simyears <- length(ls.save[[1]][1]$SSB)-(length(1966:2017))+1
  }else{
    simyears <- length(rowSums(ls.save[[1]][1]$SSB))-(length(1966:2017))+1
    
  }
  
  year <- 1966:(2017+simyears-1)
  
  for(i in 1:nruns){
    
    if(i == 1){
      ls.df <- data.frame(year = year, SSB.can = ls.save[[i]]$SSB[,1], SSB.US = ls.save[[i]]$SSB[,2],
                          amc.can = calcMeanAge(ls.save[[i]]$amc[,,1], maxage = 15), 
                          amc.US = calcMeanAge(ls.save[[i]]$amc[,,2], maxage = 15), 
                          ams.can = calcMeanAge(ls.save[[i]]$ams[,,1], maxage = 15), 
                          ams.US = calcMeanAge(ls.save[[i]]$ams[,,2], maxage = 15), 
                          run = paste('run',i, sep = '-'))
    }else{
      
      if(is.null(ls.save[[i]]) == FALSE){
        ls.tmp <-  data.frame(year = year, SSB.can = ls.save[[i]]$SSB[,1], SSB.US = ls.save[[i]]$SSB[,2],
                              amc.can = calcMeanAge(ls.save[[i]]$amc[,,1], maxage = 15), 
                              amc.US = calcMeanAge(ls.save[[i]]$amc[,,2], maxage = 15), 
                              ams.can = calcMeanAge(ls.save[[i]]$ams[,,1], maxage = 15), 
                              ams.US = calcMeanAge(ls.save[[i]]$ams[,,2], maxage = 15), 
                              run = paste('run',i, sep = '-'))
        ls.df <- rbind(ls.df, ls.tmp)}
      else{
        nfailed[i] <- 0
      }
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
  
  
  SSB.all <- ls.df[ls.df$year > 1965,] %>%
    group_by(year) %>%
    summarise(med = median(SSB.can+SSB.US), 
              p95 = quantile(SSB.can+SSB.US, 0.95),
              p5 = quantile(SSB.can+SSB.US,0.05))
  SSB.all$run <- nms
  
  
  ams.plotquant <- ls.df[ls.df$year > 2010,] %>% 
    group_by(year) %>% 
    summarise(med.can = median(ams.can,na.rm = TRUE), 
              p95.can = quantile(ams.can, 0.95,na.rm = TRUE),
              p5.can = quantile(ams.can,0.05,na.rm = TRUE),
              med.US = median(ams.US,na.rm = TRUE), 
              p95.US = quantile(ams.US, 0.95,na.rm = TRUE),
              p5.US = quantile(ams.US,0.05,na.rm = TRUE)
    ) 
  ams.plotquant$run <- nms
  
  amc.plotquant <- ls.df[ls.df$year > 2010,] %>% 
    group_by(year) %>% 
    summarise(med.can = median(amc.can,na.rm = TRUE), 
              p95.can = quantile(amc.can, 0.95,na.rm = TRUE),
              p5.can = quantile(amc.can,0.05,na.rm = TRUE),
              med.US = median(amc.US,na.rm = TRUE), 
              p95.US = quantile(amc.US, 0.95,na.rm = TRUE),
              p5.US = quantile(amc.US,0.05,na.rm = TRUE)
    ) 
  amc.plotquant$run <- nms
  
  
  
  return(list(ls.df, nfailed, 
              list(
                SSB = SSB.plotquant, 
                ams = ams.plotquant,
                amc = amc.plotquant,
                SSB.all = SSB.all
               ))
  )
}