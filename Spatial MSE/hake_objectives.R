hake_objectives <- function(ls.MSE, SSB0, move = NA){
  #'   
  #' @ls.mse # A list of individual MSE runs. Should contain Biomass, Catch, and df with parameters    
  # sim.data # Initial years of the operating model   
  nruns <- length(ls.MSE)

  # Get the number of years run in that MSE 
  if(all(is.na(ls.MSE[[1]]))){
    simyears <- length(ls.MSE[[2]][2]$Catch)-(length(1966:2017))+1
    
    
  }else{
    simyears <- length(ls.MSE[[1]][1]$Catch)-(length(1966:2017))+1
    }
    
  yr <- 1966:(2017+simyears-1)
  
  idx <- 1
  
  if(all(is.na(ls.MSE[[idx]]))){
    idx <- 2
  }
  if(all(is.na(ls.MSE[[idx]]))){
    idx <- 3
  }
  
  
  if(is.na(move)){
    SSB.plot <- data.frame(SSB = (ls.MSE[[idx]]$SSB)/(SSB0), year = yr, run = paste('run',1, sep=''))
  }else{
    SSB.plot <- data.frame(SSB = rowSums(ls.MSE[[idx]]$SSB)/sum(SSB0), year = yr, run = paste('run',1, sep=''))
  }  
  
  if(is.na(move)){
  Catch.plot <- data.frame(Catch = ls.MSE[[idx]]$Catch, year = yr, run = paste('run',1, sep=''))
  }else{
    Catch.plot <- data.frame(Catch = rowSums(ls.MSE[[idx]]$Catch), year = yr, run = paste('run',1, sep=''))
    
    quota.tot <- apply(ls.MSE[[idx]]$Catch.quota, MARGIN = 1, FUN = sum)
    quota.plot <- data.frame(Quota_frac = quota.tot/rowSums(ls.MSE[[idx]]$Catch), year = yr, run = paste('run',1, sep =''))
    
    quota.us <- rowSums(ls.MSE[[idx]]$Catch.quota[,2,])
    quota.can <- rowSums(ls.MSE[[idx]]$Catch.quota[,1,])
  }  
  AAV.plot  <- data.frame(AAV = abs(ls.MSE[[idx]]$Catch[2:length(yr)]-ls.MSE[[idx]]$Catch[1:(length(yr)-1)])/ls.MSE[[idx]]$Catch[1:(length(yr)-1)], 
                          year = yr[2:length(yr)], run = paste('run',1, sep=''))
  
  for(i in 2:nruns){
    ls.tmp <- ls.MSE[[i]]  
    
    if(is.list(ls.tmp)){
      if(is.na(move)){
        SSB.tmp <- data.frame(SSB = (ls.tmp$SSB)/(SSB0), year = yr, run =  paste('run',i, sep=''))
        Catch.tmp <- data.frame(Catch = ls.tmp$Catch, year = yr, run =  paste('run',i, sep=''))
        quota.tmp <- data.frame(Catch = ls.tmp$Catch/apply(ls.tmp$Catch.quota, MARGIN = 1, FUN = sum), 
                                year = yr, run =  paste('run',i, sep=''))
      
        
        
      }else{
        SSB.tmp <- data.frame(SSB = rowSums(ls.tmp$SSB)/sum(SSB0), year = yr, run =  paste('run',i, sep=''))
        Catch.tmp <- data.frame(Catch = rowSums(ls.tmp$Catch), year = yr, run =  paste('run',i, sep=''))
        quota.tmp <- data.frame(Quota_frac = rowSums(ls.tmp$Catch)/apply(ls.tmp$Catch.quota, MARGIN = 1, FUN = sum), year = yr, run =  paste('run',i, sep=''))
        
        if(ncol(ls.tmp$Catch) == 1){
          Catch.can <- ls.tmp$Catch*0.26
          Catch.us <- ls.tmp$Catch*0.74
        }else{
          Catch.can <- ls.tmp$Catch[,1]
          Catch.us <- ls.tmp$Catch[,2]
        }
        
        quota.tmp.can <- data.frame(Catch = Catch.can/rowSums(ls.tmp$Catch.quota[,1,]),
                                    year = yr, run =  paste('run',i, sep=''))
        quota.tmp.us <- data.frame(Catch = Catch.us/rowSums(ls.tmp$Catch.quota[,2,]),
                                   year = yr, run =  paste('run',i, sep=''))
      }
      
      
      SSB.plot <- rbind(SSB.plot,SSB.tmp)
      Catch.plot <- rbind(Catch.plot,Catch.tmp)
      quota.plot <- rbind(quota.plot, quota.tmp)
      
      AAV.tmp <- data.frame(AAV  = abs(ls.tmp$Catch[2:length(yr)]-ls.tmp$Catch[1:(length(yr)-1)])/ls.tmp$Catch[1:(length(yr)-1)], 
                            year = yr[2:length(yr)], run =  paste('run',i, sep=''))
      AAV.plot <- rbind(AAV.plot, AAV.tmp)
      
      
    }
  }

  
  SSB.plotquant <- SSB.plot %>% 
    group_by(year) %>% 
    summarise(med = median(SSB), 
              p95 = quantile(SSB, 0.95),
              p25 = quantile(SSB, 0.25),
              p75 = quantile(SSB, 0.75), 
              p5 = quantile(SSB,0.05)) 
  
  p1 <- ggplot(data=SSB.plotquant, aes(x= year,y = med)) +
    geom_ribbon(aes(ymin = p5, ymax = p95), fill = alpha('gray', alpha =0.5))+
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = alpha('gray', alpha =0.8))+
    theme_classic()+scale_y_continuous(name = 'SSB')+
    geom_line(color="black", size = 1.5)#+geom_line(data = SSB.plot, aes(y = SSB,group = run), color = alpha('black', alpha = 0.2))
  
  
  Catch.plotquant <- Catch.plot %>% 
    group_by(year) %>% 
    summarise(med = median(Catch)*1e-6, 
              p95 = quantile(Catch, 0.95)*1e-6,
              p25 = quantile(Catch, 0.25)*1e-6,
              p75 = quantile(Catch, 0.75)*1e-6, 
              p5 = quantile(Catch,0.05)*1e-6)
  
  p2 <-  ggplot(Catch.plotquant,aes(x= year,y = med)) +
    geom_ribbon(aes(ymin = p5, ymax = p95), fill = alpha('gray', alpha =0.5))+
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = alpha('gray', alpha =0.8))+
    theme_classic()+scale_y_continuous(name = 'Catch (millions)')+
    geom_line(color="black", size = 1.5)#+geom_line(data = Catch.plot, aes(y = Catch,group = run), color = alpha('black', alpha = 0.2))
  
  
  AAV.plotquant <- AAV.plot %>% 
    group_by(year) %>% 
    summarise(med = median(AAV), 
              p95 = quantile(AAV, 0.95),
              p25 = quantile(AAV, 0.25),
              p75 = quantile(AAV, 0.75), 
              p5 = quantile(AAV,0.05))
  
  p3 <-  ggplot(AAV.plotquant,aes(x= year,y = med)) +
    geom_ribbon(aes(ymin = p5, ymax = p95), fill = alpha('gray', alpha =0.5))+
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = alpha('gray', alpha =0.8))+
    theme_classic()+scale_y_continuous(name = 'Catch\nvariability')+
    geom_line(color="black", size = 1.5)#+geom_line(data = Catch.plot, aes(y = Catch,group = run), color = alpha('black', alpha = 0.2))
  
  quota.plotquant <- quota.plot[quota.plot$year>2010,] %>% 
    group_by(year) %>% 
    summarise(med = mean(Quota_frac), 
              p95 = quantile(Quota_frac, 0.95),
              p25 = quantile(Quota_frac, 0.25),
              p75 = quantile(Quota_frac, 0.75), 
              p5 = quantile(Quota_frac,0.05)) 
  
  p4 <- ggplot(data=quota.plotquant, aes(x= year,y = med)) +
    geom_ribbon(aes(ymin = p5, ymax = p95), fill = alpha('gray', alpha =0.5))+
    geom_ribbon(aes(ymin = p25, ymax = p75), fill = alpha('gray', alpha =0.8))+
    theme_classic()+scale_y_continuous(name = 'SSB')+coord_cartesian(ylim = c(0.4,1.05))+
    geom_line(color="black", size = 1.5)#+geom_line(data = SSB.plot, aes(y = SSB,group = run), color = alpha('black', alpha = 0.2))
  p4
  #cairo_pdf(filename = 'MSE_run.pdf')
  #p.plot <- list(p1,p2,p3)
  #p.export <- plot_grid(plotlist = p.plot, ncol = 1, align ='v')
  #dev.off()
  ###  Plot the performance metrics from Kristins spreadsheet 
  
  ## Probability of S < S10
  SSB.future <- SSB.plot[SSB.plot$year > 2017,]
  
  # print(paste('percentage of years where SSB < 0.1SSB0= ',
  #             round(length(which(SSB.future$SSB<0.1))/length(SSB.future$SSB)*100, digits = 2),'%', sep = ''))
  # 
  # print(paste('percentage of years where SSB > 0.1SSB0 & SSB < 0.4SSB0 = ',
  #             round(length(which(SSB.future$SSB>0.1 & SSB.future$SSB<0.4))/length(SSB.future$SSB)*100, digits = 2),'%', sep = ''))
  # print(paste('percentage of years where SSB > 0.4SSB0 = ',
  #             round(length(which(SSB.future$SSB>0.4))/length(SSB.future$SSB)*100, digits = 2),'%', sep = ''))
  # 
  # 
  # Catch.future <- Catch.plot[Catch.plot$year > 2018,]
  # 
  # print(paste('percentage of Catch  < 180000= ',
  #             round(length(which(Catch.future$Catch < 180000))/length(Catch.future$Catch)*100, digits = 2),'%', sep = ''))
  # 
  # print(paste('percentage of Catch  > 180k and < 350k= ',
  #             round(length(which(Catch.future$Catch > 180000 & Catch.future$Catch < 350000))/length(Catch.future$Catch)*100, digits = 2),'%', sep = ''))
  # 
  # print(paste('percentage of Catch > 350k= ',
  #             round(length(which(Catch.future$Catch > 350000))/length(Catch.future$Catch)*100, digits = 2),'%', sep = ''))
  # 
  # # Catch variability 
  # 
  # print(paste('median AAV = ',round(median(AAV.plotquant$med), digits = 2), sep = ''))
  
  ### 
  #p.export <- grid.arrange(p1,p2,p3)
  rns <- unique(SSB.future$run)
  p.vals <- matrix(0, length(unique(SSB.future$run)))
  
  for(i in 1:length(unique(SSB.future$run))){
    tmp <- SSB.future[SSB.future$run == rns[i],]
    for(j in 1:(length(tmp$year)-3)){
      if(tmp$SSB[j]< 0.4){
        if(tmp$SSB[j+1]<0.4 & tmp$SSB[j+2] <0.4 & tmp$SSB[j+2]<0.4){
          p.vals[i] <- 1+p.vals[i]
        } 
      }
    }
    
  }
  
  p.vals <- p.vals/(length(tmp$year)-3)
  
  ## Calculate the median number of closed years 
  nclosed <- rep(NA, length(rns))
  for(i in 1:length(unique(SSB.future$run))){
  tmp <- SSB.future[SSB.future$run == rns[i],]  
    
    nclosed[i] <- length(which(tmp$SSB < 0.1))
  }
  # Create a table with all the objective data 
  indicator =
    c('S>0.10S0',
   #   'S>0.10<0.4S0',
   #   'S>0.4S0',
  #    '3 consec yrs S<S40',
   #   'years closed fishery',
      'AAV',
   #   'Mean SSB/SSB0',
   #   'median catch',
      'short term catch',
      'long term catch'
   #   'yrs bio unavailable'
   )
  
  # Calculate the number of years the quota was met 

  
  
  t.export <- data.frame(indicator =
                           as.factor(indicator), 
                         value = c(
                           round(length(which(SSB.future$SSB>0.1))/length(SSB.future$SSB), digits = 2),
                        #   round(length(which(SSB.future$SSB>0.1 & SSB.future$SSB<0.4))/length(SSB.future$SSB), digits = 2),
                          # round(length(which(SSB.future$SSB>0.4))/length(SSB.future$SSB), digits = 2),
                          # round(mean(p.vals), digits = 2), 
                        #   mean(nclosed),
                           round(median(AAV.plotquant$med), digits = 2),
               #            median(SSB.plotquant$med[SSB.plotquant$year > 2017]),
                        #   median(1e6*Catch.plotquant$med[Catch.plotquant$year >2017])*1e-6,
                           median(1e6*Catch.plotquant$med[Catch.plotquant$year > 2018 & Catch.plotquant$year <2028])*1e-6,
                           median(1e6*Catch.plotquant$med[Catch.plotquant$year > 2025])*1e-6
                          # median(quota.plot[quota.plot$year > 2018,]$Quota_frac < 0.95)
                           )
   
                         # uncertainty = c(
                         #   round(length(which(SSB.future$SSB<0.1))/length(SSB.future$SSB)*100, digits = 2),
                         #   round(length(which(SSB.future$SSB>0.1 & SSB.future$SSB<0.4))/length(SSB.future$SSB)*100, digits = 2),
                         #   round(length(which(SSB.future$SSB>0.4))/length(SSB.future$SSB)*100, digits = 2),
                         #   round(mean(p.vals), digits = 2), 
                         #   round(length(which(SSB.future$SSB<0.1))/length(SSB.future$SSB)*100, digits = 0),
                         #   round(median(AAV.plotquant$med), digits = 2),
                         #   median(SSB.plotquant$med[SSB.plotquant$year > 2017]),
                         #   median(1e6*Catch.plotquant$med[Catch.plotquant$year >2017])*1e-6,
                         #   median(1e6*Catch.plotquant$med[Catch.plotquant$year > 2018 & Catch.plotquant$year <2030])*1e-6
                         #   )
  )
  print(t.export)
  
  p.export = NA
  return(list(p.export,t.export))
  
}
