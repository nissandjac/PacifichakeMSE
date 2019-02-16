hake_objectives <- function(ls.MSE, sim.data, move = NA){
#'   
#' @ls.mse # A list of individual MSE runs. Should contain Biomass, Catch, and df with parameters    
# sim.data # Initial years of the operating model   
  nruns <- length(ls.MSE)
  
  yr <- 1966:(2017+simyears-1)
  
  if(is.na(df$move)){
  SSB.plot <- data.frame(SSB = (ls.save[[1]]$SSB)/(sim.data$SSB_0), year = yr, run = paste('run',1, sep=''))
  }else{
    SSB.plot <- data.frame(SSB = rowSums(ls.save[[1]]$SSB)/sum(sim.data$SSB_0), year = yr, run = paste('run',1, sep=''))
  }  
  
  Catch.plot <- data.frame(Catch = ls.save[[1]]$Catch, year = yr, run = paste('run',1, sep=''))
  AAV.plot  <- data.frame(AAV = abs(ls.save[[1]]$Catch[2:length(yr)]-ls.save[[1]]$Catch[1:(length(yr)-1)])/ls.save[[1]]$Catch[1:(length(yr)-1)], 
                           year = yr[2:length(yr)], run = paste('run',1, sep=''))
  
  for(i in 2:nruns){
    ls.tmp <- ls.MSE[[i]]  
    
    if(is.list(ls.tmp)){
      if(is.na(df$move)){
      SSB.tmp <- data.frame(SSB = (ls.tmp$SSB)/(sim.data$SSB_0), year = yr, run =  paste('run',i, sep=''))
      }else{
        SSB.tmp <- data.frame(SSB = rowSums(ls.tmp$SSB)/sum(sim.data$SSB_0), year = yr, run =  paste('run',i, sep=''))
      }
      
      
      SSB.plot <- rbind(SSB.plot,SSB.tmp)
      
      Catch.tmp <- data.frame(Catch = ls.tmp$Catch, year = yr, run =  paste('run',i, sep=''))
      Catch.plot <- rbind(Catch.plot,Catch.tmp)
      
      AAV.tmp <- data.frame(AAV  = abs(ls.tmp$Catch[2:length(yr)]-ls.tmp$Catch[1:(length(yr)-1)])/ls.tmp$Catch[1:(length(yr)-1)], 
                            year = yr[2:length(yr)], run =  paste('run',i, sep=''))
      AAV.plot <- rbind(AAV.plot, AAV.tmp)
      
  
    }
  }
  
  require(ggplot2)
  require(dplyr)
  require(gridExtra)
  require(cowplot)
  
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
  
  
  #cairo_pdf(filename = 'MSE_run.pdf')
  p.plot <- list(p1,p2,p3)
  p.export <- plot_grid(plotlist = p.plot, ncol = 1, align ='v')
  #dev.off()
  ###  Plot the performance metrics from Kristins spreadsheet 
  
  ## Probability of S < S10
  SSB.future <- SSB.plot[SSB.plot$year > 2017,]
  
  print(paste('percentage of years where SSB < 0.1SSB0= ',
              round(length(which(SSB.future$SSB<0.1))/length(SSB.future$SSB)*100, digits = 2),'%', sep = ''))
  print(paste('percentage of years where SSB > 0.1SSB0 & SSB < 0.4SSB0 = ',
              round(length(which(SSB.future$SSB>0.1 & SSB.future$SSB<0.4))/length(SSB.future$SSB)*100, digits = 2),'%', sep = ''))
  print(paste('percentage of years where SSB > 0.4SSB0 = ',
              round(length(which(SSB.future$SSB>0.4))/length(SSB.future$SSB)*100, digits = 2),'%', sep = ''))
  
  
  Catch.future <- Catch.plot[Catch.plot$year > 2018,]
  
  print(paste('percentage of Catch  < 180000= ',
              round(length(which(Catch.future$Catch < 180000))/length(Catch.future$Catch)*100, digits = 2),'%', sep = ''))
  
  print(paste('percentage of Catch  > 180k and < 350k= ',
              round(length(which(Catch.future$Catch > 180000 & Catch.future$Catch < 350000))/length(Catch.future$Catch)*100, digits = 2),'%', sep = ''))
  
  print(paste('percentage of Catch > 350k= ',
              round(length(which(Catch.future$Catch > 350000))/length(Catch.future$Catch)*100, digits = 2),'%', sep = ''))
  
  # Catch variability 
  
  print(paste('median AAV = ',round(median(AAV.plotquant$med), digits = 2), sep = ''))
  
### 
  p.export <- grid.arrange(p1,p2,p3)
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
  # Create a table with all the objective data 
  
  t.export <- data.frame(indicator =
                           c('B<0.10B0',
                             'B>0.10<0.4B0',
                             'B>0.4B0',
                             '3 consec yrs B<B40',
                             'yrs closed fishery',
                             'AAV','Mean SSB/SSB0','median Catch'), 
                         value = c(
                           round(length(which(SSB.future$SSB<0.1))/length(SSB.future$SSB)*100, digits = 2),
                           round(length(which(SSB.future$SSB>0.1 & SSB.future$SSB<0.4))/length(SSB.future$SSB)*100, digits = 2),
                           round(length(which(SSB.future$SSB>0.4))/length(SSB.future$SSB)*100, digits = 2),
                           round(mean(p.vals), digits = 2), 
                           round(length(which(SSB.future$SSB<0.1))/length(SSB.future$SSB)*100, digits = 0),
                          round(median(AAV.plotquant$med), digits = 2),
                          median(SSB.plotquant$med[SSB.plotquant$year > 2017]),
                          median(1e6*Catch.plotquant$med[Catch.plotquant$year >2017])*1e-6)
                         )
                           
                           
  
  return(list(p.export,t.export))
  
}
