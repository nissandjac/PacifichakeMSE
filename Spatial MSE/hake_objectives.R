hake_objectives <- function(ls.MSE, SSB0, move = NA){
  #'   
  #' @ls.mse # A list of individual MSE runs. Should contain Biomass, Catch, and df with parameters    
  # sim.data # Initial years of the operating model
  #ls.MSE=ls.save
  nruns <- length(ls.MSE)
  nyears <- dim(ls.MSE[[1]][1]$Catch)[2]

  
  if(dim(ls.MSE[[1]][1]$Catch)[2] == 1){
    nyears <- dim(ls.MSE[[1]][1]$Catch)[1]
  }
  # Get the number of years run in that MSE 
  if(all(is.na(ls.MSE[[1]]))){
    simyears <- nyears-(length(1966:2018))+1
    
    
  }else{
    simyears <- nyears-(length(1966:2018))+1
    }
    
  yr <- 1966:(2018+simyears-1)
  
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

    SSB.plot <- data.frame(SSB = as.numeric(rowSums(ls.MSE[[idx]]$SSB))/sum(SSB0), year = yr, run = paste('run',1, sep=''))
    V.us.plot <- data.frame(V = ls.MSE[[idx]]$V[,2,3], year = yr, run = paste('run',1, sep='')) #vulnerable biomass at mid-year start of season 3
    V.ca.plot <- data.frame(V = ls.MSE[[idx]]$V[,1,3], year = yr, run = paste('run',1, sep='')) #vulnerable biomass at mid-year start of season 3

  }  
  
  if(is.na(move)){ #if there is no movement (single area model)
    Catch.plot <- data.frame(Catch = ls.MSE[[idx]]$Catch, year = yr, run = paste('run',1, sep=''))
  }else{

    
    catchtmp <- as.numeric(apply(ls.MSE[[idx]]$Catch,MARGIN = 2, FUN = sum))
    
    if(length(catchtmp) == 1){
      catchtmp <- ls.MSE[[idx]]$Catch
    }
    
    Catch.plot <- data.frame(Catch = catchtmp, year = yr, run = paste('run',1, sep=''))
    
    quota.tot <- apply(ls.MSE[[idx]]$Catch.quota, MARGIN = 1, FUN = sum)
    quota.plot <- data.frame(Quota_frac = quota.tot/catchtmp, year = yr, run = paste('run',1, sep =''))


    #sum quota over year by area
    quota.us.tot <- data.frame(quota=rowSums(ls.MSE[[idx]]$Catch.quota[,2,]), year=yr, run=paste('run',1, sep =''))
    quota.can.tot <- data.frame(quota=rowSums(ls.MSE[[idx]]$Catch.quota[,1,]), year=yr, run=paste('run',1, sep =''))
    
    #Catch by year by area
    
    catch.area<-apply(ls.MSE[[idx]]$Catch, MARGIN=c(2,3), FUN=sum)
    
    
    Catch.us.tot<- data.frame(Catch=catch.area[,2], year=yr, run=paste('run',1, sep =''))
    Catch.can.tot<- data.frame(Catch=catch.area[,1], year=yr, run=paste('run',1, sep =''))
    
    vtac.us<- data.frame(V.TAC=V.us.plot$V/Catch.us.tot$Catch, year=yr, run=paste('run',1, sep =''))
    vtac.can<- data.frame(V.TAC=V.ca.plot$V/Catch.can.tot$Catch, year=yr, run=paste('run',1, sep =''))
    
    vtac.us.seas<- data.frame(V.TAC.sp=ls.MSE[[idx]]$V[,2,2]/ls.MSE[[idx]]$Catch.quota[,2,2],
                              V.TAC.su=ls.MSE[[idx]]$V[,2,3]/ls.MSE[[idx]]$Catch.quota[,2,3],
                              V.TAC.fa=ls.MSE[[idx]]$V[,2,4]/ls.MSE[[idx]]$Catch.quota[,2,4],
                              year = yr, run =  paste('run',1, sep=''))
    
      
    vtac.can.seas<-  data.frame(V.TAC.sp=ls.MSE[[idx]]$V[,1,2]/ls.MSE[[idx]]$Catch.quota[,1,2],
                               V.TAC.su=ls.MSE[[idx]]$V[,1,3]/ls.MSE[[idx]]$Catch.quota[,1,3],
                               V.TAC.fa=ls.MSE[[idx]]$V[,1,4]/ls.MSE[[idx]]$Catch.quota[,1,4],
                               year = yr, run =  paste('run',1, sep=''))
  }  
  
  AAV.plot  <- data.frame(AAV = abs(catchtmp[2:length(yr)]-catchtmp[1:(length(yr)-1)])/catchtmp[1:(length(yr)-1)], 
                          year = yr[2:length(yr)], run = paste('run',1, sep=''))
  
  for(i in 2:nruns){
    ls.tmp <- ls.MSE[[i]]  
    
    if(is.list(ls.tmp)){
      if(is.na(move)){
        SSB.tmp <- data.frame(SSB = (ls.tmp$SSB)/(SSB0), year = yr, run =  paste('run',i, sep=''))
        Catch.tmp <- data.frame(Catch = ls.tmp$Catch, year = yr, run =  paste('run',i, sep=''))
        quota.tmp <- data.frame(Catch = ls.tmp$Catch/apply(ls.tmp$Catch.quota, MARGIN = 1, FUN = sum), 
                                year = yr, run =  paste('run',i, sep=''))
        V.tmp<-data.frame(V = ls.tmp$V[,3], year = yr, run =  paste('run',i, sep=''))
      
        
        
      }else{
        
        catchtmp <-  as.numeric(apply(ls.tmp$Catch,MARGIN = 2, FUN = sum))
        
        if(length(catchtmp) == 1){
          catchtmp <- ls.MSE[[idx]]$Catch
        }
        
        
        SSB.tmp <- data.frame(SSB = rowSums(ls.tmp$SSB)/sum(SSB0), year = yr, run =  paste('run',i, sep=''))
        Catch.tmp <- data.frame(Catch = catchtmp, year = yr, run =  paste('run',i, sep=''))
        quota.tmp <- data.frame(Quota_frac = catchtmp/apply(ls.tmp$Catch.quota, MARGIN = 1, FUN = sum), year = yr, run =  paste('run',i, sep=''))
        
        if(ncol(ls.tmp$Catch) == 1){ #if a single area model, catch by country fixed as proportion of total
          Catch.can <- ls.tmp$Catch*0.26
          Catch.us <- ls.tmp$Catch*0.74
        }else{
          Catch.can <- apply(ls.tmp$Catch,MARGIN = c(2,3), FUN = sum)[,1]
          Catch.us <- apply(ls.tmp$Catch,MARGIN = c(2,3), FUN = sum)[,2]
        }
        
        quota.tmp.can <- data.frame(Catch = Catch.can/rowSums(ls.tmp$Catch.quota[,1,]),
                                    year = yr, run =  paste('run',i, sep=''))
        quota.tmp.us <- data.frame(Catch = Catch.us/rowSums(ls.tmp$Catch.quota[,2,]),
                                   year = yr, run =  paste('run',i, sep=''))
        
        quota.tmp.can.tot<-data.frame(quota = rowSums(ls.tmp$Catch.quota[,1,]),
                                      year = yr, run =  paste('run',i, sep=''))
        quota.tmp.us.tot<-data.frame(quota = rowSums(ls.tmp$Catch.quota[,2,]),
                                     year = yr, run =  paste('run',i, sep=''))
        
        v.tmp.can <-data.frame(V = ls.tmp$V[,1,3],
                               year = yr, run =  paste('run',i, sep=''))
        
        
        v.tmp.us <-data.frame(V = ls.tmp$V[,2,3],
                               year = yr, run =  paste('run',i, sep=''))
        
        catch.tmp.can <- data.frame(Catch=Catch.can, 
                                    year = yr, run =  paste('run',i, sep=''))
        catch.tmp.us <-  data.frame(Catch=Catch.us, 
                                    year = yr, run =  paste('run',i, sep=''))
        
        vtac.tmp.can<- data.frame(V.TAC=v.tmp.can$V/quota.tmp.can.tot$quota, 
                                 year = yr, run =  paste('run',i, sep=''))
        
        vtac.tmp.us<- data.frame(V.TAC =v.tmp.us$V/quota.tmp.us.tot$quota, 
                                 year = yr, run =  paste('run',i, sep=''))
        
        vtac.tmp.us.seas<-data.frame(V.TAC.sp=ls.tmp$V[,2,2]/ls.tmp$Catch.quota[,2,2],
                                      V.TAC.su=ls.tmp$V[,2,3]/ls.tmp$Catch.quota[,2,3],
                                      V.TAC.fa=ls.tmp$V[,2,4]/ls.tmp$Catch.quota[,2,4],
                                      year = yr, run =  paste('run',i, sep=''))
        
        vtac.tmp.can.seas<-data.frame(V.TAC.sp=ls.tmp$V[,1,2]/ls.tmp$Catch.quota[,1,2],
                                      V.TAC.su=ls.tmp$V[,1,3]/ls.tmp$Catch.quota[,1,3],
                                      V.TAC.fa=ls.tmp$V[,1,4]/ls.tmp$Catch.quota[,1,4],
                                      year = yr, run =  paste('run',i, sep=''))
        
      }
      
      
      SSB.plot <- rbind(SSB.plot,SSB.tmp)
      Catch.plot <- rbind(Catch.plot,Catch.tmp)
      quota.plot <- rbind(quota.plot, quota.tmp)
      

     # quota.us<- rbind(quota.us, quota.tmp.us)
      #quota.can<- rbind(quota.can, quota.tmp.can)
        
      V.ca.plot<- rbind(V.ca.plot, v.tmp.can)
      V.us.plot<- rbind(V.us.plot, v.tmp.us)
      
      Catch.can.tot<- rbind(Catch.can.tot, catch.tmp.can)
      Catch.us.tot<- rbind(Catch.us.tot, catch.tmp.us)
      quota.can.tot<- rbind(quota.can.tot, quota.tmp.can.tot)
      quota.us.tot<- rbind(quota.us.tot, quota.tmp.us.tot)
        
      vtac.can<- rbind(vtac.can, vtac.tmp.can)
      vtac.us<-  rbind(vtac.us, vtac.tmp.us)
      
      vtac.can.seas<- rbind(vtac.can.seas, vtac.tmp.can.seas)
      vtac.us.seas<- rbind(vtac.us.seas, vtac.tmp.us.seas)

      AAV.tmp <- data.frame(AAV  = abs(catchtmp[2:length(yr)]-catchtmp[1:(length(yr)-1)])/catchtmp[1:(length(yr)-1)], 

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
  
 V.ca.plotquant<- V.ca.plot %>% 
   group_by(year) %>% 
   summarise(med = median(V), 
             p95 = quantile(V, 0.95),
             p25 = quantile(V, 0.25),
             p75 = quantile(V, 0.75), 
             p5 = quantile(V,0.05)) 
   
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
  SSB.future <- SSB.plot[SSB.plot$year > 2018,]
  
  #####KM alternative metric calculation October 10, 2019
  ###create Prob S<S10 stat by run and look at med across years
  SSB10.stat <- SSB.future %>% 
    group_by(run) %>% 
    #filter(year>2018 & year<2028) %>% 
    summarise(pcnt = length(which(SSB<0.1))/length(unique(SSB.future$year)))%>%
    summarise(med=median(pcnt),
              p95 = quantile(pcnt, 0.95),
              p25 = quantile(pcnt, 0.25),
              p75 = quantile(pcnt, 0.75), 
              p5 = quantile(pcnt,0.05))
  
  SSB40.stat <- SSB.future %>% 
    group_by(run) %>% 
    #filter(year>2018 & year<2028) %>% 
    summarise(pcnt = length(which(SSB<0.4 & SSB>0.1))/length(unique(SSB.future$year))) %>%
    summarise(med=median(pcnt),
              p95 = quantile(pcnt, 0.95),
              p25 = quantile(pcnt, 0.25),
              p75 = quantile(pcnt, 0.75), 
              p5 = quantile(pcnt,0.05))
  
  Catch.plotshort <- Catch.plot %>% 
    group_by(run) %>% 
    filter(year>2018 & year<2028) %>% 
    summarise(avg = mean(Catch)*1e-6) %>%
    summarise(med=median(avg),
              p95 = quantile(avg, 0.95),
              p25 = quantile(avg, 0.25),
              p75 = quantile(avg, 0.75), 
              p5 = quantile(avg,0.05))
  
  Catch.plotlong <- Catch.plot %>% 
    group_by(run) %>% 
    filter(year>2027) %>%
    summarise(avg = mean(Catch)*1e-6) %>%
    summarise(med=median(avg),
              p95 = quantile(avg, 0.95),
              p25 = quantile(avg, 0.25),
              p75 = quantile(avg, 0.75), 
              p5 = quantile(avg,0.05))
  
  AAV.plotshort <- AAV.plot %>% 
    group_by(run) %>% 
    summarise(avg = mean(AAV)) %>%
    summarise(med=median(avg),
              p95 = quantile(avg, 0.95),
              p25 = quantile(avg, 0.25),
              p75 = quantile(avg, 0.75), 
              p5 = quantile(avg,0.05))
  
  
  V.ca.stat <- V.ca.plot %>% 
    group_by(run) %>% 
    summarise(avg = mean(V)) %>%
    summarise(med=median(avg),
              p95 = quantile(avg, 0.95),
              p25 = quantile(avg, 0.25),
              p75 = quantile(avg, 0.75), 
              p5 = quantile(avg,0.05))
  
  V.us.stat <- V.us.plot %>% 
    group_by(run) %>% 
    summarise(avg = mean(V)) %>%
    summarise(med=median(avg),
              p95 = quantile(avg, 0.95),
              p25 = quantile(avg, 0.25),
              p75 = quantile(avg, 0.75), 
              p5 = quantile(avg,0.05))
  
  vtac.ca.stat<- vtac.can %>% 
    group_by(run) %>% 
    summarise(prop = length(which(V.TAC>(1/0.3)))/length(V.TAC)) %>%
    summarise(med=median(prop),
              p95 = quantile(prop, 0.95),
              p25 = quantile(prop, 0.25),
              p75 = quantile(prop, 0.75), 
              p5 = quantile(prop,0.05))
  
  vtac.us.stat<- vtac.us %>% 
    group_by(run) %>% 
    summarise(prop = length(which(V.TAC>1))/length(V.TAC)) %>%
    summarise(med=median(prop),
              p95 = quantile(prop, 0.95),
              p25 = quantile(prop, 0.25),
              p75 = quantile(prop, 0.75), 
              p5 = quantile(prop,0.05))
  
  vtac.us.seas.stat<- vtac.us.seas %>% 
    group_by(run) %>% 
    filter(year>2027) %>%
    summarise(avg.sp = mean(1/V.TAC.sp),
              avg.su = mean(1/V.TAC.su), 
              avg.fa= mean(1/V.TAC.fa)) %>%
    summarise(med.sp=median(avg.sp),
              med.su=median(avg.su),
              med.fa=median(avg.fa))
  
  vtac.can.seas.stat<- vtac.can.seas %>% 
    group_by(run) %>% 
    filter(year>2027) %>%
    summarise(avg.sp = mean(1/V.TAC.sp),
              avg.su = mean(1/V.TAC.su), 
              avg.fa= mean(1/V.TAC.fa)) %>%
    summarise(med.sp=median(avg.sp),
              med.su=median(avg.su),
              med.fa=median(avg.fa))
  
  
  
  # vtac.us.seas.stat<- vtac.us.seas %>% 
  #   group_by(run) %>% 
  #   summarise(prop.sp = (length(which(V.TAC.sp>1))/length(V.TAC.sp)),
  #             prop.su = (length(which(V.TAC.su>1))/length(V.TAC.su)),
  #             prop.fa= (length(which(V.TAC.fa>1))/length(V.TAC.fa))) %>%
  #   summarise(med.sp=median(prop.sp),
  #             med.su=median(prop.su),
  #             med.fa=median(prop.fa))
  # 
  # vtac.can.seas.stat<- vtac.can.seas %>% 
  #   group_by(run) %>% 
  #   summarise(prop.sp = (length(which(V.TAC.sp>1))/length(V.TAC.sp)),
  #             prop.su = (length(which(V.TAC.su>1))/length(V.TAC.su)),
  #             prop.fa= (length(which(V.TAC.fa>1))/length(V.TAC.fa))) %>%
  #   summarise(med.sp=median(prop.sp),
  #             med.su=median(prop.su),
  #             med.fa=median(prop.fa))
  
  
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
  
  
  ##calculate the probability of being between 10 and 40 percent of SSB0 for 3 consecutive years
  
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
    c('SSB <0.10 SSB0',
      'S>0.10<0.4S0',
      'S>0.4S0',
  #    '3 consec yrs S<S40',
   #   'years closed fishery',
      'AAV',
   #   'Mean SSB/SSB0',
   #   'median catch',
      'short term catch',
      'long term catch',
      'Canada TAC/V spr',
      'Canada TAC/V sum',
      'Canada TAC/V fall',
      'US TAC/V spr',
      'US TAC/V sum',
       'US TAC/V fall'
      
   #   'yrs bio unavailable'
   )
  
  # Calculate the number of years the quota was met 

  
  
  t.export <- data.frame(indicator =
                           as.factor(indicator), 
                         value = c(
                           round(length(which(SSB.future$SSB <= 0.1))/length(SSB.future$SSB), digits = 2),

                           round(length(which(SSB.future$SSB>0.1 & SSB.future$SSB<0.4))/length(SSB.future$SSB), digits = 2),

                           round(length(which(SSB.future$SSB>0.4))/length(SSB.future$SSB), digits = 2),
                          # round(mean(p.vals), digits = 2), 
                        #   mean(nclosed),
                           round(median(AAV.plotquant$med), digits = 2),
               #            median(SSB.plotquant$med[SSB.plotquant$year > 2017]),
                        #   median(1e6*Catch.plotquant$med[Catch.plotquant$year >2017])*1e-6,
                           median(1e6*Catch.plotquant$med[Catch.plotquant$year > 2018 & Catch.plotquant$year <2028])*1e-6,
                           median(1e6*Catch.plotquant$med[Catch.plotquant$year > 2025])*1e-6,
                          vtac.can.seas.stat$med.sp,
                          vtac.can.seas.stat$med.su,
                          vtac.can.seas.stat$med.fa,
                          vtac.us.seas.stat$med.sp,
                          vtac.us.seas.stat$med.su,
                          vtac.us.seas.stat$med.fa
               
                     
                          # median(quota.plot[quota.plot$year > 2018,]$Quota_frac < 0.95)
                           )
   
                         #lower = c(
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
