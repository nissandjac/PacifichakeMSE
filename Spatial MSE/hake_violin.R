hake_objectives <- function(ls.MSE, SSB0, move = NA){
  #'   
  #' @ls.mse # A list of individual MSE runs. Should contain Biomass, Catch, and df with parameters    
  # sim.data # Initial years of the operating model   
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
  }  
  
  if(is.na(move)){
    Catch.plot <- data.frame(Catch = ls.MSE[[idx]]$Catch, year = yr, run = paste('run',1, sep=''))
  }else{
    
    catchtmp <- as.numeric(apply(ls.MSE[[idx]]$Catch,MARGIN = 2, FUN = sum))
    
    if(length(catchtmp) == 1){
      catchtmp <- ls.MSE[[idx]]$Catch
    }
    
    Catch.plot <- data.frame(Catch = catchtmp, year = yr, run = paste('run',1, sep=''))
    
    quota.tot <- apply(ls.MSE[[idx]]$Catch.quota, MARGIN = 1, FUN = sum)
    quota.plot <- data.frame(Quota_frac = quota.tot/catchtmp, year = yr, run = paste('run',1, sep =''))
    
    quota.us <- rowSums(ls.MSE[[idx]]$Catch.quota[,2,])
    quota.can <- rowSums(ls.MSE[[idx]]$Catch.quota[,1,])
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
        
        
        
      }else{
        
        catchtmp <-  as.numeric(apply(ls.tmp$Catch,MARGIN = 2, FUN = sum))
        
        if(length(catchtmp) == 1){
          catchtmp <- ls.MSE[[idx]]$Catch
        }
        
        
        SSB.tmp <- data.frame(SSB = rowSums(ls.tmp$SSB)/sum(SSB0), year = yr, run =  paste('run',i, sep=''))
        Catch.tmp <- data.frame(Catch = catchtmp, year = yr, run =  paste('run',i, sep=''))
        quota.tmp <- data.frame(Quota_frac = catchtmp/apply(ls.tmp$Catch.quota, MARGIN = 1, FUN = sum), year = yr, run =  paste('run',i, sep=''))
        
        if(ncol(ls.tmp$Catch) == 1){
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
      }
      
      
      SSB.plot <- rbind(SSB.plot,SSB.tmp)
      Catch.plot <- rbind(Catch.plot,Catch.tmp)
      quota.plot <- rbind(quota.plot, quota.tmp)
      
      AAV.tmp <- data.frame(AAV  = abs(catchtmp[2:length(yr)]-catchtmp[1:(length(yr)-1)])/catchtmp[1:(length(yr)-1)], 
                            year = yr[2:length(yr)], run =  paste('run',i, sep=''))
      AAV.plot <- rbind(AAV.plot, AAV.tmp)
      
      
    }
  }
  
  
  ## Probability of S < S10
  SSB.future <- SSB.plot[SSB.plot$year > 2017,]
  
 
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

  # Calculate the number of years the quota was met 
  
  
  return()
  
}
