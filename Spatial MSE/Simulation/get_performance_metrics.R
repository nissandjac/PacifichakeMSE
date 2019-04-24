get_performance_metrics <- function(sim.data,df){
  
  # Calculate the long term performance metrics from the MSE 
  # Required
  # SSB
  # 
  #Average percent unfished biomass (SSB/SSB0)

  #  Probability that percent unfished biomass drops below 10%
  
  yr.idx <- (df$nyear-simyears+1):df$nyear
  yr.future <- df$years[yr.idx]  
    
  SSB_0 <- rowSums(sim.data$SSB[yr.idx,])/sum(sim.data$SSB_0)
    
  print(paste('% of years with biomass < 10% B0 = ',length(SSB_0[SSB_0 < 0.1])/simyears))  
  print(paste('% of years with biomass > 40% B0 = ',length(SSB_0[SSB_0 > 0.4])/simyears))  
    
  par(mfrow = c(3,2), mar = c(4,4,1,1))
  plot(yr.future,SSB_0, ylim = c(0.05,1.3), lwd = 3, col = 'black', type = 'l')
  lines(yr.future, rep(1,length(yr.future)), lty = 2, lwd = 1.2)
  lines(yr.future, rep(0.4,length(yr.future)), lty = 2, lwd = 1.2)
  lines(yr.future, rep(0.1,length(yr.future)), lty = 2, lwd = 1.2)
  
  plot(F40.save, type = 'l',xlab ='years', ylab='fishing mortality')
  
  # Average age of the population
  
  age.comps <- sim.data$age_comps_OM[,2:df$nyear,,1]
  age.comps <- apply(age.comps,c(1,2),sum)/2
  
  am <- matrix(NA, df$nyear)
  for(i in 1:(df$nyear-1)){
    am[i] <- weighted.mean(df$age,age.comps[,i])
  
  }
  
  age.comps.can <- sim.data$age_comps_OM[,2:df$nyear,1,1]
  am.can <- matrix(NA, df$nyear)
  for(i in 1:(df$nyear-1)){
    am.can[i] <- weighted.mean(df$age,age.comps.can[,i])

  }
  
  age.comps.US <- sim.data$age_comps_OM[,2:df$nyear,2,1]
  am.US <- matrix(NA, df$nyear)
  for(i in 1:(df$nyear-1)){
    am.US[i] <- weighted.mean(df$age,age.comps.US[,i])
    
  }

  plot(df$years, am, type = 'l', lwd = 3, ylab = 'average age', ylim = c(1,6), xlab = 'years')
  lines(df$years, am.US, type ='l', lwd = 1, col = 'blue', lty = 2)
  lines(df$years, am.can, type ='l', lwd = 1, col = 'red', lty = 2)
  # Average age 4+ biomass
  
  Biomass4p <- matrix(NA, df$nyear)
  
  for(i in 1:(df$nyear-1)){
    Biomass4p[i] <- sum(rowSums(sim.data$Nsave.all[5:df$nage,i,,1]*df$wage_mid[5:df$nage,i]))
    
  }
  Biomass4p.US <- matrix(NA, df$nyear)
  
  for(i in 1:(df$nyear-1)){
    Biomass4p.US[i] <- sum(sim.data$Nsave.all[5:df$nage,i,2,1]*df$wage_mid[5:df$nage,i])
    
  }
  
  Biomass4p.can <- matrix(NA, df$nyear)
  
  for(i in 1:(df$nyear-1)){
    Biomass4p.can[i] <- sum(sim.data$Nsave.all[5:df$nage,i,1,1]*df$wage_mid[5:df$nage,i])
    
  }
  plot(df$years,Biomass4p, type = 'l', ylab ='4 year+ Biomass', lwd = 3, xlab = 'years')
  lines(df$years, Biomass4p.can, lty = 2, col ='red')
  lines(df$years, Biomass4p.US, lty = 2, col ='blue')
    
  
  # Percent of fish biomass that is age 4+
  Biomass.tot  <- matrix(NA,df$nyear) 
  
    for(i in 1:(df$nyear-1)){
      Biomass.tot[i] <- sum(rowSums(sim.data$Nsave.all[,i,,1]*df$wage_mid[,i]))
      
  }  
  plot(df$years,Biomass4p/Biomass.tot, ylim= c(0,1), type ='l', lwd = 3, ylab= 'fraction of biomass 4+',
       xlab = 'years')
  lines(df$years,Biomass4p.can/Biomass.tot, lwd = 1, col ='red')
  lines(df$years,Biomass4p.US/Biomass.tot, lwd = 1, col ='blue')
  
  # Yield metrics
  Catch <- sim.data$Catch
  plot(df$years,Catch/mean(Catch), type='l', lwd = 3, ylab = 'Average Catches', xlab = 'years')
  # Average TAC 
  # 
  # 
  # Average annual variability in catch
  CV.catch.future <- sd(Catch[yr.idx])/mean(Catch[yr.idx]) 
  CV.catch.past <- sd(Catch[1:yr.idx[1]])/mean(Catch[1:yr.idx[1]])
  print(paste('CV for future catches = ',CV.catch.future))
  print(paste('CV for past catches = ',CV.catch.past))
  
  # Probability that fishery is closed (TAC=0)
  # 
  # Probability TAC is below 180k tons
  # 
  # Probability TAC is between 180k and 370k tons
  # Probability TAC is above 370k tons
  
  # 
  # 
  # library(fmsb)
  # 
  # # Create data: note in High school for several students
  # set.seed(99)
  # data=as.data.frame(matrix( sample( 0:20 , 15 , replace=F) , ncol=5))
  # colnames(data)=c("math" , "english" , "biology" , "music" , "R-coding" )
  # rownames(data)=paste("mister" , letters[1:3] , sep="-")
  # 
  # # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
  # data=rbind(rep(20,5) , rep(0,5) , data)
  # 
  # 
  # 
  # 
  # 
  # #==================
  # # Plot 1: Default radar chart proposed by the library:
  # radarchart(data)
  # 
  # 
  # #==================
  # # Plot 2: Same plot with custom features
  # colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
  # colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
  # radarchart( data  , axistype=1 , 
  #             #custom polygon
  #             pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
  #             #custom the grid
  #             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
  #             #custom labels
  #             vlcex=0.8 
  # )
  # legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
  # 
  
  
  
  
  
  
  
  
  
  
  
  
  
}