runfuture_OM <- function(df,nruns = 100){
  
  AC <- function(x){
    x_m <- sum(1:df$age_maxage*x)
    
    return(x_m)
  }  
  
  
  # AC.median <- function(x, xtot){ # x is the ratio, xtot is the totoal (to scale back up)
  #  # N.save.age[,idx,space,df$surveyseason]
  #   xN <- round(x*xtot/min(xN[xN> 0])+1) # To save space 
  #   
  #   age <- 1:15
  #   
  #   ans <- median(rep.int(age, xN))
  #   
  #   return(ans)
  # }
  
  seedz <- round(runif(n = nruns, min = 1, max = 1e6))
  
  #sim.data <- run.agebased.true.catch(df,seed =  56)
  
  
  SSB.save <- array(NA,dim = c(nruns, df$nyear,  df$nspace))
  SSB.tot <- array(NA, dim = c(nruns, df$nyear))
  SSB.weight <- array(NA, dim = c(nruns, df$nyear))
  survey.save <- matrix(NA, df$nyear, nruns)
  R.save <- matrix(NA, df$nyear, nruns)
  Catch.save <- matrix(NA,df$nyear,nruns)
  AC.catch.save <- array(NA, dim = c(nruns, df$nyear,df$nspace))
  AC.survey.save <- array(NA, dim = c(nruns,df$nyear, df$nspace))
  survey.space.save <- array(NA, dim = c(nruns,df$nyear, df$nspace))
  if(nruns == 1){
    AC.catch.tot <- array(NA, dim = c(df$age_maxage, df$nyear,df$nspace))
    AC.survey.tot <- array(NA, dim = c(df$age_maxage, df$nyear,df$nspace))
  }
  
  
  run.true <- matrix(1, nruns)
  
  
  start.time <- Sys.time()
  
  
  for(i in 1:nruns){
    set.seed(seedz[i])
    
    sim.data <- run.agebased.true.catch(df,seed =  seedz[i])
    
    if(is.list(sim.data)){
      SSB.save[i,,] <- sim.data$SSB.all[,,df$surveyseason]
      SSB.tot[i,] <- rowSums(sim.data$SSB)
      SSB.weight[i,] <- rowSums(sim.data$SSB.weight)
      R.save[,i] <- sim.data$N.save[1,1:df$nyear]
      survey.save[,i] <- sim.data$survey
      survey.space.save[i,,] <- sim.data$survey.true
      Catch.save[,i] <- sim.data$Catch
      
      
      for(j in 1:df$nspace){
        AC.catch.save[i,,j] <- apply(sim.data$age_comps_catch_space[,,j],MARGIN = 2, FUN = AC)
        AC.survey.save[i,,j] <- apply(sim.data$age_comps_country[,,j],MARGIN = 2, FUN = AC)
      }
      
      if(nruns == 1){
        AC.catch.tot <- sim.data$age_comps_catch_space
        AC.survey.tot <- sim.data$age_comps_country
        survey.space.save <- sim.data$survey.true
        
      }
      
      SSB0 <- sum(sim.data$SSB_0)
      
      #rm(sim.data)
    }else{
      run.true[i] <- 0  
    }
  }
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  print(paste('Fraction of successful runs = ',sum(run.true)/nruns))
  if(sum(run.true) == 0){
    stop('no succesfull runs')
  }
  
  #SSB.tot <- apply(SSB.save[which(run.true == 1),,],MARGIN = c(1,2),FUN = sum)
  SSB.tot <- SSB.tot[run.true == 1, ]
  R.tot <- R.save[,which(run.true == 1)]
  survey.tot <- survey.save[,which(run.true == 1)]
  
  if(nruns > 1){
    SSB <- as.data.frame(t(apply(SSB.tot,2,quantile, probs = c(0.5,0.95,0.75,0.25,0.05), names = F)))
    SSB <- SSB#/SSB0
    names(SSB) <- c('p50','p95','p75','p25','p5')
  }else{
    
    SSB <- as.data.frame(SSB.tot)
    SSB.weight <- as.data.frame(SSB.weight)
    names(SSB.weight) <- 'SSB'
    names(SSB) <- 'SSB'
  }
  
  SSB$year <- df$years
  #SSB.weight$year <- df$years
  
  # if(nruns >1)
  # p1 <- ggplot(SSB, aes(x = year, y = p50))+
  #   theme_classic()+geom_point(size = 1.5)+
  #   geom_ribbon(aes(ymin = p5,ymax = p95), fill = alpha('gray',alpha = 0.5), linetype = 0)+
  #   geom_ribbon(aes(ymin = p25,ymax = p75), fill = alpha('gray',alpha = 0.8), linetype = 0)+  geom_line(size = 1.5)+
  #   scale_y_continuous(name = 'SSB/SSB_0')+geom_hline(aes(yintercept = 0.4),linetype =2, col = 'green')+
  #   geom_hline(aes(yintercept = 0.1),linetype =2, col = 'red')+
  #   geom_text(aes(x = 1965, y = 1.9, label = 'a'))
  # 
  # p1
  if(nruns>1){
    ls.save <-  list(SSB = SSB,SSB.save = SSB.save[run.true == 1,,] ,success.runs = sum(run.true)/nruns,
                     catch.AC = AC.catch.save[run.true == 1,,],
                     survey.AC = AC.survey.save[run.true == 1,,],
                     survey.space = survey.space.save[run.true == 1,,])
  }else{
    ls.save <- list(SSB = SSB,SSB.weight  = SSB.weight,SSB.save = SSB.save[run.true == 1,,] ,success.runs = sum(run.true)/nruns,
                    catch.AC = AC.catch.save[run.true == 1,,], catch.AC.tot = AC.catch.tot,
                    survey.AC = AC.survey.save[run.true == 1,,],survey.AC.tot = AC.survey.tot,
                    survey.save = survey.save[,run.true == 1],
                    survey.space = survey.space.save)
  }
  
  return(ls.save)
}