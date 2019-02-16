##### Movement parameters ####### 

function(df, ac.surv, ac.catch, biomass, move){
  
  sum3 <- 0
  sum4 <- 0
  
  ans_catchcomp <- 0 
  
  ### Load the data 
  
  # Catch comps 
  
  cc <- read.csv("catch_comps_n.csv")
  phi_catch <- exp(df$parms$logphi_catch)
  
  for(space in 1:df$nspace){
    if(df$nspace == 2){
      spaces <- c('Can','US')
    }
    
     yrs.tmp <- unique(cc$year)  
     
     
     for(time in 1:df$yrs.tmp){ # Loop not necessary in R but whatever 
       
       cc.tmp <- cc[cc$Country == spaces[i] & cc$year == yrs.tmp[time],]
       ss_catch <- cc.tmp$n[1]
       age_catch_tmp <- ac.catch[,df$years == cc.tmp$year[1],space]
       
       
       
        for(i in 1:df$age_maxage){ #Loop over other ages (first one is empty for survey)
          sum3[time] <- sum3 + lgamma(ss_catch*age_catch_tmp[i]+1)
          sum4[time] <- sum4 +lgamma(ss_catch*age_catch_tmp[i] + 
                                       phi_catch*ss_catch*age_catch_tmp[i,time]) - 
            lgamma(phi_catch*ss_catch[time]*age_catch_est[i,time])
        }
        ans_catchcomp <- ans_catchcomp+ 
          lgamma(ss_catch[time]+1)-sum3[time]+lgamma(phi_catch*ss_catch[time])-
          lgamma(ss_catch[time]+phi_catch*ss_catch[time])+sum4[time]
        
      }
  }
  
  
  
  
  
  
  return(ans)
}