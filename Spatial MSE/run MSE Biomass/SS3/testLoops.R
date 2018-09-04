## Test the loops
ans <- 0

flag_catch <- df$flag_catch
tEnd <- df$tEnd
ss_catch <- df$ss_catch
age_maxage <- df$age_maxage
kappa <- 1e-7

age_catch_est <- reps$age_catch_est

age_catch <- reps$age_catch

for(time in 1:tEnd){ # Loop over available years
  
  if(flag_catch[time] == 1){ #// Flag if  there was a measurement that year
    
    for(i in 1:age_maxage){ #// Loop over other ages
      ans <-  (ss_catch[time]*age_catch_est[i,time]*log((age_catch_est[i,time]+kappa)/(age_catch[i,time]+kappa)))+ans
    }
  }
}

print(ans)
