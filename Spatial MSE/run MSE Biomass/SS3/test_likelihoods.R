# Test the likelihoods 

# Catch composition 
tEnd <- df$tEnd
flag_catch <- df$flag_catch
phi_catch <- exp(parms$logphi_catch)
age_maxage <- df$age_maxage
ss_catch <- df$ss_catch
age_catch <- df$age_catch
age_catch_est <- df$age_catch
#age_catch_est <- reps$age_catch_est

ans_catchcomp <- 0

for(time in 1:tEnd){ #// Loop over available years
  
  if(flag_catch[time] == 1){ #// Flag if  there was a measurement that year
    beta = phi_catch*ss_catch[time]
    ans_catchcomp =ans_catchcomp+ (lgamma(beta)-lgamma(ss_catch[time]+beta))
    
    for(i in 1:age_maxage){ #// Loop over other ages (first one is empty for survey)
      if(age_catch[i,time]> 0){ #// Ignore zero entries
        ans_catchcomp <- ans_catchcomp+lgamma(ss_catch[time]*age_catch[i,time]+beta*age_catch_est[i,time])-lgamma(beta*age_catch_est[i,time])
      }
    }
  }
}

print(-ans_catchcomp)


#from Thorson:  NLL -= gammln(A) - gammln(ninput_t(t)+A) + sum(gammln(ninput_t(t)*extract_row(pobs_ta,t) + A*extract_row(pexp_ta,t))) - sum(lgamma(A*extract_row(pexp_ta,t))) \
