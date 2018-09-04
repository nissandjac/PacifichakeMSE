## Test the multinominal likelihood
reps <- obj$report()

age_survey <- reps$age_catch
#age_survey[age_survey == -1] <- NA
age_survey_est <- reps$age_catch_est

ss_survey <- df$ss_catch
ss_flag <- df$flag_catch
age_maxage <- df$age_maxage

tEnd <- df$tEnd
nage <- df$age_maxage

#dmultinom(x, size = NULL, prob, log = FALSE)


phi <- exp(-0.5619) # From the assessment model;
ans <- 0 

for (time in 1:tEnd){
  
  if (ss_flag[time] == 1){
  alpha = phi*ss_survey[time];
    
  for (i in 2:age_maxage){
    
  ans <- lgamma(alpha)-lgamma(ss_survey[time]+alpha)+lgamma(ss_survey[time]*age_survey[i,time]+alpha*age_survey_est[i,time])-lgamma(alpha*age_survey[i,time])+ans
  
  if (is.nan(ans)){
    break()
  }
  }
  }
}


