
psel_surv<- c(0,parms$psel_surv)

nage <- df$nage
age <- df$age
surveyselc <- rep(NA,nage)
Smin_survey <- df$Smin_survey
Smax_survey <- df$Smax_survey
pmax <- max(cumsum(psel_surv))

for(j in 1:nage){ # Fix the survey selectivity
  if (age[j] < Smin_survey){
    surveyselc[j] = 0;
  }
  if (age[j] == Smin_survey){
    ptmp = psel_surv[j-Smin_survey];
    surveyselc[j] = exp(ptmp-pmax)
    print(ptmp)
  }
  if (age[j] > Smin_survey & (age[j] <= Smax_survey)){
    ptmp = psel_surv[j-Smin_survey]+ptmp;
    surveyselc[j] = exp(ptmp-pmax);
    print(ptmp)
  }
  if(age[j] > (Smax_survey)){
    surveyselc[j] = surveyselc[Smax_survey+1];
  }
}

plot(age,surveyselc)
lines(age, reps$surveyselc)


nage <- df$nage
years <- df$years
psel_fish<- c(0,parms$psel_fish)
#psel_surv[3] <- 2
catchselec <- matrix(NA,nage, df$tEnd)
Smin <- df$Smin
Smax <- df$Smax
age <- df$age
pmax <- matrix(NA, df$tEnd)
PTMP <- matrix(NA, length(psel_fish), df$tEnd)

for (i in 1:df$tEnd){
  
  for(j in 1:nage){ # Fix the survey selectivit
    if(years[i] < years[df$selYear]){
   
     pmax[i] <- max(cumsum(psel_fish))
     PTMP[,i] <- psel_fish
      if (age[j] < Smin){
        catchselec[j,i] = 0;
      }
      if (age[j] == Smin){
        ptmp = psel_fish[j-Smin];
        catchselec[j,i] = exp(ptmp-pmax[i]);
      }
      if (age[j] > Smin & age[j] <= Smax){
        ptmp = psel_fish[j-Smin]+ptmp;
        catchselec[j,i] = exp(ptmp-pmax[i]);
      }
      if(age[j] > (Smax)){
        catchselec[j,i] = catchselec[Smax+1,i];
      }
  }else{
    psel_fish_new = c(0,parms$psel_fish)+c(0,parms$PSEL[,i-df$selYear+1])
    pmax[i] <- max(cumsum(psel_fish_new))
    PTMP[,i] <- psel_fish_new
    
    if (age[j] < Smin){
      catchselec[j,i] = 0;
    }
    if (age[j] == Smin){
      ptmp = psel_fish_new[j-Smin];
      print(ptmp)
      catchselec[j,i] = exp(ptmp-pmax[i]);
    }
    if ((age[j] > Smin) & (age[j] <= Smax)){
      ptmp = psel_fish_new[j-Smin]+ptmp;
      catchselec[j,i] = exp(ptmp-pmax[i]);
    }
    if(age[j] > (Smax)){
      catchselec[j,i] = catchselec[Smax+1,i];
    }
    
    }
  } 
}


# Plot all the selectivities 
par(mfrow = c(5,6), mar = c(0.5,0.5,0.5,0.5))
plot(df$age,catchselec[,1], xlab ='', xaxt = 'n',yaxt = 'n', axes = F, ylim = c(0,2), type ='l', main = '1990')
for(i in 3:df$tEnd){
  if (years[i] < years[df$selYear]){
    lines(df$age,catchselec[,1])
    points(df$age, reps$selectivity_save[,1])
  }else{
    plot(df$age,catchselec[,i], type = 'l', xlab ='', ylab = '', axes = F, ylim = c(0,2), main = years[i])
    points(df$age, reps$selectivity_save[,i])   
  
  }
}


plot(pmax, ylim = c(3.5,10))
lines(reps$pmax_catch_save)
