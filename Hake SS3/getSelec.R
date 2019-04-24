getSelec <- function(age,psel, Smin, Smax){
  
  psel<- c(0,psel)
  
  nage <- length(age)
  
  selectivity <- rep(NA,nage)
  pmax <- max(cumsum(psel))
  
  for(j in 1:nage){ # Find the  selectivity
    if (age[j] < Smin){
      selectivity[j] = 0;
    }
    if (age[j] == Smin){
      ptmp = psel[j-Smin]
      selectivity[j] = exp(ptmp-pmax)
      
    }
    if (age[j] > Smin & (age[j] <= Smax)){
      ptmp = psel[j-Smin]+ptmp;
      selectivity[j] = exp(ptmp-pmax);
    }
    if(age[j] > (Smax)){
      selectivity[j] = selectivity[Smax+1];
    }
  }
  
  
  return(selectivity)
}