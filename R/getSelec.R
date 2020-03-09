getSelec <- function(age,psel, Smin, Smax){
#' Calculate selectivity 
#' @age ages 
#' @psel selectivity parameters 
#' @Smin minimum age for selectivity 
#' @Smax maximum age for selectivty (constant after this age)
  
#psel <- pseltmp
psel<- c(0,psel)

nage <- length(age)

selectivity <- rep(NA,nage)

pmax <- max(cumsum(psel))

for(j in 1:nage){ # Find the  selectivity
  if (age[j] < Smin){
    selectivity[j] = 0;
    ptmp <- 0
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
 # print(ptmp-pmax)
}


return(selectivity)
}