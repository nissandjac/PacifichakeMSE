dnorm_test <- function(x, mean, sd, give_log=0){

  logres= -log(sqrt(2*pi)*sd)-0.5*((x-mean)/sd)^2

  if(give_log){
    return(logres)}
  else{ return(exp(logres))
      }

}

