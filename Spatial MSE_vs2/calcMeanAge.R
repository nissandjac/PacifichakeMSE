calcMeanAge <- function(agemat, maxage){
  
  age <- 1:maxage
  dims <- dim(agemat)
  agemat[agemat < 0] <- NA
  agecalc <- agemat
  
  agemean <- rep(0,dims[2])
  
  for(i in 1:dims[2]){
    agemean[i] <- sum(age*agecalc[,i])
  }
  
  
  
  return(agemean)
}