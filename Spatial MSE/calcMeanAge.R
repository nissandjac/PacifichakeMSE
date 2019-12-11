calcMeanAge <- function(agemat, maxage){
  
  age <- 1:maxage
  dims <- dim(agemat)
  ## Adjust the agematrix to fit the maxage

  if(dims[1] == maxage){
    agecalc <- agemat
  }else{
    agecalc <- matrix(NA, maxage, dims[2])  
    agecalc[1:(maxage-1),] <- agemat[1:(maxage-1),]
    agecalc[maxage,] <- colSums(agemat[maxage:dims[1],])
    agecalc <- agecalc/colSums(agecalc)  
  }
  
  
  
  if(all(colSums(agecalc) != 1)){
    for(i in 1:dims[2]){
    agecalc[,i] <- agecalc[,i]/sum(agecalc[,i])
    }
  }
  
  agemean <- rep(0,dims[2])
  
  for(i in 1:dims[2]){
    agemean[i] <- sum(age*agecalc[,i])
  }
  
  
  
  return(agemean)
}