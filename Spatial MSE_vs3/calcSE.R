calcSE <- function(ls.MSE){
  
  
  # Calculate the error on SSB
  nruns <- length(ls.MSE)
  nyear <- length(ls.MSE[[1]]$SSB.hes$year)
  
  df.E <- data.frame(run = rep(1:nruns,each =nyear),
                               SE.SSB = NA, year = rep(ls.MSE[[1]]$SSB.hes$year,nruns))
  
  for(i in 1:nruns){
  ls.tmp <- ls.MSE[[i]]
  
  if(is.na(ls.tmp[1]) != 1){
  SSB.true <- rowSums(ls.tmp$SSB)
  SSB.est <- ls.tmp$SSB.hes$name
  
  E.tmp <- (SSB.est-SSB.true)/SSB.true
  
  df.E[df.E$run == i,]$SE.SSB <- E.tmp
    }
  }
  
  return(df.E)
}