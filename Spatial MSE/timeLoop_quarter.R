
for (space in 1:nspace){
  
  # Get the indices for the surrounding spaces
  if(((space-1) == 0)){
    spaceidx <- 2
  }
  if(space == nspace){
    spaceidx <- nspace-1
  }
  if(space > 1 & space < nspace){
    spaceidx <- c(space-1,space+1)
  }
  
  #   if (ssn == 1){
  R <- ((4*h*R_0[space]*SSB[idx-1,space])/
          (SSB_0[space]*(1-h)+ SSB[idx-1,space]*(5*h-1)))*exp(-0.5*1*SDR^2+Ry)*recruitmat[space,4]
  # 
  N.save.age[1,idx,space] <- R
  N.save.age[2:(nage-1),idx,space] <- N.save.age[1:(nage-2), idx-1,space]*exp(-Z[1:(nage-2)])*(1-movemat[space,1:(nage-2),ssn])-
    N.save.age[1:(nage-2), idx-1,space]*exp(-Z[1:(nage-2)])*(movemat[space,1:(nage-2),ssn])+ # Remove the ones that leave``
    N.save.age[1:(nage-2), idx-1,spaceidx]*exp(-Z[1:(nage-2)])*(movemat[spaceidx,1:(nage-2),ssn])# add the ones come to the surrounding areas
  
  N.save.age[nage, idx,space] <-  (N.save.age[nage-1, idx-1,space]*exp(-Z[nage-1])+N.save.age[nage, idx-1,space]*exp(-Z[nage]))*(1-movemat[space,nage,ssn])-
    (N.save.age[nage-1, idx-1,space]*exp(-Z[nage-1])+N.save.age[nage, idx-1,space]*exp(-Z[nage]))*(movemat[spaceidx,nage,ssn])+# Plus group
    (N.save.age[nage-1, idx-1,spaceidx]*exp(-Z[nage-1])+N.save.age[nage, idx-1,spaceidx]*exp(-Z[nage]))*(movemat[spaceidx,nage,ssn])# Plus group
  #    }else{
  #     # 
  #     N.save.age[,idx,space] <-N.save.age[, idx-1,space]*exp(-Z)*(1-movemat[space,,ssn])-
  #     N.save.age[, idx-1,space]*exp(-Z)*(movemat[space,,ssn])+ # Remove the ones that leave``
  #     N.save.age[, idx-1,spaceidx]*exp(-Z)*(movemat[spaceidx,,ssn])# add the ones come to the surrounding areas
  #        
  # }
  SSB[idx,space] <- sum(N.save.age[,idx,space]*Mat.sel)*0.5
  if(is.na(SSB[idx,space])){
    stop('SSB is NA')
  }
  
  Catch.age[,idx]  <- (Fyear/(Fyear+Myear))*(1-exp(-(Fyear+Myear)))*rowSums(N.save.age[,idx,])*w_catch # Calculate the catch in kg 
  Catch[idx] <- sum(Catch.age[,idx]) 
  