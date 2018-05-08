<<<<<<< HEAD
timeLoop <- function(nyear = 1, nspace = 2, nage = NA, F0 = 0, M = 0.2, Msel = NA, SSBinit, Ninit,
                     Ry.in = 0,
                     )
  # if (is.na(nage)){
  #   stop('Number of ages lacking')
  # }
  # 
  # if(length(F0 == 1)){
  #   F0 <- rep(F0,nyear)
  # }
  # 
  # if(length(M == 1)){
  #   F0 <- rep(M,nyear)
  # }

SSB2 <- matrix(NA,nyear+1, nspace)
Catch2 <- matrix(NA,nyear+1)
Catch.age2 <- matrix(NA,nage,nyear+1)
R.save2 <- matrix(NA,nyear+1, nspace)

N.save.age2 <- array(NA,dim = c(nage,nyear+1, nspace))


# Distribute over space 

for (space in 1:nspace){
  N.save.age2[,1,space] <- Ninit*move.init[space,] # Just to initialize 
  SSB2[1,space] <- sum(N.save.age2[,1,space]*Mat.sel, na.rm = T)*0.5
}


Catch.age2[,1] <- 0 # Assumed no fishing before data started 
Catch2[1]<- 0
  
for (yr in 2:(nyear+1)){ # Loop over years add one year for initial distribution
  
  # Calculate the mortality at length
  Fyear <- F0[yr]*Fsel
  Myear <- M*Msel # Natural mortality 
  Z <- Fyear+Myear
  # Assume that recruitment is in the beginning of the year (bc why not)
  # Recruitment
  
  if (year[yr-1] < 2018){
    Ry <- df$parms$Rin[yr-1]
  }else{
    Ry <- rnorm(1,mean = 0, sd = sd.rec)
  }
  
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
    
    R <- ((4*h*R_0[space]*SSB2[yr-1,space])/
            (SSB_0[space]*(1-h)+ SSB2[yr-1,space]*(5*h-1)))*exp(-0.5*1*SDR^2+Ry)*recruitmat[space,4]
    # 
    N.save.age2[1,yr,space] <- R
    N.save.age2[2:(nage-1),yr,space] <- N.save.age2[1:(nage-2), yr-1,space]*exp(-Z[1:(nage-2)])*(1-movemat[space,1:(nage-2),1])-
      N.save.age2[1:(nage-2), yr-1,space]*exp(-Z[1:(nage-2)])*(movemat[space,1:(nage-2),1])+ # Remove the ones that leave``
      N.save.age2[1:(nage-2), yr-1,spaceidx]*exp(-Z[1:(nage-2)])*(movemat[spaceidx,1:(nage-2),1])# add the ones come to the surrounding areas
    
    N.save.age2[nage, yr,space] <-  (N.save.age2[nage-1, yr-1,space]*exp(-Z[nage-1])+N.save.age2[nage, yr-1,space]*exp(-Z[nage]))*(1-movemat[space,nage,1])-
      (N.save.age2[nage-1, yr-1,space]*exp(-Z[nage-1])+N.save.age2[nage, yr-1,space]*exp(-Z[nage]))*(movemat[spaceidx,nage,1])+# Plus group
      (N.save.age2[nage-1, yr-1,spaceidx]*exp(-Z[nage-1])+N.save.age2[nage, yr-1,spaceidx]*exp(-Z[nage]))*(movemat[spaceidx,nage,1])# Plus group
    
    SSB2[yr,space] <- sum(N.save.age2[,yr,space]*Mat.sel)*0.5
    
    if(is.na(SSB2[yr,space])){
      stop('SSB is NA')
    }
    
  }
  
}
=======
timeLoop <- function(nyear = 1, nspace = 2, nage = NA, F0 = 0, M = 0.2, Msel = NA, SSBinit, Ninit,
                     Ry.in = 0,
                     )
  # if (is.na(nage)){
  #   stop('Number of ages lacking')
  # }
  # 
  # if(length(F0 == 1)){
  #   F0 <- rep(F0,nyear)
  # }
  # 
  # if(length(M == 1)){
  #   F0 <- rep(M,nyear)
  # }

SSB2 <- matrix(NA,nyear+1, nspace)
Catch2 <- matrix(NA,nyear+1)
Catch.age2 <- matrix(NA,nage,nyear+1)
R.save2 <- matrix(NA,nyear+1, nspace)

N.save.age2 <- array(NA,dim = c(nage,nyear+1, nspace))


# Distribute over space 

for (space in 1:nspace){
  N.save.age2[,1,space] <- Ninit*move.init[space,] # Just to initialize 
  SSB2[1,space] <- sum(N.save.age2[,1,space]*Mat.sel, na.rm = T)*0.5
}


Catch.age2[,1] <- 0 # Assumed no fishing before data started 
Catch2[1]<- 0
  
for (yr in 2:(nyear+1)){ # Loop over years add one year for initial distribution
  
  # Calculate the mortality at length
  Fyear <- F0[yr]*Fsel
  Myear <- M*Msel # Natural mortality 
  Z <- Fyear+Myear
  # Assume that recruitment is in the beginning of the year (bc why not)
  # Recruitment
  
  if (year[yr-1] < 2018){
    Ry <- df$parms$Rin[yr-1]
  }else{
    Ry <- rnorm(1,mean = 0, sd = sd.rec)
  }
  
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
    
    R <- ((4*h*R_0[space]*SSB2[yr-1,space])/
            (SSB_0[space]*(1-h)+ SSB2[yr-1,space]*(5*h-1)))*exp(-0.5*1*SDR^2+Ry)*recruitmat[space,4]
    # 
    N.save.age2[1,yr,space] <- R
    N.save.age2[2:(nage-1),yr,space] <- N.save.age2[1:(nage-2), yr-1,space]*exp(-Z[1:(nage-2)])*(1-movemat[space,1:(nage-2),1])-
      N.save.age2[1:(nage-2), yr-1,space]*exp(-Z[1:(nage-2)])*(movemat[space,1:(nage-2),1])+ # Remove the ones that leave``
      N.save.age2[1:(nage-2), yr-1,spaceidx]*exp(-Z[1:(nage-2)])*(movemat[spaceidx,1:(nage-2),1])# add the ones come to the surrounding areas
    
    N.save.age2[nage, yr,space] <-  (N.save.age2[nage-1, yr-1,space]*exp(-Z[nage-1])+N.save.age2[nage, yr-1,space]*exp(-Z[nage]))*(1-movemat[space,nage,1])-
      (N.save.age2[nage-1, yr-1,space]*exp(-Z[nage-1])+N.save.age2[nage, yr-1,space]*exp(-Z[nage]))*(movemat[spaceidx,nage,1])+# Plus group
      (N.save.age2[nage-1, yr-1,spaceidx]*exp(-Z[nage-1])+N.save.age2[nage, yr-1,spaceidx]*exp(-Z[nage]))*(movemat[spaceidx,nage,1])# Plus group
    
    SSB2[yr,space] <- sum(N.save.age2[,yr,space]*Mat.sel)*0.5
    
    if(is.na(SSB2[yr,space])){
      stop('SSB is NA')
    }
    
  }
  
}
>>>>>>> baeb649a73da2a87886096ac53b590dddb82de24
