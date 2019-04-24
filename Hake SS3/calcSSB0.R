### Calculate the Spawning biomass at equilibrium 
calcSSB0 <- function(R0,Z,nage, mat){

age <- 1:nage

M <- rep(Z,nage)

N0 <- NA
N0[1] <- R0
N0[2:(nage-1)] <-R0 * exp(-M[1:(nage-2)]*age[1:(nage-2)])
N0[nage] <- R0*exp(-(M[nage-1]*age[nage-1]))/(1-exp(-M[nage]))# Plus group (ignore recruitment dev's in first year )


SSB.age <- mat*N0*0.5 # In G
SSB_0 <- sum(SSB.age)



return(SSB_0)

}