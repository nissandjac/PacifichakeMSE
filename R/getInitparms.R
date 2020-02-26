


N1 <- (read.csv('N1.csv'))[,1] # Age distribution in year 1965 from base model (MLE)
mat <- read.csv('maturity.csv')[,2]
Ndev <- read.csv('initN.csv', header = F)[,2] # Initial recruitment deviations from base model (MCMC)
R0 <- exp(14.8353) # R0 from the base model (MCMC)
nage <- 21
M  <- 0.23 # From base model (MCMC)
Mage <- cumsum(rep(M, nage)) # More accurate than multiplying by age (if theres mortality difference between ages)
h <- 0.812 # From base model (MCMC)
Rdev <- rep(0,nage)
# Initial distribution 
N0 <- matrix(NA, nage)  
N0 <- R0
N0[2:(nage-1)] <-R0 * exp(-Mage[1:(nage-2)])
N0[nage] <- R0*exp(-(Mage[nage-1]))/(1-exp(-M))

runTime <- function(time,Rdev){
  N <- matrix(NA,nage,nage)
  SSB <- matrix(NA,nage)
  N[,1] <- N0 # Age distribution at EQ
  SSB[1] <- sum(N0*data$mat)*0.5
  
  for (i in 2:(time)){
      N[1,i] = (4*h*R0*SSB[i-1])/(SSB[1]*(1-h)+ SSB[i-1]*(5*h-1))*exp(Rdev)
      N[2:(nage-1),i] = N[1:(nage-2),i-1]*exp(-M)
      N[nage,i] =  N[nage-1,i-1]*exp(-M)+N[nage,i-1]*exp(-M)
      SSB[i] <- sum(N[,i]*mat)*0.5 
  }
return(list(N = N, SSB = SSB))
}

# Get the parms to get Ninit to 0! 

# Compare with Nint from MCMC 

plot(opt$par)
points((Ndev), col = 'red')
lines(rep(0,nage))

