### test h prior 
library(ggplot2)
hmin <- 0.2
hmax <- 1
hprior <- 0.777
hsd <- 0.117

mu <- (hprior-hmin)/(hmax-hmin)
tau <- ((hprior-hmin)*(hmax-hprior))/hsd^2-1


Bprior= tau*mu
Aprior = tau*(1-mu)
Pconst <- 0.000001
hrange <- seq(0.2,1, length.out = 10000)

Prior_Like =  (1.0-Bprior)*log(Pconst+hrange-hmin) + 
  (1.0-Aprior)*log(Pconst+hmax-hrange)-
  (1.0-Bprior)*log(Pconst+hprior-hmin) - 
  (1.0-Aprior)*log(Pconst+hmax-hprior)

logL <- dbeta(hrange, Bprior,Aprior, TRUE)

plot(hrange,-logL, type = 'l', ylim = c(-5,5))
lines(hrange,Prior_Like)
lines(rep(hprior,10),seq(-100,11, length.out = 10), lty = 2)
points(hrange[which.min(-logL)],-logL[which.min(-logL)])
points(hrange[which.min(-logL)],Prior_Like[which.min(Prior_Like)])

# 
betaf <- function(parms){
  
  
}


optimize()