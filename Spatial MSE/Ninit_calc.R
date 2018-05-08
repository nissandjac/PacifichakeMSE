<<<<<<< HEAD


# Ntest <- matrix(NA,nage,nage+1)
# SSBtest <- matrix(NA, nage+1)
# SSBtest[1] <- SSB_0
# initN <- rev(Ninit_dev)
# Ntest[,1] <- N0
# 
# for (i in 2:(nage)){
#   
#   Ntest[1,i] = ((4*h*R0*SSBtest[i-1])/
#     (SSB_0*(1-h)+ SSBtest[i-1]*(5*h-1)))*exp(initN[i-1])
#   
#   Ntest[2:(nage-1),i] = Ntest[1:(nage-2),i-1]*exp(-M0)
#   Ntest[nage,i] =  Ntest[nage-1,i-1]*exp(-M0)+Ntest[nage,i-1]*exp(-M0)
#   SSBtest[i] <- sum(Ntest[,i]*df$Matsel)*0.5
#   
# }
# 
# Ninit <- Ntest[,nage]
=======


# Ntest <- matrix(NA,nage,nage+1)
# SSBtest <- matrix(NA, nage+1)
# SSBtest[1] <- SSB_0
# initN <- rev(Ninit_dev)
# Ntest[,1] <- N0
# 
# for (i in 2:(nage)){
#   
#   Ntest[1,i] = ((4*h*R0*SSBtest[i-1])/
#     (SSB_0*(1-h)+ SSBtest[i-1]*(5*h-1)))*exp(initN[i-1])
#   
#   Ntest[2:(nage-1),i] = Ntest[1:(nage-2),i-1]*exp(-M0)
#   Ntest[nage,i] =  Ntest[nage-1,i-1]*exp(-M0)+Ntest[nage,i-1]*exp(-M0)
#   SSBtest[i] <- sum(Ntest[,i]*df$Matsel)*0.5
#   
# }
# 
# Ninit <- Ntest[,nage]
>>>>>>> baeb649a73da2a87886096ac53b590dddb82de24
# 