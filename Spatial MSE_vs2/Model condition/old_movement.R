# movemat[1,3,] <- 0.1
# movemat[1,4,] <- 0.1
# movemat[1,5,] <- 0.2
# movemat[1,6:11,] <- 0.3
# movemat[1,12:nage,] <- 0.4
# movemat[1,,] <- movemat[1,,]
# 
# movemat[2,3,] <- 0.1
# movemat[2,4,] <- 0.1
# movemat[2,5,] <- 0.2
# movemat[2,6:11,] <- 0.2
# movemat[2,12:nage,] <- 0.25
# 
# # Rarely move south during the year
# movemat[1,3:nage,2:3] <- 0.05
# 
# # Force all the SSB to move south in the last season
# movemat[1,mat$mat > 0,nseason] <- 0.8
# movemat[2,mat$mat > 0,nseason] <- 0
# 
# movemat <- movemat*0.8
# Initial size distribution (make sure they add up to 1) 
