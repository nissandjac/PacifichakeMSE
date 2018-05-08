<<<<<<< HEAD
# plotSpace 


# plot the spatial pattern from the operating model 

load('simulated_space_OM.Rdata') # sim.data 
load('sim_data_parms.Rdata')

N.save <- sim.data$Nsave.all
N.save.mean <- apply(N.save, c(1,3,4), FUN = mean)

age <- df$age

plot(df$age, N.save[,30,1,4]/N.save[,30,2,4])


=======
# plotSpace 


# plot the spatial pattern from the operating model 

load('simulated_space_OM.Rdata') # sim.data 
load('sim_data_parms.Rdata')

N.save <- sim.data$Nsave.all
N.save.mean <- apply(N.save, c(1,3,4), FUN = mean)

age <- df$age

plot(df$age, N.save[,30,1,4]/N.save[,30,2,4])


>>>>>>> baeb649a73da2a87886096ac53b590dddb82de24
