###### Plot potential movement
library(ggplot2)

nseason <- 5
age <- 0:20
nage <- length(age)

movemax <- seq(0.2,0.7, length.out = nseason)
movefifty <- 5
moveslope <- 0.7


movemat <- array(0, dim = c(nseason, nage)) # Chances of moving in to the other grid cell 


for(i in 1:nseason){
    movemat[i,] <- movemax[i]/(1+exp(-moveslope*(age-movefifty)))
    
}

df.plot <- as.data.frame(t(movemat))
names(df.plot) <- movemax
df.plot$age <- age

df.plot <- melt(df.plot, id.vars = 'age', measure.vars = 1:5, variable.name = 'maxslope', value.name = 'movement')


png('OM_movement.png', width = 16, height = 8, unit = 'cm', res = 600)

ggplot(df.plot, aes(x = age, y = movement, color = maxslope))+geom_line()+theme_classic()+scale_y_continuous(limit = c(0,1))+
  geom_vline(xintercept = movefifty, linetype = 2)
dev.off()
