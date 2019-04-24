### Load all the models and plot the results 
source('init_scripts.R')
load('MSErun.Rdata')

df <- load_data_seasons()

df$Catch <- Catch.obs$Fishery
sim.data <- run.agebased.true.catch(df)

# Rearrange some of the data 
###  Plot the true SSB ###
SSB.plot <- data.frame(SSB = rowSums(ls.save[[1]]$SSB), year = ls.save[[1]]$df$years, run = paste('run',1, sep=''))
Catch.plot <- data.frame(Catch = ls.save[[1]]$Catch, year = ls.save[[1]]$df$years, run = paste('run',1, sep=''))

for(i in 2:nruns){
  ls.tmp <- ls.save[[i]]  
  
  if(is.list(ls.tmp)){
    SSB.tmp <- data.frame(SSB = rowSums(ls.tmp$SSB), year = ls.tmp$df$years, run =  paste('run',i, sep=''))
    SSB.plot <- rbind(SSB.plot,SSB.tmp)
    
    Catch.tmp <- data.frame(Catch = ls.tmp$Catch, year = ls.tmp$df$years, run =  paste('run',i, sep=''))
    Catch.plot <- rbind(Catch.plot,Catch.tmp)
    
  }
}

p1 <- SSB.plot %>% 
  group_by(year) %>% 
  summarise(med = median(SSB), 
            p95 = quantile(SSB, 0.95),
            p25 = quantile(SSB, 0.25),
            p75 = quantile(SSB, 0.75), 
            p5 = quantile(SSB,0.05)) %>% 
  ggplot(aes(x= year,y = med)) +
  geom_ribbon(aes(ymin = p5, ymax = p95), fill = alpha('gray', alpha =0.5))+
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = alpha('gray', alpha =0.8))+
  theme_classic()+scale_y_continuous(name = 'SSB')+
  geom_line(color="black", size = 1.5)#+geom_line(data = SSB.plot, aes(y = SSB,group = run), color = alpha('black', alpha = 0.2))


p2 <- Catch.plot %>% 
  group_by(year) %>% 
  summarise(med = median(Catch), 
            p95 = quantile(Catch, 0.95),
            p25 = quantile(Catch, 0.25),
            p75 = quantile(Catch, 0.75), 
            p5 = quantile(Catch,0.05)) %>% 
  ggplot(aes(x= year,y = med)) +
  geom_ribbon(aes(ymin = p5, ymax = p95), fill = alpha('gray', alpha =0.5))+
  geom_ribbon(aes(ymin = p25, ymax = p75), fill = alpha('gray', alpha =0.8))+
  theme_classic()+scale_y_continuous(name = 'Catch')+
  geom_line(color="black", size = 1.5)#+geom_line(data = Catch.plot, aes(y = Catch,group = run), color = alpha('black', alpha = 0.2))

grid.arrange(p1,p2)





