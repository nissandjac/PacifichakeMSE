plotValues <- function(est, data, name){
  
df.plot <- est 
df.plot$value <- 'estimated'

data.tmp <- data.frame(name = data$y, SE = NA, min = NA, max = NA, year = data$x, value = 'observed')
  
df.plot <- rbind(df.plot, data.tmp)
  
p1 <- ggplot(df.plot, aes(x = year, y = name, color = value))+
  geom_line(data = df.plot[df.plot$value == 'estimated',], size =1, col = 'black')+
  geom_point(data = df.plot[df.plot$value == 'observed',], size =2 )+ theme_bw()+
  geom_ribbon(data = df.plot[df.plot$value == 'estimated',],aes(ymin=min, ymax=max), alpha=0.2, linetype = 0)+
  scale_y_continuous(name)+
  theme(legend.position="none")
  
p1
return(p1)
}