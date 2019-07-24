## Plot the selectivity in the two selectivity scenarios 
source('getSelec.R')
source('load_data_seasons.R')

df <- load_data_seasons()
cols <- brewer.pal(6, 'Dark2')


psel.US <- c(0.05,0,0,0,0)  
psel.CAN <- df$parms$psel_fish+df$parms$PSEL[,ncol(df$parms$PSEL)]*df$sigma_psel
psel.baseline <- df$parms$psel_fish

sel.us <- getSelec(df$age,psel.US, df$Smin,df$Smax)
sel.CAN <- getSelec(df$age,psel.CAN, df$Smin,df$Smax)
sel.baseline <- getSelec(df$age, psel.baseline, df$Smin, df$Smax)

df.plot <- data.frame(age = rep(df$age,3), selectivity = c(sel.us,sel.CAN, sel.baseline), country = rep(c('USA', 'CAN','baseline'), each = df$nage))

p1 <- ggplot(df.plot, aes(x = age, y = selectivity, color = country))+theme_classic()+geom_line(size = 2)+
  scale_color_manual(values = cols[1:3])

png('Figs/selectivity/selecplot.png', width = 16, height = 12, res = 400, units = 'cm')
p1
dev.off()


