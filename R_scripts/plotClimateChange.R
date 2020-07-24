library(reshape2)
library(cowplot)
library(RColorBrewer)
library(PacifichakeMSE)
library(patchwork)

simyears <- 30
plot.figures <- FALSE
df <- load_data_seasons()
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))

cincrease <- 0
mincrease <- 0

# Plot the way climate works
cincrease <- c(0,0.02,0.04)
mincrease <- c(0,0.005,0.02)

movemax <- matrix(NA, length(cincrease), simyears)
moveout <- matrix(NA, length(cincrease), simyears)

moveout[,1] <- df$moveout
movemax[,1] <- df$movemax[1]

for(j in 1:length(cincrease)){
  for(time in 2:simyears){

    movemax[j,time] <- movemax[j,time-1]+cincrease[j]
    moveout[j,time] <- moveout[j,time-1]-mincrease[j]


    if(movemax[j,time] >0.9){
      movemax[j,time] <- 0.9 # Not moving more than 90% out t

      if(moveout[j,time] <= 0.5){
        moveout[j,time] <- 0.5
      }
    }
  }
}

cols <- PNWColors::pnw_palette('Starfish',n = 4, type = 'discrete')[2:4]

df.tmp <- as.data.frame(t(movemax))
names(df.tmp) <- c('base \nscenario', 'moderate \nincrease ','high \nincrease')
df.tmp$year <- year.future[year.future > 2018]

df.plot <- melt(df.tmp, id.vars = 'year', measure.vars = 1:length(cincrease), value.name = 'movemax', variable.name = 'Scenario')

p1 <- ggplot(df.plot, aes(x = year, y = movemax, color = Scenario))+theme_classic()+geom_line(size = 1.4)+
  coord_cartesian(ylim = c(0.2,1))+
  scale_color_manual(values = cols)+
  scale_y_continuous('max movement rate')+
  theme(legend.position = 'top', legend.title = element_blank(),
        axis.line.x = element_blank(),
        axis.title.x.bottom = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.text = element_text(size = 10),
        legend.spacing.x =unit(0.001,'cm'))
p1


df.tmp <- as.data.frame(t(moveout))
names(df.tmp) <- c('base scenario', 'moderate increase','high increase')
df.tmp$year <- year.future[year.future > 2018]

df.plot <- melt(df.tmp, id.vars = 'year', measure.vars = 1:length(cincrease), value.name = 'moveout', variable.name = 'Scenario')
p2 <- ggplot(df.plot, aes(x = year, y = moveout, color = Scenario))+theme_classic()+geom_line(size = 1.4)+coord_cartesian(ylim = c(0.3,0.9))+
  scale_color_manual(values = cols)+scale_y_continuous('return rate')+
  theme(legend.position = 'none')

if(plot.figures == TRUE){
png('results/Figs/climate_movement.png', width= 8, height = 10, units = 'cm', res = 400)
p1 / p2 + plot_annotation(tag_levels = 'a')
dev.off()
}

# Make additional figure of how the movement changes based on climate

# Plot the movement rates
df.movement <- data.frame(age = rep(df$age, 8), movement = NA, country = rep(c('CAN','USA'), each = df$nage),
                          season = rep(1:4, each = df$nage*2))
df.movement$age[df.movement$country == 'CAN'] <-  df.movement$age[df.movement$country == 'CAN']+0.3 # For plotting
for(i in 1:df$nseason){
  mm.tmp <- df$movemat[,,i,1]

  df.movement[df.movement$season == i & df.movement$country == 'USA',]$movement <- mm.tmp[2,]
  df.movement[df.movement$season == i & df.movement$country == 'CAN',]$movement <- mm.tmp[1,]
}

#png('survey_comps.png', width = 16, height = 10, res= 400, unit = 'cm')
p.move <-ggplot(df.movement, aes(x = age, y = movement, color = country))+facet_wrap(~season)+theme_classic()+
  geom_line(size = 1.45)+scale_y_continuous(limit = c(0,1),name = 'movement rate')+
  scale_color_manual(values = c('darkred','blue4'))+
  theme(legend.title = element_blank(),legend.position = c(0.85,0.85), legend.direction = 'horizontal',
        legend.background = element_rect(fill=NA))

if(plot.figures == TRUE){
  png(filename = 'results/Figs/Movement.png', width = 16, height = 10, res = 400, units = 'cm')
  print(p.move)
  dev.off()

}




# Plot the movement rates
nplot <- 3 # Movement scenarios to include in plot
pyears <- 2019:2048
nyears <- length(pyears)


for(k in 1:nyears){

  df.med <- load_data_seasons(movemaxinit = movemax[2,k], moveout = moveout[2,k])
  df.high <- load_data_seasons(movemaxinit = movemax[3,k], moveout = moveout[3,k])

  for(i in 1:df$nseason){
    mm.tmp <- df$movemat[,,i,1]
    mm.med <- df.med$movemat[,,i,1]
    mm.high <- df.high$movemat[,,i,1]

    tmp  <-   data.frame(age = df$age, movement = mm.tmp[1,],
                         model = 'base', season = i, country = 'CAN', year = pyears[k])
    tmp.med <- data.frame(age = df$age, movement = mm.med[1,],
                          model = 'medium', season = i, country = 'CAN', year = pyears[k])
    tmp.high <- data.frame(age = df$age, movement = mm.high[1,],
                           model = 'high', season = i, country = 'CAN', year = pyears[k])

    tmp.us  <-   data.frame(age = df$age, movement = mm.tmp[2,],
                            model = 'base', season = i, country = 'USA', year = pyears[k])
    tmp.med.us <- data.frame(age = df$age, movement = mm.med[2,],
                             model = 'medium', season = i, country = 'USA', year = pyears[k])
    tmp.high.us <- data.frame(age = df$age, movement = mm.high[2,],
                              model = 'high', season = i, country = 'USA', year = pyears[k])

    if(i == 1 & k == 1){
      df.movement.all <- rbind(tmp,tmp.med,tmp.high, tmp.us, tmp.med.us,tmp.high.us)

    }else{
      df.movement.all <- rbind(df.movement.all, tmp.med,tmp.high, tmp.us, tmp.med.us,tmp.high.us)
    }

  }
}

#png('survey_comps.png', width = 16, height = 10, res= 400, unit = 'cm')
ggplot(df.movement.all[df.movement.all$year %in% c(2019) &
                   df.movement.all$model == 'base' & df.movement.all$country == 'USA' &
                   df.movement.all$season %in% 1:4,],
       aes(x = age, y = movement, color = factor(year),group = (year)))+
  geom_line(color = 'blue')+facet_wrap(~season)+theme_classic()+
  geom_line(data = df.movement.all[df.movement.all$year %in% c(2019) &
                          df.movement.all$model == 'high' & df.movement.all$country == 'CAN' &
                          df.movement.all$season %in% 1:4,], linetype = 2, color = 'red')+
  labs(color = 'year')+
  scale_y_continuous('movement\nrate')+
  geom_line(data = df.movement.all[df.movement.all$year %in% c(2048) &
                                 df.movement.all$model == 'medium' & df.movement.all$country == 'USA' &
                                 df.movement.all$season %in% 1:4,], color = 'blue', linetype = 2)+
  geom_line(data = df.movement.all[df.movement.all$year %in% c(2048) &
                                 df.movement.all$model == 'high' & df.movement.all$country == 'USA' &
                                 df.movement.all$season %in% 1,], color = 'blue', linetype = 3)+
  geom_line(data = df.movement.all[df.movement.all$year %in% c(2048) &
                                 df.movement.all$model == 'medium' & df.movement.all$country == 'CAN' &
                                 df.movement.all$season %in% 1:4,], color = 'red', linetype = 2)+
  geom_line(data = df.movement.all[df.movement.all$year %in% c(2048) &
                                 df.movement.all$model == 'high' & df.movement.all$country == 'CAN' &
                                 df.movement.all$season %in% 1,], color = 'red', linetype = 3)


 #  scale_colour_brewer(palette = "Greys")+

# Other figures
# Text for publication
ann_mm <- data.frame(age = 4,movement = 0.75, season = 1, country = 'USA')
ann_return <- data.frame(age = 10,movement = 0.35, season = 4, country = 'USA')

df.rib <- data.frame(ymin = df.movement$movement,
                     ymax = df.rib$movement[df.rib$model == 'high'],
                     season = df.rib$season[df.rib$model == 'base'],
                     age = df.rib$age[df.rib$model == 'base'],
                     movement =  df.rib$movement[df.rib$model == 'base'], country = df.rib$country[df.rib$model == 'high'])


#png('survey_comps.png', width = 16, height = 10, res= 400, unit = 'cm')
p.move <-ggplot(df.movement, aes(x = age, y = movement, color = country))+facet_wrap(~season)+theme_classic()+
  geom_line(size = 1.45)+scale_y_continuous(limit = c(0,1),name = 'movement rate')+
  scale_color_manual(values = c('darkred','blue4'))+
  theme(legend.title = element_blank(),legend.position = c(0.1,0.3), legend.direction = 'vertical',
        legend.background = element_rect(fill=NA))+
  geom_hline(data = data.frame(season = 1),
             aes(yintercept = max(df.movement$movement[df.movement$country == 'USA' & df.movement$season == 1])),
             linetype = 2)+
  geom_text(data = ann_mm, label = 'max\n movement', color = 'black')+
  geom_text(data = ann_return, label = 'return\n rate', color = 'black')+
  geom_segment(data = data.frame(x= 4, y= 0.57, xend = 4, yend = 0.38, season = 1),
               aes(x=x,  y= y, xend = xend, yend=yend), color = 'black',
               arrow = arrow(length = unit(0.1, "inches"))
  )+
  geom_segment(data = data.frame(x= 10, y= 0.50, xend = 10, yend = 0.80, season = 4),
               aes(x=x,  y= y, xend = xend, yend=yend), color = 'black',
               arrow = arrow(length = unit(0.1, "inches"))
  )+
  geom_ribbon(data = df.rib, aes(x = age,ymin = ymin, ymax=ymax), fill = 'gray', alpha = 0.4, linetype =)+guides(fill = NA)

p.move
