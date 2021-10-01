load('inst/extdata/quarterly_catch.Rdata')


catch.df <- catch2a4s %>% 
  pivot_longer(1:2, names_to = 'country', values_to = 'catch')

catch.df.mean <- catch.df[catch.df$year %in% 2008:2017,] %>% 
  group_by(seas, country) %>% 
  summarise(catchmean = mean(catch))


sumUS <- sum(catch.df.mean[catch.df.mean$country == 'US',]$catchmean)
sumCAN <- sum(catch.df.mean[catch.df.mean$country == 'CAN',]$catchmean)

catch.df.mean$catchrel <- NA
catch.df.mean$catchrel[catch.df.mean$country == 'US'] <- catch.df.mean$catchmean[catch.df.mean$country == 'US']/sumUS
catch.df.mean$catchrel[catch.df.mean$country == 'CAN'] <- catch.df.mean$catchmean[catch.df.mean$country == 'CAN']/sumCAN  

print(catch.df.mean)

# Plot relative seasonal catch over time 

pcatch <- ggplot(catch.df, aes(x = year,  y = catch, color = country))+geom_line()+
                   facet_wrap(~seas, scales = 'free_y')+theme_classic()+theme(legend.position = c(0.1,0.8),
                                                                              legend.title = element_blank())+
  scale_color_manual(values = c('darkred','blue'))

pcatch

p1 <- ggplot(catch.df.mean, aes(x = seas, y = catchrel, fill = country, group = country))+geom_col(position = 'dodge')+
  theme_classic()+
  scale_fill_manual(values = c('darkred','blue'))+
  scale_y_continuous('average relative\ncatch (2008-2017)', expand = c(0,0))+
  scale_x_continuous('season',expand = c(0,0))+
  theme(legend.position = c(0.2, 0.7),
        legend.title = element_blank())
  

png('results/Climate/Publication/Resubmission/Supplementary/relative_catch.png', width = 8, height = 8,
    res = 400, units = 'cm')
p1
dev.off()
