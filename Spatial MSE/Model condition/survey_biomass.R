### Get the survey data 
library(ggplot2)
library(dplyr)

setwd('C:/Users/Nis/Dropbox/NOAA/Hake MSE/Acoustic surveys')


ls <- dir()

ls <- ls[grep(pattern = '.csv',x = ls)]

for(i in 1:length(ls)){
  
  df.tmp <- read.csv(ls[i], sep = '\t')
  
  if(i == 1){
  df.save <- df.tmp  
  }else{
    df.save <- rbind(df.save, df.tmp)
  }
  
  
}

df.save$Country <- NA
df.save$Country[df.save$Lat < 49] <- 'USA'
df.save$Country[df.save$Lat >= 49] <- 'CAN'


df.tot <- df.save %>% 
  group_by(year, Country) %>% 
  summarise( Bio = sum(wgt_total))


ggplot(df.tot, aes(x = year, y = Bio*1e-9, color = Country))+geom_line()+theme_bw()+scale_y_continuous('Survey biomass')


write.csv(df.tot,'survey_country.csv', row.names = FALSE)
