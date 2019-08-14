#### compile the age distributions from Aarons Excel file 
library(dplyr)
library(reshape2)
library(ggplot2)

df.can <- readxl::read_excel('data/FleetAgeComp_byNation.xlsx', sheet = 1, skip = 1)
df.can$Country <- 'CAN'
df.us <- readxl::read_excel('data/FleetAgeComp_byNation.xlsx', sheet = 2, skip = 1)
df.us$Country <- 'USA'

# Compare with old estimates
df.old <- read.csv("data/age_in_catch_obs.csv")

df.all <- rbind(df.can, df.us)
ages <- 1:15
age.idx <- grep('Age', names(df.all))
df.all$ac <- apply(df.all[,age.idx], MARGIN = 1, FUN = function(x){sum((x*ages))})

ggplot(df.all, aes(x = Year, y= ac, color = Country))+geom_line()+theme_classic()+
  geom_line(data = df.old, aes(x= as.numeric(year), y= am, color = Country), linetype = 2)
  
write.csv(df.all,file = 'data/ac_catch_new.csv', row.names = FALSE)
