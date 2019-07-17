##### Find the best movement rates for the conditioned om

###### Initialize the operating model ###### 
library(TMB)
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(gridExtra)
library(dplyr)
library(cowplot)

plot.figures = FALSE # Set true for printing to file 
# Run the simulation model
source('load_files_OM.R')
source('load_files.R')
source('run_agebased_model_true_catch_move.R')
source('runfuture_OM.R')
source('load_data_seasons_future.R')
source('calcMeanAge.R')
assessment <- read.csv('data/asssessment_MLE.csv')
assessment$move <- 3
assessment$SSB <- assessment$SSB*2

assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]
survey.obs <- read.csv('data/survey_country_2.csv')
survey.obs <- read.csv('data/df_survey.csv')
catch.ac.obs <- read.csv('data/age_in_catch_obs.csv')

### Add movement dependent on temperature 

temp <- read.csv('data/temp100_annual.csv')

tempdf <- data.frame(year = 1966:2019, anomaly = NA)

tempdf$anomaly[tempdf$year %in% temp$year] <- temp$temp100_anomaly
tempdf$anomaly[tempdf$year %in% 1966:1994] <- 0

for(i in 1:nrow(tempdf)){
  if(is.na(tempdf$anomaly[i])){
    tempdf$anomaly[i] <- tempdf$anomaly[i-1]
  }
}

source('load_data_seasons_climate.R')


yr.future <- 2

df <- load_data_seasons_climate(yr.future, movemaxinit = 0.5, movefiftyinit = 10, anomaly = tempdf$anomaly)

sim.data <- run.agebased.true.catch(df, 123)
sim.data.move <- run.agebased.true.catch.move(df, 123)
#sim.data <- sim.data.move

# Non spatial parameters 
df.us <- data.frame(SSB = c(sim.data$SSB[,2],sim.data.move$SSB[,2]), move = rep(c(1,0), each = df$nyear), 
                    Country = "USA", year = rep(df$years,2)) 
df.can <- data.frame(SSB = c(sim.data$SSB[,1],sim.data.move$SSB[,1]), move = rep(c(1,0), each = df$nyear), 
                     Country = "CAN", year = rep(df$years,2)) 
df.plot <- rbind(df.us,df.can)

ggplot(df.plot[df.plot$move ==1,], aes(x = year, y = SSB, color = Country))+geom_line()+theme_bw()+
  geom_line(data = df.plot[df.plot$move ==0,], linetype =2)


## Total biomass
df.plot <- df.plot %>% 
  group_by(year,move) %>% 
  summarise(SSB = sum(SSB))

ggplot(df.plot, aes(x = year, y = SSB, group = move))+geom_line()+theme_bw()+
  geom_line(data = assessment)

# Age in the catch 
df.move <- data.frame(AC = c(calcMeanAge(sim.data$age_comps_catch_space[,,2], df$age_maxage),
                             calcMeanAge(sim.data$age_comps_catch_space[,,1], df$age_maxage)),
                      Country = rep(c('USA','CAN'), each = df$nyear),
                      year = rep(df$years,2),
                      move = 1)
df.2 <- data.frame(AC = c(calcMeanAge(sim.data.move$age_comps_catch_space[,,2], df$age_maxage),
                          calcMeanAge(sim.data.move$age_comps_catch_space[,,1], df$age_maxage)),
                   Country = rep(c('USA','CAN'), each = df$nyear),
                   year = rep(df$years,2),
                   move = 0)

df.plot <- rbind(df.move,df.2)
ggplot(df.move, aes(x = year, y = AC, color = Country))+geom_line()+theme_bw()+
  geom_line(data = df.2, linetype = 2)+
  geom_line(data = catch.ac.obs, aes(y = am), linetype = 3)+
  geom_point(data = catch.ac.obs, aes(y = am), size = 2)

# Age in the survey 

df.s1 <- data.frame(AC = c(calcMeanAge(sim.data$age_comps_country[,,2], df$age_maxage),
                           calcMeanAge(sim.data$age_comps_country[,,1], df$age_maxage)),
                    Country = rep(c('USA','CAN'), each = df$nyear),
                    year = rep(df$years,2),
                    move = 1)
df.s2 <- data.frame(AC = c(calcMeanAge(sim.data.move$age_comps_country[,,2], df$age_maxage),
                           calcMeanAge(sim.data.move$age_comps_country[,,1], df$age_maxage)),
                    Country = rep(c('USA','CAN'), each = df$nyear),
                    year = rep(df$years,2),
                    move = 0)

df.plot <- rbind(df.s1,df.s2)

ggplot(df.s1, aes(x = year, y = AC, color = Country))+geom_line()+theme_bw()+
  geom_line(data = df.s2, linetype = 2)+
  geom_line(data = survey.obs, aes(y = am), linetype = 3)+
  geom_point(data = survey.obs, aes(y = am), size = 2)+scale_y_continuous(limit = c(3,10))


# Biomas in the survey 

df.b1 <- data.frame(Bio = c(sim.data$surv.tot[,2], sim.data$surv.tot[,1]),
                    Country = rep(c('USA','CAN'), each = df$nyear), move = 1,
                    year = rep(df$years,2))


df.b2 <- data.frame(Bio = c(sim.data.move$surv.tot[,2], sim.data.move$surv.tot[,1]),
                    Country = rep(c('USA','CAN'), each = df$nyear), move = 0,
                    year = rep(df$years,2))

ggplot(df.b1, aes(x = year, y = Bio, color = Country))+theme_bw()+geom_line()+
  geom_line(data = df.b2, linetype = 2)+
  geom_point(data = survey.obs, aes(y = Biomass*1e-2, group = Country))+
  geom_line(data = survey.obs, aes(y = Biomass*1e-2, group = Country))

# 
# minParam <- function(data, par){
#   #with(data,
#   df <- data$df
#   
#   df <- load_data_seasons_future(yr.future, movemaxinit = par[1], movefiftyinit = par[2])
#   sim.data <- run.agebased.true.catch(df, 123)
#   
#   df.move <- data.frame(AC = c(calcMeanAge(sim.data$age_comps_catch_space[,,2], df$age_maxage),
#                                calcMeanAge(sim.data$age_comps_catch_space[,,1], df$age_maxage)),
#                         Country = rep(c('USA','CAN'), each = df$nyear),
#                         year = rep(df$years,2),
#                         move = 1)
#   catch.ac.est <- calcMeanAge(sim.data$age_comps_catch_space[,,2], 15)
#   ans1 <- sum((catch.ac.est[df$years %in% catch.ac.obs$year]-catch.ac.obs$am[catch.ac.obs$Country == 'US'])^2)
#   catch.ac.est <- calcMeanAge(sim.data$age_comps_catch_space[,,1], 15)
#   ans2 <- sum((catch.ac.est[df$years %in% catch.ac.obs$year]-catch.ac.obs$am[catch.ac.obs$Country == 'Can'])^2)
#   
#   
#   
# 
# }



