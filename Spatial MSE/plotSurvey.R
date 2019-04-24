#### Plot the survey 
###### Initialize the operating model ###### 
library(TMB)
compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))
library(dplyr)
seedz <- 12345
set.seed(seedz)
# Run the simulation model
source('run_agebased_model_true_Catch.R')


####  Plotting and uncertainty calculation functions #### 
source('ylimits.R')
source('plotUncertainty.R')
source('getUncertainty.R')
source('plotValues.R')
source('run_multiple_MSEs.R')
source('getSelec.R') # Calculate hake selectivity

source('load_data_seasons.R') # Loads data for Operating model
source('create_TMB_data.R') # Compiles operating model data to tmb data

source('getRefpoint.R') # Calculate refrence points 
source('Check_Identifiable_vs2.R') # see if hessian is positive definite 

source('getParameters.R')
source('calcSSB0.R')

assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

parms.true <- getParameters(TRUE)
Catch.obs <- read.csv('hake_totcatch.csv')

df <- load_data_seasons(move = TRUE, nseason = 4, nspace = 2,
                          movemaxinit = 0.5, movefiftyinit = 5)

df$Catch <- Catch.obs$Fishery
time <- 1
yrinit <- df$nyear
nruns <- 100
seeds <- floor(runif(n = nruns, min = 1, max = 1e6))
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 
#
# df$parms$Rin <- df$parms$Rin*0
# df$F0 <- 0*df$F0

simyears <- 30 # Project 25 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df)
df.survey <- read.csv('survey_country_2.csv')
df.survey$Bio <- df.survey$Bio*1e-3 # Harmonize units

p1 <- ggplot(df.survey, aes(x = year, y = Bio/1e6, color = Country))+geom_line()+geom_point()+theme_classic()
p1



idx <- which(df$flag_survey == 1)
ny <- length(idx)

sf.us <- rep(NA, df$nyear)
sf.can <- rep(NA, df$nyear)

sf.us[idx] <- df.survey$Bio[df.survey$Country == 'USA']
sf.can[idx] <- df.survey$Bio[df.survey$Country == 'CAN']

# Spread it out on the survey
survey.plot <- data.frame(year = rep(df$years, 4), 
                          country = c(rep(c('USA','CAN'),  each = df$nyear),rep(c('USA','CAN'),  each = df$nyear)), 
                          survey = c(sf.us,sf.can, sim.data$survey.true[2,],
                                     sim.data$survey.true[1,]),
                          source = c(rep('observation',df$nyear*2),rep('simulation', df$nyear*2))
)

p.survey <- ggplot(data = survey.plot, aes(x = year, y = survey*1e-6, color = country))+
  geom_line(data = survey.plot[is.na(survey.plot$survey) == 0 & survey.plot$source == 'observation',])+
  geom_point(data = survey.plot[is.na(survey.plot$survey) == 0 & survey.plot$source == 'observation',])+
  geom_line(data = survey.plot[survey.plot$source == 'simulation',], linetype = 2)+
  theme_classic()+scale_y_continuous(name = 'survey biomass (millions tons)')+
  scale_x_continuous(limit = c(1994, 2018))


p.survey        

sss <- df.survey %>% 
  group_by(year) %>% 
  summarise(survey = sum(Bio))

plot(df$years[df$flag_survey == 1],df$survey[df$flag_survey == 1], ylim = c(1e5,6e6))
lines(sss$year,sss$survey)
points(df$years[df$flag_survey == 1],sim.data$survey[df$flag_survey == 1], col = 'red')
lines(df$years,colSums(sim.data$survey.true), col = 'red')

plot(df$years,sim.data$survey.true[1,]*1e-5, type = 'l', ylim = c(1,25), col = 'red')
points(df.survey$year[df.survey$Country == 'CAN'],df.survey$Bio[df.survey$Country == 'CAN']*1e-5, col = 'red')
lines(df$years,sim.data$survey.true[2,]*1e-5, type = 'l', ylim = c(1,25), col = 'blue')
points(df.survey$year[df.survey$Country == 'USA'],df.survey$Bio[df.survey$Country == 'USA']*1e-5, col = 'blue')
