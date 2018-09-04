### Run the Operating model and make some relevant plots 

direc <- "~/GitHub/PacifichakeMSE/Spatial MSE"
setwd(direc)
###### Initialize the operating model ###### 

seedz <- 125
set.seed(seedz)
dyn.load(dynlib("runHakeassessment2"))
# Run the simulation model
source('run_agebased_model_true_seasons.R')
source('ylimits.R')
source('SSB0calc.R')
source('getSelec.R')
source('load_data_seasons.R')
source('create_TMB_data.R')
source('SSB0calc.R')
assessment <- read.csv('asssessment_MLE.csv')
assessment <- assessment[assessment$year > 1965 &assessment$year < 2018 ,]

df <- load_data_seasons()
df$logSDR <- -Inf

time <- 1
yrinit <- df$nyear
### Run the OM and the EM for x number of years in the MSE 
### Set targets for harvesting etc 
# df$parms$initN <- df$parms$initN*0
# df$parms$Rin <- df$parms$Rin*0
# df$F0 <- 0*df$F0

simyears <- 25 # Project 30 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true_seasons(df)
simdata0 <- sim.data # The other one is gonna get overwritten. 

plot(rowSums(sim.data$SSB)/sum(sim.data$SSB_0), type = 'l', ylab = 'SSB/SSB_0')
# 
# save(sim.data,file = 'simulated_space_OM.Rdata')
# save(df,file = 'sim_data_parms.Rdata')

S.year.future <- seq(2019,2019+simyears, by = 2)
# Before the MSE starts 
##
#SSB.test.om <- list() # Test if SSB is the same in the OM

#
for (time in 1:simyears){

  year <- yrinit+(time-1)



  if (time > 1){

    if(sum(year.future[year] == S.year.future)>0){
      df$flag_survey <- c(df$flag_survey,1)
      df$survey_x <- c(df$survey_x,2)
      # df$ss_catch <- c(df$ss_catch,ceiling(mean(df$ss_catch[df$ss_catch > 0])))
      df$ss_survey <- c(df$ss_survey,ceiling(mean(df$ss_survey[df$ss_survey > 0])))
      df$survey_err <- c(df$survey_err,mean(df$survey_err[df$survey_err < 1]))

    }else{
      df$flag_survey <- c(df$flag_survey,-1)
      df$survey_x <- c(df$survey_x,-2)
      df$ss_survey <- c(df$ss_survey,-1)
      df$survey_err <- c(df$survey_err,1)
    }

    df$ss_catch <- c(df$ss_catch,ceiling(mean(df$ss_catch[df$ss_catch > 0])))
    df$flag_catch <- c(df$flag_catch,1)
    df$years <- year.future[1:year]
    df$nyear <- length(df$years)


    sim.data <- run.agebased.true_seasons(df, seedz)


  }

  df.new <- create_TMB_data(sim.data, df)

  # Fsel <- getSelec(df$age,rep$par.fixed[names(rep$par.fixed) == 'psel_fish'], df$Smin, df$Smax)
  # F40 <- referencepoints(SSB$name[length(SSB$name)])
  #
  Fnew <- 0 # Just fish at 0.3 into the future


  # Update the data data frame
  df$F0 <- c(df$F0,Fnew)
  Ntmp <- sim.data$Nout
  df$tEnd <- df$tEnd+1 # Just run one more year in subsequent runs
  df$wage_catch <- df.new$wage_catch
  df$wage_survey <- df.new$wage_survey
  df$wage_mid <- df.new$wage_mid
  df$wage_ssb <- df.new$wage_ssb


}

nyear <- df$nyear
years <- df$years
nseason <- df$nseason

idx <- seq(1,nyear*nseason, by = nseason)

SSB.all <- data.frame(time = seq(years[1], years[nyear],
                                 by = 1/df$nseason),
                                 SSBcan = NA,
                                 SSBus = NA  
                      )

for(i in 1:(nyear-1)){
  SSB.all$SSBcan[idx[i]:(idx[i+1]-1)] <- sim.data$SSB.all[i,,1]
  SSB.all$SSBus[idx[i]:(idx[i+1]-1)] <- sim.data$SSB.all[i,,2]
  
}


#png(file = 'OM_output.png',width = 800, height = 400)
par(mfrow = c(2,2), mar = c(5,4,1,1))
plot(SSB.all$time,SSB.all$SSBus, type = 'l', col = 'blue', ylim = c(40000, 2500000),
     ylab = 'Spawning biomass', xlab = 'years', lwd = 1)
lines(SSB.all$time, SSB.all$SSBcan, col = 'red', lwd = 1)
lines(assessment$year,assessment$SSB, col = 'green', lwd = 2)
#lines(SSB.all$time, SSB.all$SSBcan+SSB.all$SSBus, col = 'gray')
lines(df$years, rowSums(sim.data$SSB), lwd = 2)
# Plot the average age in each country 
# plot(df$years,sim.data$SSB[,1], type = 'l', col = 'blue', ylim = c(40000, 2500000),
#            ylab = 'Spawning biomass', xlab = 'years')
# lines(SSB.all$time, SSB.all$SSBcan, col = 'red')
# lines(assessment$year,assessment$SSB, col = 'green')
# lines(df$years,rowSums(sim.data$SSB))



age.comps.can <- sim.data$age_comps_OM[,2:df$nyear,1,2]
am.can <- matrix(NA, df$nyear)
for(i in 1:(df$nyear-1)){
  am.can[i] <- sum(df$age*age.comps.can[,i])
  
}

age.comps.US <- sim.data$age_comps_OM[,2:df$nyear,2,2]
am.US <- matrix(NA, df$nyear)
for(i in 1:(df$nyear-1)){
  am.US[i] <- sum(df$age*age.comps.US[,i])
  
}

plot(df$years,am.US, type = 'l', ylab = 'average age', xlab = 'year', col = 'blue', ylim = c(0,5))
lines(df$years,am.can, type = 'l', col = 'red')

# Average size in Catch 
# Plot the average age in each country 
Catch.save.age.can <- apply(sim.data$Catch.save.age[,,1,],MARGIN = c(1,2), FUN = sum )
Catch.save <- apply(sim.data$Catch.save.age,MARGIN = c(2,3), FUN = sum )

am.can.catch <- matrix(NA, df$nyear)
for(i in 1:(df$nyear-1)){
  
  ac.tmp <- rep(NA,df$age_maxage)
  ac.tmp[1:(df$age_maxage-1)] <-  Catch.save.age.can[2:(df$age_maxage),i]/Catch.save[i,1]
  ac.tmp[df$age_maxage] <- sum(Catch.save.age.can[(df$age_maxage+1):df$nage,i])/Catch.save[i,1]
  
  am.can.catch[i] <- sum(df$age[1:df$age_maxage]*ac.tmp)
  
}

Catch.save.age.US <- apply(sim.data$Catch.save.age[,,2,],MARGIN = c(1,2), FUN = sum )
Catch.save <- apply(sim.data$Catch.save.age,MARGIN = c(2,3), FUN = sum )

am.US.catch <- matrix(NA, df$nyear)
for(i in 1:(df$nyear-1)){
  
  ac.tmp <- rep(NA,df$age_maxage)
  ac.tmp[1:(df$age_maxage-1)] <-  Catch.save.age.US[2:(df$age_maxage),i]/Catch.save[i,2]
  ac.tmp[df$age_maxage] <- sum(Catch.save.age.US[(df$age_maxage+1):df$nage,i])/Catch.save[i,2]
  
  am.US.catch[i] <- sum(df$age[1:df$age_maxage]*ac.tmp)
  
}

plot(df$years,am.US.catch, type = 'o', ylab = '', xlab = 'year', col = 'blue', ylim = c(3,8))
lines(df$years,am.can.catch, type = 'l', col = 'red')
title(ylab='average age\n in catch', line=2)

# Catches in the two countries 
YL <- ylimits(Catch.save[,1],Catch.save[,2])
plot(df$years,Catch.save[,1], col ='red', type = 'l', ylim = YL, ylab = 'Catch per country', xlab = 'years')
lines(df$years,Catch.save[,2], col = 'blue')

dev.off()
# Plot map distribution over time 

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(raster)

states    <- c('Oregon', 'Washington','California')
provinces <- c("British Columbia")

us <- getData("GADM",country="USA",level=1)
canada <- getData("GADM",country="CAN",level=1)

us.states <- us[us$NAME_1 %in% states,]
ca.provinces <- canada[canada$NAME_1 %in% provinces,]

us.bbox <- bbox(us.states)
ca.bbox <- bbox(ca.provinces)

p1 <- ggplot(us.states,aes(x=long,y=lat,group=group))+
  geom_path()+
  geom_path(data=ca.provinces)+
  coord_map()+theme_classic()+scale_x_continuous(limit = c(-143,-121),expand=c(0,0), name = '')+
  geom_line(data = data.frame(long = seq(-146,-120, length.out = 100),
                              lat = rep(49, 100)), group = NA, linetype = 2)+ 
  scale_y_continuous(expand=c(0,0), limit = c(35,60), name = '')+
  geom_text(aes(x=-135, y = 40), label = 'USA')+  geom_text(aes(x=-135, y = 50), label = 'Canada')


# Test the density plot 
png(filename = 'CanUSA.png', width = 10, height = 16,units = 'cm', res = 400)
p1
dev.off()

ntime <- length(SSB.all$time)


df.new <- data.frame(SSB = c(SSB.all$SSBcan, SSB.all$SSBus), 
                     group = rep(c('can','usa'), each= ntime),
                     time = rep(SSB.all$time,2),
                     lat = rep(c(55,41), each = ntime),
                     long = rep(c(-137,-133),each = ntime))


## Create bins 
nbins <- 6
bns <- round_any(seq(min(df.new$SSB, na.rm= T), max(df.new$SSB, na.rm= T), length.out = nbins),100000)
szs <- seq(3,30, length.out = nbins)
cols <- rainbow(nbins)

#i <- 60

# png(file="Biomass figures/example%02d.png", width=400, height=400)
# 
# for(i in 1:ntime){
#    # Find the bin that the two SSBs are in 
#   df.tmp <- df.new[df.new$time == SSB.all$time[i],]  
#   
#   bin.tmp <- base::cut(df.tmp$SSB,bns, labels = FALSE)
#   # 
#   # bin.us <- bin.tmp[2]
#   # bin.can = bin.tmp[1]
#   # 
#   df.tmp$fll <- bns[bin.tmp]
#   
#   # Create dummy variables outside area for the color palet to work 
#   df.dummy <- data.frame(SSB = bns, group = rep('none',nbins), time = df.tmp$time,
#                          lat = 100, long = 100, fll = bns)
#   
#   df.tmp <- rbind(df.tmp, df.dummy)
#   
#   # Create a title txt 
#   #ttl <- strsplit(as.character(df.tmp$time[1]), split = '[.]')
#   
#   
#   # # US first 
#   p2  <- p1 + geom_point(data = df.tmp, aes(fill = fll), size = 0)+
#     scale_fill_gradientn(colours = cols,
#                          breaks = bns, name = 'Spawning biomass')+
#     geom_point(data = df.tmp, aes(fill = fll), size = 0)+
#     geom_point(data = df.tmp[df.tmp$group == 'usa',],
#                size = szs[bin.tmp[2]], col = cols[bin.tmp[2]])+
#     geom_point(data = df.tmp[df.tmp$group == 'can',],
#                size = szs[bin.tmp[1]], col = cols[bin.tmp[1]])+
#     ggtitle(label = paste('year and season = ',df.tmp$time[1]))
#   # 
#   print(p2)
# }
# 
# dev.off()
