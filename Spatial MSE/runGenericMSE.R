##### Run the hake MSE with different options 
# Load required packages and files 
require(TMB)
require(ggplot2)
require(dplyr)
require(gridExtra)
require(cowplot)
require(scales)
require(RColorBrewer)



compile("runHakeassessment.cpp")
dyn.load(dynlib("runHakeassessment"))
source('load_files.R')
source('load_files_OM.R')
source('fnMSE.R')
source('hake_objectives.R')

###### Load the data to run the MSE ######
df <- load_data_seasons(nseason = 4, nspace = 2,
                        nsurvey= 2, movemax = 0.4) # Prepare data for operating model 

simyears <- 30
TAC <- 1
nruns <- 50
sim.data <- run.agebased.true.catch(df)

yr.season <- seq(df$years[1],df$years[df$nyear]+1,by = 1/df$nseason)
yr.season <- yr.season[-length(yr.season)]

plot(yr.season,as.numeric(t(sim.data$V.save[,1,])), type = 'l', ylim = c(20000,4e6), col = 'red')
lines(yr.season, as.numeric(sim.data$V.save[,2,]), col = 'blue', lty = 1)
lines(df$years,sim.data$SSB[,1],  col = 'red', lty = 1)
lines(df$years,sim.data$SSB[,2],  col = 'blue', lty = 1)
#lines(yr.season, as.numeric(t(sim.data$SSB.all[,,1])), col = 'red', lty = 2)
#lines(yr.season,as.numeric(t(sim.data$SSB.all[,,2])), col = 'blue')
# 
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears-1))

###### Run the MSE using the data above #####
ls <- fnMSE(df, simyears = simyears, TAC = TAC, nruns = nruns) # TAC 1) HCR, 2) JTC, 3) Realized catch
MSE <- ls[[1]]
sim.data <- ls[[2]]

sum(MSE[[1]]$Catch.quota.N)
Catch.season <- apply(MSE[[1]]$Catch.save.age, MAR = c(2,3,4), FUN = sum)

MSE[[1]]$Catch.quota[MSE[[1]]$Catch.quota.N == 1]/Catch.season[MSE[[1]]$Catch.quota.N == 1]

quota <- apply(MSE[[1]]$Catch.quota, MARGIN = 1, FUN = sum, na.rm =TRUE)
plot(year.future,MSE[[1]]$Catch/quota, type = 'l', xlim = c(2015,max(year.future)))
lines(year.future,rep(1, length(year.future)) , col ='black', lty =2)


objectives <- hake_objectives(MSE,sim.data,move = 1)
objectives[[2]]

###### Plot some results ######

df.plot <- df_lists(MSE, 'HCR', simyears = 50)
names(df.plot[[3]])

cols <- brewer.pal(6, 'Dark2')


p2.1 <- ggplot(df.plot[[3]]$Catchplot, aes(x = year, y = med*1e-6))+geom_line(size = 2)+
  geom_ribbon(aes(ymin = p5*1e-6, ymax = p95*1e-6), linetype = 2, fill = alpha(cols[1], alpha = 0.2))+
  scale_color_manual(values=cols[1:3])+scale_y_continuous(name = 'Catch (million tonnes)')+
  geom_line(aes(y = p5*1e-6, color = run), linetype = 2)+geom_line(aes(y = p95*1e-6, color = run), linetype = 2)+
  facet_wrap(~run)

p2.1
