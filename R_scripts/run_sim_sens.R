library(TMB)
library(r4ss)
library(devtools)
library(PacifichakeMSE)
library(tidyverse)

mod <- SS_output('inst/extdata/SS32018', printstats=FALSE, verbose = FALSE) # Read the true selectivity

# Set the seed
seedz <- 12345
set.seed(seedz)

df <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.5) # Prepare data for operating model

parms.true <- getParameters_OM(TRUE,mod, df) # Load parameters from assessment

time <- 1
yrinit <- df$nyear
nruns <- 500

seeds <- floor(runif(n = nruns, min = 1, max = 1e6))
### Run the OM and the EM for x number of years in the MSE
### Set targets for harvesting etc
#

simyears <- 30 # Project 30 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df) # Run the operating model until 2018

simdata0 <- sim.data # The other one is gonna get overwritten.
SSB.ss3 <- mod$derived_quants$Value[grep('SSB_1966', mod$derived_quants$Label):
                                      grep('SSB_2018', mod$derived_quants$Label)]


# plot data and simulated stuff with the baseline model 
ssb <- data.frame(ssb = c(rowSums(sim.data$SSB), SSB.ss3/2),
                  year = rep(df$years,2),
                  model = rep(c('OM','SS3'), each = df$nyear),
                  cmove = rep(c(.25,NA), each = df$nyear)
                  )

ggplot(ssb, aes(x= year, y=  ssb, color = model))+geom_line()+theme_bw()


# Test the impact of the move init parameter 

init.p <- seq(0.5,.99, length.out = 25)

dftmp <- df
for(i in 1:length(init.p)){
  
  
df$move.init[2] <- init.p[i]
df$move.init[1] <- 1-init.p[i]
  
tmp <-  run.agebased.true.catch(df) 

ssb <- rbind(ssb,data.frame(ssb = rowSums(tmp$SSB),
                            year = df$years,
                            model = paste('OM',init.p[i], sep = '-'),
                            cmove = init.p[i]              
                            )
             )
  
  
}

cols <- RColorBrewer::brewer.pal(n = 9, name = YlOrRd)

p1 <- ggplot(ssb[!ssb$model %in% c('OM','SS3'),], aes(x= year, y= ssb/sum(sim.data$SSB0), 
                                               color = cmove, group = model))+
  geom_line()+theme_bw()+
  geom_line(data = ssb[ssb$model %in%  c('SS3'),], color = c('black'), size = 1.5, linetype = 2)+
  geom_line(data = ssb[ssb$model %in%  c('OM'),], color = c('red'), size = 1.2, linetype = 2)+
  scale_y_continuous('SSB/SSB0')


png(filename = 'results/sensitivity/R0_dist.png', width = 16, height = 12, res = 400,units = 'cm')
p1                  
dev.off()


