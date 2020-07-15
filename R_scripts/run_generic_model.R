## Run a generic hake model # 

library(TMB)
library(r4ss)
library(devtools)
library(PacifichakeMSE)
load_all()

mod <- SS_output('inst/extdata/SS32018', printstats=FALSE, verbose = FALSE) # Read the true selectivity

# Set the seed
seedz <- 12345
set.seed(seedz)

df <- load_data_seasons(nseason = 4, nspace = 4, bfuture = 0.5, species = 'NA') # Prepare data for operating model

sim.data <- run.agebased.true.catch(df) # Run the operating model until 2018



ggplot(melt(sim.data$SSB, value.name = 'ssb'), aes(x = year, y = ssb, color = as.factor(space)))+geom_line()+theme_classic()+scale_x_log10()
