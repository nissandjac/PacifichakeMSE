## Load MSE's and compare ## 

source('df_lists.R')
require(ggplot2)
require(dplyr)
require(scales)
require(RColorBrewer)
require(cowplot)
library(gridExtra)
source('load_files_OM.R')
source('fn_plot_MSE.R')

# Selectivity scenarios 
load('results/b = 0.5/MSErun_move_JMC.Rdata')
ls.JMC <- ls.save 
# Reference scenario 
load('results/b = 0.5/MSErun_move_JTC.Rdata')
ls.JTC <- ls.save
# Reference scenario 
load('results/b = 0.5/MSErun_move_realized.Rdata')
ls.Real <- ls.save

# load('results/MSErun_move_realized_move1.Rdata')
# ls.move1 <- ls.save
# load('results/MSErun_move_realized_move2.Rdata')
# ls.move2 <- ls.save
# load('results/MSErun_move_realized_move3.Rdata')
# ls.move3 <- ls.save

# load('results/Selectivity/MSErun_move_JTC_sel.Rdata')
# ls.JTC.sel <- ls.save



# Climate scenario 

simyears <- 30
yr <- 1966:(2017+simyears-1)
nruns <- 100
source('hake_objectives.R')


df <- load_data_seasons(nseason = 4, nspace = 2) # Prepare data for operating model
sim.data <- run.agebased.true.catch(df)

ls.plot <- list(JMC = ls.JMC, 
                JTC = ls.JTC,
                Real = ls.Real)

fn_plot_MSE(ls.plot, sim.data,plotfolder = 'Figs/b = 0.5/',plotexp = TRUE)



ls.plot <- list(Move1 = ls.move1, 
                Move2 = ls.move2,
                Move3 = ls.move3)

fn_plot_MSE(ls.plot, sim.data,plotfolder = 'Figs/Movement/',plotexp = TRUE)


