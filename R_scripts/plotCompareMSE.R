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
load('results/Selectivity/MSErun_move_JMC_sel.Rdata')
ls.JMC.sel <- ls.save 
load('results/Selectivity/MSErun_move_JTC_sel.Rdata')
ls.JTC.sel <- ls.save


# Reference scenario 
load('results/b = 0.5/MSErun_move_JMC.Rdata')
ls.JMC <- ls.save

# Climate scenario 

simyears <- 30
yr <- 1966:(2017+simyears-1)
nruns <- 100
source('hake_objectives.R')


df <- load_data_seasons(nseason = 4, nspace = 2) # Prepare data for operating model
sim.data <- run.agebased.true.catch(df)

ls.plot <- list(JMC.sel = ls.JMC.sel, 
                JTC.sel = ls.JTC.sel,
                JMC = ls.JMC)

fn_plot_MSE(ls.plot, sim.data,plotfolder = 'Figs/selectivity/',plotexp = TRUE)
dev.off()
