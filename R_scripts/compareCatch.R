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
source('MSE_list_to_df.R')
# Climate Scenarios
load('results/Climate/MSErun_move_JMC_climate_0.Rdata')
ls.0 <- ls.save 
load('results/Climate/MSErun_move_JMC_climate_0_02.Rdata')
ls.002 <- ls.save
load('results/Climate/MSErun_move_JMC_climate_0_04.Rdata')
ls.004 <- ls.save

df1 <- MSE_list_to_df(ls.0)
df2 <- MSE_list_to_df(ls.002)
df3 <- MSE_list_to_df(ls.004)