## Load MSE's and compare ## 
plotMSE <- function(results, plotnames = NA, plotexp = FALSE,
                     pidx = NA){

source('df_lists.R')
source('load_files_OM.R')
source('fn_plot_MSE.R')
source('hake_objectives.R')

require(ggplot2)
require(dplyr)
require(scales)
require(RColorBrewer)
require(cowplot)
require(gridExtra)

# Load data 

fls <- dir(results)
ls.plots <- list()

fls <- fls[grep('.Rdata', x = fls)]
fls <- fls[grep('MSE', x = fls)]

print(paste('order = ', fls))

for(i in 1:length(fls)){
 load(paste(results,fls[i], sep= ''))
 ls.plots[[i]] <- ls.save
}



if(is.na(plotnames[1])){
  plotnames <- rep(NA, length(fls))
  for(i in 1:length(fls)){
  plotnames[i] <- strsplit(fls[i],split = '\\.')[[1]][1] 
  
  }
}else{
  
  if(length(plotnames) != length(ls.plots)){
    stop('incorrect number of names')
  }
}
# 
# simyears <- 50
# yr <- 1966:(2017+simyears-1)
# nruns <- 100
df <- load_data_seasons(nseason = 4, nspace = 2) # Save these in future runs - calculates SSB0
sim.data <- run.agebased.true.catch(df)

names(ls.plots) <- plotnames

fn_plot_MSE(ls.plots, sim.data, plotfolder = results,plotexp = plotexp, pidx = pidx)


# Plot the realized catch vs the quota 

}