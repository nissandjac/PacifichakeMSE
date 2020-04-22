# Plot the climate MSE climate results
library(TMB)
library(r4ss)
library(devtools)
library(PacifichakeMSE)
library(purrr)
library(dplyr)
library(reshape2)
library(patchwork)

mod <- SS_output('inst/extdata/SS32018', printstats=FALSE, verbose = FALSE) # Read the true selectivity

# Set the seed
seedz <- 12345
set.seed(seedz)

df <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.5) # Prepare data for operating model

parms.true <- getParameters_OM(TRUE,mod, df) # Load parameters from assessment

time <- 1
yrinit <- df$nyear
nruns <- 100

seeds <- floor(runif(n = nruns, min = 1, max = 1e6))
### Run the OM and the EM for x number of years in the MSE
### Set targets for harvesting etc
#

simyears <- 30 # Project 30 years into the future (2048 that year)
year.future <- c(df$years,(df$years[length(df$years)]+1):(df$years[length(df$years)]+simyears))
N0 <- NA
sim.data <- run.agebased.true.catch(df) # Run the operating model until 2018

simdata0 <- sim.data # The other one is gonna get overwritten.




folder <- 'results/Climate/'


files <- dir(folder)[grep(dir(folder),pattern = '.Rdata')]

# Recreate the violin pltos



yrs <- 1966:2047

perc <- c(0.25,0.75) # Percentiles for plotting

load_all()

for(i in 1:length(files)){
  load(paste(folder,files[i], sep = '')) # Prints as 'ls.save'
  df.MSE <- flatten(ls.save) # Change the list a little bit


  catchcdf <- processMSE(df.MSE, 'Catch', idx = c(2,3), spacenames = c('CAN', 'USA'), runs = 100,nspace = 2)
  catchdf <- processMSE(df.MSE, 'Catch', idx = 2, spacenames = c('CAN', 'USA'), runs = 100,nspace = 2)
  SSB_mid <- processMSE(df.MSE, id = 'SSB.mid', idx = c(1,2), spacenames = c('CAN', 'USA'), runs = 100,nspace = 2)
  SSB_tot <- processMSE(df.MSE, id = 'SSB', idx = 1, spacenames = c('CAN', 'USA'), runs = 100,nspace = 2)
  # Calculate AAV
  AAVdf <- AAV(catchcdf)
  AAVtottmp <- AAV(catchdf)

  catch.tot.tmp <- catchdf %>%
    group_by(year) %>%
    summarise(catchmean = median(value),
              quants95 = quantile(value, probs =perc[2]),
              quants5 = quantile(value, probs= perc[1]))


  SSB.tot.tmp <- SSB_tot  %>%
    group_by(year) %>%
    summarise(SSBmean = median(value),
              quants95 = quantile(value, probs = perc[2]),
              quants5 = quantile(value, probs= perc[1]))


  AAV.tot.tmp <- AAVtottmp[AAVtottmp$year > 1966,] %>%
    group_by(year) %>%
    summarise(AAVmean = median(AAV, na.rm = TRUE),
              quants95 = quantile(AAV, probs = perc[2]),
              quants5 = quantile(AAV, probs= perc[1]))


  strs <- strsplit(files[i], split = '_')[[1]]

  runname <- paste(strs[4],strs[6],strsplit(strs[8], split = '.Rdata')[[1]], sep = '_')

  catchdf$run <- runname
  catchcdf$run <- runname
  catch.tot.tmp$run <- runname

  SSB_mid$run <- runname
  SSB.tot.tmp$run <- runname

  AAVdf$run <- runname
  AAV.tot.tmp$run <- runname

  HCRname <- strsplit(strs[8], split = '.Rdata')[[1]]

  if(HCRname == 'TAC1'){
    HCRname <- 'HCR0'
  }
  if(HCRname == 'TAC2'){
    HCRname <- 'historical'
  }
  if(HCRname == 'TAC3'){
    HCRname <- 'Realized'
  }

  catchdf$HCR <- HCRname
  catchcdf$HCR <- HCRname
  catch.tot.tmp$HCR <- HCRname

  SSB_mid$HCR <- HCRname
  SSB.tot.tmp$HCR <- HCRname

  AAVdf$HCR <- HCRname
  AAV.tot.tmp$HCR <- HCRname

  catchdf$climate <- strs[6]
  catchcdf$climate <- strs[6]
  catch.tot.tmp$climate <- strs[6]

  SSB_mid$climate <- strs[6]
  SSB.tot.tmp$climate <- strs[6]
  catch.tot.tmp$climate <- strs[6]
  AAV.tot.tmp$climate <- strs[6]
  AAVdf$climate <- strs[6]

    if(i == 1){
    catchdfexp <- catchdf
    catchcdfexp <- catchcdf
    catch.tot <- catch.tot.tmp
    SSBdf <- SSB_mid
    SSB.tot <- SSB.tot.tmp
    AAVdfexp <- AAVdf
    AAV.tot <- AAV.tot.tmp

    }else{
    catchdfexp <- rbind(catchdfexp,catchdf)
    catchcdfexp <- rbind(catchcdfexp,catchcdf)
    catch.tot <- rbind(catch.tot,catch.tot.tmp)

    SSBdf <- rbind(SSBdf, SSB_mid)
    SSB.tot <- rbind(SSB.tot, SSB.tot.tmp)

    AAVdfexp <- rbind(AAVdfexp, AAVdf)
    AAV.tot <- rbind(AAV.tot, AAV.tot.tmp)
  }



  }


cols <- PNWColors::pnw_palette('Starfish',n = 3, type = 'discrete')

lsize <- 0.5
usize <-  0.3
pyear <- 2015

cplot <- ggplot(catch.tot[catch.tot$year>pyear,], aes(x = year, y = catchmean*1e-6, color = climate))+
  geom_line(size = lsize)+theme_bw()+facet_wrap(~HCR, ncol = 3)+
  theme(legend.position = 'top',
        text = element_text(size = 8),
        legend.title = element_blank(),
        strip.text = element_text(size=5),
        axis.text.x = element_text(size = 5, angle = 90),
        plot.margin = unit(c(1,1,0,1), 'pt'),
        legend.margin = margin(c(0,0,0,0)))+
  scale_color_manual(values = cols, labels = c('no change', 'medium', 'high'))+
  geom_line(aes(y = quants5*1e-6), linetype = 2, size = usize)+
  geom_line(aes(y = quants95*1e-6), linetype = 2, size = usize)+scale_y_continuous( 'Catch')+
  scale_x_continuous('')+
  geom_line(data = catch.tot[catch.tot$year< 2019 & catch.tot$year> pyear,] ,aes(x = year, y= catchmean*1e-6), color = 'black', size = lsize)
cplot

ssbplot <- ggplot(SSB.tot[SSB.tot$year>pyear,], aes(x = year, y = SSBmean*1e-6, color = climate))+geom_line(size = lsize)+theme_bw()+
  theme(legend.position = 'none',
        text = element_text(size = 8),
        strip.text = element_text(size=5),
        axis.text.x = element_text(size = 5, angle = 90),
        plot.margin = unit(c(1,1,0,1), 'pt'))+
  geom_line(aes(y = quants5*1e-6), linetype = 2, size = usize)+scale_color_manual(values = cols)+
  scale_x_continuous('')+
  geom_line(aes(y = quants95*1e-6), linetype = 2, size = usize)+facet_wrap(~HCR, ncol = 3)+scale_y_continuous('SSB')+
  geom_line(data = SSB.tot[SSB.tot$year< 2019 & SSB.tot$year>pyear,] ,aes(x = year, y= SSBmean*1e-6), color = 'black', size = lsize)

AAVplot <- ggplot(AAV.tot[AAV.tot$year>pyear,], aes(x = year, y = AAVmean, color = climate))+geom_line(size = lsize)+theme_bw()+
  theme(legend.position = 'none',
        strip.text = element_text(size=5),
        text = element_text(size = 8),
        axis.text.x = element_text(size = 6, angle = 90),
        plot.margin = unit(c(1,1,0,1), 'pt'))+
  scale_x_continuous('')+
  geom_line(aes(y = quants5), linetype = 2, size = usize)+scale_color_manual(values = cols)+
  geom_line(aes(y = quants95), linetype = 2, size = usize)+facet_wrap(~HCR, ncol = 3)+scale_y_continuous('AAV')+coord_cartesian(ylim = c(0,2))+
  geom_line(data = AAV.tot[AAV.tot$year< 2019  & AAV.tot$year> pyear,] ,aes(x = year, y= AAVmean), color = 'black', size = lsize)



png(paste('results/Climate/','objectives_publication.png', sep = ''), width = 8, height =10, res = 400, unit = 'cm')

cplot/ssbplot/AAVplot

dev.off()

dodge <- position_dodge(width = 0.5)

rmout <- quantile(catchcdfexp$value[catchdfexp$year>2019]*1e-6, probs = perc)

vplot1 <- ggplot(catchcdfexp[catchcdfexp$year>2019,], aes(x = HCR,y = value*1e-6, group = run, fill = climate))+
  geom_violin(position = dodge)+scale_y_continuous(name = 'Catch \n(million tonnes)', limit = rmout)+geom_line()+
  theme_bw()+theme(legend.position = 'top',
                   text = element_text(size = 8),
                   legend.title = element_blank(),
                   legend.text = element_text(size =10),
                   strip.text = element_text(size=5),
                   legend.key.size = unit(0.5, 'lines'),
                   axis.text.x = element_blank(),
                   plot.margin = unit(c(1,1,0,1), 'pt'),
                   legend.margin = margin(c(0,0,0,0)))+
  scale_x_discrete('')+
  geom_boxplot(width=0.2, col = 'black', outlier.shape = NA, position = dodge,
                show.legend = FALSE)+scale_fill_manual(values= cols, labels = c('no change','medium', 'high'))+
  guides(shape = guide_legend(override.aes = list(shape = 2)))+
  facet_wrap(~space)

rmout <- quantile(SSBdf$value[SSBdf$year>2019]*1e-6, probs = perc)

vplot2 <- ggplot(SSBdf[SSBdf$year>2019,], aes(x = HCR,y = value*1e-6, group = run, fill = climate))+
  geom_violin(position = dodge)+scale_y_continuous(name = 'SSB \n(million tonnes)', limit = rmout)+geom_line()+theme_bw()+
  theme(legend.position = 'none',
        text = element_text(size = 8),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(1,1,0,1), 'pt'))+scale_x_discrete('')+
  geom_boxplot(width=0.2, col = 'black', outlier.shape = NA, position = dodge)+scale_fill_manual(values= cols)+facet_wrap(~space)

# Remove stupid outliers
rmout <- quantile(AAVdfexp$AAV[AAVdfexp$year>2019], probs = perc)

vplot3 <- ggplot(AAVdfexp[AAVdfexp$year>2019,], aes(x = HCR,y = AAV, group = run, fill = climate))+
  geom_violin(position = dodge)+scale_y_continuous(name = 'AAV', limit = rmout)+geom_line()+theme_bw()+
  theme(legend.position = 'none',
        text = element_text(size = 8),
        axis.text.x = element_text(size =6),
        plot.margin = unit(c(1,1,0,1), 'pt'),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  geom_boxplot(width=0.2, col = 'black', outlier.shape = NA, position = dodge)+scale_fill_manual(values= cols)+
  facet_wrap(~space, nrow =1)


png(paste('results/Climate/','violin_publication.png', sep = ''), width = 8, height =8, res = 400, unit = 'cm')

vplot1+vplot2+vplot3

dev.off()




