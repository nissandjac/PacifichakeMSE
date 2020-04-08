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

perc <- c(0.1,0.9) # Percentiles for plotting

load_all()

for(i in 1:length(files)){
  load(paste(folder,files[i], sep = '')) # Prints as 'ls.save'
  df.MSE <- flatten(ls.save) # Change the list a little bit



  catch.tot.tmp <- catchtmp %>%
    group_by(years) %>%
    summarise(catchmean = median(value),
              quants95 = quantile(value, probs =perc[2]),
              quants5 = quantile(value, probs= perc[1]))


  SSB.tot.tmp <- SSBtmp  %>%
    group_by(years) %>%
    summarise(SSBmean = median(value),
              quants95 = quantile(value, probs = perc[2]),
              quants5 = quantile(value, probs= perc[1]))


  AAV.tot.tmp <- AAVtmp %>%
    group_by(years) %>%
    summarise(AAVmean = median(value),
              quants95 = quantile(value, probs = perc[2]),
              quants5 = quantile(value, probs= perc[1]))


  strs <- strsplit(files[i], split = '_')[[1]]

  runname <- paste(strs[4],strs[6],strsplit(strs[8], split = '.Rdata')[[1]], sep = '_')

  catchtmp$run <- runname
  catch.tot.tmp$run <- runname

  SSBtmp$run <- runname
  SSB.tot.tmp$run <- runname

  AAVtmp$run <- runname
  AAV.tot.tmp$run <- runname

  HCRname <- strsplit(strs[8], split = '.Rdata')[[1]]

  catchtmp$HCR <- HCRname
  catch.tot.tmp$HCR <- HCRname

  SSBtmp$HCR <- HCRname
  SSB.tot.tmp$HCR <- HCRname

  AAVtmp$HCR <- HCRname
  AAV.tot.tmp$HCR <- HCRname

  catchtmp$climate <- strs[6]
  catch.tot.tmp$climate <- strs[6]

  SSBtmp$climate <- strs[6]
  catch.tot.tmp$climate <- strs[6]
  AAVtmp$climate <- strs[6]

    if(i == 1){
    catchdf <- catchtmp
    catch.tot <- catch.tot.tmp
    SSBdf <- SSBtmp
    SSB.tot <- SSB.tot.tmp
    AAVdf <- AAVtmp
    AAV.tot <- AAV.tot.tmp

    }else{
    catchdf <- rbind(catchdf,catchtmp)
    catch.tot <- rbind(catch.tot,catch.tot.tmp)

    SSBdf <- rbind(SSBdf, SSBtmp)
    SSB.tot <- rbind(SSB.tot, SSB.tot.tmp)

    AAVdf <- rbind(AAVdf, AAVtmp)
    AAV.tot <- rbind(AAV.tot, AAV.tot.tmp)
  }



  }


cols <- PNWColors::pnw_palette('Starfish',n = 3, type = 'discrete')


cplot <- ggplot(catch.tot, aes(x = years, y = catchmean*1e-6, color = run))+geom_line()+theme_bw()+theme(legend.position = 'none')+
  geom_line(aes(y = quants5*1e-6), linetype = 2)+scale_color_manual(values = rep(cols, each = 3))+
  geom_line(aes(y = quants95*1e-6), linetype = 2)+facet_wrap(~HCR)+scale_y_continuous( 'Catch')+
  geom_line(data = catch.tot[catch.tot$years< 2019,] ,aes(x = years, y= catchmean*1e-6), color = 'black', size = 1.2)

ssbplot <- ggplot(SSB.tot, aes(x = years, y = SSBmean*1e-6, color = run))+geom_line()+theme_bw()+theme(legend.position = 'none')+
  geom_line(aes(y = quants5*1e-6), linetype = 2)+scale_color_manual(values = rep(cols, each = 3))+
  geom_line(aes(y = quants95*1e-6), linetype = 2)+facet_wrap(~HCR)+scale_y_continuous('SSB')+
  geom_line(data = SSB.tot[SSB.tot$years< 2019,] ,aes(x = years, y= SSBmean*1e-6), color = 'black', size = 1.2)

AAVplot <- ggplot(AAV.tot, aes(x = years, y = AAVmean, color = run))+geom_line()+theme_bw()+theme(legend.position = 'none')+
  geom_line(aes(y = quants5), linetype = 2)+scale_color_manual(values = rep(cols, each = 3))+
  geom_line(aes(y = quants95), linetype = 2)+facet_wrap(~HCR)+scale_y_continuous('AAV')+coord_cartesian(ylim = c(0,2))+
  geom_line(data = AAV.tot[AAV.tot$years< 2019,] ,aes(x = years, y= AAVmean), color = 'black', size = 1.2)



png(paste('results/Climate/','objectives_publication.png', sep = ''), width = 20, height =20, res = 400, unit = 'cm')

cplot/ssbplot/AAVplot

dev.off()

dodge <- position_dodge(width = 0.5)

rmout <- quantile(catchdf$value[catchdf$years>2019]*1e-6, probs = perc)

vplot1 <- ggplot(catchdf[catchdf$years>2019,], aes(x = HCR,y = value*1e-6, group = run, fill = climate))+
  geom_violin(position = dodge)+scale_y_continuous(name = 'Catch \n(million tonnes)', limit = rmout)+geom_line()+theme_bw()+theme(legend.position = 'none')+
  geom_boxplot(width=0.2, col = 'black', outlier.shape = NA, position = dodge)+scale_fill_manual(values= cols)

rmout <- quantile(SSBdf$value[SSBdf$years>2019]*1e-6, probs = perc)

vplot2 <- ggplot(SSBdf[SSBdf$years>2019,], aes(x = HCR,y = value*1e-6, group = run, fill = climate))+
  geom_violin(position = dodge)+scale_y_continuous(name = 'SSB \n(million tonnes)', limit = rmout)+geom_line()+theme_bw()+
  geom_boxplot(width=0.2, col = 'black', outlier.shape = NA, position = dodge)+scale_fill_manual(values= cols)

# Remove stupid outliers
rmout <- quantile(AAVdf$value[AAVdf$years>2019], probs = perc)

vplot3 <- ggplot(AAVdf[AAVdf$years>2019,], aes(x = HCR,y = value, group = run, fill = climate))+
  geom_violin(position = dodge)+scale_y_continuous(name = 'AAV', limit = rmout)+geom_line()+theme_bw()+theme(legend.position = 'none')+
  geom_boxplot(width=0.2, col = 'black', outlier.shape = NA, position = dodge)+scale_fill_manual(values= cols)


png(paste('results/Climate/','violin_publication.png', sep = ''), width = 20, height =20, res = 400, unit = 'cm')

vplot1/vplot2/vplot3

dev.off()




