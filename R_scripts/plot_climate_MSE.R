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




folder <- 'C:/Users/Nis/Dropbox/NOAA/Hake MSE/MSE results/final results/'

#folder <- 'C:/Users/nsja/Dropbox/NOAA/Hake MSE/MSE results/final results/'



files <- dir(folder)[grep(dir(folder),pattern = '.Rdata')]

# Recreate the violin pltos

yrs <- 1966:2047

perc <- c(0.25,0.75) # Percentiles for plotting

load_all()

for(i in 1:length(files)){
  load(paste(folder,files[i], sep = '')) # Prints as 'ls.save'
  df.MSE <- flatten(ls.save) # Change the list a little bit


  catchcdf <- processMSE(df.MSE, 'Catch', idx = c(2,3), spacenames = c('CAN', 'USA'), runs = 500,nspace = 2)
  catchdf <- processMSE(df.MSE, 'Catch', idx = 2, spacenames = c('value'), runs = 500,nspace = 2)

  SSB_mid <- processMSE(df.MSE, id = 'SSB.mid', idx = c(1,2), spacenames = c('CAN', 'USA'), runs = 500,nspace = 2)
  SSB_tot <- processMSE(df.MSE, id = 'SSB', idx = 1, spacenames = c('value'), runs = 500,nspace = 2)

  SSB.om <- processMSE(df.MSE, id = 'SSB.hes', idx = 1, fn = 'rows', spacenames = c('value'), runs = 500,nspace = 2)

  # Calculate AAV
  AAVdf <- AAV(catchcdf, idx = 4)
  AAVtottmp <- AAV(catchdf, idx = 2)

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

  catchdf$MP <- runname
  catchcdf$MP <- runname
  catch.tot.tmp$MP <- runname

  SSB_mid$MP <- runname
  SSB_tot$MP <- runname
  SSB.tot.tmp$MP <- runname
  SSB.om$MP <- runname


  AAVdf$MP <- runname
  AAVtottmp$MP <- runname
  AAV.tot.tmp$MP <- runname

  HCRname <- strsplit(strs[8], split = '.Rdata')[[1]]

  if(HCRname == 'TAC1'){
    HCRname <- as.factor('HCR0')
  }
  if(HCRname == 'TAC2'){
    HCRname <- as.factor('MD')
  }
  if(HCRname == 'TAC3'){
    HCRname <- as.factor('AC')
  }

  catchdf$HCR <- HCRname
  catchcdf$HCR <- HCRname
  catch.tot.tmp$HCR <- HCRname

  SSB_mid$HCR <- HCRname
  SSB.tot.tmp$HCR <- HCRname
  SSB_tot$HCR <- HCRname
  SSB.om$HCR <- HCRname

  AAVdf$HCR <- HCRname
  AAV.tot.tmp$HCR <- HCRname
  AAVtottmp$HCR <- HCRname

  catchdf$climate <- strs[6]
  catchcdf$climate <- strs[6]
  catch.tot.tmp$climate <- strs[6]

  SSB_mid$climate <- strs[6]
  SSB.tot.tmp$climate <- strs[6]
  SSB_tot$climate <- strs[6]
  SSB.om$climate <- strs[6]

  AAV.tot.tmp$climate <- strs[6]
  AAVdf$climate <- strs[6]
  AAVtottmp$climate <- strs[6]

  if(i == 1){
    catchdfexp <- catchdf
    catchcdfexp <- catchcdf
    catch.tot <- catch.tot.tmp

    SSBdf <- SSB_mid
    SSB.tot <- SSB.tot.tmp
    SSB_cdf <- SSB_tot
    ssb.om <- SSB.om

    AAVdfexp <- AAVdf
    AAV.tot <- AAV.tot.tmp
    AAVcdf <- AAVtottmp

    }else{
    catchdfexp <- rbind(catchdfexp,catchdf)
    catchcdfexp <- rbind(catchcdfexp,catchcdf)
    catch.tot <- rbind(catch.tot,catch.tot.tmp)

    SSBdf <- rbind(SSBdf, SSB_mid)
    SSB.tot <- rbind(SSB.tot, SSB.tot.tmp)
    SSB_cdf <- rbind(SSB_cdf, SSB_tot)
    ssb.om <- rbind(ssb.om,SSB.om)

    AAVdfexp <- rbind(AAVdfexp, AAVdf)
    AAV.tot <- rbind(AAV.tot, AAV.tot.tmp)
    AAVcdf <- rbind(AAVcdf, AAVtottmp)

  }



  }


cols <- PNWColors::pnw_palette('Starfish',n = 3, type = 'discrete')

lsize <- 0.5
usize <-  0.3
pyear <- 2018

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
  geom_line(aes(y = quants95*1e-6), linetype = 2, size = usize)+
  scale_y_continuous('Catch')+
  scale_x_continuous('')+
  geom_line(data = catch.tot[catch.tot$year< 2019 & catch.tot$year> pyear,] ,aes(x = year, y= catchmean*1e-6), color = 'black', size = lsize)
cplot

ssbplot <- ggplot(SSB.tot[SSB.tot$year>pyear,], aes(x = year, y = SSBmean*1e-6, color = climate))+geom_line(size = lsize)+theme_bw()+
  theme(legend.position = 'none',
        text = element_text(size = 8),
        strip.text = element_text(size=5),
        axis.text.x = element_text(size = 5, angle = 90),
        plot.margin = unit(c(1,1,0,1), 'pt'))+
  geom_line(aes(y = quants5*1e-6), linetype = 2, size = usize)+
  geom_line(aes(y = quants95*1e-6), linetype = 2, size = usize)+
  scale_color_manual(values = cols)+
  scale_x_continuous('')+facet_wrap(~HCR, ncol = 3)+scale_y_continuous('SSB')+
  geom_line(data = SSB.tot[SSB.tot$year< 2019 & SSB.tot$year>pyear,] ,aes(x = year, y= SSBmean*1e-6), color = 'black', size = lsize)

AAVplot <- ggplot(AAV.tot[AAV.tot$year>pyear,], aes(x = year, y = AAVmean, color = climate))+
  geom_line(size = lsize)+theme_bw()+
  theme(legend.position = 'none',
        strip.text = element_text(size=5),
        text = element_text(size = 8),
        axis.text.x = element_text(size = 6, angle = 90),
        plot.margin = unit(c(1,1,0,1), 'pt'))+
  scale_x_continuous('')+coord_cartesian(ylim = c(0,1.5))+
  geom_line(aes(y = quants5), linetype = 2, size = usize)+
  geom_line(aes(y = quants95), linetype = 2, size = usize)+
  scale_color_manual(values = cols)+
  facet_wrap(~HCR, ncol = 3)+scale_y_continuous('AAV')+
  geom_line(data = AAV.tot[AAV.tot$year< 2019  & AAV.tot$year> pyear,] ,
            aes(x = year, y= AAVmean), color = 'black', size = lsize)



png('results/Climate/objectives_publication.png', width = 16, height =12, res = 400, unit = 'cm')

cplot/ssbplot/AAVplot

dev.off()

dodge <- position_dodge(width = 0.5)
perc <- c(0.1,0.90)

rmout <- quantile(catchcdfexp$value[catchdfexp$year>2019]*1e-6, probs = perc)

vplot1 <- ggplot(catchcdfexp[catchcdfexp$year>2019,], aes(x = HCR,y = value*1e-6, group = MP, fill = climate))+
  geom_violin(position = dodge)+scale_y_continuous(name = 'Catch \n(million tonnes)')+geom_line()+
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
  facet_wrap(~variable)+coord_cartesian(ylim = rmout)

rmout <- quantile(SSBdf$value[SSBdf$year>2019]*1e-6, probs = perc)

vplot2 <- ggplot(SSBdf[SSBdf$year>2019,], aes(x = HCR,y = value*1e-6, group = MP, fill = climate))+
  geom_violin(position = dodge)+scale_y_continuous(name = 'SSB \n(million tonnes)')+geom_line()+theme_bw()+
  theme(legend.position = 'none',
        text = element_text(size = 8),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(1,1,0,1), 'pt'))+scale_x_discrete('')+
  geom_boxplot(width=0.2, col = 'black', outlier.shape = NA, position = dodge)+
  scale_fill_manual(values= cols)+facet_wrap(~variable)+coord_cartesian(ylim = rmout)

# Remove stupid outliers
rmout <- quantile(AAVdfexp$AAV[AAVdfexp$year>2019], probs = perc)
rmout <- c(0,4)

vplot3 <- ggplot(AAVdfexp[AAVdfexp$year>2019,], aes(x = HCR,y = AAV, group = MP, fill = climate))+
  geom_violin(position = dodge)+scale_y_continuous(name = 'AAV')+geom_line()+theme_bw()+
  theme(legend.position = 'none',
        text = element_text(size = 8),
        axis.text.x = element_text(size =6),
        plot.margin = unit(c(1,1,0,1), 'pt'),
        strip.background = element_blank(),
        strip.text.x = element_blank())+
  geom_boxplot(width=0.2, col = 'black', outlier.shape = NA, position = dodge)+scale_fill_manual(values= cols)+
  facet_wrap(~variable, nrow =1)+coord_cartesian(ylim = rmout)


png(paste('results/Climate/','violin_publication.png', sep = ''), width = 16, height =12, res = 400, unit = 'cm')

vplot1/vplot2/vplot3

dev.off()

# Minor calculations for manuscript

cmean <- median(catch.tot[catch.tot$year>2040 & catch.tot$HCR == 'HCR0' & catch.tot$climate ==0,]$catchmean)
print(cmean)
csd <- sd(catch.tot[catch.tot$year>2040 & catch.tot$HCR == 'HCR0' & catch.tot$climate ==0,]$catchmean)

cc <- median(catch.tot[catch.tot$year>2040 & catch.tot$HCR == 'HCR0' & catch.tot$climate =='04',]$catchmean)
cc2 <- median(catch.tot[catch.tot$year>2040 & catch.tot$HCR == 'HCR0' & catch.tot$climate =='0',]$catchmean)
cc/cc2


ccc <- median(SSB.tot$SSBmean[catch.tot$year>2040 & catch.tot$HCR == 'HCR0' & catch.tot$climate =='0'])
ccc
pp <- median(SSB.tot$SSBmean[SSB.tot$year>2040 & SSB.tot$HCR == 'HCR0' & catch.tot$climate =='04'])

ccc-pp

median(AAVdfexp$AAV[AAVdfexp$year>2019 & AAVdfexp$HCR == 'HCR0' & AAVdfexp$climate =='0'])

# Relative SSB
median(SSB_cdf$value[SSB_cdf$year>2040 & SSB_cdf$HCR == 'HCR0' & SSB.tot$climate == '0']/sum(sim.data$SSB0))# Country catch
sd(SSB_cdf$value[SSB_cdf$year>2040 & SSB_cdf$HCR == 'HCR0' & SSB.tot$climate == '0']/sum(sim.data$SSB0))# Country catch
median(SSB_cdf$value[SSB_cdf$year>2040 & SSB_cdf$HCR == 'HCR0' & SSB.tot$climate == '0'])# Country catch



HCRtest <- 'HCR0'

p <- median(catchcdfexp[catchcdfexp$year>2040 & catchcdfexp$variable == 'CAN' &
                          catchcdfexp$climate == '0' & catchcdfexp$HCR == HCRtest,]$value)/
  median(catchcdfexp[catchcdfexp$year>2040 & catchcdfexp$variable == 'CAN' &
                       catchcdfexp$climate == '04' & catchcdfexp$HCR == HCRtest,]$value)
p <- median(catchcdfexp[catchcdfexp$year>2040 & catchcdfexp$variable == 'USA' & catchcdfexp$climate == '0'& catchcdfexp$HCR == HCRtest,]$value)/
  median(catchcdfexp[catchcdfexp$year>2040 & catchcdfexp$variable == 'USA' & catchcdfexp$climate == '04' & catchcdfexp$HCR == HCRtest,]$value)
p


HCRtest <- 'HCR0'

p <- median(SSBdf[SSBdf$year>2040 & SSBdf$variable == 'CAN' &
                          SSBdf$climate == '0' & SSBdf$HCR == HCRtest,]$value)/
  median(SSBdf[SSBdf$year>2040 & SSBdf$variable == 'CAN' &
                       SSBdf$climate == '04' & SSBdf$HCR == HCRtest,]$value)
p <- median(SSBdf[SSBdf$year>2040 & SSBdf$variable == 'USA' & SSBdf$climate == '0'& SSBdf$HCR == HCRtest,]$value)/
  median(SSBdf[SSBdf$year>2040 & SSBdf$variable == 'USA' & SSBdf$climate == '04' & SSBdf$HCR == HCRtest,]$value)
p


HCRtest <- 'HCR0'
ytest <- 2040

p <- (1-(median(AAVdfexp[AAVdfexp$year>ytest &AAVdfexp$variable == 'CAN' &
                   AAVdfexp$climate == '0' &AAVdfexp$HCR == HCRtest,]$AAV)/
  median(AAVdfexp[AAVdfexp$year>ytest & AAVdfexp$variable == 'CAN' &
                AAVdfexp$climate == '04' & AAVdfexp$HCR == HCRtest,]$AAV)))*100
print(p)

p <- (1-median(AAVdfexp[AAVdfexp$year>ytest & AAVdfexp$variable == 'USA' &AAVdfexp$climate == '0'& AAVdfexp$HCR == HCRtest,]$AAV)/
  median(AAVdfexp[AAVdfexp$year>ytest & AAVdfexp$variable == 'USA' & AAVdfexp$climate == '04' & AAVdfexp$HCR == HCRtest,]$AAV))*100
p

# Biggest differerence
ccc <- median(SSB.tot$SSBmean[catch.tot$year>2040 & catch.tot$HCR == 'AC' & catch.tot$climate =='02'])
cc1 <- median(SSB.tot$SSBmean[catch.tot$year>2040 & catch.tot$HCR == 'MD' & catch.tot$climate =='02'])
pp <- median(SSB.tot$SSBmean[SSB.tot$year>2040 & SSB.tot$HCR == 'HCR0' & catch.tot$climate =='02'])


(cc1/pp-1)*100

cc <- median(catch.tot[catch.tot$year>2040 & catch.tot$HCR == 'AC' & catch.tot$climate =='0',]$catchmean)
cc <- median(catch.tot[catch.tot$year>2040 & catch.tot$HCR == 'MD' & catch.tot$climate =='0',]$catchmean)
cc2 <- median(catch.tot[catch.tot$year>2040 & catch.tot$HCR == 'HCR0' & catch.tot$climate =='04',]$catchmean)


(cc/cc2-1)*100




# Presentation plots

cplot <- ggplot(catch.tot[catch.tot$year>pyear,], aes(x = year, y = catchmean*1e-6, color = climate))+
  geom_line(size = lsize)+theme_bw()+facet_wrap(~HCR, ncol = 3)+
  theme(legend.position = 'top',
        text = element_text(size = 12),
        legend.title = element_blank(),
        strip.text = element_text(size=10),
        axis.text.x = element_text(size = 10, angle = 90),
        plot.margin = unit(c(1,1,0,1), 'pt'),
        legend.margin = margin(c(0,0,0,0)))+
  scale_color_manual(values = cols, labels = c('no change', 'medium', 'high'))+
  geom_line(aes(y = quants5*1e-6), linetype = 2, size = usize)+
  geom_line(aes(y = quants95*1e-6), linetype = 2, size = usize)+scale_y_continuous( 'Catch')+
  scale_x_continuous('')+
  geom_line(data = catch.tot[catch.tot$year< 2019 & catch.tot$year> pyear,] ,aes(x = year, y= catchmean*1e-6), color = 'black', size = lsize)


ssbplot <- ggplot(SSB.tot[SSB.tot$year>pyear,], aes(x = year, y = SSBmean*1e-6, color = climate))+geom_line(size = lsize)+theme_bw()+
  theme(legend.position = 'none',
        text = element_text(size = 12),
        strip.text = element_text(size=10),
        axis.text.x = element_text(size = 10, angle = 90),
        plot.margin = unit(c(1,1,0,1), 'pt'))+
  geom_line(aes(y = quants5*1e-6), linetype = 2, size = usize)+scale_color_manual(values = cols)+
  scale_x_continuous('')+
  geom_line(aes(y = quants95*1e-6), linetype = 2, size = usize)+facet_wrap(~HCR, ncol = 3)+scale_y_continuous('SSB')+
  geom_line(data = SSB.tot[SSB.tot$year< 2019 & SSB.tot$year>pyear,] ,aes(x = year, y= SSBmean*1e-6), color = 'black', size = lsize)


png(paste('results/Climate/','objectives_presentation.png', sep = ''), width = 16, height =20, res = 400, unit = 'cm')
cplot/ssbplot
dev.off()


vplot2 <- ggplot(SSBdf[SSBdf$year>2019,], aes(x = HCR,y = value*1e-6, group = MP, fill = climate))+
  geom_violin(position = dodge)+scale_y_continuous(name = 'SSB \n(million tonnes)', limit = rmout)+
  geom_line()+theme_bw()+
  theme(legend.position = 'none',
        text = element_text(size = 12),
        plot.margin = unit(c(1,1,0,1), 'pt'))+scale_x_discrete('')+
  geom_boxplot(width=0.2, col = 'black', outlier.shape = NA, position = dodge)+scale_fill_manual(values= cols)+
  facet_wrap(~variable)

png(paste('results/Climate/','SSB_presentation.png', sep = ''), width = 16, height =10, res = 400, unit = 'cm')
vplot2
dev.off()



# Group catches by decade

catchdfexp$decade <- 0
catchdfexp$decade[catchdfexp$year >2019 & catchdfexp$year<2030] <- 2020
catchdfexp$decade[catchdfexp$year >2029 & catchdfexp$year<2040] <- 2030
catchdfexp$decade[catchdfexp$year >2039 & catchdfexp$year<2050] <- 2040

# Remove outliers

ctmp <- catchdfexp

# ctmp <- ctmp %>%
#   group_by(run,year,HCR,climate,decade) %>%
#   summarise(value = median(value))
# quants <- quantile(ctmp$value, probs = c(0.01,0.99))
# ctmp$value[ctmp$value < quants[1]] <- NA
# ctmp$value[ctmp$value > quants[2]] <- NA

pdec.c <- ggplot(ctmp[ctmp$decade != 0,], aes(x = as.factor(decade), y= value*1e-6, color = climate, fill = climate))+
  facet_wrap(~HCR)+coord_cartesian(ylim = c(0,.6))+
  theme_classic()+geom_line()+
  geom_violin(position = dodge, show.legend = FALSE)+
  geom_boxplot(color = 'black', position = dodge, width = 0.2, show.legend = FALSE, outlier.alpha = 0)+
  scale_fill_manual(values = cols)+
  scale_color_manual(values = cols, labels = c('no change', 'medium', 'high'))+
  scale_x_discrete(name = '',breaks = c(2020,2030,2040), labels = c("","",""))+
  scale_y_continuous('catch\n(million tonnes)') +
  theme(legend.position = 'top',
        text = element_text(size = 8),
        legend.title = element_blank(),
        legend.text = element_text(size =10),
        strip.text = element_text(size=5),
        legend.key.size = unit(0.5, 'lines'),
        axis.text.x = element_blank(),
        plot.margin = unit(c(1,1,0,1), 'pt'),
        legend.margin = margin(c(0,0,0,0))
  )#+

pdec.c


ssbtmp <- SSB_cdf
ssbtmp$decade <- 0
ssbtmp$decade[ssbtmp$year >2019 & ssbtmp$year<2030] <- 2020
ssbtmp$decade[ssbtmp$year >2029 & ssbtmp$year<2040] <- 2030
ssbtmp$decade[ssbtmp$year >2039 & ssbtmp$year<2050] <- 2040

# ssbtmp <- ssbtmp %>%
#   group_by(run,year,HCR,climate,decade) %>%
#   summarise(value = median(value))



pdec.ssb <- ggplot(ssbtmp[ssbtmp$decade != 0,],
                 aes(x = as.factor(decade), y= value*1e-6, color = climate, fill = climate))+
  facet_wrap(~HCR)+coord_cartesian(ylim = c(0,1.6))+
  theme_classic()+
  geom_violin(position = dodge)+
  geom_boxplot(color = 'black', position = dodge, width = 0.2, show.legend = FALSE, outlier.alpha = 0)+
  scale_fill_manual(values = cols)+
  scale_color_manual(values = cols)+
  scale_x_discrete(name = '',breaks = c(2020,2030,2040), labels = c("","",""))+
  scale_y_continuous('spawning biomass\n(million tonnes)')+
  theme(legend.position = 'none') +
  theme(text = element_text(size = 8),
        axis.text.x = element_text(size =6),
        plot.margin = unit(c(1,1,0,1), 'pt'),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray")
  )
pdec.ssb


aavtmp <- AAVcdf
aavtmp$decade <- 0
aavtmp$decade[aavtmp$year >2019 & aavtmp$year<2030] <- 2020
aavtmp$decade[aavtmp$year >2029 & aavtmp$year<2040] <- 2030
aavtmp$decade[aavtmp$year >2039 & aavtmp$year<2050] <- 2040

#
 # aavtmp <- aavtmp %>%
 #   group_by(run,year,HCR,climate,decade) %>%
 #   summarise(AAV = median(AAV))



pdec.aav <- ggplot(aavtmp[aavtmp$decade != 0,],
                   aes(x = as.factor(decade), y= AAV, color = climate, fill = climate))+
  facet_wrap(~HCR)+coord_cartesian(ylim = c(0,2))+
  theme_classic()+
  geom_violin(position = dodge)+
  geom_boxplot(color = 'black', position = dodge, width = 0.2, show.legend = FALSE, outlier.alpha = 0)+
  scale_fill_manual(values = cols)+
  scale_color_manual(values = cols)+
  scale_x_discrete(name = '',breaks = c(2020,2030,2040), labels = c("2020s","2030s","2040s"))+
  scale_y_continuous('AAV')+
  theme(legend.position = 'none',
        text = element_text(size = 8),
        axis.text.x = element_text(size =6),
        plot.margin = unit(c(1,1,0,1), 'pt'),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major.y = element_line(colour = "gray"))
pdec.aav


png(paste('results/Climate/','objectives_decade.png', sep = ''), width = 16, height =12, res = 400, unit = 'cm')
pdec.c/pdec.ssb/pdec.aav
dev.off()



# Plot the performance of the operating model

EE <- data.frame(EE.ssb = (ssb.om$value - SSB_cdf$value)/SSB_cdf$value, year = as.numeric(rownames(ls.save[[1]]$SSB)),
                 run = ssb.om$run, MP = ssb.om$MP, HCR = ssb.om$HCR,
                 climate = ssb.om$climate)

ggplot(EE[EE$HCR == 'HCR0' & EE$year > 2010,], aes(x = as.factor(year), y = EE.ssb))+
  geom_boxplot(outlier.alpha = 0.1)+facet_wrap(~climate)+coord_cartesian(ylim = c(-2,2))+theme_classic()+
  scale_x_discrete(breaks = seq(1970,2050,by = 10), labels= seq(1970,2050,by = 10))+scale_y_continuous('relative\nerror')


# summarize data
EE.tot <- EE %>%
  group_by(year, MP, HCR, climate) %>%
  summarise(Emedian = median(EE.ssb),
            Emin = quantile(EE.ssb, probs = 0.05),
            Emax = quantile(EE.ssb, probs = 0.95))

p.ee <- ggplot(EE.tot[EE.tot$year > 2018 & EE.tot$HCR == 'HCR0',], aes(x = year, y = Emedian, color = climate, fill = climate, linetype = climate))+
  geom_line(size = 1.2)+theme_classic()+
  scale_y_continuous('relative\nerror')+
  geom_ribbon(aes(ymin = Emin, ymax = Emax), alpha = 0.2, linetype = 0,show.legend = FALSE)+coord_cartesian(ylim = c(-0.8,0.8))+
  scale_color_manual(values = cols, labels = c('no change', 'medium', 'high'))+
  scale_fill_manual(values = cols)+theme(legend.position = c(0.12,0.85))+guides(linetype = FALSE)+
  geom_hline(aes(yintercept = 0), linetype = 2, color = 'black')#+facet_wrap(~HCR)


p.ee

p.ee2 <- ggplot(EE.tot[EE.tot$year > 2018 & EE.tot$HCR == 'HCR0' ,], aes(x = year, y = Emedian))+
  geom_line(size = 1.2)+theme_classic()+
  scale_y_continuous('relative\nerror')+
  geom_ribbon(aes(ymin = Emin, ymax = Emax), alpha = 0.2, linetype = 0,show.legend = FALSE)+coord_cartesian(ylim = c(-0.8,0.8))+
  scale_color_manual(values = cols, labels = c('no change', 'medium', 'high'))+
  scale_fill_manual(values = cols)+theme(legend.position = c(0.12,0.85))+guides(linetype = FALSE)+
  geom_hline(aes(yintercept = 0), linetype = 2, color = 'black')+
  facet_wrap(~climate)+
  geom_line(data = EE[EE$HCR == 'HCR0' & EE$year > 2018,], aes(x= year, y = EE.ssb, group = run),alpha = 0.05, color ='black')

p.ee2




ggplot(EE[EE$MP == 'climate_0_TAC1',], aes(x= year, y = EE.ssb, group = run))+geom_line(size = 0.2, alpha = 0.1)+coord_cartesian(ylim = c(-2,2))+
  theme_classic()


