# Plot the climate MSE climate results
library(TMB)
library(r4ss)
library(devtools)
library(PacifichakeMSE)
library(purrr)
library(dplyr)
library(reshape2)
library(patchwork)
source('R/AAV.R')
load_all()

mod <- SS_output('inst/extdata/SS32018', printstats=FALSE, verbose = FALSE) # Read the true selectivity

# Set the seed
seedz <- 12345
set.seed(seedz)

df <- load_data_seasons(nseason = 4, nspace = 2, bfuture = 0.5, myear = 2018) # Prepare data for operating model

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



#
# folder <- 'C:/Users/Nis/Dropbox/NOAA/Hake MSE/MSE results/final results/'
#
folder <- 'C:/Users/nsja/Dropbox/NOAA/Hake MSE/MSE results/final results/'
# #


files <- dir(folder)[grep(dir(folder),pattern = '.Rdata')]

# Recreate the violin pltos

yrs <- 1966:2047

perc <- c(0.2,0.8) # Percentiles for plotting


for(i in 1:length(files)){
  load(paste(folder,files[i], sep = '')) # Prints as 'ls.save'
  df.MSE <- flatten(ls.save) # Change the list a little bit

  nruns <- length(ls.save)


   # ProcessMSE is in the df_MSE function
  catchcdf <- processMSE(df.MSE, 'Catch', idx = c(2,3), spacenames = c('CAN', 'USA'), runs = nruns,nspace = 2) # df_MSE.R calc
  catchdf <- processMSE(df.MSE, 'Catch', idx = 2, spacenames = c('value'), runs = nruns, nspace = 2)

  SSB_mid <- processMSE(df.MSE, id = 'SSB.mid', idx = c(1,2), spacenames = c('CAN', 'USA'), runs = nruns,nspace = 2)
  SSB_tot <- processMSE(df.MSE, id = 'SSB', idx = 1, spacenames = c('value'), runs = nruns,nspace = 2)

  SSB.EM <- processMSE(df.MSE, id = 'SSB.hes', idx = 1, spacenames = c('value'), runs = nruns,nspace = 2,
                      fn = 'rows')

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
  SSB.EM$MP <- runname


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
  SSB.EM$HCR <- HCRname

  AAVdf$HCR <- HCRname
  AAV.tot.tmp$HCR <- HCRname
  AAVtottmp$HCR <- HCRname

  catchdf$climate <- strs[6]
  catchcdf$climate <- strs[6]
  catch.tot.tmp$climate <- strs[6]

  SSB_mid$climate <- strs[6]
  SSB.tot.tmp$climate <- strs[6]
  SSB_tot$climate <- strs[6]
  SSB.EM$climate <- strs[6]

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
    ssb.EM <- SSB.EM

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
    ssb.EM <- rbind(ssb.EM,SSB.EM)

    AAVdfexp <- rbind(AAVdfexp, AAVdf)
    AAV.tot <- rbind(AAV.tot, AAV.tot.tmp)
    AAVcdf <- rbind(AAVcdf, AAVtottmp)

  }



  }

# Calculate risk
risk <- SSB_cdf %>% mutate(rel = value/sum(sim.data$SSB_0))

risk.year <- risk %>% group_by(year, MP, HCR, climate) %>%
  summarise(riskmed = median(rel),
            quants95 = quantile(rel, probs =0.99),
            quants5 = quantile(rel, probs= 0.01)
            )



risk.sum <- risk[risk$year>2018,]
risk.sum$closed <- 1
risk.sum$closed[which(risk.sum$rel < 0.1)] <- 0

risk.sum$decline <- 1
risk.sum$decline[which(risk.sum$rel < 0.4)] <- 0


# Risk per metric long term
risk.time <- risk.sum %>%
  group_by(MP, HCR, climate, year) %>%
  summarise(risk = length(which(rel< 0.1))/length(which(rel > 0.1)))
            # ymin = quantile(length(closed[closed ==1])/n(), probs = perc[1]),
            # ymax = quantile(length(closed[closed ==1])/n(), probs = perc[2]))


cols <- PNWColors::pnw_palette('Starfish',n = length(unique(risk.time$HCR)), type = 'discrete')
cols <- RColorBrewer::brewer.pal(n = length(unique(risk.time$HCR)), name = 'Accent')


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
  scale_color_nejm(labels = c('baseline', 'moderate', 'high'))+
  geom_line(aes(y = quants5*1e-6), linetype = 2, size = usize)+
  geom_line(aes(y = quants95*1e-6), linetype = 2, size = usize)+
  scale_y_continuous('Catch/\n(million tonnes)')+
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

riskplot <- ggplot(risk.year[risk.year$year>pyear,], aes(x = year, y = riskmed, color = climate))+geom_line(size = lsize)+theme_bw()+
  theme(legend.position = 'none',
        text = element_text(size = 8),
        strip.text = element_text(size=5),
        axis.text.x = element_text(size = 5, angle = 90),
        plot.margin = unit(c(1,1,0,1), 'pt'))+
  geom_line(aes(y = quants5), linetype = 2, size = usize)+
  geom_line(aes(y = quants95), linetype = 2, size = usize)+
  scale_color_manual(values = cols)+coord_cartesian(ylim = c(0,0.7))+
  geom_hline(aes(yintercept = .1), linetype = 2, color = 'black')+
  scale_x_continuous('')+facet_wrap(~HCR, ncol = 3)+scale_y_continuous('SSB/\nSSB0')+
  geom_line(data = risk.year[risk.year$year< 2019 & risk.year$year>pyear,] ,aes(x = year, y= riskmed), color = 'black', size = lsize)

riskplot



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

AAVplot
# Risk plots

riskrun <- risk[risk$year>2020,] %>%
  group_by(run, MP, HCR, climate) %>%
  summarise(nyears = n(),
            nover = sum(rel>0.1),
            frac = nover/nyears)
#
# riskmean <- riskrun %>%
#   group_by(MP, HCR, climate, year) %>%
#   summarise(riskmed = median(frac),
#             quants95 = quantile(frac, probs =0.95),
#             quants5 = quantile(frac, probs= 0.05)
#   )


riskviolin <- ggplot(riskrun,aes(x = HCR, y= 1-frac, color = HCR,fill = HCR))+
    geom_violin(fill = NA)+
    facet_wrap(~climate)+
    theme_bw()+geom_hline(aes(yintercept = 0.05), linetype = 2)+coord_cartesian(ylim = c(0,0.2))+
    scale_color_manual(values = cols)+scale_fill_manual(values = cols)+scale_x_discrete("")#+

riskviolin

# riskplot <-  ggplot(riskmean,aes(x = year, y= 1-riskmed, color = climate,fill = climate))+
#   geom_line()+geom_ribbon(aes(ymin = quants5, ymax = quants95))+
#   theme_bw()+geom_hline(aes(yintercept = 0.05), linetype = 2)+coord_cartesian(ylim = c(0,0.2))+
#   scale_color_manual(values = cols)+scale_fill_manual(values = cols)+scale_x_discrete("")
#
# riskplot


#risk.time$climate <- levels()

risklines <- ggplot(risk.time, aes(x = year, y = risk, color = climate, linetype = climate))+
  geom_line(size = 1.2)+
  facet_wrap(~HCR)+
  theme_classic()+geom_hline(aes(yintercept = 0.05), linetype = 2)+
  scale_color_npg(labels = c("baseline", "moderate", "high"))+
  scale_linetype_manual(values = c(1,2,3), labels = c("baseline", "moderate", "high"))+
  coord_cartesian(ylim = c(0,0.2)) + scale_y_continuous('risk \n of fisheries closure')+
  theme(strip.background =element_rect(fill="white"))+theme(legend.position = 'top',
                                                            legend.title = element_blank(),
                                                            legend.background = element_blank())


risklines

png('results/Climate/Publication/Resubmission/Figure6.png', width = 16, height =8, res = 400, unit = 'cm')
risklines
dev.off()

# Pdf for publication
pdf('results/Climate/Publication/Resubmission/Figure6.pdf', width = 16/cm(1), height =8/cm(1))
risklines
dev.off()



png('results/Climate/objectives_publication.png', width = 16, height =14, res = 400, unit = 'cm')

cplot/riskplot/AAVplot

dev.off()

dodge <- position_dodge(width = 0.5)
perc <- c(0.1,0.90)
tag.x <- .04

rmout <- quantile(catchcdfexp$value[catchdfexp$year>2019]*1e-6, probs = perc)

vplot1 <- ggplot(catchcdfexp[catchcdfexp$year>2030,], aes(x = HCR,y = value*1e-6, group = MP, fill = climate))+
  geom_violin(position = dodge)+scale_y_continuous(name = 'C \n(million tonnes)')+geom_line()+
  scale_x_discrete('')+
  geom_boxplot(width=0.2, col = 'black', outlier.shape = NA, position = dodge,
               show.legend = FALSE)+
  scale_fill_npg(labels = c('baseline', 'moderate', 'high'))+
  guides(shape = guide_legend(override.aes = list(shape = 2)))+
  facet_wrap(~variable)+coord_cartesian(ylim = c(0,0.5))+
  theme_classic()+theme(legend.position = 'top',
                   strip.background =element_rect(fill="white"),
                   text = element_text(size = 10),
                   legend.title = element_blank(),
                   strip.text = element_text(size=5),
                   legend.key.size = unit(0.5, 'lines'),
                   axis.text.x = element_blank(),
                   plot.margin = unit(c(1,1,0,1), 'pt'),
                   legend.margin = margin(c(0,0,0,0)),
                   plot.tag.position = c(tag.x, 0.90),
                   plot.tag = element_text(size = 8)
                   )+
  labs(tag = "(A)")

rmout <- quantile(SSBdf$value[SSBdf$year>2019]*1e-6, probs = perc)

vplot2 <- ggplot(SSBdf[SSBdf$year>2030,], aes(x = HCR,y = value*1e-6, group = MP, fill = climate))+
  geom_violin(position = dodge)+scale_y_continuous(name = 'S\n(million tonnes)')+geom_line()+
  theme_classic()+scale_x_discrete('')+
  geom_boxplot(width=0.2, col = 'black', outlier.shape = NA, position = dodge)+
  scale_fill_npg()+facet_wrap(~variable)+coord_cartesian(ylim = c(0,2))+
  theme(legend.position = 'none',
        text = element_text(size = 10),
        strip.background =element_rect(fill="white"),
        strip.text.x = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(1,1,0,1), 'pt'),
        plot.tag.position = c(tag.x, 1.03),
        plot.tag = element_text(size = 8)
        )+
  labs(tag = "(B)")
# Remove stupid outliers
rmout <- quantile(AAVdfexp$AAV[AAVdfexp$year>2019], probs = perc)
rmout <- c(0,4)

vplot3 <- ggplot(AAVdfexp[AAVdfexp$year>2030,], aes(x = HCR,y = AAV, group = MP, fill = climate))+
  geom_violin(position = dodge)+scale_y_continuous(name = 'AAV')+geom_line()+theme_classic()+
  geom_boxplot(width=0.2, col = 'black', outlier.shape = NA, position = dodge)+
  scale_fill_npg()+
  facet_wrap(~variable, nrow =1)+coord_cartesian(ylim = c(0,3))+
  theme(legend.position = 'none',
        text = element_text(size = 10),
        axis.text.x = element_text(size =8),
        plot.margin = unit(c(1,1,0,1), 'pt'),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        plot.tag.position = c(tag.x, 1.03),
        plot.tag = element_text(size = 8)
        )+
  labs(tag = "(C)")
# Test differe
AAVtest <- AAVdfexp[AAVdfexp$year > 2030,] %>%
  group_by(MP,HCR,climate, variable) %>%
  summarise(mAAv = median(AAV))


# Pdf for publication
pdf('results/Climate/Publication/Resubmission/Figure7.pdf', width = 16/cm(1), height =12/cm(1))
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


# Relative

# Relative SSB
median(SSB_cdf$value[SSB_cdf$year>2040 & SSB_cdf$HCR == 'HCR0' & SSB.tot$climate == '0']/sum(sim.data$SSB0))# Country catch
sd(SSB_cdf$value[SSB_cdf$year>2040 & SSB_cdf$HCR == 'HCR0' & SSB.tot$climate == '0']/sum(sim.data$SSB0))# Country catch
median(SSB_cdf$value[SSB_cdf$year>2040 & SSB_cdf$HCR == 'HCR0' & SSB.tot$climate == '0'])# Country catch



HCRtest <- 'HCR0'

p <- median(catchcdfexp[catchcdfexp$year>2040 & catchcdfexp$variable == 'CAN' &
                          catchcdfexp$climate == '0' & catchcdfexp$HCR == HCRtest,]$value)/
  median(catchcdfexp[catchcdfexp$year>2040 & catchcdfexp$variable == 'CAN' &
                       catchcdfexp$climate == '04' & catchcdfexp$HCR == HCRtest,]$value)
p

p <- median(catchcdfexp[catchcdfexp$year>2040 & catchcdfexp$variable == 'USA' & catchcdfexp$climate == '0'& catchcdfexp$HCR == HCRtest,]$value)/
  median(catchcdfexp[catchcdfexp$year>2040 & catchcdfexp$variable == 'USA' & catchcdfexp$climate == '04' & catchcdfexp$HCR == HCRtest,]$value)
p

x <- median(SSB_cdf$value[SSB_cdf$year>2040 & SSB_cdf$climate == '0' & SSB_cdf$HCR == HCRtest])/
  median(SSB_cdf$value[SSB_cdf$year>2040 & SSB_cdf$climate == '04' & SSB_cdf$HCR == HCRtest])
x

HCRtest <- 'HCR0'

p <- median(SSBdf[SSBdf$year>2040 & SSBdf$variable == 'CAN' &
                          SSBdf$climate == '0' & SSBdf$HCR == HCRtest,]$value)/
  median(SSBdf[SSBdf$year>2040 & SSBdf$variable == 'CAN' &
                       SSBdf$climate == '04' & SSBdf$HCR == HCRtest,]$value)
p <- median(SSBdf[SSBdf$year>2040 & SSBdf$variable == 'USA' & SSBdf$climate == '0'& SSBdf$HCR == HCRtest,]$value)/
  median(SSBdf[SSBdf$year>2040 & SSBdf$variable == 'USA' & SSBdf$climate == '04' & SSBdf$HCR == HCRtest,]$value)
p


p <- median(catchcdf[catchcdfexp$year>2040 & catchcdfexp$variable == 'USA' & catchcdfexp$climate == '0'& catchcdfexp$HCR == HCRtest,]$value)/
  median(catchcdfexp[catchcdfexp$year>2040 & catchcdfexp$variable == 'USA' & catchcdfexp$climate == '04' & catchcdfexp$HCR == HCRtest,]$value)
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
  scale_color_manual(values = cols, labels = c('baseline', 'moderate', 'high'))+
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
  scale_fill_npg()+
  scale_color_npg(labels = c('baseline', 'moderate', 'high'))+
  scale_x_discrete(name = '',breaks = c(2020,2030,2040), labels = c("","",""))+
  scale_y_continuous('C\n(million tonnes)') +
  theme(legend.position = 'top',
        strip.background =element_rect(fill="white"),
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
                 aes(x = as.factor(decade), y= value/sum(sim.data$SSB_0), color = climate, fill = climate))+
  facet_wrap(~HCR)+coord_cartesian(ylim = c(0,1))+
  theme_classic()+
  geom_violin(position = dodge)+
  geom_boxplot(color = 'black', position = dodge, width = 0.2, show.legend = FALSE, outlier.alpha = 0)+
  scale_fill_npg()+
  scale_color_npg()+
  scale_x_discrete(name = '',breaks = c(2020,2030,2040), labels = c("","",""))+
  scale_y_continuous(expression(paste('S/','S'['0'])))+
  theme(legend.position = 'none') +
  theme(text = element_text(size = 8),
        axis.text.x = element_text(size =6),
        plot.margin = unit(c(1,1,0,1), 'pt'),
        strip.background = element_blank(),
        strip.text.x = element_blank()
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
  scale_fill_npg()+
  scale_color_npg()+
  scale_x_discrete(name = '',breaks = c(2020,2030,2040), labels = c("2020s","2030s","2040s"))+
  scale_y_continuous('AAV')+
  theme(legend.position = 'none',
        text = element_text(size = 8),
        axis.text.x = element_text(size =6),
        plot.margin = unit(c(1,1,0,1), 'pt'),
        strip.background = element_blank(),
        strip.text.x = element_blank())
pdec.aav


png(paste('results/Climate/Publication/Resubmission/Supplementary/','objectives_decade.png', sep = ''), width = 16, height =12, res = 400, unit = 'cm')
pdec.c/pdec.ssb/pdec.aav+plot_annotation(tag_levels = 'a')
dev.off()


pdf(paste('results/Climate/Publication/Resubmission/Supplementary/','Figure9.pdf', sep = ''), width = 16/cm(1), height =12/cm(1))
pdec.c/pdec.ssb/pdec.aav+plot_annotation(tag_levels = 'a')
dev.off()

# Plot the performance of the operating model

EE <- data.frame(EE.ssb = (ssb.EM$value - SSB_cdf$value)/SSB_cdf$value, year = as.numeric(rownames(ls.save[[1]]$SSB)),
                 run = ssb.EM$run, MP = ssb.EM$MP, HCR = ssb.EM$HCR,
                 climate = ssb.EM$climate)

ggplot(EE[EE$HCR == 'HCR0' & EE$year > 2010,], aes(x = as.factor(year), y = EE.ssb))+
  geom_boxplot(outlier.alpha = 0.1)+facet_wrap(~climate)+coord_cartesian(ylim = c(-2,2))+theme_classic()+
  scale_x_discrete(breaks = seq(1970,2050,by = 10), labels= seq(1970,2050,by = 10))+scale_y_continuous('relative\nerror')


# summarize data
EE.tot <- EE %>%
  group_by(year, MP, HCR, climate) %>%
  summarise(Emedian = mean(EE.ssb),
            Emin = quantile(EE.ssb, probs = 0.05),
            Emax = quantile(EE.ssb, probs = 0.95))

p.ee <- ggplot(EE.tot[EE.tot$year > 2018 & EE.tot$HCR == 'HCR0',], aes(x = year, y = Emedian, color = climate, fill = climate, linetype = climate))+
  geom_line(size = 1.2)+theme_classic()+
  scale_y_continuous('relative\nerror')+
  geom_ribbon(aes(ymin = Emin, ymax = Emax), alpha = 0.2, 
              linetype = 0,show.legend = FALSE)+
  coord_cartesian(ylim = c(-0.8,0.8))+
  scale_color_npg(labels = c('baseline', 'moderate', 'high'))+
  scale_fill_npg()+
  scale_linetype_manual(values = c(1,2,3), labels =  c('baseline', 'moderate', 'high'))+
  geom_hline(aes(yintercept = 0), linetype = 2, color = 'black')+
    theme(legend.position = c(0.45,0.94), legend.direction = 'horizontal', legend.title = element_blank(),
          legend.text = element_text(size = 8))

p.ee


windows(width = 8/cm(1), height = 8/cm(1))
p.ee

dev.off()

png('results/Climate/Publication/Resubmission/Figure5.png', width = 8, height = 6, res = 400, units = 'cm')
p.ee
dev.off()

pdf('results/Climate/Publication/Resubmission/Figure5.pdf', width = 8/cm(1), height = 8/cm(1))
p.ee
dev.off()



# print uncertainty
median(EE.tot[EE.tot$HCR == 'HCR0' & EE.tot$climate == '0',]$Emin)
median(EE.tot[EE.tot$HCR == 'HCR0' & EE.tot$climate == '0',]$Emax)

p.ee2 <- ggplot(EE.tot[EE.tot$year > 2018 & EE.tot$HCR == 'HCR0' ,], aes(x = year, y = Emedian))+
  geom_line(size = 1.2)+theme_classic()+
  scale_y_continuous('relative\nerror')+
  geom_ribbon(aes(ymin = Emin, ymax = Emax), alpha = 0.2, linetype = 0,show.legend = FALSE)+coord_cartesian(ylim = c(-0.8,0.8))+
  scale_color_manual(values = cols, labels = c('baseline', 'moderate', 'high'))+
  scale_fill_manual(values = cols)+theme(legend.position = c(0.12,0.85))+guides(linetype = FALSE)+
  geom_hline(aes(yintercept = 0), linetype = 2, color = 'black')+
  facet_wrap(~climate)+
  geom_line(data = EE[EE$HCR == 'HCR0' & EE$year > 2018,], aes(x= year, y = EE.ssb, group = run),alpha = 0.05, color ='black')

p.ee2

png('results/climate_alternative.png', width = 8, height = 6, res = 400, units = 'cm')
p.ee2
dev.off()


ggplot(EE[EE$MP == 'climate_0_TAC1',], aes(x= year, y = EE.ssb, group = run))+geom_line(size = 0.2, alpha = 0.1)+coord_cartesian(ylim = c(-2,2))+
  theme_classic()

median(EE$EE.ssb[EE$year>2018 & EE$climate == '0' & EE$HCR == 'HCR0'])*100
median(EE$EE.ssb[EE$year>2018 & EE$climate == '02' & EE$HCR == 'HCR0'])*100
median(EE$EE.ssb[EE$year>2018 & EE$climate == '04' & EE$HCR == 'HCR0'])*100


# For the mins and maxes
quantile(EE$EE.ssb[EE$year>2018 & EE$climate == '0' & EE$HCR == 'HCR0'], probs = 0.05)*100
quantile(EE$EE.ssb[EE$year>2018 & EE$climate == '0' & EE$HCR == 'HCR0'], probs = 0.95)*100

quantile(EE$EE.ssb[EE$year>2018 & EE$climate == '02' & EE$HCR == 'HCR0'], probs = 0.05)*100
quantile(EE$EE.ssb[EE$year>2018 & EE$climate == '02' & EE$HCR == 'HCR0'], probs = 0.95)*100

quantile(EE$EE.ssb[EE$year>2018 & EE$climate == '04' & EE$HCR == 'HCR0'], probs = 0.05)*100
quantile(EE$EE.ssb[EE$year>2018 & EE$climate == '04' & EE$HCR == 'HCR0'], probs = 0.95)*100


p.ee.all <- ggplot(EE.tot[EE.tot$year > 2018,], aes(x = year, y = Emedian, color = climate, fill = climate, linetype = climate))+
  geom_line(size = 1.2)+theme_classic()+
  scale_y_continuous('relative\nerror')+
  geom_ribbon(aes(ymin = Emin, ymax = Emax), alpha = 0.2, linetype = 0,show.legend = FALSE)+coord_cartesian(ylim = c(-0.8,0.8))+
  scale_color_npg(labels = c('baseline', 'moderate', 'high'))+
  scale_fill_npg()+
  scale_linetype_manual(values = c(1,2,3), labels = c('baseline', 'moderate', 'high'))+
  theme(legend.position = 'top', legend.direction = 'horizontal', legend.title = element_blank(),
        legend.text = element_text(size = 6))+
  facet_wrap(~HCR)+
  geom_hline(aes(yintercept = 0), linetype = 2, color = 'black')

png('results/Climate/publication/climate_alternative_all.png', width = 16, height = 12, res = 400, units = 'cm')
p.ee.all
dev.off()





# Plot the total catch as violin

ptest <- ggplot(catch.tot[catch.tot$year>2018 & catch.tot$climate == "04",],
       aes(x = HCR, y= catchmean, group = HCR, fill = HCR))+geom_violin()+
  geom_boxplot(width = .10)+theme_bw()+scale_y_continuous('median catch')



ptest2 <- ggplot(SSB.tot[SSB.tot$year>2018 & SSB.tot$climate == "04",],
       aes(x = HCR, y= SSBmean, group = HCR, fill = HCR))+geom_violin()+
  geom_boxplot(width = .10)+theme_bw()+scale_y_continuous('median SSB')+
  theme(legend.position = 'none')


ptest3 <- ggplot(AAV.tot[AAV.tot$year>2018 & AAV.tot$climate == "04",],
                 aes(x = HCR, y= AAVmean, group = HCR, fill = HCR))+geom_violin()+
  geom_boxplot(width = .10)+theme_bw()+scale_y_continuous('median AAV')+
  theme(legend.position = 'none')



png('results/Climate/summed_results_test.png', width = 16, height = 12, res = 400, units = 'cm')
ptest/ptest2/ptest3
dev.off()





